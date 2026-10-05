-- Adds schema.org structured data to each page's <head>: the person on the
-- home and About pages, and an article record on posts. It reads metadata
-- only, so frozen posts are not re-executed.
local site_url = "https://www.mitchhenderson.dev"

local person = {
  ["@type"] = "Person",
  ["@id"] = site_url .. "/#person",
  name = "Mitch Henderson",
  url = site_url,
  image = site_url .. "/img/headshot-840.jpg",
  jobTitle = "Data scientist",
  description = "Data scientist with a background in professional sport.",
  sameAs = {
    "https://www.linkedin.com/in/mitchjameshenderson/",
    "https://github.com/mitchhenderson",
    "https://bsky.app/profile/mitchhenderson.bsky.social",
    "https://scholar.google.com/citations?user=AUWu1tMAAAAJ",
  },
}

local function text(value)
  if value == nil then
    return nil
  end
  local result = pandoc.utils.stringify(value)
  -- Titles and descriptions can carry a trailing newline from YAML blocks
  result = result:gsub("^%s+", ""):gsub("%s+$", "")
  if result == "" then
    return nil
  end
  return result
end

-- Path of the page from the site root, e.g. "posts/a-post/index.qmd"
local function relative_input()
  local input = quarto.doc.input_file
  local root = quarto.project.directory
  if not input or not root then
    return nil
  end
  if input:sub(1, #root) == root then
    input = input:sub(#root + 2)
  end
  return input
end

local function page_url(relative)
  local page = relative:gsub("%.qmd$", ".html")
  page = page:gsub("index%.html$", "")
  return site_url .. "/" .. page
end

local months = {
  January = 1, February = 2, March = 3, April = 4, May = 5, June = 6,
  July = 7, August = 8, September = 9, October = 10, November = 11, December = 12,
}

-- Quarto has already formatted the date for display ("January 4, 2026") by
-- the time filters run; structured data needs it back as 2026-01-04.
local function iso_date(value)
  local year, month, day = value:match("^(%d%d%d%d)-(%d%d)-(%d%d)")
  if year then
    return year .. "-" .. month .. "-" .. day
  end
  local name, d, y = value:match("^(%a+)%s+(%d+),%s*(%d%d%d%d)")
  if name and months[name] then
    return string.format("%s-%02d-%02d", y, months[name], tonumber(d))
  end
  return nil
end

local function absolute(image)
  if image:match("^https?://") then
    return image
  end
  return site_url .. "/" .. image:gsub("^/", "")
end

function Meta(meta)
  if not quarto.doc.is_format("html") then
    return nil
  end

  local relative = relative_input()
  if not relative then
    return nil
  end

  local data
  if relative == "index.qmd" then
    data = {
      ["@context"] = "https://schema.org",
      ["@graph"] = {
        {
          ["@type"] = "WebSite",
          ["@id"] = site_url .. "/#website",
          url = site_url,
          name = "Mitch Henderson",
          description = text(meta.description),
          inLanguage = "en-AU",
          publisher = { ["@id"] = person["@id"] },
        },
        person,
      },
    }
  elseif relative == "about.qmd" then
    data = {
      ["@context"] = "https://schema.org",
      ["@type"] = "ProfilePage",
      url = page_url(relative),
      mainEntity = person,
    }
  elseif relative:match("^posts/") and meta.date then
    data = {
      ["@context"] = "https://schema.org",
      ["@type"] = "BlogPosting",
      headline = text(meta.title),
      description = text(meta.description),
      datePublished = iso_date(text(meta.date)),
      url = page_url(relative),
      mainEntityOfPage = page_url(relative),
      inLanguage = "en-AU",
      author = { ["@id"] = person["@id"], ["@type"] = "Person", name = person.name, url = site_url },
    }
    local image = text(meta.image)
    if image then
      data.image = absolute(image)
    end
  end

  if data then
    quarto.doc.include_text(
      "in-header",
      '<script type="application/ld+json">' .. quarto.json.encode(data) .. "</script>"
    )
  end
  return nil
end
