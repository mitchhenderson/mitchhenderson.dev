-- Names the R and Python logos used as tab labels, so the tabs are announced
-- as "R" and "Python" and not as unlabelled links. Runs at render time on the
-- frozen output, so it also covers posts frozen before their source named the
-- logos; no post code is re-executed.

local logos = {
  ["Rlogo.svg"] = "R",
  ["python-logo-only.svg"] = "Python",
}

local function file_name(path)
  return path:match("([^/]+)$") or path
end

function Image(image)
  local name = file_name(image.src)

  if logos[name] and #image.caption == 0 then
    image.caption = { pandoc.Str(logos[name]) }
    return image
  end

  return nil
end
