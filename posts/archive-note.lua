-- Adds a short note to the top of posts dated before 2024, so readers know
-- they are older tutorials. Runs at render time on the frozen output, so no
-- post code is re-executed.
local cutoff_year = 2024

function Pandoc(doc)
  if not doc.meta.date then
    return doc
  end

  local year = tonumber(pandoc.utils.stringify(doc.meta.date):match("%d%d%d%d"))
  if not year or year >= cutoff_year then
    return doc
  end

  local note = pandoc.Div(
    pandoc.Para(pandoc.Str(
      "Written in " .. year .. ". Kept for reference; it doesn't reflect how I work now."
    )),
    pandoc.Attr("", { "archive-note" })
  )
  table.insert(doc.blocks, 1, note)
  return doc
end
