-- Marks the charts and tables in a post so the theme can let them run wider
-- than the text column. Prose stays at a comfortable reading width; a figure
-- gets the room it needs to be read. Runs on the frozen output, so no post
-- code is re-executed.
--
-- Quarto's own column-body-outset class is avoided on purpose: it reaches
-- into the contents column, and Quarto then collapses the contents list
-- every time a figure scrolls past it.

local function holds_figure_or_table(div)
  local found = false
  div:walk({
    Image = function()
      found = true
    end,
    RawBlock = function(raw)
      -- gt tables arrive as a block of raw HTML
      if raw.format == "html" and raw.text:find("gt_table", 1, true) then
        found = true
      end
    end,
  })
  return found
end

function Div(div)
  if div.classes:includes("cell-output-display") and holds_figure_or_table(div) then
    div.classes:insert("wide-output")
    return div
  end
  return nil
end
