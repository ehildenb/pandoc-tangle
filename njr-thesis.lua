-- Only show code blocks with the njr-thesis class
-- TODO: Make this more generic and useful across the board

function table.contains(table, element)
  for _, value in pairs(table) do
    if value == element then
      return true
    end
  end
  return false
end

function CodeBlock(block)
    if table.contains(block.classes, 'njr-thesis') then
        return block
    end
    return pandoc.Null()
end
