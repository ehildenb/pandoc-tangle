-- This filter replaces CodeBlocks with class `include` and contents a path
-- with the contents of that path. Paths are absolute, or relative to the working
-- directory.

-- TODO: Allow included files to include other files.

function CodeBlock(block)
    local included = io.open(block.text, 'r')
    local content  = included:read("*a")
    local parsed   = pandoc.read(content)
    local content  = included:close()
    return parsed.blocks
end
