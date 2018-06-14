require "css-selector"

function Doc(doc)
    local targets = parse(doc.meta.code or '*')
    local newBody = {}
    for _,blk in pairs(doc.blocks) do
        if blk.t == "CodeBlock" then
            if eval(targets, blk.attr[2]) then
                table.insert(newBody, pandoc.RawBlock("html", blk.text .. '\n\n'))
            end
        end
    end
    return pandoc.Pandoc(newBody, metadata)
end
