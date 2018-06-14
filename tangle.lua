require "css-selector"

local blocks={}

--- Utilities
--- =========

local function split_classes(str)
    local classes = {}
    for class in string.gmatch(str, "[^ ]+") do
        table.insert(classes, class)
    end
    return classes
end

local function keep_only_lines(str)
    local count = 0
    for _ in string.gmatch(str,'\n') do
        count = count + 1
    end
    return string.rep('\n',count)
end

--- Document Printers
--- =================

function Doc(body, metadata, variables)
    local targets = parse(metadata.code or '*')
    local block = 1
    local function replace()
        local classes = blocks[block][1]
        local code    = blocks[block][2]
        block = block + 1
        if eval(targets, classes) then
            return code
        else
            return keep_only_lines(code)
        end
    end
    return string.gsub(body,'#',replace)
end

--- Blocks
--- ------

function Plain(s)
    return s..' '
end

function Para(s)
    return s..'\n'
end

--- LineBlock

function CodeBlock(s, attr)
    table.insert(blocks,{split_classes(attr.class),s})
    return '\n#\n\n'
end

--- RawBlock

--- BlockQuote

function OrderedList(items)
    return BulletList(items)
end

function BulletList(items)
    if #items == 0 then
        return ''
    else
        if items[#items]:byte(-1) ~= 10 then
            items[#items] = items[#items]..'\n'
        end
        return keep_only_lines(table.concat(items,'\n'))
    end
end

--- DefinitionList

function Header(lvl,text)
    text = keep_only_lines(text)
    if lvl == 1 or lvl == 2 then
        return text..'\n\n'
    else
        return text..'\n'
    end
end

--- HorizontalRule

function Table(s)
    return keep_only_lines(s)
end

--- Div

--- Null

--- Inlines
--- -------

local function InlineMarkup(s)
    return keep_only_lines(s)
end

function Str(s)
    return keep_only_lines(s)
end

Emph        = InlineMarkup
Strong      = InlineMarkup
Strikeout   = InlineMarkup
Superscript = InlineMarkup
Subscript   = InlineMarkup
SmallCaps   = InlineMarkup

--- Quoted

--- Cite

Code = InlineMarkup

function Space()
    return ''
end

function SoftBreak()
    return '\n'
end

function LineBreak()
    return '\n'
end

--- Math

--- RawInline

function Link(text,target,title,attr)
    return keep_only_lines(text)..keep_only_lines(target)
end

function Image()
    return ''
end

function Note()
    return ''
end

Span = InlineMarkup

--- Backwards compatibility
--- -----------------------

function DisplayMath(s)
    return keep_only_lines(s)
end

function Blocksep()
    return '\n'
end

local function IgnoreBlock(key)
    return function(s,a,b,c)
        return (type(s) == 'string' and keep_only_lines(s)) or ''
    end
end

InlineMath   = InlineMarkup
DiplayMath   = InlineMarkup
DoubleQuoted = InlineMarkup

--- Ignore Non-Existant Document Elements
--- =====================================

local meta = {}
meta.__index =
    function(_, key)
        io.stderr:write(string.format("WARNING: Undefined tangle function '%s'\n",key))
        _G[key] = IgnoreBlock(key)
        return _G[key]
    end
setmetatable(_G, meta)
