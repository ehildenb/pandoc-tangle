require "../css-selector"

function deepcompare(t1,t2,ignore_mt)
    local ty1 = type(t1)
    local ty2 = type(t2)
    if ty1 ~= ty2 then return false end
    -- non-table types can be directly compared
    if ty1 ~= 'table' and ty2 ~= 'table' then return t1 == t2 end
    -- as well as tables which have the metamethod __eq
    local mt = getmetatable(t1)
    if not ignore_mt and mt and mt.__eq then return t1 == t2 end
    for k1,v1 in pairs(t1) do
        local v2 = t2[k1]
        if v2 == nil or not deepcompare(v1,v2) then return false end
    end
    for k2,v2 in pairs(t2) do
        local v1 = t1[k2]
        if v1 == nil or not deepcompare(v1,v2) then return false end
    end
    return true
end

function table.val_to_str ( v )
    if "string" == type( v ) then
        v = string.gsub( v, "\n", "\\n" )
        if string.match( string.gsub(v,"[^'\"]",""), '^"+$' ) then
            return "'" .. v .. "'"
        end
        return '"' .. string.gsub(v,'"', '\\"' ) .. '"'
    else
        return "table" == type( v ) and table.tostring( v ) or tostring( v )
    end
end

function table.key_to_str ( k )
    if "string" == type( k ) and string.match( k, "^[_%a][_%a%d]*$" ) then
        return k
    else
        return "[" .. table.val_to_str( k ) .. "]"
    end
end

function table.tostring( tbl )
    local result, done = {}, {}
    for k, v in ipairs( tbl ) do
        table.insert( result, table.val_to_str( v ) )
        done[ k ] = true
    end
    for k, v in pairs( tbl ) do
        if not done[ k ] then
            table.insert( result,
            table.key_to_str( k ) .. "=" .. table.val_to_str( v ) )
        end
    end
    return "{" .. table.concat( result, "," ) .. "}"
end

--- Test triples
--- ------------

tests = {}

tests[1] = { input  = ".test .input"
           , tokens = { "." , "test" , " " , "." , "input" }
           , groups = { { id = "test" } , { id = "input" } }
           , ast    = { orExp = { { andExp = { { id = "test" } , { id = "input" } } } } }
           }

tests[2] = { input  = ".test , .input"
           , tokens = { "." , "test"  , " , " , "." , "input" }
           , groups = { { id = "test" } , OR , { id = "input" } }
           , ast    = { orExp = { { andExp = { { id = "input" } } } , { andExp = { { id = "test" } } } } }
           }

tests[3] = { input  = ".k.transferFrom-then-branch:not(.transferFrom-else-branch)"
           , tokens = { "." , "k" , "." , "transferFrom-then-branch" , ":not(" , "." , "transferFrom-else-branch" , ")" }
           , groups = { { id = "k" } , { id = "transferFrom-then-branch" } , { notExp = { { id = "transferFrom-else-branch" } } } }
           , ast    = { orExp = { { andExp = { { id = "k" } , { id = "transferFrom-then-branch" } , { notExp = { orExp = { { andExp = { { id = "transferFrom-else-branch" } } } } } } } } } }
           }

tests[4] = { input  = ".k .blah :not(.foo.bar)"
           , tokens = { "." , "k" , " " , "." , "blah" , " " , ":not(" , "." , "foo" , "." , "bar" , ")" }
           , groups = { { id = "k" } , { id = "blah" } , { notExp = { { id = "foo" } , { id = "bar" } } } }
           , ast    = { orExp = { { andExp = { { id = "k" } , { id = "blah" } , { notExp = { orExp = { { andExp = { { id = "foo" } , { id = "bar" } } } } } } } } } }
           }

tests[5] = { input  = ".k , .rvk , :not(.uiuck)"
           , tokens = { "." , "k" , " , " , "." , "rvk" , " , " , ":not(" , "." , "uiuck" , ")" }
           , groups = { { id = "k" } , OR , { id = "rvk" } , OR , { notExp = { { id = "uiuck" } } } }
           , ast    = { orExp = { { andExp = { { notExp = { orExp = { { andExp = { { id = "uiuck" } } } } } } } } , { andExp = { { id = "rvk" } } } , { andExp = { { id = "k" } } } } }
           }

--- Run Tests
--- ---------

function test_error(expected, actual)
    print("expected: " .. table.tostring(expected) .. "\nactual:   " .. table.tostring(actual))
    error("test failure!", 1)
end

for i,_ in pairs(tests) do
    print("Test: " .. i)

    print("testing tokenizer...")
    local tokenized = tokenize(tests[i]["input"])
    if not deepcompare(tokenized, tests[i]["tokens"]) then
        test_error(tests[i]["tokens"], tokenized)
    end

    print("testing token grouper...")
    local grouped = group_tokens(tests[i]["tokens"])
    if not deepcompare(grouped, tests[i]["groups"]) then
        test_error(tests[i]["groups"], grouped)
    end

    print("testing group parser...")
    local parsed = parse_groups(tests[i]["groups"])
    if not deepcompare(parsed, tests[i]["ast"]) then
        test_error(tests[i]["ast"], parsed)
    end
end
