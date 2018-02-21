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

--- Test triples
--- ------------
tests = {}

tests[1] = { input  = ".test .input"
           , tokens = {".", "test", " ", ".", "input"}
           , ast    = { andExp = { { idExp = "test" } , { idExp = "input" } } }
           }

tests[2] = { input  = "test input"
           , tokens = { "test" , " ", "input" }
           }

tests[3] = { input  = ".k.transferFrom-then-branch:not(.transferFrom-else-branch)"
           , tokens = { "." , "k" , "." , "transferFrom-then-branch" , ":not(" , "." , "transferFrom-else-branch" , ")" }
           }

tests[4] = { input  = ".k .blah :not(.foo.bar)"
           , tokens = { "." , "k" , " " , "." , "blah" , " " , ":not(" , "." , "foo" , "." , "bar" , ")" }
           }

--- Test Tokenizer
--- --------------

for i,_ in pairs(tests) do
    print("Test: " .. i)
    print("testing tokenizer...")
    if not deepcompare(tokenize(tests[i]["input"]), tests[i]["tokens"]) then
        error("Tokenizer test failure: " .. i, 1)
    end
end
