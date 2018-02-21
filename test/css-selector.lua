require "../css-selector"

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
