require "../css-selector"

--- Test triples
--- ------------

tests = {}

tests[1] = { input  = ".test .input"
           , tokens = { "." , "test" , " " , "." , "input" }
           , groups = { { id = "test" } , { id = "input" } }
           , ast    = { orExp = { { andExp = { { id = "test" } , { id = "input" } } } } }
           , eval   = { { tset = { }                            , result = false }
                      , { tset = { "test" }                     , result = false }
                      , { tset = { "test" , "input" }           , result = true  }
                      , { tset = { "test" , "input" , "extra" } , result = true  }
                      , { tset = { "extra" }                    , result = false }
                      }
           }

tests[2] = { input  = ".test , .input"
           , tokens = { "." , "test"  , " , " , "." , "input" }
           , groups = { { id = "test" } , OR , { id = "input" } }
           , ast    = { orExp = { { andExp = { { id = "input" } } } , { andExp = { { id = "test" } } } } }
           , eval   = { { tset = { }                            , result = false }
                      , { tset = { "test" }                     , result = true  }
                      , { tset = { "test" , "input" }           , result = true  }
                      , { tset = { "test" , "input" , "extra" } , result = true  }
                      , { tset = { "extra" }                    , result = false }
                      }
           }

tests[3] = { input  = ".k.transferFrom-then-branch:not(.transferFrom-else-branch)"
           , tokens = { "." , "k" , "." , "transferFrom-then-branch" , ":not(" , "." , "transferFrom-else-branch" , ")" }
           , groups = { { id = "k" } , { id = "transferFrom-then-branch" } , { notExp = { { id = "transferFrom-else-branch" } } } }
           , ast    = { orExp = { { andExp = { { id = "k" } , { id = "transferFrom-then-branch" } , { notExp = { orExp = { { andExp = { { id = "transferFrom-else-branch" } } } } } } } } } }
           , eval   = { { tset = { } , result = false }
                      , { tset = { "k" }                                                           , result = false }
                      , { tset = { "k" , "transferFrom-then-branch" }                              , result = true  }
                      , { tset = { "k" , "transferFrom-then-branch" , "transferFrom-else-branch" } , result = false }
                      , { tset = { "k" , "transferFrom-else-branch" }                              , result = false }
                      }
           }

tests[4] = { input  = ".k .blah :not(.foo.bar)"
           , tokens = { "." , "k" , " " , "." , "blah" , " " , ":not(" , "." , "foo" , "." , "bar" , ")" }
           , groups = { { id = "k" } , { id = "blah" } , { notExp = { { id = "foo" } , { id = "bar" } } } }
           , ast    = { orExp = { { andExp = { { id = "k" } , { id = "blah" } , { notExp = { orExp = { { andExp = { { id = "foo" } , { id = "bar" } } } } } } } } } }
           , eval   = { { tset = { }                              , result = false }
                      , { tset = { "k" , "blah" }                 , result = true  }
                      , { tset = { "k" , "blah" , "foo" }         , result = true  }
                      , { tset = { "k" , "blah" , "bar" }         , result = true  }
                      , { tset = { "k" , "blah" , "foo" , "bar" } , result = false }
                      , { tset = { "k" , "foo" }                  , result = false }
                      }
           }

tests[5] = { input  = ".k , .rvk , :not(.uiuck)"
           , tokens = { "." , "k" , " , " , "." , "rvk" , " , " , ":not(" , "." , "uiuck" , ")" }
           , groups = { { id = "k" } , OR , { id = "rvk" } , OR , { notExp = { { id = "uiuck" } } } }
           , ast    = { orExp = { { andExp = { { notExp = { orExp = { { andExp = { { id = "uiuck" } } } } } } } } , { andExp = { { id = "rvk" } } } , { andExp = { { id = "k" } } } } }
           , eval   = { { tset = { }                 , result = false }
                      , { tset = { "k" }             , result = true  }
                      , { tset = { "rvk" }           , result = true  }
                      , { tset = { "extra" }         , result = true  }
                      , { tset = { "uiuck" }         , result = false }
                      , { tset = { "rvk" , "uiuck" } , result = true  }
                      }
           }

tests[6] = { input  = ".test .input *"
           , tokens = { "." , "test" , " " , "." , "input" , " " , "*" }
           , groups = { { id = "test" } , { id = "input" } , ANY }
           , ast    = { orExp = { { andExp = { { id = "test" } , { id = "input" } , ANY } } } }
           , eval   = { { tset = { }                            , result = false }
                      , { tset = { "test" }                     , result = false }
                      , { tset = { "test" , "input" }           , result = true  }
                      , { tset = { "test" , "input" , "extra" } , result = true  }
                      , { tset = { "extra" }                    , result = false }
                      }
           }

tests[7] = { input  = ".test , .input , *"
           , tokens = { "." , "test"  , " , " , "." , "input" , " , " , "*" }
           , groups = { { id = "test" } , OR , { id = "input" } , OR , ANY }
           , ast    = { orExp = { { andExp = { ANY } } , { andExp = { { id = "input" } } } , { andExp = { { id = "test" } } } } }
           , eval   = { { tset = { }                            , result = false }
                      , { tset = { "test" }                     , result = true  }
                      , { tset = { "test" , "input" }           , result = true  }
                      , { tset = { "test" , "input" , "extra" } , result = true  }
                      , { tset = { "extra" }                    , result = true  }
                      }
           }

tests[8] = { input  = ".sh"
           , tokens = { "." , "sh" }
           , groups = { { id = "sh" } }
           , ast    = { orExp = { { andExp = { { id = "sh" } } } } }
           , eval   = { { tset = { "sh" }         , result = true  }
                      , { tset = { "sh" , "one" } , result = true  }
                      , { tset = { "one" }        , result = false }
                      , { tset = { }              , result = false }
                      }
           }

tests[9] = { input  = ".sh.one"
           , tokens = { "." , "sh" , "." , "one" }
           , groups = { { id = "sh" } , { id = "one" } }
           , ast    = { orExp = { { andExp = { { id = "sh" } , { id = "one" } } } } }
           , eval   = { { tset = { "sh" }         , result = false }
                      , { tset = { "sh" , "one" } , result = true  }
                      , { tset = { "one" }        , result = false }
                      , { tset = { }              , result = false }
                      }
           }

--- Run Tests
--- ---------

function assert_equal(expected, actual)
    if not deepcompare(expected, actual) then
        print("expected: " .. table.tostring(expected) .. "\nactual:   " .. table.tostring(actual))
        error("test failure!", 1)
    end
end

for i,_ in pairs(tests) do
    print("Test: " .. i)

    print("testing tokenizer...")
    local tokenized = tokenize(tests[i]["input"])
    assert_equal(tests[i]["tokens"], tokenized)

    print("testing token grouper...")
    local grouped = group_tokens(tests[i]["tokens"])
    assert_equal(tests[i]["groups"], grouped)

    print("testing group parser...")
    local parsed = parse_groups(tests[i]["groups"])
    assert_equal(tests[i]["ast"], parsed)

    print("testing main parser...")
    local parsed = parse(tests[i]["input"])
    assert_equal(tests[i]["ast"], parsed)

    print("testing expression evaluator...")
    for _,etest in pairs(tests[i]["eval"]) do
        local evaled = { tset = etest["tset"] , result = eval(tests[i]["ast"], etest["tset"]) }
        assert_equal(etest, evaled)
    end
end
