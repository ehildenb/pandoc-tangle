require "css-selector"

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

function test_tokenizer(input, output)
    return deepcompare(tokenize(input), output)
end

print(test_tokenizer("test input", {"test", " ", "input"}))
print(test_tokenizer("test input", {"test", " "}))
print(test_tokenizer(".k.transferFrom-then-branch:not(.transferFrom-else-branch)",
            {".", "k", ".", "transferFrom-then-branch", ":not(", ".", "transferFrom-else-branch", ")"}))
print(test_tokenizer(".k .blah :not(.foo.bar)",
            {".", "k", " ", ".", "blah", " ", ":not(", ".", "foo", ".", "bar", ")"}))
print(test_tokenizer(".k .blah :not(.foo.bar)",
            {".", "k", " ", ".", "blah", ".", "foo", ".", "bar", ")"}))
