IDSTART  = '%.'
ID       = '[%a%d-]+'
NOTSTART = ':not%('
NOTEND   = '%)'
AND      = '%s+'
OR       = '%s*,%s*'

tokens = { IDSTART , ID , NOTSTART , NOTEND , OR , AND }

function matches_token(input, token)
    return string.match(input, '^' .. token, 1)
end

function tokenize(selector_string)
    local tokenized_input = {}
    local found_token = false
    local match = ""

    while string.len(selector_string) > 0 do
        --- print("current string: " .. selector_string)
        found_token = false
        for _,token in pairs(tokens) do
            --- print("matching token: " .. token)
            match = matches_token(selector_string, token)
            if match then
                --- print("token matched!")
                found_token = true
                selector_string,_ = string.gsub(selector_string, token, '', 1)
                table.insert(tokenized_input, match)
                break
            end
        end
        if not found_token then
            error("Could not tokenize: " .. selector_string)
        end
    end
    return tokenized_input
end

function parse_tokens(tokenized)
    return tokenized
end
