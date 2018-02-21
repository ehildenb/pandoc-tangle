require "utilities"

--- Parser
--- ======

--- # Tokenizer

IDSTART  = '%.'
ID       = '[%a%d-]+'
NOTSTART = ':not%('
NOTEND   = '%)'
AND      = '%s+'
OR       = '%s*,%s*'

tokens = { IDSTART , ID , NOTSTART , NOTEND , OR , AND }

function tokenize(selector_string)
    local tokenized_input = {}
    local found_token = false
    local match = ""

    while string.len(selector_string) > 0 do
        found_token = false
        for _,token in pairs(tokens) do
            match = matches_token(selector_string, token)
            if match then
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

--- # Token Grouper

function group_tokens(tokenized)
    local grouped_tokens = {}
    local current_token = 1
    while tokenized[current_token] do
        if matches_token(tokenized[current_token], IDSTART) and matches_token(tokenized[current_token + 1], ID) then
            table.insert(grouped_tokens, { id = tokenized[current_token + 1] })
            current_token = current_token + 2
        elseif matches_token(tokenized[current_token], NOTSTART) then
            local not_group = {}
            current_token = current_token + 1
            while not matches_token(tokenized[current_token], NOTEND) do
                table.insert(not_group, tokenized[current_token])
                current_token = current_token + 1
            end
            current_token = current_token + 1
            table.insert(grouped_tokens, { notExp = group_tokens(not_group) })
        elseif matches_token(tokenized[current_token], OR) then
            table.insert(grouped_tokens, OR)
            current_token = current_token + 1
        elseif matches_token(tokenized[current_token], AND) then
            current_token = current_token + 1
        else
            error("Unexpected token: " .. tokenized[current_token], 1)
        end
    end
    return grouped_tokens
end

--- # Group Parser

function parse_groups(grouped_tokens)
    local current_expression = { orExp = { { andExp = {} } } }
    local current_group = 1
    while grouped_tokens[current_group] do
        if grouped_tokens[current_group]["id"] then
            table.insert(current_expression["orExp"][1]["andExp"], grouped_tokens[current_group])
            current_group = current_group + 1
        elseif grouped_tokens[current_group]["notExp"] then
            local parsed_not = { notExp = parse_groups(grouped_tokens[current_group]["notExp"]) }
            table.insert(current_expression["orExp"][1]["andExp"], parsed_not)
            current_group = current_group + 1
        elseif grouped_tokens[current_group] == OR then
            table.insert(current_expression["orExp"], 1, { andExp = {} })
            current_group = current_group + 1
        end
    end
    return current_expression
end
