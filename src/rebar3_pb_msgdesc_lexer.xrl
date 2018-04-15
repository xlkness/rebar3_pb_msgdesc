Definitions.

TypeID = [0-9]+
MsgName = [,][a-zA-Z]([0-9a-zA-Z]*_?)*[,]
MsgModule = [a-zA-Z]([0-9a-zA-Z]*_?)*(\n)?
NoneLine = [\n]

Rules.


{TypeID} : {token, {msg_number, TokenLine, list_to_integer(TokenChars)}}.
{MsgName} : {token, {msg_name, TokenLine, drop_tokens(TokenChars)}}.
{MsgModule} : {token, {msg_module, TokenLine, drop_tokens(TokenChars)}}.
{NoneLine} : skip_token.

Erlang code.

drop_tokens(TokenChars) ->
    [Chars] = string:tokens(TokenChars, ","),
    [Chars1] = string:tokens(Chars, "\s"),
    [Chars2] = string:tokens(Chars1, "."),
    [Chars3] = string:tokens(Chars2, "\n"),
    Chars3.
