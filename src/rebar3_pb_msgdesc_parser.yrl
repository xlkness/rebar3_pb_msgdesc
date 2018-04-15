Nonterminals
combines combine.

Terminals msg_number msg_name msg_module.

Rootsymbol combines.

combines -> combine : '$1'.
combines -> combine combines : '$1' ++ '$2'.
combine -> msg_number msg_name msg_module :
    [{{save('$1'), save('$2')}, {save('$2'), save('$1')}, {save('$1'), save('$3')}}].

Erlang code.

save({msg_number, _, Value}) -> Value;
save({msg_name, _, Value}) -> list_to_atom(Value);
save({msg_module, _, Value}) -> list_to_atom(Value).