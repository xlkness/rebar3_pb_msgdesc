-module(rebar3_pb_msgdesc_prv_compile).

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, compile).
-define(NAME_SPACE, msgdesc).
-define(DEPS, [{default, app_discovery}]).

%% ===================================================================
%% Public API
%% ===================================================================
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
            {name, ?PROVIDER},            % The 'user friendly' name of the task
            {namespace, ?NAME_SPACE},
            {module, ?MODULE},            % The module implementation of the task
            {bare, true},                 % The task can be run by the user, always true
            {deps, ?DEPS},                % The list of dependencies
            {example, "rebar3 rebar3_pb_msgdesc"}, % How to use the plugin
            {opts, []},                   % list of options understood by the plugin
            {short_desc, "A rebar plugin"},
            {desc, "A rebar plugin"}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.


-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    [AppInfo] =
        case rebar_state:current_app(State) of
            undefined -> rebar_state:project_apps(State);
            AppInfo1 -> [AppInfo1]
        end,
    Opts = dict:to_list(rebar_app_info:opts(AppInfo)),
    MsgDescOpts = proplists:get_value(msgdesc_opt, Opts, []),
    DescFile = proplists:get_value(desc_file, MsgDescOpts, undefined_file),
    case file:read_file_info(DescFile) of
        {ok, _} ->
            DefaultOutputName = filename:basename(DescFile, filename:extension(DescFile))
                                ++ "_msgdesc.erl",
            OutputFileName = proplists:get_value(out_put_file, MsgDescOpts, DefaultOutputName),
            exec_compile(DescFile, OutputFileName);
        {error, _} ->
            skip
    end ,
    {ok, State}.

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

exec_compile(DescFile, OutputFileName) ->
    {ok, Lines} = file:read_file(DescFile),
    {ok, Tokens, _} = rebar3_pb_msgdesc_lexer:string(binary_to_list(Lines)),
    {ok, OutputList} = rebar3_pb_msgdesc_parser:parse(Tokens),
    {ok, Fd} = file:open(OutputFileName, [write, binary]),
    ExchangeFun = fun({MsgType, MsgCode, DecodeFor}, {A, B, C}) ->
        {Id, Name} = MsgType,
        {Name, Id} = MsgCode,
        {Id, Module} = DecodeFor,
        {A ++ [{Id, list_to_atom(Name)}],
         B ++ [{list_to_atom(Name), Id}],
         C ++ [{Id, list_to_atom(Module)}]}
        end,
    {MsgTypeList, MsgCodeList, DecodeForList} =
        lists:foldl(ExchangeFun, {[], [], []}, OutputList),

    rebar_api:info("MsgTypeList:~p~n~n", [MsgTypeList]),
    rebar_api:info("MsgCodeList:~p~n~n", [MsgCodeList]),
    rebar_api:info("DecodeForList:~p~n~n", [DecodeForList]),

    FileModule = filename:basename(OutputFileName, filename:extension(OutputFileName)),
    io:format(Fd, "-module(" ++ FileModule ++ ")."),
    io:format(Fd, "-export([msg_type/1])."),
    io:format(Fd, "-export([msg_code/1])."),
    io:format(Fd, "-export([decode_for/1])."),
    OutputMsgTypeFun = fun({Id, Name}) ->
        String = lists:concat(["msg_type(", Id, ") ->\n\t", Name, ";\n"]),
        io:format(Fd, String, [])
                       end,
    lists:foreach(OutputMsgTypeFun, MsgTypeList),
    io:format(Fd, "msg_type(Other) -> {error, msgdesc, msg_type, Other}."),

    OutputMsgCodeFun = fun({Name, Id}) ->
        String = lists:concat(["msg_code(", Name, ") ->\n\t", Id, ";\n"]),
        io:format(Fd, String, [])
                       end,
    lists:foreach(OutputMsgCodeFun, MsgCodeList),
    io:format(Fd, "msg_code(Other) -> {error, msgdesc, msg_code, Other}."),

    OutputDecodeForFun = fun({Id, Module}) ->
        String = lists:concat(["decode_for(", Id, ") ->\n\t", Module, ";\n"]),
        io:format(Fd, String, [])
                       end,
    lists:foreach(OutputDecodeForFun, DecodeForList),
    io:format(Fd, "decode_for(Other) -> {error, msgdesc, decode_for, Other}."),

    ok.