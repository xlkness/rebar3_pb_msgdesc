-module(rebar3_pb_msgdesc_prv_clean).

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, clean).
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
    case proplists:get_value(out_put_file, MsgDescOpts, []) of
        [] -> skip;
        OutputFileName -> file:delete(OutputFileName)
    end,
    {ok, State}.

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).
