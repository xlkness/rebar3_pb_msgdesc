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
    Apps = case rebar_state:current_app(State) of
               undefined ->
                   rebar_state:project_apps(State);
               AppInfo ->
                   [AppInfo]
           end,

    [begin
         Opts = rebar_app_info:opts(AppInfo),
         SourceDir = filename:join(rebar_app_info:dir(AppInfo), "exc_files"),
         FoundFiles = rebar_utils:find_files(SourceDir, ".*\\.exc\$"),
         rebar_api:info("SourceDir:~p~n~n", [SourceDir]),
         rebar_api:info("FoundFiles:~p~n~n", [FoundFiles]),
         CompileFun = fun(_Source, Opts1) ->
             rebar_api:info("_Source:~p~n~n", [_Source]),
             exc_compile(Opts1)
                      end,

         rebar_base_compiler:run(Opts, [], FoundFiles, CompileFun)
     end || AppInfo <- Apps],


    {ok, State}.

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

exc_compile(Opts) ->
    rebar_api:info("Opts:~p~n~n", [Opts]).