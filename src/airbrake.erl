%%% Author: Ken Pratt <ken@kenpratt.net>
%%% Modified by Andrew Hodges <betawaffle@gmail.com>
%%% Modified by Tilman Holschuh <tilman.holschuh@gmail.com>
%%% Created: 2010-02-09

%%% A bridge to the Airbrake exception notification service

-module(airbrake).
-behaviour(gen_server).

-type str() :: binary() 
             | string()
             | atom().

-type airbrake_element() :: {reason, str()}
                          | {message, str()}
                          | {module, str()}
                          | {function, str()}
                          | {line, integer()}
                          | {trace, list()} %% erlang stacktrace
                          | {node, str()}
                          | {application, str()}
                          | {version, str()}
                          | {request, request()}.

-type request() :: {URL::term(), Vars::term()} 
                 | {URL::term(), Controller::term(), Vars::term()} 
                 | {URL::term(), Controller::term(), Action::term(), Vars::term()} 
                 | {URL::term(), Controller::term(), Action::term(), Params::term(), Vars::term()}.
        
%% API
-export([start_link/2]).
-export([notify/1,
         notify/5,
         notify/6,
         notify/7,
         notify/8]).


%% Generic Server Callbacks
-export([init/1,
         code_change/3,
         terminate/2,
         handle_call/3,
         handle_cast/2,
         handle_info/2]).

%% Constants
-define(NOTIFICATION_API, case application:get_env(notification_api) of
  {ok, NotificationAPI} -> 
    NotificationAPI;
  _ ->
    "http://airbrake.io/notifier_api/v2/notices"
end).
-define(SERVER, ?MODULE).

-record(state, {environment, api_key :: string(), queue = queue:new() :: queue() , locked = false :: boolean(), pause_time = 1000 :: integer(), max_queue = 30 :: integer()}).
-record(notice, {
    %% Keys map to elements in Airbrake XSD (see http://airbrake.io/airbrake_2_2.xsd)
    %% reason      : /notice/error/class
    reason,
    %% message     : /notice/error/message
    message,
    %% module      : /notice/error/backtrace/line@file
    module,
    %% function    : /notice/error/backtrace/line@method
    function,
    %% line        : /notice/error/backtrace/line@number
    line,
    %% trace       : /notice/error/backtrace
    trace,  
    %% node        : /server-environment/hostname
    node,
    %% application : /server-environment/project-root
    application,
    %% version     : /server-environment/app-version
    version,
    %% request     : /request
    request
    }).


%% =============================================================================
%% API Functions
%% =============================================================================

start_link(Environment, ApiKey)
  when is_list(Environment) andalso
       is_list(ApiKey) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Environment, ApiKey], []).

-spec notify([airbrake_element()]) -> ok.
notify(MsgProps) when is_list(MsgProps) ->
  AirbrakeNotice = #notice{
      reason      = proplists:get_value(reason, MsgProps),
      message     = proplists:get_value(message, MsgProps),
      trace       = proplists:get_value(trace, MsgProps, []),
      module      = proplists:get_value(module, MsgProps),
      function    = proplists:get_value(function, MsgProps),
      line        = proplists:get_value(line, MsgProps, 0),
      node        = proplists:get_value(node, MsgProps),
      application = proplists:get_value(application, MsgProps),
      version     = proplists:get_value(version, MsgProps),
      request     = proplists:get_value(request, MsgProps)
      },
  gen_server:cast(?MODULE, AirbrakeNotice).
    
notify(Type, Reason, Message, Module, Line) ->
    notify(Type, Reason, Message, Module, Line, []).

notify(Type, Reason, Message, Module, Line, Trace) ->
    notify(Type, Reason, Message, Module, Line, Trace, undefined).

notify(Type, Reason, Message, Module, Line, Trace, Request) ->
    notify(Type, Reason, Message, Module, Line, Trace, Request, undefined).
    
notify(Type, Reason, Message, Module, Line, Trace, Request, ProjectRoot)
  when is_atom(Module),
       is_integer(Line),
       is_list(Trace) ->
    gen_server:cast(?MODULE, {exception, Type, Reason, Message, Module, Line, Trace, Request, ProjectRoot}).


%% =============================================================================
%% Generic Server Callbacks
%% =============================================================================

init([Environment, ApiKey]) ->
    {ok, #state{environment = Environment, api_key = ApiKey}}.

code_change(_, S, _) ->
    {ok, S}.

terminate(_, _) ->
    ok.

handle_call(_, _, S) ->
    {reply, ok, S}.
    
% Old versions, for backwardcompatiblity (old versions, full inbox, ...)
handle_cast({exception, Type, Reason, Message, Module, Line, Trace}, S) ->
    handle_cast({exception, Type, Reason, Message, Module, Line, Trace, undefined, undefined}, S);
handle_cast({exception, _Type, Reason, Message, Module, Line, Trace, Request, ProjectRoot}, S) ->
  Notice = #notice{
      reason      = Reason,
      message     = Message,
      trace       = Trace,
      module      = Module,
      line        = Line,
      application = ProjectRoot,
      request     = Request
      },
  handle_cast(Notice, S);


handle_cast(Raw = #notice{}, S) ->
    case S#state.locked of
        true ->
            case queue:len(S#state.queue) < S#state.max_queue of
                true -> {noreply, S#state{queue = queue:in(Raw, S#state.queue)}};
                false -> {noreply, S}
            end;
        false ->
            do_request(Raw, S)
    end;



handle_cast(_, S) ->
    {noreply, S}.

handle_info(unlock, S) ->
    case queue:out(S#state.queue) of
        {empty, Q1} ->
            {noreply, S#state{queue = Q1, locked = false}};
        {{value, Raw}, Q1} ->
            do_request(Raw, S#state{queue = Q1, locked = false})
    end;
handle_info(_, S) ->
    {noreply, S}.

do_request(Raw, S) ->
    process_request(Raw, S),
    {noreply, S#state{locked = true}}.

schedule_unlock(S, Pid) ->
    timer:send_after(S#state.pause_time, Pid, unlock).

process_request(Raw, S) ->
    Parent = self(),
    spawn_link(fun() ->
        XML = generate_xml(Raw, S),
        case send_to_airbrake(XML) of
            ok ->
                noop;
            {error, {unexpected_response_status, "422"}} ->
                Reason = "The submitted notice was invalid - please ensure the API key is correct. If it is, it could be a bug in the erlbrake XML generation.",
                io:format("Erlbrake notification failed: ~s~n", [Reason]);
            {error, Reason} ->
                %% can't generate an error report, because if the erlbrake error
                %% logger is being used, it will create an infinite failure loop.
                io:format("Erlbrake notification failed: ~1024p~n", [Reason])
        end,

        schedule_unlock(S, Parent)
    end).




%% =============================================================================
%% Internal Functions
%% =============================================================================

%% Convert some exception data into Airbrake API format
%%{exception, _Type, Reason, Message, Module, Line, Trace, Request, ProjectRoot}
generate_xml(N = #notice{}, S) ->
    Server0 = [{'environment-name', [to_s(S#state.environment)]}],
    Server1 = maybe_prepend('project-root', to_s(N#notice.application), Server0),
    Notice0 = [{'server-environment', Server1}],
    Notice1 = maybe_prepend(request, N#notice.request, Notice0),
    Notice2 = [{'api-key', [S#state.api_key]},
               {'notifier',
                [{name,    ["erlbrake"]},
                 {version, ["0.3.1"]},
                 {url,     ["http://github.com/ypaq/erlbrake"]}
                ]},
               {'error',
                [{class,     [to_s(N#notice.reason)]},
                 {message,   [to_s(N#notice.message)]},
                 {backtrace, stacktrace_to_xml_struct([{N#notice.module, N#notice.line}|N#notice.trace])}
                ]}
               |Notice1],
    Root = [{notice, [{version,"2.2"}], Notice2}],
    lists:flatten(xmerl:export_simple(Root, xmerl_xml)).

maybe_prepend(_, undefined, Acc) ->
    Acc;
maybe_prepend(Key = request, Request, Acc) ->
    Size = tuple_size(Request),
    Vars = element(Size, Request),

    %% Parse the several supported tuple types.
    %% {URL, Vars}
    %% {URL, Controller, Vars}
    %% {URL, Controller, Action, Vars}
    %% {URL, Controller, Action, Params, Vars}
    Val0 = if
               length(Vars) > 0 ->
                   [{'cgi-data', vars_to_xml_struct(Vars)}];
               true ->
                   []
           end,
    Val1 = if
               Size >= 5 ->
                   [{params, vars_to_xml_struct(element(4, Request))}|Val0];
               true ->
                   Val0
           end,
    Val2 = if
               Size >= 4 ->
                   [{action, [element(3, Request)]}|Val1];
               true ->
                   Val1
           end,
    Val3 = if
               Size >= 3 ->
                   [{component, [element(2, Request)]}|Val2];
               true ->
                   Val2
           end,
    [{Key, [{url, [element(1, Request)]}|Val3]}|Acc];
maybe_prepend(Key = 'project-root', ProjectRoot, Acc) ->
    [{Key, [ProjectRoot]}|Acc].

%% Use ibrowse to process an HTTP request
send_http_request(URL, Headers, Method, Body) ->
    case ibrowse:send_req(URL, Headers, Method, Body) of
        {ok, "200", _ResponseHeaders, ResponseBody} ->
            {ok, ResponseBody};
        {ok, Status, _Headers, _Body} ->
            {error, {unexpected_response_status, Status}};
        Error = {error, _} ->
            Error;
        Other ->
            {error, {unexpected_response, Other}}
    end.

%% POST some XML to Airbrake's notify API
send_to_airbrake(XML) ->
    Res = send_http_request(?NOTIFICATION_API, [{"Content-Type", "text/xml"}], post, XML),
    case Res of
        {ok, _} -> ok;
        Error   -> Error
    end.

stacktrace_to_xml_struct(Trace)
  when is_list(Trace) ->
    [stacktrace_line_to_xml_struct(L) || L <- Trace].

stacktrace_line_to_xml_struct({M, Line})
  when is_atom(M) andalso
       is_integer(Line) ->
    {line,
     [{file,   atom_to_list(M)},
      {number, Line}],
     []};
stacktrace_line_to_xml_struct({M, F, Arity})
  when is_atom(M) andalso
       is_atom(F) andalso
       is_integer(Arity) ->
    {line,
     [{method, to_s("~w/~B", [F, Arity])},
      {file,   atom_to_list(M)},
      {number, 0}],
     []};
stacktrace_line_to_xml_struct({M, F, Arity, Pos})
  when is_atom(M) andalso
       is_atom(F) andalso
       is_integer(Arity) andalso
       is_list(Pos) ->
    File = proplists:get_value(file, Pos, atom_to_list(M)),
    Line = proplists:get_value(line, Pos, 0),
    {line,
     [{method, to_s("~w/~B", [F, Arity])},
      {file,   File},
      {number, Line}],
     []};
stacktrace_line_to_xml_struct({M, F, Args, Pos})
  when is_atom(M) andalso
       is_atom(F) andalso
       is_list(Args) andalso
       is_list(Pos) ->
    File = proplists:get_value(file, Pos, atom_to_list(M)),
    Line = proplists:get_value(line, Pos, 0),
    {line,
     [{method, to_s("~w/~B ~w", [F, length(Args), Args])},
      {file,   File},
      {number, Line}],
     []};
stacktrace_line_to_xml_struct({M, F, Args}) when is_atom(M), is_atom(F), is_list(Args) ->
    {line,
     [{method, to_s("~w/~B ~w", [F, length(Args), Args])},
      {file, atom_to_list(M)},
      {number, 0}],
     []};
stacktrace_line_to_xml_struct(_) ->
    %% catch all in case of unknown structure
    {line,
     [{file, unknown},
      {number, 0}],
     []}.


to_s(Str)
  when is_binary(Str) orelse
       is_list(Str) ->
    to_s("~s", [Str]);
to_s(Any) ->
    to_s("~p", [Any]).

to_s(Fmt, Args) ->
    lists:flatten(io_lib:format(Fmt, Args)).

vars_to_xml_struct(CgiVars) when is_list(CgiVars) ->
    vars_to_xml_struct(CgiVars, []).

vars_to_xml_struct([], Result) ->
    lists:reverse(Result);
vars_to_xml_struct([{Key, Value} | Rest], Result) ->
    vars_to_xml_struct(Rest, [{var, [{key, Key}], [Value]}] ++ Result).


%% ============================================================================
%% Tests
%% ============================================================================
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

test() ->
    % Test the reformat of the stacktraces
    test([],
         stacktrace_to_xml_struct([]),
         "Empty stacktrace list test"),
    test([{line, [{file, "test"}, {number, 7}], []}],
         stacktrace_to_xml_struct([{test, 7}]),
         "Simple stacktrace list test"),
    
    % Test the internal reformat of the provided CgiVars
    test([],
         vars_to_xml_struct([]),
         "Empty test"),
    test([{var, [{key, "SERVER_NAME"}], ["localhost"]}],
         vars_to_xml_struct([{"SERVER_NAME", "localhost"}]),
         "One parameter test 1"),
    test([{var, [{key, "SERVER_NAME"}], ["localhost"]}],
         vars_to_xml_struct([{"SERVER_NAME", "localhost"}]),
         "One parameter atom test"),
    test([{var, [{key, "SERVER_NAME"}], ["localhost"]},
          {var, [{key, "HTTP_USER_AGENT"}], ["Mozilla"]}
         ],
         vars_to_xml_struct([{"SERVER_NAME", "localhost"}, {"HTTP_USER_AGENT", "Mozilla"}]),
         "Two parameters test"),
         
    test("<?xml version=\"1.0\"?>"
         "<notice version=\"2.0\"><api-key>12345678901234567890</api-key><notifier><name>erlbrake</name><version>0.1</version><url>http://github.com/betawaffle/erlbrake</url></notifier><error><class>mismatch</class><message>Mismatch on right hand side</message><backtrace><line file=\"client_tests\" number=\"124123\"/></backtrace></error><server-environment><environment-name>Development</environment-name></server-environment></notice>",
         generate_xml({exception, error, mismatch, "Mismatch on right hand side", client_tests, 124123, [], undefined, undefined}, #state{environment = "Development", api_key = "12345678901234567890"}),
         "Test generating simple xml"),
    test("<?xml version=\"1.0\"?>"
         "<notice version=\"2.0\"><api-key>12345678901234567890</api-key><notifier><name>erlbrake</name><version>0.1</version><url>http://github.com/betawaffle/erlbrake</url></notifier><error><class>mismatch</class><message>Mismatch on right hand side</message><backtrace><line file=\"client_tests\" number=\"124123\"/></backtrace></error><request><url>http://localhost/test</url><component>web</component><action>index</action><cgi-data><var key=\"HTTP_AGENT\">Mozilla</var><var key=\"SERVER_NAME\">localhost</var></cgi-data></request><server-environment><project-root>/srv/web</project-root><environment-name>Development</environment-name></server-environment></notice>",
         generate_xml({exception, error, mismatch, <<"Mismatch on right hand side">>, client_tests, 124123, [], {"http://localhost/test", "web", "index", [{"HTTP_AGENT", "Mozilla"}, {"SERVER_NAME", "localhost"}]}, "/srv/web"}, #state{environment = "Development", api_key = "12345678901234567890"}),
         "Test generating xml with request and root"),
    ok.

test(A, B, Msg) ->
    if
        A =:= B ->
            true;
        true ->
            io:format("~s failed.~n"
                      "  Expected     ~p~n"
                      "  But received ~p~n"
                      "~n",
                      [Msg, A, B]),
            false
    end.

-endif.
