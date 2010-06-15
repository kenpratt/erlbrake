%%% Author: Ken Pratt <ken@kenpratt.net>
%%% Created: 2010-02-09

%%% A bridge to the Hoptoad exception notification service

-module(hoptoad).

-behaviour(gen_server).

%% API
-export([start_link/2, notify/5, notify/6, notify/7, notify/8]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(NOTIFICATION_API, "http://hoptoadapp.com/notifier_api/v2/notices").

-define(SERVER, ?MODULE).

-record(state, {environment, api_key}).

%%====================================================================
%% API
%%====================================================================

start_link(Environment, ApiKey) when is_list(Environment), is_list(ApiKey) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Environment, ApiKey], []).

notify(Type, Reason, Message, Module, Line) ->
  notify(Type, Reason, Message, Module, Line, []).

notify(Type, Reason, Message, Module, Line, Stacktrace) ->
  notify(Type, Reason, Message, Module, Line, Stacktrace, no_request).

%%
% Stacktrace = [MFA]
% MFA =        {Module, Line} |
%              {Module, Function, Arbitary} |
%              {Module, Function, [Argument]} |
%
% Module = Function = atom()
% Arbitary = Line = integer()
% 
%
% Request = no_request |
%           {Url, CgiVars}
%           {Url, Component, CgiVars} |
%           {Url, Component, Action, CgiVars}
% Url = Component = Action = Key = Value = string()
% CgiVars = [{Key, Value}]
%
% Request = none | string()
notify(Type, Reason, Message, Module, Line, Stacktrace, Request) ->
  notify(Type, Reason, Message, Module, Line, Stacktrace, Request, none).
    
notify(Type, Reason, Message, Module, Line, Stacktrace, Request, ProjectRoot)
  when is_atom(Module), is_integer(Line), is_list(Stacktrace),
        Request =:= no_request orelse is_tuple(Request) ->
    gen_server:cast(?SERVER, {exception, Type, Reason, Message, Module, Line, Stacktrace, Request, ProjectRoot}).


%%====================================================================
%% gen_server callbacks
%%====================================================================

init([Environment, ApiKey]) ->
    {ok, #state{environment = Environment, api_key = ApiKey}}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.
    
% Old version, for backwardcompatiblity (old versions, full inbox, ...)
handle_cast({exception, Type, Reason, Message, Module, Line, Stacktrace}, State) ->
    handle_cast({exception, Type, Reason, Message, Module, Line, Stacktrace, no_request, none}, State);
    
% New Version with additional Request + ProjectRoot
handle_cast(Raw = {exception, _Type, _Reason, _Message, _Module, _Line, _Stacktrace, _Request, _ProjectRoot}, State) ->
    Xml = generate_xml(Raw, State),
    case send_to_hoptoad(Xml) of
        ok ->
            noop;
        {error, {unexpected_response_status, "422"}} ->
            Reason = "The submitted notice was invalid - please ensure the API key is correct. If it is, it could be a bug in the erlhoptoad XML generation.",
            io:format("Erlhoptoad notification failed: ~s~n~p~n", [Reason, Xml]);
        {error, Reason} ->
            %% can't generate an error report, because if the erlhoptoad error
            %% logger is being used, it will create an infinite failure loop.
            io:format("Erlhoptoad notification failed: ~1024p~n", [Reason])
    end,
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================

%% convert some exception data into Hoptoad API format
generate_xml({exception, _Type, Reason, Message, Module, Line, Stacktrace, Request, ProjectRoot},
             #state{environment = Environment, api_key = ApiKey}) ->
    
    %% Parse the several supported Tuple types
    % Each result is encapsulated in a list, so it can latter be used with lists:flatten()
    RequestElement = case Request of
        no_request ->
            [];
        none -> % Ignore some more stuff
            [];
        false -> 
            [];
        {Url, CgiVars} ->
            [{request, [{url, [Url]}, {'cgi-data', vars_to_xml_struct(CgiVars)}]}];
        {Url, Component, CgiVars} ->
            [{request, [{url, [Url]}, {component, [Component]}, {'cgi-data', vars_to_xml_struct(CgiVars)}]}];
        {Url, Component, Action, CgiVars} ->
            [{request, [{url, [Url]}, {component, [Component]}, {action, [Action]}, {'cgi-data', vars_to_xml_struct(CgiVars)}]}]
    end,
    
    ProjectElement = case ProjectRoot of
        none -> [];
        false -> [];
        Str  -> [{'project-root', [Str]}]
    end,
        
    
    Data =
        [{notice,
          [{version,"2.0"}],
          lists:flatten([
            [
             {'api-key', [ApiKey]},
             {notifier,
              [{name, ["erlhoptoad"]},
               {version, ["0.1"]},
               {url, ["http://github.com/kenpratt/erlhoptoad"]}]},
             {error,
              [{class, [to_s(Reason)]},
               {message, [Message]},
               {backtrace, stacktrace_to_xml_struct([{Module, Line}|Stacktrace])}]}
            ],
            RequestElement,
            [
             {'server-environment',
                lists:flatten(
                    ProjectElement,
                    [{'environment-name', [Environment]}]
                )
             }
            ]
          ])
        }],
        
    lists:flatten(xmerl:export_simple(Data, xmerl_xml)).
    
    
vars_to_xml_struct(CgiVars) when is_list(CgiVars) ->
    vars_to_xml_struct(CgiVars, []).
    
vars_to_xml_struct([], Result) ->
    lists:reverse(Result);
vars_to_xml_struct([{Key, Value} | Rest], Result) ->
    vars_to_xml_struct(Rest, [{var, [{key, Key}], [Value]}] ++ Result).



stacktrace_to_xml_struct(Stacktrace) when is_list(Stacktrace) ->
    [stacktrace_line_to_xml_struct(L) || L <- Stacktrace].

stacktrace_line_to_xml_struct({M, Line}) when is_atom(M), is_integer(Line) ->
    {line,
     [{file, atom_to_list(M)},
      {number, Line}],
     []};
stacktrace_line_to_xml_struct({M, F, Arity}) when is_atom(M), is_atom(F), is_integer(Arity) ->
    {line,
     [{method, to_s("~p/~B", [F, Arity])},
      {file, atom_to_list(M)},
      {number, 0}],
     []};
stacktrace_line_to_xml_struct({M, F, Args}) when is_atom(M), is_atom(F), is_list(Args) ->
    {line,
     [{method, to_s("~p/~B ~1024p", [F, length(Args), Args])},
      {file, atom_to_list(M)},
      {number, 0}],
     []}.

%% POST some XML to Hoptoad's notify API
send_to_hoptoad(Xml) ->
    case send_http_request(?NOTIFICATION_API, [{"Content-Type", "text/xml"}], post, Xml) of
        {ok, _ResponseBody} ->
            ok;
        Error ->
            Error
    end.

%% use ibrowse to process an HTTP request
send_http_request(Url, Headers, Method, Body) ->
    case ibrowse:send_req(Url, Headers, Method, Body) of
        {ok, "200", _ResponseHeaders, ResponseBody} ->
            {ok, ResponseBody};
        {ok, OtherStatus, _Headers, _Body} ->
            {error, {unexpected_response_status, OtherStatus}};
        Error = {error, _} ->
            Error;
        Other ->
            {error, {unexpected_response, Other}}
    end.

to_s(Any) ->
    to_s("~1024p", [Any]).
to_s(Fmt, Args) ->
    lists:flatten(io_lib:format(Fmt, Args)).
