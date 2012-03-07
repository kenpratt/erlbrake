-module(erlbrake_error_logger).

-compile(export_all).

% Internal alias
% Type = atom(), gets dropped
% Reason = atom() | list(), the error class (api), not seen in the UI, but used for grouping errors in airbrake
% Message = string() - shown in the UI
% Module = string() - Shown in the UI
% Line = int() shown in the UI
%
airbrake_notify(Type, Reason, Message, Module, Line) ->
  airbrake:notify(Type, Reason, Message, Module, Line).


%% Gen_Event Callbacks

init(_InitArgs) -> {ok, []}.

% Generated when error_msg/1,2 or format is called
handle_event({error, _Gleader, {_Pid, Format, Data}}, State) ->
  Message = lists:flatten(io_lib:format(Format, Data)),
  airbrake_notify(error, Format, Message, ?MODULE, ?LINE),
  {ok, State};

% Generated when error_report/1,2 is called
handle_event({error_report, _Gleader, {_Pid, Type, Report}}, State) ->
  airbrake_notify(error_report, Type, Report, ?MODULE, ?LINE),
  {ok, State};

% Generated when warning_msg/1,2 is called
% Generated when warning_report/1 is called
handle_event({warning_msg, _, _}, State) ->
  {ok, State};

% Generated when info_msg/1,2 is called
handle_event({info_msg, _, _}, State) -> {ok, State};
handle_event({info_report, _, _}, State) -> {ok, State};
handle_event(_, State) -> {ok, State}.

handle_call(_Request, State) ->
  {ok, ok, State}.

handle_info(_Info, State) ->
  {ok, State}.

terminate(_Arg, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.


