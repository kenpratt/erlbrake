-module(erlbrake_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    {ok, Environment} = application:get_env(environment),

    case application:get_env(api_key) of
       {ok, "ENTER_API_KEY"} -> {error, no_api_key};
       {ok, ApiKey} -> 
       
            case application:get_env(error_logger) of
                {ok, true} ->
                      error_logger:add_report_handler(erlbrake_error_logger);
                _ -> ok
            end,
             
            erlbrake_sup:start_link(Environment, ApiKey);
       
       
       
       undefined -> {error, no_api_key}
    end.
    
    
stop(_State) ->
    ok.
