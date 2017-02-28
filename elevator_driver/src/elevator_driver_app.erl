-module(elevator_driver_app).
 
-behavior(application).
 
%% application callbacks
-export([start/2, 
         stop/1]).
 
start(_Type, _Args) ->
	{ok, Environment} = application:get_env(beastly_elevator, environment),
    PrivDir = code:priv_dir(elevator_driver),
    {ok, ExtProg} = application:get_env(elevator_driver, extprog),
    ExtProgWithPath = filename:join([PrivDir, ExtProg]),
    elevator_driver_sup:start_link(ExtProgWithPath, Environment).
 
stop(_State) ->
    ok.