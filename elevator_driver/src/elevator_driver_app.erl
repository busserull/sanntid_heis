-module(elevator_driver_app).
 
-behavior(application).
 
%% application callbacks
-export([start/2, 
         stop/1]).
 
start(_Type, _Args) ->
    PrivDir = code:priv_dir(elevator_driver),
    {ok, ExtProg} = application:get_env(elevator_driver, extprog),
    elevator_driver_sup:start_link(filename:join([PrivDir, ExtProg])).
 
stop(_State) ->
    ok.