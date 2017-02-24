-module(elevator_driver_sup).
-behavior(supervisor).
 
%% External exports
-export([start_link/1]).
 
%% supervisor callbacks
-export([init/1]).
 
start_link(ExtProg) ->
    supervisor:start_link(elevator_driver_sup, ExtProg).
 
init(ExtProg) ->
    {ok, {{one_for_one, 0, 10},
          [{elevator_driver, {elevator_driver, start_link, [ExtProg]},
            permanent, 10, worker, [elevator_driver]}]}}.