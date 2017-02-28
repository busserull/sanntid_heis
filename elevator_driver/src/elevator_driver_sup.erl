-module(elevator_driver_sup).
-behavior(supervisor).
 
%% External exports
-export([start_link/2]).
 
%% supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).
 
start_link(ExtProg, Environment) ->
    supervisor:start_link({local, ?SERVER},?MODULE, {ExtProg, Environment}).
 
init({ExtProg, Environment}) ->
    {ok, {{one_for_all, 0, 2},
          [{elevator_driver, {elevator_driver, start_link, [ExtProg]},
            permanent, 10, worker, [elevator_driver]},
            {elevator_poller, {elevator_poller, start_link, [Environment]},
            permanent, 10, worker, [elevator_poller]}
            ]}}.