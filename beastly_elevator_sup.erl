%%%-------------------------------------------------------------------
%% @doc beastly_elevator top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(beastly_elevator_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

init([]) ->
    {ok, { {one_for_all, 0, 10},
    [{elevator_driver,
        {elevator_controller, start_elevator, []},
        permanent, 5000, worker, [elevator_driver]},

     {elevator_controller,
    	{elevator_controller, start_link, []},
    	permanent, 5000, worker, [elevator_controller]},

     {order_backlog,
        {order_backlog, start, []},
        permanent, 5000, worker, [order_backlog]},

     {peer_finder,
        {peer_finder, start, []},
        permanent, 5000, worker, [peer_finder]}
    ]} }.
