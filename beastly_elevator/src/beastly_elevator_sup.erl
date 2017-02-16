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

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    {ok, { {one_for_one, 3, 60}, % not restart more than 3 times within 60 sek
    [{environment_controller,  % worker ID
    	{environment_controller, start_link, [[1,2,3]]}, % mod, startFun, [arg]
    	permanent, 5000, worker, [environment_controller]}, %restart strat, shutdown timer, type, [module]
     {environment_poller,  
    	{environment_poller, start_link, []}, 
    	permanent, 5000, worker, [environment_poller]}
    ]} }.

%%====================================================================
%% Internal functions
%%====================================================================
