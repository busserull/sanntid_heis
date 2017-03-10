-module (test_backlog).

-behaviour(gen_server).

-export([start_link/0]).

%% API functions
-export([distribute_order/1, finnish_order/1, get_order/1]).   

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {orders, current_order}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
	process_flag(trap_exit, true),
	io:format("Test backlog initialised.~n"),
    {ok, #state{orders = queue:new(), current_order = empty}}.

distribute_order(Order) -> 
    gen_server:call(?MODULE, {distribute_order, Order}).

finnish_order(Order) ->
	gen_server:call(?MODULE, {finnish_order, Order}).

get_order(ElevatorState) ->
	gen_server:call(?MODULE, {get_order, ElevatorState}).	

handle_call({distribute_order, Order}, _From, State) ->
	case queue:is_empty(State#state.orders) of
		true when State#state.current_order =:= empty ->
			environment_controller:goto_order(Order),
			{reply, ok, State#state{current_order = Order}};
		_anything ->
			{reply,ok,State#state{orders = queue:in(Order, State#state.orders)}}
	end;

handle_call({finnish_order, _Order}, _From, State) ->
	{reply, ok, State#state{current_order = empty}};

handle_call({get_order, ElevatorState}, _From, State) ->
	io:format("Elevator_state =~p~n",[ElevatorState]),
	case State#state.current_order of
		empty ->
			case queue:out(State#state.orders) of
				{{value, NewOrder}, RemainingOrders} ->
					{reply, NewOrder, State#state{current_order = NewOrder, 
						orders = RemainingOrders}};
				{empty, _} ->
					{reply, empty, State#state{current_order = empty}}
			end; 
		OldOrder ->
			{reply, OldOrder, State}
	end.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
	io:format("Terminating ~p!~n",[?MODULE]),
    io:format("done~n"),
    ok.

code_change(_OldVsn, State, _Extra) ->
	io:format("~n~nPerforming Code change in ~p!~n~n~n",[?MODULE]),
    {ok, State}.

%% Internal functions