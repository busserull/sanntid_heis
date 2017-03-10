-module(bl3).
-behavior(gen_server).

% Names of ETS order table
-define(ORTAB, ordertable).
% Time before an order times out in seconds
-define(ORTOUT, 5).

-export([start/0, store_order/2, claim_order/2, clear_order/2, list_orders/0]).
-export([init/1, handle_call/3, handle_cast/2,
		handle_info/2, terminate/2, code_change/3]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%% Test functions %%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%These kind of functions should maybe be placed in a separate module
-export ([distribute_order/2, list_all_orders/0]).
-define(NUMBER_OF_FLOORS, 4).
distribute_order(Type, Floor) ->
	rpc:multicall(bl3,store_order,[Type, Floor]).

list_all_orders() ->
	rpc:multicall(bl3, list_orders, []).

get_order({Pos, State, LastFloor}) ->
	ok.

%Borrowed from Sivert Bakken
cost_function({ElevState, ElevFloor, ElevLastDir}, OrderFloor, OrderDir) ->
  case ElevState of
  	stuck -> 3*?NUMBER_OF_FLOORS;
  	_ ->
      Difference = OrderFloor - ElevFloor, %if negative, order is below
      erlang:abs(Difference) + 
      movement_penalty(ElevState, ElevLastDir, Difference) + 
      turn_penalty(ElevState, ElevFloor, ElevLastDir, OrderFloor) + 
      order_dir_penalty(ElevLastDir, OrderFloor, OrderDir)
  end.
  
% Cost modifier if traveling to or from order floor
movement_penalty(stopped, _Dir, _Dif) -> 0;
movement_penalty(_State, _Dir, 0) -> 1.5;

movement_penalty(_State, Dir, FloorDif) when 
(Dir == up andalso FloorDif > 0) orelse 
(Dir == down andalso FloorDif < 0) -> -0.5;

movement_penalty(_State, Dir, FloorDif) when 
(Dir == up andalso FloorDif < 0) orelse 
(Dir == down andalso FloorDif > 0) -> 1.5.

% Penalty for changing direction of travel
turn_penalty(stopped, ElevFloor, _ElevDir, _OrderFloor) when 
ElevFloor == 0 orelse ElevFloor == (?NUMBER_OF_FLOORS-1) -> 0;

turn_penalty(moving, ElevFloor, ElevDir, _OrderFloor) when 
(ElevFloor == 1 andalso ElevDir == down); 
(ElevFloor == (?NUMBER_OF_FLOORS-2) andalso ElevDir == up) -> 0;

turn_penalty(_ElevState, ElevFloor, ElevDir, OrderFloor) when 
(ElevDir == up andalso OrderFloor < ElevFloor) orelse 
(ElevDir == down andalso OrderFloor > ElevFloor) -> 0.75;

turn_penalty(_ElevState, _ElevFloor, _ElevDir, _OrderFloor) -> 0.

% Elevators are not allowed to handle orders in wrong 
% direction if there are orders beyond, cost must agree
order_dir_penalty(_ElevDir, OrderFloor, _OrderDir) when 
OrderFloor == 0 orelse OrderFloor == ?NUMBER_OF_FLOORS-1 -> 0;

order_dir_penalty(ElevDir, _OrderFloor, OrderDir) when 
OrderDir /= int andalso ElevDir /= OrderDir -> ?NUMBER_OF_FLOORS-2+0.25;

order_dir_penalty(_ElevDir, _OrderFloor, _OrderDir) -> 0.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%% Test functions %%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%%% Backlog interface

start() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec store_order(up|down|int, Floor :: integer()) -> ok.
store_order(Type, Floor) ->
	gen_server:call(?MODULE, {store, {Type, Floor}}).

-spec claim_order(up|down|int, Floor :: integer()) -> ok.
claim_order(Type, Floor) ->
	gen_server:call(?MODULE, {claim, {Type, Floor}}).

clear_order(Type, Floor) ->
	gen_server:call(?MODULE, {clear, {Type, Floor}}).

clear_order(Type, Floor) ->
	gen_server:call(?MODULE, {clear, {Type, Floor}}).

list_orders() ->
	gen_server:call(?MODULE, {list}).

%%% Server callbacks

init([]) ->
	erlang:send_after(1000, self(), {timer}),
	ets:new(?ORTAB, [set, named_table]),
	State = 0,
	{ok, State}.

% Store order
handle_call({store, {Type, Floor}}, _From, State) ->
	Key = case Type of
		   int ->
			{int, node()};
		   _ ->
		   	{ext, Type}
	end,
	Order = {{Key, Floor}, queued, erlang:monotonic_time()},
	ets:insert(?ORTAB, Order),
	rpc:multicall(nodes(), ets, insert, [?ORTAB, {12,34,56}]),
	{reply, ok, (State + 1)};
% Claim order
%handle_call({claim, {int, Floor}}, _From, State) ->
%	ok;
%	% Just update the order everywhere
%handle_call({claim, {Type, Floor}}, _From, State) ->
%	ok;
	

handle_call({claim, {Type, Floor, IP}}, _From, State) ->
	Now = erlang:monotonic_time(),
	ets:update_element(?ORTAB,{Type, Floor},[{2,IP},{3,claimed},{4,Now}]),
	{reply, ok, State};

handle_call({clear, {Type, Floor}}, _From, State) ->
	Types = case Type of
		internal ->
			[internal, up, down];
		_UpDown ->
			[up, down]
	end,
	NumberOfDeletedOrders = deleteOrders(Types, Floor),
	{reply, ok, State - NumberOfDeletedOrders};

handle_call({list}, _From, State) ->
	io:format("Number of elements = ~p~n",[State]),
	list_orders(ets:first(?ORTAB)),
	{reply, ok, State}.

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info({timer}, State) ->
	erlang:send_after(1000, self(), {timer}),
	% Check all external orders for timeout
	check_for_timeout(ets:match(?ORTAB, {{up,'$1'},'_',claimed,'$2'})),
	check_for_timeout(ets:match(?ORTAB, {{down,'$1'},'_',claimed,'$2'})),
	%check_for_timeout(ets:first(?ORTAB)),
	{noreply, State};
handle_info(_Msg, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%% Help functions
%% Deletes all Types at Floor and return number of deleted orders 
deleteOrders(Types, Floor) ->
	length([ets:delete(?ORTAB, { T, Floor}) || T <- Types,
		ets:member(?ORTAB, {T, Floor})]).

list_orders('$end_of_table') ->
	ok;
list_orders(Order) ->
	io:format("~p~n", [ets:lookup(?ORTAB, Order)]),
	list_orders(ets:next(?ORTAB, Order)).

check_for_timeout([]) ->
	ok;
check_for_timeout([[Floor|[Timestamp]]|Tail]) ->
	Now = erlang:monotonic_time(),
	Elapsed = erlang:convert_time_unit(Now - Timestamp, native, second),
	if Elapsed >= ?ORTOUT ->
		   io:format("{~p, ~p} timed out!~n", [ext, Floor]),
		   ets:update_element(?ORTAB, {ext, Floor}, {3, timeout});
	   true ->
		   ok
	end,
	check_for_timeout(Tail).
