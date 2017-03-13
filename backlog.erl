-module(backlog).
-behavior(gen_server).

-define(ORTOUT, 5000). % Order timeout (ms)
-define(TOUTCHINT, 1000). % Timeout check interval (ms)
-define(ORTAB, ordertable).

%%% API
-export([start/0, store_order/2, alter_order/3, notify_state/3,
         assign_order/1, get_order/0, list/0]).
%%% Helper functions
-export([sync_orders/0, helper_sync/0, make_order_key/1]).
%%% Server callback functions
-export([init/1, handle_call/3, handle_info/2,
		handle_cast/2, terminate/2, code_change/3]).

%%% Backlog interface

-spec start() -> ok.
-spec store_order(up|down|int, Floor::integer()) -> ok.
-spec alter_order(up|down|int, Floor::integer(), claimed|timeout|complete) -> ok.
-spec notify_state(ElevFloor::integer(), up|down|stop, at_floor|in_the_void) -> ok.
-spec get_order() -> up|down|none|open_door.

start() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

store_order(Type, Floor) ->
    Key = make_order_key(Type),
	Order = {{Key, Floor}, queued, erlang:monotonic_time()},
    rpc:multicall(gen_server, call, [?MODULE, {store, Order}]).
    %case lists:keyfind(size,1,ets:info(?ORTAB)) of
    %	0 ->
    %		assign_order(Order);
    %	_Size ->
    %		ok
    %end.

notify_state(ElevFloor, ElevDir, AtFloor) ->
    gen_server:call(?MODULE, {notify, {ElevFloor, ElevDir, AtFloor}}).




alter_order(Type, Floor, NewState) ->
    Key = {make_order_key(Type), Floor},
    rpc:multicall(gen_server, call, [?MODULE, {alter, Key, NewState}]).

get_order() ->
    gen_server:call(?MODULE, get_order).
%    {{Type, Dir}, Floor} = cost:optimal(ElevFloor, ElevDir, ets:first(?ORTAB)),
%    Key = case Type of
%              ext ->
%                  {Dir, Floor};
%              int ->
%                  {int, Floor}
%          end,
%    io:format("Best fit: ~p~n", [Key]).

assign_order(Key) ->
    {Costs, _} = rpc:multicall(gen_server, call, [?MODULE, {get_cost, Key}]),
    {_Cost, _Node} = lists:min(Costs),
    %io:format("Before rpc~n"),
    %rpc:call([Node()], gen_server, cast, [?MODULE, {assign, Key}]),
    %io:format("After rpc~n"),
    gen_server:cast(?MODULE, {assign, Key}),
    {{Type, _Dir}, Floor} = Key,
    rpc:multicall(?MODULE, alter_order, [Type, Floor, claimed]).


%%%%%%%%%


%%% Server callbacks

% Init
init([]) ->
	ets:new(?ORTAB, [set, named_table]),
    erlang:send_after(?TOUTCHINT, self(), timer),
	{ok, {{0, stop, at_floor}, []}}.
    % Likely incorrect inital state, this only
    % impacts the first time get_cost is called

% Store order
handle_call({store, Order}, _From, State) ->
    ets:insert(?ORTAB, Order),
    {Key, _Status, _Timestamp} = Order,
    set_button_light(Key, on),
    {reply, ok, State};

% Alter order
handle_call({alter, Key, complete}, _From, State) ->
    {{Type, Node}, Floor} = Key,
    ets:delete(?ORTAB, {{ext, up}, Floor}),
    ets:delete(?ORTAB, {{ext, down}, Floor}),
    set_button_light({{ext, up}, Floor}, off),
    set_button_light({{ext, down}, Floor}, off),
    case Type of
        int ->
            ets:delete(?ORTAB, {{int, Node}, Floor}),
            set_button_light({{int, Node}, Floor}, off);
        _ ->
            ok
    end,
    {reply, ok, State};
handle_call({alter, Key, NewState}, _From, State) ->
    Now = erlang:monotonic_time(),
    ets:update_element(?ORTAB, Key, [{2, NewState}, {3, Now}]),
    {reply, ok, State};

% Get order
handle_call(get_order, _From, {State, OldList}) ->
    io:format("get_order: (~p)~n", [State]),
    OrderList = OldList,
    {ElevFloor, _Dir, _AtFloor} = State,
    Diff = case OrderList of
                  [] ->
                      none;
                  _ ->
                      element(2, hd(OrderList)) - ElevFloor
           end,
    Command = case Diff of
                  none ->
                      none;
                  Num when Num == 0 ->
                      open_door;
                  Num when Num > 0 ->
                      up;
                  Num when Num < 0 ->
                      down
              end,
    io:format("Returning ~p~n", [Command]),
    {reply, Command, {State, OrderList}};

% Update known elevator state
handle_call({notify, NewState}, _From, {_OldState, OrderList}) ->
    {reply, ok, {NewState, OrderList}};

% Calculate cost for particular order
handle_call({get_cost, Key}, _From, {State, OrderList}) ->
    {Floor, Dir, AtFloor} = State,
    Cost = case AtFloor of
               in_the_void when Dir == up ->
                   cost:get_cost(Floor + 1, Dir, Key);
               in_the_void when Dir == down ->
                   cost:get_cost(Floor - 1, Dir, Key);
               at_floor ->
                   cost:get_cost(Floor, Dir, Key)
           end,
    {reply, {Cost, node()}, {State, OrderList}};

% List orders
handle_call({list}, _From, State) ->
    {{Floor, Dir, AtFloor}, OrderList} = State,
    io:format("At ~p, moving ~p; ~p~n", [Floor, Dir, AtFloor]),
    io:format("~p~n", [OrderList]),
    io:format("~p orders in backlog~n", [length(OrderList)]),
	list_orders(ets:first(?ORTAB)),
	{reply, ok, State}.


% Timeout
handle_info(timer, State) ->
	erlang:send_after(?TOUTCHINT, self(), timer),
    check_for_timeout(ets:match(?ORTAB, {{{ext, '$1'}, '$2'}, claimed, '$3'})),
	{noreply, State};

handle_info(_Msg, State) ->
	{noreply, State}.

% Be assigned order
handle_cast({assign, Key}, {State, OldList}) ->
    NewList = case lists:member(Key, OldList) of
                  true ->
                      OldList;
                  %false when length(OldList) == 0 ->
                  %    environment_controller:goto_order(Key),
                  %    OldList ++ [Key];
                  false ->
                  	OldList ++ [Key]
              end,
    {noreply, {State, NewList}};

handle_cast(_Msg, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%% Helper functions
set_button_light(Key, State) ->
    {{Type, Dir}, Floor} = Key,
    Node = node(),
    case Type of
        ext ->
            elevator_driver:set_button_light(Dir, Floor, State);
            %io:format("Floor ~p, Dir ~p, ~p~n", [Floor, Dir, State]);
        int when Dir == Node ->
            %io:format("Floor ~p, internal, ~p~n", [Floor, State]),
            elevator_driver:set_button_light(int, Floor, State);
        _ -> ok
    end.


sync_orders() ->
    rpc:multicall(?MODULE, helper_sync, []).

list() ->
	gen_server:call(?MODULE, {list}).

list_orders('$end_of_table') ->
	ok;
list_orders(Order) ->
	io:format("~p~n", [ets:lookup(?ORTAB, Order)]),
	list_orders(ets:next(?ORTAB, Order)).

check_for_timeout([]) ->
	ok;
check_for_timeout([[Type, Floor, Timestamp]|Tail]) ->
	Now = erlang:monotonic_time(),
	Elapsed = erlang:convert_time_unit(Now - Timestamp, native, millisecond),
	if Elapsed >= ?ORTOUT ->
           alter_order(Type, Floor, timeout);
	   true ->
		   ok
	end,
	check_for_timeout(Tail).

make_order_key(Type) ->
	Key = case Type of
		      int ->
			      {int, node()};
		      _ ->
			      {ext, Type}
	      end,
    Key.

helper_sync() ->
    helper_sync(ets:first(?ORTAB)).
helper_sync('$end_of_table') ->
    ok;
helper_sync(Key) ->
    Order = ets:lookup(?ORTAB, Key),
    rpc:multicall(gen_server, call, [?MODULE, {store, Order}]),
    helper_sync(ets:next(?ORTAB, Key)).
