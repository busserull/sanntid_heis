-module(backlog).
-behavior(gen_server).

-define(ORTOUT, 5000). % Order timeout (ms)
-define(TOUTCHINT, 1000). % Timeout check interval (ms)
-define(ORTAB, ordertable).

%%% API
-export([start/0, store_order/2, alter_order/3, notify_state/3,
         get_order/0, list/0]).
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

notify_state(ElevFloor, ElevDir, AtFloor) ->
    gen_server:call(?MODULE, {notify, {ElevFloor, ElevDir, AtFloor}}).

alter_order(Type, Floor, NewState) ->
    Key = {make_order_key(Type), Floor},
    rpc:multicall(gen_server, call, [?MODULE, {alter, Key, NewState}]).

get_order() ->
    gen_server:call(?MODULE, get_order).

%%% Server callbacks

% Init
init([]) ->
	ets:new(?ORTAB, [set, named_table]),
    erlang:send_after(?TOUTCHINT, self(), timer),
	{ok, {{0, stop, at_floor}, none}}.
    % Likely incorrect inital state, this only
    % impacts the first time get_cost is called

% Store order
handle_call({store, Order}, _From, State) ->
    ets:insert(?ORTAB, Order),
    {Key, _Status, _Timestamp} = Order,
    Key,
    %set_button_light(Key, on), %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% -> X
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

% Get order %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Updates State
handle_call(get_order, _From, {State, OldOrder}) ->
    {ElevFloor, Dir, AtFloor} = State,
    % If none, attempt to get some, then update global backlog
    CurrentOrder = case OldOrder of
                       none when AtFloor == at_floor ->
                           cost:optimal(ElevFloor, Dir, ets:first(?ORTAB));
                       none when AtFloor == in_the_void andalso Dir == up ->
                           cost:optimal(ElevFloor + 1, Dir, ets:first(?ORTAB));
                       none when AtFloor == in_the_void andalso Dir == down ->
                           cost:optiaml(ElevFloor - 1, Dir, ets:first(?ORTAB));
                       _ ->
                           OldOrder
                   end,
    %
    Diff = case CurrentOrder of
               none ->
                   none;
               _ ->
                   element(2, CurrentOrder) - ElevFloor
           end,
    {NewOrder, Command} = case Diff of
                              none ->
                                  {none, none};
                              Num when Num == 0 ->
                                  {none, open_door};
                              Num when Num > 0 ->
                                  {CurrentOrder, up};
                              Num when Num < 0 ->
                                  {CurrentOrder, down}
                          end,
    {reply, Command, {State, NewOrder}};

% Update known elevator state %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Updates State
handle_call({notify, NewState}, _From, {_OldState, OrderList}) ->
    io:format("New state: ~p~n", [NewState]),
    {reply, ok, {NewState, OrderList}};

% List orders
handle_call({list}, _From, State) ->
    {{Floor, Dir, AtFloor}, CurrentOrder} = State,
    io:format("At ~p, moving ~p; ~p~n", [Floor, Dir, AtFloor]),
    io:format("Current order: ~p~n", [CurrentOrder]),
	list_orders(ets:first(?ORTAB)),
	{reply, ok, State}.


% Timeout
handle_info(timer, State) ->
	erlang:send_after(?TOUTCHINT, self(), timer),
    check_for_timeout(ets:match(?ORTAB, {{{ext, '$1'}, '$2'}, claimed, '$3'})),
	{noreply, State};

handle_info(_Msg, State) ->
	{noreply, State}.

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
