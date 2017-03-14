-module(backlog).
-behavior(gen_server).

-define(ORTOUT, 5000). % Order timeout (ms)
-define(TOUTCHINT, 1000). % Timeout check interval (ms)
-define(ORTAB, ordertable).

%%% API
-export([start/0, store_order/2, get_order/3]).
%%% Helper functions
-export([sync_orders/0, helper_sync/0, alter_order/2]).
%%% Server callback functions
-export([init/1, handle_call/3, handle_info/2,
		handle_cast/2, terminate/2, code_change/3]).

%%% Backlog interface

-spec start() -> ok.
-spec store_order(up|down|int, Floor::integer()) -> ok.
-spec get_order(ElevFloor::integer(), up|down|stop, at_floor|in_the_void) ->
    up|down|none|open_door.

start() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

store_order(Type, Floor) ->
	Key = case Type of
		      int ->
			      {int, node()};
		      _ ->
			      {ext, Type}
	      end,
	Order = {{Key, Floor}, queued, erlang:monotonic_time()},
    rpc:eval_everywhere(gen_server, call, [?MODULE, {store, Order}]).

get_order(ElevFloor, ElevDir, AtFloor) ->
    gen_server:call(?MODULE, {get_order, ElevFloor, ElevDir, AtFloor}).

%%% Server callbacks

% Init
init([]) ->
	ets:new(?ORTAB, [set, named_table]),
    erlang:send_after(?TOUTCHINT, self(), timer),
	{ok, none}.

% Store order
handle_call({store, Order}, _From, State) ->
    ets:insert(?ORTAB, Order),
    {Key, _Status, _Timestamp} = Order,
    set_button_light(Key, on),
    {reply, ok, State};

% Get order
handle_call({get_order, ElevFloor, Dir, AtFloor}, _From, OldOrder) ->
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
    case CurrentOrder of
        OldOrder ->
            ok;
        _ ->
            alter_order(CurrentOrder, claimed)
    end,
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
                                  alter_order(CurrentOrder, complete),
                                  {none, open_door};
                              Num when Num > 0 ->
                                  {CurrentOrder, up};
                              Num when Num < 0 ->
                                  {CurrentOrder, down}
                          end,
    {reply, Command, NewOrder}.

% Timeout
handle_info(timer, State) ->
	erlang:send_after(?TOUTCHINT, self(), timer),
    check_for_timeout(ets:match(?ORTAB, {{{ext, '$1'}, '$2'}, claimed, '$3'})),
	{noreply, State};

handle_info(_Msg, State) ->
	{noreply, State}.

% Alter order
handle_cast({alter, Key, complete}, State) ->
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
    {noreply, State};
handle_cast({alter, Key, NewState}, State) ->
    Now = erlang:monotonic_time(),
    ets:update_element(?ORTAB, Key, [{2, NewState}, {3, Now}]),
    {noreply, State};

handle_cast(_Msg, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%% Helper functions
alter_order(Key, NewState) ->
    rpc:eval_everywhere(gen_server, cast, [?MODULE,
                 {alter, Key, NewState}]).

set_button_light(Key, State) ->
    {{Type, Dir}, Floor} = Key,
    Node = node(),
    case Type of
        ext ->
            elevator_driver:set_button_light(Dir, Floor, State);
        int when Dir == Node ->
            elevator_driver:set_button_light(int, Floor, State);
        _ -> ok
    end.


sync_orders() ->
    rpc:eval_everywhere(?MODULE, helper_sync, []).

check_for_timeout([]) ->
	ok;
check_for_timeout([[Type, Floor, Timestamp]|Tail]) ->
	Now = erlang:monotonic_time(),
	Elapsed = erlang:convert_time_unit(Now - Timestamp, native, millisecond),
	if Elapsed >= ?ORTOUT ->
           alter_order({{ext, Type}, Floor}, timeout);
	   true ->
		   ok
	end,
	check_for_timeout(Tail).

helper_sync() ->
    helper_sync(ets:first(?ORTAB)).
helper_sync('$end_of_table') ->
    ok;
helper_sync(Key) ->
    [Order] = ets:lookup(?ORTAB, Key),
    rpc:eval_everywhere(gen_server, call, [?MODULE, {store, Order}]),
    helper_sync(ets:next(?ORTAB, Key)).
