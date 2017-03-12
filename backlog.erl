-module(backlog).
-behavior(gen_server).

% Names of ETS order table
-define(ORTAB, ordertable).
% Time before a claimed order times out in ms
-define(ORTOUT, 5000).
% Order timeout check interval in ms
-define(TOUTCHINT, 1000).

-export([start/0, store_order/2, alter_order/3, claim_order/2, list/0, sync_orders/0]).
-export([helper_sync/0, make_order_key/1]).

-export([init/1, handle_call/3, handle_cast/2,
		handle_info/2, terminate/2, code_change/3]).

%%% Backlog interface

-spec start() -> ok.
-spec store_order(up|down|int, Floor::integer()) -> ok.
-spec alter_order(up|down|int, Floor::integer(), claimed|timeout|complete) -> ok.
-spec claim_order(up|down|int, Floor::integer()) -> ok.

start() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

store_order(Type, Floor) ->
    Key = make_order_key(Type),
	Order = {{Key, Floor}, queued, erlang:monotonic_time()},
    rpc:multicall(gen_server, call, [?MODULE, {store, Order}]).

alter_order(Type, Floor, NewState) ->
    Key = {make_order_key(Type), Floor},
    rpc:multicall(gen_server, call, [?MODULE, {alter, Key, NewState}]).

claim_order(Type, Floor) ->
    gen_server:call(?MODULE, {claim, {Type, Floor}}).

sync_orders() ->
    rpc:multicall(?MODULE, helper_sync, []).

-spec list() -> ok.
list() ->
	gen_server:call(?MODULE, {list}).

%%% Server callbacks

% Init
init([]) ->
	erlang:send_after(?TOUTCHINT, self(), {timer}),
	ets:new(?ORTAB, [set, named_table]),
	State = 0,
	{ok, State}.

% Store order
handle_call({store, Order}, _From, _State) ->
    ets:insert(?ORTAB, Order),
    {reply, ok, ets:info(?ORTAB, size)};

% Alter order
handle_call({alter, Key, complete}, _From, _State) ->
    {{Type, Node}, Floor} = Key,
    ets:delete(?ORTAB, {{ext, up}, Floor}),
    ets:delete(?ORTAB, {{ext, down}, Floor}),
    case Type of
        int ->
            ets:delete(?ORTAB, {{int, Node}, Floor});
        _ ->
            ok
    end,
    {reply, ok, ets:info(?ORTAB, size)};
handle_call({alter, Key, NewState}, _From, State) ->
    Now = erlang:monotonic_time(),
    ets:update_element(?ORTAB, Key, [{2, NewState}, {3, Now}]),
    {reply, ok, State};

handle_call(sync, _From, State) ->
    io:format("Syncing...~n"),
    helper_sync(),
    %helper_sync(ets:first(?ORTAB)),
%    gen_server:call(?MODULE, {sync, ets:first(?ORTAB)}),
    {reply, ok, State};

handle_call({claim, {Type, Floor, IP}}, _From, State) ->
	Now = erlang:monotonic_time(),
	ets:update_element(?ORTAB,{Type, Floor},[{2,IP},{3,claimed},{4,Now}]),
	{reply, ok, State};

handle_call({list}, _From, State) ->
	io:format("Number of elements = ~p~n",[State]),
	list_orders(ets:first(?ORTAB)),
	{reply, ok, State}.

handle_cast(_Msg, State) ->
	{noreply, State}.

% Timeout
handle_info({timer}, State) ->
	erlang:send_after(?TOUTCHINT, self(), {timer}),
    check_for_timeout(ets:match(?ORTAB, {{{ext, '$1'}, '$2'}, claimed, '$3'})),
	{noreply, State};

handle_info(_Msg, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%% Helper functions

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
		   io:format("{~p, ~p} timed out!~n", [ext, Floor]),
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
