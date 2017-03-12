-module(backlog).
-behavior(gen_server).

% Names of ETS order table
-define(ORTAB, ordertable).
% Time before an order times out in ms
-define(ORTOUT, 5000).
% Order timeout check interval in ms
-define(TOUTCHINT, 1000).

-export([start/0, store_order/2, claim_order/2, clear_order/2, list/0, sync_orders/0]).

-export([init/1, handle_call/3, handle_cast/2,
		handle_info/2, terminate/2, code_change/3]).

%%% Backlog interface

-spec start() -> ok.
-spec store_order(up|down|int, Floor::integer()) -> ok.
-spec claim_order(up|down|int, Floor::integer()) -> ok.
-spec clear_order(up|down|int, Floor::integer()) -> ok.

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
    rpc:multicall(gen_server, call, [?MODULE, {store, Order}]).

claim_order(Type, Floor) ->
    gen_server:call(?MODULE, {claim, {Type, Floor}}).

clear_order(Type, Floor) ->
	gen_server:call(?MODULE, {clear, {Type, Floor}}).

sync_orders() ->
    share_all().
%    rpc:multicall(?MODULE, helper_sync, []).
    %gen_server:call(?MODULE, sync).
%    rpc:multicall(gen_server, call, [?MODULE, sync]).
%
%%%%%%%%%%%%%%%%%%%%%
share_all() ->
    share_all(ets:first(?ORTAB)).
share_all('$end_of_table') ->
    ok;
share_all(Key) ->
    Order = ets:lookup(?ORTAB, Key),
    rpc:multicall(gen_server, call, [?MODULE, {store, Order}]),
    share_all(ets:next(?ORTAB, Key)).


-spec list() -> ok.
list() ->
	gen_server:call(?MODULE, {list}).


%%% Server callbacks

init([]) ->
	erlang:send_after(?TOUTCHINT, self(), {timer}),
	ets:new(?ORTAB, [set, named_table]),
	State = 0,
	{ok, State}.

% Store order
handle_call({store, Order}, _From, State) ->
    ets:insert(?ORTAB, Order),
    {reply, ok, (State + 1)};

handle_call(sync, _From, State) ->
    io:format("Syncing...~n"),
    %helper_sync(ets:first(?ORTAB)),
%    gen_server:call(?MODULE, {sync, ets:first(?ORTAB)}),
    {reply, ok, State};

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
	{reply, ok, State};

handle_call({helper_insert, Order}, _From, State) ->
    ets:insert(?ORTAB, Order),
    {reply, ok, State};

handle_call({share_all, '$end_of_table'}, _From, State) ->
    {reply, ok, State};
handle_call({share_all, Order}, _From, State) ->
    rpc:multicall(?MODULE, insert, [Order]),
    {reply, ok, State}.

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info({timer}, State) ->
	erlang:send_after(?TOUTCHINT, self(), {timer}),
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

%%% Helper functions
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
	Elapsed = erlang:convert_time_unit(Now - Timestamp, native, millisecond),
	if Elapsed >= ?ORTOUT ->
		   io:format("{~p, ~p} timed out!~n", [ext, Floor]),
		   ets:update_element(?ORTAB, {ext, Floor}, {3, timeout});
	   true ->
		   ok
	end,
	check_for_timeout(Tail).

