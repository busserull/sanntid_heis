-module(bl3).
-behavior(gen_server).

% Names an ETS table
-define(ORTAB, ordertable).

-export([start/0, store_order/2, list_orders/0]).
-export([init/1, handle_call/3, handle_cast/2,
		handle_info/2, terminate/2, code_change/3]).

%%% Backlog interface

start() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

store_order(Type, Floor) ->
	gen_server:call(?MODULE, {store, {Type, Floor}}).

list_orders() ->
	gen_server:call(?MODULE, {list}).

%%% Server callbacks

init([]) ->
	ets:new(?ORTAB, [set, named_table]),
	State = 0,
	{ok, State}.

handle_call({store, {Type, Floor}}, _From, State) ->
	Order = {{Type, Floor}, {127,0,0,1}, queued, erlang:monotonic_time(), 0},
	ets:insert(?ORTAB, Order),
	{reply, ok, (State + 1)};
handle_call({list}, _From, State) ->
	list_orders(ets:first(?ORTAB)),
	{reply, ok, State}.

handle_cast(_Msg, State) ->
	{noreply, State}.
handle_info(_Msg, State) ->
	{noreply, State}.
terminate(_Reason, _State) ->
	ok.
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%% Help functions

list_orders('$end_of_table') ->
	ok;
list_orders(Order) ->
	io:format("~p~n", [ets:lookup(?ORTAB, Order)]),
	list_orders(ets:next(?ORTAB, Order)).

