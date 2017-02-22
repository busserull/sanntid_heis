-module(backlog2).
-behavior(gen_server).

-define(ORDERDB, ordertable).
-define(STORETIMEOUT, 500).

% Interface
-export([start_link/0, stop/0, store_order/2, list_order/0]).
% Server callbacks
-export([init/1, handle_call/3, handle_cast/2,
		handle_info/2, terminate/2, code_change/3]).

start_link() ->
	io:format("Called start_link()~n"),
	gen_server:start_link(?MODULE, [], []).

stop() ->
	io:format("Shutting server down~n"),
	gen_server:cast(?MODULE, terminate).

store_order(Type, Floor) ->
	Order = {{Type, Floor}, {127,0,0,1}, queued, erlang:monotonic_time(), 0},
	ets:insert(?ORDERDB, Order).
	%gen_server:call(?MODULE, {store, Order}, ?STORETIMEOUT).

list_order() ->
	list_order(ets:first(?ORDERDB)).
list_order(Order) ->
	case Order of
		'$end_of_table' ->
			{ok};
		_ ->
			io:format("~p~n", [Order]),
			list_order(ets:next(?ORDERDB, Order))
	end.

%% ===================
%% Server callbacks
%% ===================
init([]) ->
	io:format("Called init()~n"),
	ets:new(?ORDERDB, [set, protected, named_table, {keypos, 1},
						 {heir, none}, {read_concurrency, false},
						 {write_concurrency, false}]),
	{ok, ?ORDERDB}.

handle_call(Msg, From, State) ->
	io:format("Received message [~p, ~p, ~p]~n", [Msg, From, State]),
	{reply, ok, State}.

handle_cast(shutdown, State) ->
	io:format("Received shutdown command~n"),
	{shutdown, normal, State};
handle_cast(Msg, State) ->
	io:format("Generic async ~p, ~p~n", [Msg, State]),
	{noreply, State}.

handle_info(_Msg, _Server) ->
	io:format("Generic info handler, ~p, ~p~n", [_Msg, _Server]),
	{noreply, _Server}.

terminate(_Reason, _Server) ->
	io:format("Generic termination handler~n").

code_change(_OldVsn, _Server, _Extra) ->
	{ok, _Server}.
