-module(backlog2).
-behavior(gen_server).

-define(ORDERDB, ordertable).
-define(STORETIMEOUT, 500).

% Interface
-export([start/1, stop/0, store_order/2]).
% Server callbacks
-export([init/1, handle_call/3, handle_cast/2,
		handle_info/2, terminate/2, code_change/3]).

start(Args) ->
	io:format("Starting server with ~p~n", [Args]),
	gen_server:start_link({local, ?MODULE}, ?MODULE, [Args], []).

stop() ->
	io:format("Shutting server down~n"),
	gen_server:cast(?MODULE, shutdown).

store_order(Type, Floor) ->
	Order = {{Type, Floor}, {127,0,0,1}, queued, erlang:monotonic_time(), 0},
	gen_server:call(?MODULE, {store, Order}, ?STORETIMEOUT).

%% ===================
%% Server callbacks
%% ===================
init([Args]) ->
	io:format("Initializing server with ~p~n", [Args]),
	ets:new(?ORDERDB, [set, protected, named_table, {keypos, 5},
						 {heir, none}, {read_concurrency, false},
						 {write_concurrency, false}]),
	%process_flag(trap_exit, true),
	{ok, initialized}.

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
