-module(dist2).
-behavior(gen_server).

-define(ELEVNAME, "beastly_beast@").
-define(COOKIE, 'beistheis').
-define(PEER_DISC_PORT, 23600).
-define(BROADCAST_TIME, 1000).

%% public API
-export([start/0, send/0]).
%% gen_server callback functions
-export([init/1, handle_call/3, handle_cast/2,
		handle_info/2, terminate/2, code_change/3]).

%% API
start() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

send() ->
	gen_server:call(?MODULE, {broadcast}).
	

%% callbacks
init([]) ->
	erlang:send_after(?BROADCAST_TIME, self(), {broadcast}),
	net_kernel:start([get_hostname(), longnames]),
	erlang:set_cookie(node(), ?COOKIE),
	{ok, Socket} = gen_udp:open(?PEER_DISC_PORT, [{broadcast, true}, binary]),
	{ok, Socket}.

handle_call({broadcast}, _From, State) ->
	Msg = list_to_binary(io_lib:format("~p", [get_hostname()])),
	gen_udp:send(State, {255,255,255,255}, ?PEER_DISC_PORT, Msg),
	{reply, ok, State}.

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info({broadcast}, State) ->
	erlang:send_after(?BROADCAST_TIME, self(), {broadcast}),
	Msg = list_to_binary(io_lib:format("~p", [get_hostname()])),
	gen_udp:send(State, {255,255,255,255}, ?PEER_DISC_PORT, Msg),
	{noreply, State};
handle_info({udp, _ErPort, _IP, _Port, Bin_msg}, State) ->
	[_ | Node] = lists:droplast(binary_to_list(Bin_msg)),
	net_kernel:connect(list_to_atom(Node)),
	{noreply, State};
handle_info(_Msg, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% help functions
get_hostname() ->
	{ok, [IPTuple | _]} = inet:getif(),
	OurIP = tuple_to_list(element(1, IPTuple)),
	[_ | Tail] = lists:flatmap(fun(Seg) -> [".", Seg] end, OurIP),
	list_to_atom(lists:concat([?ELEVNAME | Tail])).
