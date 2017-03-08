-module(dist).
-behavior(gen_statem).

%% public API
-export([start/0, send/0]).
%% gen_statem callback functions
-export([terminate/3, code_change/4, init/1, callback_mode/0]).
%% states
-export([broadcast/3]).

-define(ELEVNAME, "beastly_beast@").
-define(COOKIE, 'beistheis').
-define(PEER_DISC_PORT, 23600).
-define(BROADCAST_TIME, 15000).

%% public API
start() ->
	gen_statem:start({local, ?MODULE}, ?MODULE, [], []).

send() ->
	gen_statem:call(?MODULE, broadcast).

%% gen_statem callback functions
terminate(_Reason, _State, _Data) ->
	ok.

code_change(_OldVsn, State, Data, _Extra) ->
	{ok, State, Data}.

init([]) ->
	net_kernel:start([get_hostname(), longnames]),
	erlang:set_cookie(node(), ?COOKIE),
	{ok, Socket} = gen_udp:open(?PEER_DISC_PORT, [{broadcast, true}, binary]),
	{ok, broadcast, Socket}.

callback_mode() ->
	state_functions.

%% states
broadcast({call, From}, broadcast, Socket) ->
	Msg = list_to_binary(io_lib:format("~p", [get_hostname()])),
	gen_udp:send(Socket, {255,255,255,255}, ?PEER_DISC_PORT, Msg),
	{next_state, broadcast, Socket, [{reply, From, sent}]}.

%% help functions
get_hostname() ->
	{ok, [IPTuple | _]} = inet:getif(),
	OurIP = tuple_to_list(element(1, IPTuple)),
	[_ | Tail] = lists:flatmap(fun(Seg) -> [".", Seg] end, OurIP),
	list_to_atom(lists:concat([?ELEVNAME | Tail])).
