-module(dist).
-export([start/0]).

-define(ELEVNAME, "beastly_beast@").
-define(COOKIE, 'beistheis').
-define(PEER_DISC_PORT, 23600).
-define(BROADCAST_TIME, 15000).

start() ->
	net_kernel:start([get_hostname(), longnames]),
	erlang:set_cookie(node(), ?COOKIE),
	{ok, Socket} = gen_udp:open(?PEER_DISC_PORT, [{broadcast, true}, binary]).

get_hostname() ->
	{ok, [IPTuple | _]} = inet:getif(),
	OurIP = tuple_to_list(element(1, IPTuple)),
	[_ | Tail] = lists:flatmap(fun(Seg) -> [".", Seg] end, OurIP),
	list_to_atom(lists:concat([?ELEVNAME | Tail])).
