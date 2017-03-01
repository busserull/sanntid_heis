-module(dist).
-export([start/0]).

-define(ELEVNAME, "beastly_beast@").
-define(COOKIE, 'beistheis').

start() ->
	net_kernel:start([get_ip(), longnames]),
	erlang:set_cookie(node(), ?COOKIE).

get_ip() ->
	{ok, [IPTuple | _]} = inet:getif(),
	OurIP = tuple_to_list(element(1, IPTuple)),
	[_ | Tail] = lists:flatmap(fun(Seg) -> [".", Seg] end, OurIP),
	list_to_atom(lists:concat([?ELEVNAME | Tail])).
