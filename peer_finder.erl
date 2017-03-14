-module(peer_finder).
-behavior(gen_server).

-define(ELEVNAME, "beastly_beast@").
-define(COOKIE, 'beistheis').
-define(PEER_DISC_PORT, 23600).
-define(BROADCAST_INTERVAL, 1000).
-define(BACKLOG_MODULE, order_backlog).

%% public API
-export([start/0]).
%% gen_server callback functions
-export([init/1, handle_info/2, terminate/2,
         handle_call/3, handle_cast/2, code_change/3]).

%% API
start() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% callbacks
init([]) ->
	net_kernel:start([get_hostname(), longnames]),
	erlang:set_cookie(node(), ?COOKIE),
	{ok, Socket} = gen_udp:open(?PEER_DISC_PORT, [{broadcast, true}, binary]),
    erlang:send_after(?BROADCAST_INTERVAL, self(), broadcast),
	{ok, Socket}.

handle_info(broadcast, Socket) ->
	Msg = list_to_binary(io_lib:format("~p", [get_hostname()])),
	gen_udp:send(Socket, {255,255,255,255}, ?PEER_DISC_PORT, Msg),
    erlang:send_after(?BROADCAST_INTERVAL, self(), {broadcast}),
	{noreply, Socket};

handle_info({udp, _ErPort, _IP, _Port, BinMsg}, Socket) ->
	[_ | Node] = lists:droplast(binary_to_list(BinMsg)),
    LengthBefore = length(nodes()),
	net_kernel:connect(list_to_atom(Node)),
    LengthAfter = length(nodes()),
    Change = LengthAfter - LengthBefore,
    case Change of
        0 ->
            ok;
        _ ->
            ?BACKLOG_MODULE:sync_orders()
    end,
	{noreply, Socket};

handle_info(_Msg, Socket) ->
	{noreply, Socket}.

terminate(_Reason, Socket) ->
    gen_udp:close(Socket),
    ok.

handle_call(_Msg, _From, Socket) -> {reply, ok, Socket}.

handle_cast(_Msg, Socket) -> {noreply, Socket}.


code_change(_OldVsn, Socket, _Extra) ->
	{ok, Socket}.

%% help functions
get_hostname() ->
	{ok, [IPTuple | _]} = inet:getif(),
	OurIP = tuple_to_list(element(1, IPTuple)),
	[_ | Tail] = lists:flatmap(fun(Seg) -> [".", Seg] end, OurIP),
	list_to_atom(lists:concat([?ELEVNAME | Tail])).
