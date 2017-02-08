-module(backlog).
-compile(export_all).
%% I swear this is the last time!
%% God damn it Earl!

-include("order.hrl").

storeOrder(From, Pid, Order) ->
    Pid ! {From, {store, Order}}.

completeOrder(From, Pid, Order) ->
    Pid ! {From, {complete, Order}}.

runBacklog(Backlog) ->
    receive
        {From, {store, Order}} ->
            From ! ok,
            runBacklog([Backlog|[Order]]);
            %case lists:member(Order, Backlog) of
            %    true ->
            %        From ! ok,
            %        runBacklog(Backlog);
            %    false ->
            %        From ! ok,
            %        runBacklog([Backlog|[Order]])
            %end;
        {From, {complete, Order}} ->
            From ! ok,
            runBacklog(lists:delete(Order, Backlog));
        {From, {update, Order}} ->
            OldOrder = #order{type = Order#order.type, floor = Order#order.floor},
            From ! ok,
            runBacklog([lists:delete(OldOrder, Backlog)|[Order]]);
        debug ->
            debugPrint(Backlog),
            runBacklog(Backlog);
        {From, _} ->
            From ! unrecognized_msg,
            runBacklog(Backlog)
    end.

%% This will read the local file later
init() ->
    runBacklog([]).

debugPrint([]) ->
    io:format("~n");
debugPrint([A|Rest]) ->
    {order, Type, Floor, State} = A,
    io:format("~p, ~p, ~p~n", [Type, Floor, State]),
    debugPrint(Rest).

debug(Pid) ->
    Pid ! debug.
