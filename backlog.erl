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
            case lists:member(Order, Backlog) of
                true ->
                    From ! ok;
                false ->
                    From ! ok,
                    runBacklog([Backlog|Order])
            end;
        {From, {complete, Order}} ->
            From ! ok,
            runBacklog(lists:delete(Order, Backlog));
        {From, {update, Order}} ->
            OldOrder = #order{type = Order#order.type, floor = Order#order.floor},
            From ! ok,
            runBacklog([lists:delete(OldOrder, Backlog)|Order]);
        {From, _} ->
            From ! unrecognized_msg
    end.

%% This will read the local file later
init() ->
    runBacklog([]).
