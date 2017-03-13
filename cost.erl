-module(cost).

-define(ORTAB, ordertable).
-define(INT_PENALTY, 0).
-define(EXT_PENALTY, 1).
-define(TIMEOUT_PENALTY, 0).
-define(QUEUED_PENALTY, 1).
-define(SAME_DIR_PENALTY, 0).
-define(DIFF_DIR_PENALTY, 1).
-define(INITIAL_COST, 500).

-export([optimal/3, get_cost/3, sort_on_distance/2]).

optimal(ElevFloor, ElevDir, Key) ->
    optimal(ElevFloor, ElevDir, Key, ?INITIAL_COST, none).

%%% Helper functions
optimal(_ElevFloor, _ElevDir, '$end_of_table', _BestCost, Best) ->
    Best;

optimal(ElevFloor, ElevDir, Key, BestCost, Best) ->
    TotalCost = get_cost(ElevFloor, ElevDir, Key),
    if
        TotalCost < BestCost ->
            optimal(ElevFloor, ElevDir, ets:next(?ORTAB, Key), TotalCost, Key);
        true ->
            optimal(ElevFloor, ElevDir, ets:next(?ORTAB, Key), BestCost, Best)
    end.

get_cost(ElevFloor, ElevDir, Key) ->
    Order = ets:lookup(?ORTAB, Key),
    [{{{Type, Dir}, Floor}, Status, _}] = Order,
    TypePen = case Type of
                  int ->
                      ?INT_PENALTY;
                  _Ext ->
                      ?EXT_PENALTY
              end,
    Node = node(),
    DirPen = case Dir of
                 Node ->
                     ?INT_PENALTY;
                 ElevDir ->
                     ?SAME_DIR_PENALTY;
                 _ ->
                     ?DIFF_DIR_PENALTY
             end,
    FloorPen = abs(Floor - ElevFloor),
    StatusPen = case Status of
                    queued ->
                        ?QUEUED_PENALTY;
                    timeout ->
                        ?TIMEOUT_PENALTY
                end,
    TotalCost = TypePen + DirPen + FloorPen + StatusPen,
    TotalCost.

sort_on_distance(List, {Floor, Dir, Pos}) ->
    Sort_fun = 
    fun({{int, _Node}, Floor}, {_State, _Floor}) when Pos =:= at_floor ->
        true;
        ({{ext, Dir}, Floor}, {_State, _Floor}) when Pos =:= at_floor ->
        true;
        ({{int, _Node}, FloorA}, {{int, _Node}, FloorB}) ->
            case dir of
              up when FloorA > Floor, FloorB > Floor ->
                FloorA =< FloorB;
              up when FloorA > Floor ->
                true;
              up when FloorB > Floor ->
                false;
              up ->
                FloorA > FloorB;
              down when FloorA < Floor, FloorB < Floor ->
                FloorA >= FloorB;
              down when FloorA < Floor ->
                true;
              down when FloorB < Floor ->
                false;
              down ->
                FloorA < FloorB
            end;
        ({{ext, up}, FloorA}, {{ext, up}, FloorB}) when FloorA > Floor, FloorB > Floor ->
        FloorA =< FloorB;
        ({{ext, up}, FloorA}, {{ext, up}, FloorB}) when FloorA > Floor, FloorB > Floor ->
        FloorA =< FloorB
    end,
    lists:sort(Sort_fun, List).