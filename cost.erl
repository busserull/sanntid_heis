-module(cost).

-define(ORTAB, ordertable).
-define(INT_PENALTY, 0).
-define(EXT_PENALTY, 1).
-define(TIMEOUT_PENALTY, 0).
-define(QUEUED_PENALTY, 1).
-define(SAME_DIR_PENALTY, 0).
-define(DIFF_DIR_PENALTY, 1).
-define(INITIAL_COST, 500).

-export([optimal/3]).

optimal(ElevFloor, ElevDir, Key) ->
    optimal(ElevFloor, ElevDir, Key, ?INITIAL_COST, none).

%%% Helper functions
optimal(_ElevFloor, _ElevDir, '$end_of_table', _BestCost, Best) ->
    Best;

optimal(ElevFloor, ElevDir, Key, BestCost, Best) ->
    TotalCost = get_cost(ElevFloor, ElevDir, Key),
    case TotalCost of
        invalid ->
            optimal(ElevFloor, ElevDir, ets:next(?ORTAB, Key), BestCost, Best);
        Cost when Cost < BestCost ->
            optimal(ElevFloor, ElevDir, ets:next(?ORTAB, Key), TotalCost, Key);
        _ ->
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
                        ?TIMEOUT_PENALTY;
                    _ ->
                        invalid
                end,
    TotalCost = case StatusPen of
                    invalid ->
                        invalid;
                    _ ->
                        TypePen + DirPen + FloorPen + StatusPen
                end,
    TotalCost.