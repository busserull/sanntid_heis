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
    {{{Type, Dir}, _Floor}, Status, _Time} = ets:lookup(?ORTAB, Key),
    Node = node(),
    LegalType = case Type of
                    int when Node == Dir ->
                        true;
                    ext ->
                        true;
                    _ ->
                        false
                end,
    LegalStatus = case Status of
                      queued ->
                          true;
                      timeout ->
                          true;
                      _ ->
                          false
                  end,
    Legal = LegalType and LegalStatus,

    TotalCost = case Legal of
                    false ->
                        invalid;
                    true ->
                        get_cost(ElevFloor, ElevDir, Key)
                end,
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
                  ext ->
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