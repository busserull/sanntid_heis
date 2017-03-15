-module(cost).

-define(ORTAB, ordertable).
-define(INT_PENALTY, 0).
-define(EXT_PENALTY, 1).
-define(TIMEOUT_PENALTY, 0).
-define(QUEUED_PENALTY, 2).
-define(MINE_ORDER_PENALTY, 2).
-define(SAME_DIR_PENALTY, 0).
-define(DIFF_DIR_PENALTY, 10).
-define(INITIAL_COST, 500).

-export([optimal/4]).

optimal(ElevFloor, ElevDir, StartKey, OldOrder) ->
    OldCost = case OldOrder of
      none -> ?INITIAL_COST;
      _Old -> get_cost(ElevFloor, ElevDir, OldOrder)
    end,
    optimal(ElevFloor, ElevDir, StartKey, OldCost, OldOrder).


%%% Helper functions
optimal(_ElevFloor, _ElevDir, '$end_of_table', _BestCost, Best) ->
    Best;

optimal(ElevFloor, ElevDir, Key, BestCost, Best) ->
    [{{{Type, Dir}, _Floor}, Status, _Time}] = ets:lookup(?ORTAB, Key),
    Node = node(),
    LegalType = case Type of
                    int when Node == Dir ->
                        true;
                    ext ->
                        true;
                    _ ->
                        false
                end,
    Node = node(),
    LegalStatus = case Status of
                      queued ->
                          true;
                      timeout ->
                          true;
                      {claimed, Node} ->
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
    IntNode = node(),
    TopFloor = get_env(number_of_floors) - 1,
    DirPen = 
    case Dir of
        IntNode when ElevDir == up, ElevFloor =< Floor -> ?SAME_DIR_PENALTY;
        IntNode when ElevDir == down, ElevFloor >= Floor -> ?SAME_DIR_PENALTY;
        ElevDir when ElevDir == up, ElevFloor =< Floor -> ?SAME_DIR_PENALTY;
        ElevDir when ElevDir == down, ElevFloor >= Floor -> ?SAME_DIR_PENALTY;
        _OrdDir when ElevDir == up, Floor == TopFloor -> ?SAME_DIR_PENALTY;
        _OrdDir when ElevDir == down, Floor == 0 -> ?SAME_DIR_PENALTY;
        _OrdDir when ElevDir == stop -> ?SAME_DIR_PENALTY;
        _OrdDir -> ?DIFF_DIR_PENALTY
    end,

    FloorPen = 2*abs(Floor - ElevFloor),

    StatusPen = case Status of
                    queued ->
                        ?QUEUED_PENALTY;
                    timeout ->
                        ?TIMEOUT_PENALTY;
                    {claimed, IntNode} -> 
                        ?MINE_ORDER_PENALTY
                end,
    TotalCost = TypePen + DirPen + FloorPen + StatusPen,
    TotalCost.

get_env(Environment)->
    {ok,Value} = application:get_env(Environment),
    Value.

