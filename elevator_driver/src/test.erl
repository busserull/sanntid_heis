-module (test).
-export ([test/0, testExecutionTime/0, pollFloor/1]).

-define(NUMBER_OF_FLOORS,4).
-define(POLL_PERIOD, 50).
-define(BUTTTON_TYPES, [up,down,internal]).

test() -> 
    elevator_driver:setMotorDir(up),
    pollingLoop(elevator_driver:getFloor()).

pollingLoop(State) ->
    Floor=elevator_driver:getFloor(),
    case Floor =:= State of
        false ->
            case Floor of
            	-1 -> 
            		pollingLoop(State);
            	 0 -> 
                	elevator_driver:setFloorIndicator(Floor),
                	elevator_driver:setMotorDir(stop),
                	ok; 
                	 
                 3 -> 
                	elevator_driver:setFloorIndicator(Floor),
                	elevator_driver:setMotorDir(down), 
                	pollingLoop(Floor);

                 Int when is_integer(Int), Int < 4, Int >= 0 ->  
                	elevator_driver:setFloorIndicator(Floor),
                	pollingLoop(Floor);
                 Error -> 
                	{error,{undefined_floor,Error}}
            end;
        true ->
            pollingLoop(Floor)
    end.


pollFloor(LastFloor) ->
    CurrentFloor = elevator_driver:getFloor(),
    if
        CurrentFloor =:= LastFloor; LastFloor =:= -1 ->
            timer:sleep(?POLL_PERIOD), pollFloor(LastFloor);
        is_integer(CurrentFloor), CurrentFloor < ?NUMBER_OF_FLOORS, CurrentFloor >= 0 ->
            CurrentFloor;
        true -> {error,{undefined_floor,CurrentFloor}}
    end.





testExecutionTime() ->
	io:format("setLight:~n"),
	test_avg(elevator_driver,setLight,[up,2,on],10),
	io:format("setDoorLight:~n"),
	test_avg(elevator_driver,setDoorLight,[on],10),
	io:format("setFloorIndicator:~n"),
	test_avg(elevator_driver,setFloorIndicator,[2],10). 




test_avg(M, F, A, N) when N > 0 ->
    L = test_loop(M, F, A, N, []),
    Length = length(L),
    Min = lists:min(L),
    Max = lists:max(L),
    Med = lists:nth(round((Length / 2)), lists:sort(L)),
    Avg = round(lists:foldl(fun(X, Sum) -> X + Sum end, 0, L) / Length),
    io:format("Range: ~b - ~b mics~n"
          "Median: ~b mics~n"
          "Average: ~b mics~n",
          [Min, Max, Med, Avg]),
    Med.
 
test_loop(_M, _F, _A, 0, List) ->
    List;
test_loop(M, F, A, N, List) ->
    {T, _Result} = timer:tc(M, F, A),
    test_loop(M, F, A, N - 1, [T|List]).