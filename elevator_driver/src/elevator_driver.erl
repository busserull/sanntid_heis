-module(elevator_driver).
-export([init_elevator/1,        %simulator/real
         set_motor_dir/1,         %up/down/stop
         set_button_light/3,            %up/down/internal , int , on/off
         get_floor/0,            %Return 0 index
         get_button_signal/2,     %up/down/internal , int
         set_door_light/1,        %on/off
         set_floor_indicator/1,   % int
         set_stop_light/1,         % int
         start_link/1,
         stop/0
        ]).


-define(NUMBER_OF_FLOORS,4).
-define(POLL_PERIOD, 50).
-define(BUTTTON_TYPES, [up,down,internal]).

%% Module API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init_elevator(ElevatorType) -> call_port({elev_init, ElevatorType}).
set_motor_dir(Direction) -> call_port({elev_set_motor_direction, Direction}).
set_door_light(State) -> call_port({elev_set_door_open_lamp, State}).
set_stop_light(State) -> call_port({elev_set_stop_lamp, State}).
set_floor_indicator(Floor) -> call_port({elev_set_floor_indicator, Floor}).
get_floor() -> call_port({elev_get_floor_sensor_signal}).

get_button_signal(ButtonType, Floor) -> 
    call_port({elev_get_button_signal, ButtonType, Floor}).

set_button_light(ButtonType, Floor, State) -> 
    call_port({elev_set_button_lamp, ButtonType, Floor, State}).

start_link(ElevatorType) ->
    spawn_link(fun() -> init_port("../elevator_driver/priv/elevator_driver") end),
    timer:sleep(10),
    init_elevator(ElevatorType).

stop() ->
    io:format("  Closing port to external program "),
    driver ! {stop, self()},
    io:format("1"),
    receive
        port_stopped -> 
            io:format("Port is closed.~n")
    end.

init_port(ExtPrg) ->
    register(driver, self()),
    process_flag(trap_exit, true),
    Port = open_port({spawn_executable, ExtPrg}, [{packet, 2}]),
    loop(Port).


loop(Port) ->
    receive
    {call, Caller, Msg} ->
        Port ! {self(), {command, encode(Msg)}},
        receive
        {Port, {data, Data}} ->
            Caller ! {self(), Data}
        end,
        loop(Port); 
    {stop, Caller} ->
        io:format("2"),
        Port ! {self(), close},
        receive
        {Port, closed} ->
            io:format("3 "),
            Caller ! port_stopped,
            exit(normal)
        end;
    {'EXIT', Port, Reason} ->
        io:format("External program exit reason: ~p~n",[Reason]),
        exit(elevator_driver_port_terminated)
    end.

call_port(Msg) ->
    driver ! {call, self(), Msg},
    receive 
    {_PID, [Result]} ->
        Result
    end.

encode({elev_init, simulator}) -> [1, 1];
encode({elev_init, real}) -> [1, 2];
encode({elev_set_motor_direction, stop}) -> [2, 1];
encode({elev_set_motor_direction, up}) -> [2, 2];
encode({elev_set_motor_direction, down}) -> [2, 0];
encode({elev_set_door_open_lamp, off}) -> [3, 0];
encode({elev_set_door_open_lamp, on}) -> [3, 1];
encode({elev_get_obstruction_signal}) -> [4];
encode({elev_get_stop_signal}) -> [5];
encode({elev_set_stop_lamp, off}) -> [6, 0];
encode({elev_set_stop_lamp, on}) -> [6, 1];
encode({elev_get_floor_sensor_signal}) -> [7];
encode({elev_set_floor_indicator, Floor}) -> [8, Floor];
encode({elev_get_button_signal, up, Floor}) -> [9, 0, Floor];
encode({elev_get_button_signal, down, Floor}) -> [9, 1, Floor];
encode({elev_get_button_signal, internal, Floor}) -> [9, 2, Floor];
encode({elev_set_button_lamp, up, Floor, on}) -> [10, 0, Floor, 1];
encode({elev_set_button_lamp, up, Floor, off}) -> [10, 0, Floor, 0];
encode({elev_set_button_lamp, down, Floor, on}) -> [10, 1, Floor, 1];
encode({elev_set_button_lamp, down, Floor, off}) -> [10, 1, Floor, 0];
encode({elev_set_button_lamp, internal, Floor, on}) -> [10, 2, Floor, 1];
encode({elev_set_button_lamp, internal, Floor, off}) -> [10, 2, Floor, 0].