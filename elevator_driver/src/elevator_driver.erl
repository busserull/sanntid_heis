%%%----------------------------------------------------------------------
%%% File    : echo.erl
%%% Author  : Pete Kazmier <pete-trapexit@kazmier.com>
%%% Purpose : Port Tutorial
%%% Created : Fri Jan 13 12:39:27 EST 2006
%%%----------------------------------------------------------------------

-module(elevator_driver).
-author('andrevaa@stud.ntnu.no').

-behavior(gen_server).

%% External exports
-export([start_link/1]).

%% API functions
-export([init_elevator/1,        %simulator/elevator
         set_motor_dir/1,        %up/down/stop
         set_button_light/3,     %up/down/internal , int , on/off
         get_floor/0,            %Return 0 index
         get_button_signal/2,    %up/down/internal , int
         set_door_light/1,       %on/off
         set_floor_indicator/1,  % int
         set_stop_light/1        % int
        ]).

%% gen_server callbacks
-export([init/1, 
         handle_call/3,
         handle_cast/2, 
         handle_info/2,
         code_change/3,
         terminate/2]).
%% Server state
-record(state, {port}).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------

start_link(ExtProg) ->
    gen_server:start_link({local, ?MODULE}, elevator_driver, ExtProg, []).

init_elevator(ElevatorType) -> 
    gen_server:call(?MODULE, {elev_init, ElevatorType}, get_timeout()).
set_motor_dir(Direction) -> 
    gen_server:call(?MODULE, {elev_set_motor_dir, Direction}, get_timeout()).
set_door_light(State) -> 
    gen_server:call(?MODULE, {elev_set_door_open_lamp, State}, get_timeout()).
set_stop_light(State) -> 
    gen_server:call(?MODULE, {elev_set_stop_lamp, State}, get_timeout()).
set_floor_indicator(Floor) -> 
    gen_server:call(?MODULE, {elev_set_floor_indicator, Floor}, get_timeout()).
get_floor() -> 
    gen_server:call(?MODULE, {elev_get_floor_sensor_signal}, get_timeout()).
get_button_signal(ButtonType, Floor) -> 
    gen_server:call(?MODULE, {elev_get_order, ButtonType,Floor}, get_timeout()).
set_button_light(ButtonType, Floor, State) -> 
    gen_server:call(?MODULE, {elev_l, ButtonType, Floor, State}, get_timeout()).
%%%----------------------------------------------------------------------
%%% Callback functions from gen_server
%%%----------------------------------------------------------------------

init(ExtPrg) ->
    process_flag(trap_exit, true),
    Port = open_port({spawn_executable, ExtPrg}, [{packet, 2}]),
    io:format("Elevator driver initialised.~n"),
    {ok, #state{port = Port}}.

handle_call(Msg, _From, #state{port = Port} = State) ->
    port_command(Port, encode(Msg)),
    case collect_response(Port) of
        {response, Response} -> 
            {reply, Response, State};
        timeout -> 
            {stop, port_timeout, State}
    end.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'EXIT', Port, Reason}, #state{port = Port} = State) ->
    io:format("Port closed for reason: ~p~n",[Reason]),
    {stop, {port_terminated, Reason}, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate({port_terminated, _Reason}, _State) ->
    io:format("Terminating ~p!~n",[?MODULE]),
    io:format("done~n");
terminate(_Reason, #state{port = Port} = _State) ->
    io:format("Terminating ~p!~n",[?MODULE]),
    io:format("    Closing port~n"),
    port_close(Port),
    io:format("done~n").

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

get_timeout() ->
    {ok, Value} = application:get_env(elevator_driver, timeout),
    Value.

collect_response(Port) ->
    receive
        {Port, {data, [Data]}} ->
            {response, Data}
    %% Prevent the gen_server from hanging indefinitely in case the
    %% spawned process is taking too long processing the request.
    after get_timeout() -> 
            timeout
    end.

encode({elev_init, elevator}) -> [1, 0];
encode({elev_init, simulator}) -> [1, 1];
encode({elev_set_motor_dir, stop}) -> [2, 1];
encode({elev_set_motor_dir, up}) -> [2, 2];
encode({elev_set_motor_dir, down}) -> [2, 0];
encode({elev_set_door_open_lamp, off}) -> [3, 0];
encode({elev_set_door_open_lamp, on}) -> [3, 1];
encode({elev_get_obstruction_signal}) -> [4];
encode({elev_get_stop_signal}) -> [5];
encode({elev_set_stop_lamp, off}) -> [6, 0];
encode({elev_set_stop_lamp, on}) -> [6, 1];
encode({elev_get_floor_sensor_signal}) -> [7];
encode({elev_set_floor_indicator, Floor}) -> [8, Floor];
encode({elev_get_order, up, Floor}) -> [9, 0, Floor];
encode({elev_get_order, down, Floor}) -> [9, 1, Floor];
encode({elev_get_order, internal, Floor}) -> [9, 2, Floor];
encode({elev_l, up, Floor, on}) -> [10, 0, Floor, 1];
encode({elev_l, up, Floor, off}) -> [10, 0, Floor, 0];
encode({elev_l, down, Floor, on}) -> [10, 1, Floor, 1];
encode({elev_l, down, Floor, off}) -> [10, 1, Floor, 0];
encode({elev_l, internal, Floor, on}) -> [10, 2, Floor, 1];
encode({elev_l, internal, Floor, off}) -> [10, 2, Floor, 0].