-module(elevator_driver).
-author('andrevaa@stud.ntnu.no').

-behavior(gen_server).

%% API functions
-export([start_link/2, set_motor_dir/1, set_button_light/3,
         set_door_light/1, set_floor_indicator/1, set_stop_light/1]).

%%% Interface to the callback module where this behaviour is used.
-spec start_link(Module :: module(), simulator|elevator) -> 
    {ok, Pid::pid()} | ignore | {error, {already_started, Pid::pid()} | term()}.

-spec set_motor_dir(up|down|stop) -> ok.
-spec set_button_light(up|down|int, Floor :: integer(), on|off) -> ok.
-spec set_door_light(on|off) -> ok.
-spec set_floor_indicator(Floor::integer()) -> ok.
-spec set_stop_light(on|off) -> ok.

-callback event_button_pressed({up|down|int, Floor::integer()}) -> ok.
-callback event_reached_new_floor(Floor::integer() | the_void) -> ok.

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3,
         terminate/2]).
%% Server state
-record(state, {last_floor, button_list, elevator_type, port, callback_module,
                top_floor = 3,
                number_of_elevators = 1,
                poll_period = 50,
                external_program = "elevator_driver",
                external_timeout = 3000,
                simulator_ip = {127,0,0,1},
                simulator_port = 15657,
                simulator_socket
                }).
%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------

start_link(Module, ElevatorType) ->
    gen_server:start_link(
        {local, ?MODULE}, ?MODULE, [Module, ElevatorType], []).

set_motor_dir(Direction) ->
    gen_server:call(?MODULE, {elev_set_motor_dir, Direction}).
set_door_light(State) ->
    gen_server:call(?MODULE, {elev_set_door_open_lamp, State}).
set_stop_light(State) ->
    gen_server:call(?MODULE, {elev_set_stop_lamp, State}).
set_floor_indicator(Floor) ->
    gen_server:call(?MODULE, {elev_set_floor_indicator, Floor}).
set_button_light(ButtonType, Floor, State) ->
    gen_server:call(?MODULE, {elev_l, ButtonType, Floor, State}).
%%%----------------------------------------------------------------------
%%% Callback functions from gen_server
%%%----------------------------------------------------------------------

init([Module, ElevatorType]) ->
    process_flag(trap_exit, true),
    Rec = #state{},
    TopFloor = Rec#state.top_floor,
    State = #state{
    button_list = combine_lists([up, down, int], lists:seq(0,TopFloor), 0),
    elevator_type = ElevatorType,
    callback_module = Module},
    init_continue(State).

init_continue(#state{elevator_type = elevator} = State) ->
    PrivDir = code:priv_dir(elevator_driver),
    ExtProg = State#state.external_program,
    ExtProgWithPath = filename:join([PrivDir, ExtProg]),
    Port = open_port({spawn_executable, ExtProgWithPath}, [{packet, 2}]),
    {reply, 0} = 
    call_elevator({elev_init, elevator}, Port, State#state.external_timeout),
    init_finish(State#state{port = Port});

init_continue(#state{elevator_type = simulator} = State) ->
    #state{simulator_ip = Ip, simulator_port = Port} = State,
    {ok, Socket} = gen_tcp:connect(Ip, Port, [binary, {active, false}]),
    gen_tcp:send(Socket, encode({elev_init, simulator})),
    init_finish(State#state{simulator_socket = Socket}).

init_finish(State) ->
    io:format("Elevator driver initialised.~n"),
    erlang:send_after(State#state.poll_period, self(), time_to_poll),
    {ok, State}.

handle_call(Msg, _From, #state{port=Port, elevator_type = elevator} = State) ->
    0 = call_elevator(Msg, Port, State#state.external_timeout),
    {reply, ok, State};

handle_call(Msg, _From, #state{elevator_type = simulator} = State) ->
    ok = gen_tcp:send(State#state.simulator_socket, encode(Msg)),
    {reply, ok, State}.

handle_info(time_to_poll, State) ->
    #state{external_timeout = Timeout, simulator_socket = Socket, port = Port,
           callback_module = Mod, button_list = OldButtonList,
           last_floor = LastFloor, elevator_type = ElevatorType} = State,
    %Helper function
    Poll_button = fun({Button, _Value}) ->
        Value = case ElevatorType of
            simulator ->
                call_simulator({elev_get_order, Button}, Socket, Timeout);
            elevator ->
                call_elevator({elev_get_order, Button}, Port, Timeout)
        end,
        {Button, Value}
    end,
    %1 Poll
    NewButtonList = lists:map(Poll_button, OldButtonList),

    NewFloor = case ElevatorType of
        simulator ->
            call_simulator({elev_get_floor_sensor_signal}, Socket, Timeout);
        elevator ->
            call_elevator({elev_get_floor_sensor_signal}, Port, Timeout)
    end,
    %2 Look for change
    ChangedButtons = NewButtonList -- OldButtonList,
    FloorChange = (LastFloor =/= NewFloor),
    %3 Call callback module
    [Mod:event_button_pressed(Button) || {Button,Val} <-ChangedButtons, Val==1],
    FloorChange andalso Mod:event_reached_new_floor(NewFloor),

    erlang:send_after(State#state.poll_period, self(), time_to_poll),
    {noreply, State#state{button_list = NewButtonList, last_floor = NewFloor}};

handle_info({'EXIT', Port, Reason}, #state{port = Port} = State) ->
    io:format("Port closed for reason: ~p~n",[Reason]),
    {stop, {port_terminated, Reason}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate({port_terminated, Reason}, _State) ->
    io:format("Terminating ~p, because port terminated for reason: ~p ~n",
        [?MODULE, Reason]),
    io:format("done~n");
terminate(Reason, {port, Port}) ->
    io:format("Terminating ~p, because: ~p!~n",[?MODULE, Reason]),
    io:format("    Closing port~n"),
    port_close(Port),
    io:format("done~n");
terminate(Reason, {socket, Socket}) ->
    io:format("Terminating ~p, because: ~p!~n",[?MODULE, Reason]),
    io:format("    Closing socket~n"),
    gen_tcp:close(Socket),
    io:format("done~n").


%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

combine_lists(L1, L2, Value) ->
    [{{X, Y}, Value} || X <- L1, Y <- L2].

call_elevator(Msg, Port, Timeout) ->
    port_command(Port, encode(Msg)),
    receive
        {Port, {data, [Data]}} ->
            decode(Data)
    %% Prevent the gen_server from hanging indefinitely in case the
    %% spawned process is taking too long processing the request.
    after Timeout -> 
            throw({stop, port_timeout, {port, Port}})
    end.

call_simulator(Msg, Socket, Timeout) ->
    gen_tcp:send(Socket, encode(Msg)),
    case gen_tcp:recv(Socket, 4, Timeout) of
        {ok, Packet} ->
            decode(Packet);
        {error, closed} ->
            throw({stop, lost_conection_to_simulator, {socket, Socket}});
        {error, timeout} ->
            throw({stop, simulator_timeout, {socket, Socket}});
        {error, Reason} ->
            throw({stop, Reason, {socket, Socket}})
    end.

encode({elev_init, elevator}) -> [0, 0];
encode({elev_init, simulator}) -> [0, 1];
encode({elev_set_motor_dir, stop}) -> [1, 0];
encode({elev_set_motor_dir, up}) -> [1, 1];
encode({elev_set_motor_dir, down}) -> [1, 255];
encode({elev_l, up, Floor, on}) -> [2, 0, Floor, 1];
encode({elev_l, up, Floor, off}) -> [2, 0, Floor, 0];
encode({elev_l, down, Floor, on}) -> [2, 1, Floor, 1];
encode({elev_l, down, Floor, off}) -> [2, 1, Floor, 0];
encode({elev_l, int, Floor, on}) -> [2, 2, Floor, 1];
encode({elev_l, int, Floor, off}) -> [2, 2, Floor, 0];
encode({elev_set_floor_indicator, Floor}) -> [3, Floor];
encode({elev_set_door_open_lamp, off}) -> [4, 0];
encode({elev_set_door_open_lamp, on}) -> [4, 1];
encode({elev_set_stop_lamp, off}) -> [5, 0];
encode({elev_set_stop_lamp, on}) -> [5, 1];
encode({elev_get_order, {up, Floor}}) -> [6, 0, Floor];
encode({elev_get_order, {down, Floor}}) -> [6, 1, Floor];
encode({elev_get_order, {int, Floor}}) -> [6, 2, Floor];
encode({elev_get_floor_sensor_signal}) -> [7];
encode({elev_get_stop_signal}) -> [8];
encode({elev_get_obstruction_signal}) -> [9].

decode(<<7,1,Floor,0>>) -> Floor;
decode(<<7,0,_Floor,0>>) -> the_void;
decode(<<_Cmd,Val,0,0>>) -> Val;
decode(255) -> the_void;
decode(Value) -> Value.