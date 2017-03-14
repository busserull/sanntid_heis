-module(elevator_controller).
-behaviour (gen_statem).

-define(BACKLOG, order_backlog).

%%%supervisor
-export([start_link/0, start_elevator/0]).

%%%Environment poller callbacks
-export([event_button_pressed/1, event_reached_new_floor/1]).

%%%Backlog
-export([set_button_light/2, get_state/0]).

%%%Gen_statem callbacks
-export([init/1,callback_mode/0,terminate/3,code_change/4]).
-export([handle_event/4]).

%%%State data
-record(state, {last_floor, dir, pos, top_floor, door_open_period}).

%%%supervisor
start_link() ->
    gen_statem:start_link({local,?MODULE}, ?MODULE, [], []).
start_elevator() ->
    elevator_driver:start_link(?MODULE, elevator).

%%%Elevator driver callbacks
event_button_pressed(Button) ->
    gen_statem:cast(?MODULE, {button_pressed, Button}).
event_reached_new_floor(Floor) ->
    gen_statem:cast(?MODULE, {reached_new_floor, Floor}).

%%%Backlog
set_button_light(Order,Value) ->
    gen_statem:cast(?MODULE, {set_button_light, Order, Value}).
get_state() ->
    gen_statem:call(?MODULE, get_state).

%%%Gen_statem callbacks
callback_mode() ->
    handle_event_function.

init([]) ->
    process_flag(trap_exit, true),
    Data = #state{
            top_floor = get_env(number_of_floors) -1 ,
            door_open_period = get_env(door_open_period)},
    io:format("Environment controller initialised ~n"),
    elevator_driver:set_motor_dir(stop),
    {ok, idle, Data#state{dir = stop}, [{state_timeout, 1000, nothing}]}.

handle_event({call, Caller}, get_state, _State, Data) ->
    gen_statem:reply(Caller,
        {Data#state.last_floor, Data#state.dir, Data#state.pos}),
    keep_state_and_data;

handle_event(cast, {button_pressed, {ButtonType, Floor}}, _State, _Data) ->
    ?BACKLOG:store_order(ButtonType, Floor),
    keep_state_and_data;

handle_event(cast,{set_button_light,{ButtonType,Floor},Value},_State,_Data) ->
    elevator_driver:set_button_light(ButtonType,Floor,Value),
    keep_state_and_data;

handle_event(cast, {reached_new_floor, the_void}, _State, Data) ->
    {keep_state, Data#state{pos = in_the_void}};

handle_event(cast, {reached_new_floor, NewFloor}, _State, Data) ->
    elevator_driver:set_floor_indicator(NewFloor),
    NewData = Data#state{last_floor = NewFloor, pos = at_floor},
    deside_what_to_do(?BACKLOG:get_order(NewFloor, Data#state.dir, at_floor), NewData);

handle_event(state_timeout, _arg, door_open, Data) ->
    elevator_driver:set_door_light(off),
    deside_what_to_do(?BACKLOG:get_order(Data#state.last_floor, Data#state.dir, Data#state.pos), Data);

handle_event(state_timeout, _arg, idle, Data) ->
    deside_what_to_do(?BACKLOG:get_order(Data#state.last_floor, Data#state.dir, Data#state.pos), Data).

terminate(_Reason, _State, _Data) ->
    io:format("Terminating ~p!~n",[?MODULE]),
    io:format("done~n"),
    ok.
code_change(_Vsn, State, Data, _Extra) ->
    io:format("~n~nPerforming Code change in ~p!~n~n~n",[?MODULE]),
    {ok, State, Data}.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

deside_what_to_do(Order, Data) ->
    case Order of
        open_door ->
            enter_door_open_state(Data);
        none ->
            enter_idle_state(Data);
        OrderedDir ->
            enter_moving_state(OrderedDir, Data)
    end.

enter_door_open_state(Data) ->
    elevator_driver:set_motor_dir(stop),
    elevator_driver:set_door_light(on),
    {next_state, door_open, Data#state{dir = stop},
        [{state_timeout, Data#state.door_open_period, nothing}]}.

enter_idle_state(Data) ->
    elevator_driver:set_motor_dir(stop),
    {next_state, idle, Data#state{dir = stop},
    [{state_timeout, 1000, nothing}]}.

enter_moving_state(Dir, Data) ->
    elevator_driver:set_motor_dir(Dir),
    {next_state, moving, Data#state{dir = Dir}}.

get_env(Environment)->
    {ok,Value} = application:get_env(Environment),
    Value.