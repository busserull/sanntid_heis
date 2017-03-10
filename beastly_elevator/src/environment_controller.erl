-module(environment_controller).
-include_lib("eunit/include/eunit.hrl").
-behaviour (gen_statem).
-behaviour(elevator_driver).

%%%supervisor 
-export([start_link/0, start_elevator/0]).

%%%Environment poller callbacks
-export([event_button_pressed/1, event_reached_new_floor/1]).

%%%Backlog 
-export([goto_order/1, set_button_light/2, get_state/0]).

%%%Gen_statem callbacks
-export([init/1,callback_mode/0,terminate/3,code_change/4]).
-export([handle_event/4]).

%%%State data
-record(state, {pos, last_floor, order, top_floor, 
                door_open_period, traveling_timeout}).

%%%supervisor
start_link() ->
    gen_statem:start_link({local,?MODULE}, ?MODULE, [], []).
start_elevator() ->
    elevator_driver:start_link(?MODULE, simulator).

%%%Elevator driver callbacks
event_button_pressed(Button) ->
    gen_statem:cast(?MODULE, {button_pressed, Button}).
event_reached_new_floor(Floor) ->
    gen_statem:cast(?MODULE, {reached_new_floor, Floor}).

%%%Backlog 
goto_order(Order) ->
    gen_statem:cast(?MODULE, {goto_order, Order}).
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
            order = empty,
            top_floor = get_env(number_of_floors) -1 ,
            door_open_period = get_env(door_open_period),
            traveling_timeout = get_env(traveling_timeout)},
    io:format("Environment controller initialised ~n"),
    {ok, {stopped, idle}, Data}.

handle_event({call, Caller}, get_state, State, Data) ->
    #state{pos = Pos, last_floor = LastFloor} = Data,
    gen_statem:reply(Caller, {Pos, State, LastFloor}),
    keep_state_and_data;

%handle_event(cast, {button_pressed, {_ButtonType, Floor}}, {stopped, door_open}, 
%    #state{pos = at_floor, last_floor = Floor} = Data) ->
%    {keep_state_and_data, 
%    [{state_timeout, Data#state.door_open_period, nothing}]};

handle_event(cast, {button_pressed, {ButtonType, Floor}}, _State, _Data) ->
    ok = test_backlog:distribute_order({ButtonType, Floor}),
    elevator_driver:set_button_light(ButtonType, Floor, on),
    keep_state_and_data;

handle_event(cast,{set_button_light,{ButtonType,Floor},Value},_State,_Data) ->
    elevator_driver:set_button_light(ButtonType,Floor,Value),
    keep_state_and_data;

handle_event(state_timeout, _arg, {moving, Dir}, Data) ->
    io:format("Elevator is stuck~n"),
    %Give away external orders, give them the time-out?
    %if ordered_dir =/= int ->
    %Data#state{ordered_floor = empty}
    {next_state, {stuck, Dir}, Data}; 

handle_event(cast, {reached_new_floor, the_void}, _State, Data) ->
    {keep_state, Data#state{pos = in_the_void}};

handle_event(cast, {reached_new_floor, NewFloor}, State, Data) ->
    elevator_driver:set_floor_indicator(NewFloor),
    NewData = Data#state{last_floor = NewFloor, pos = at_floor},
    deside_what_to_do(update_order(State, NewData));

handle_event(state_timeout, _arg, State = {stopped, door_open}, Data) ->
    elevator_driver:set_door_light(off),
    deside_what_to_do(update_order(State, Data));
        
handle_event(cast, {goto_order, Order}, _State, Data) ->
    deside_what_to_do(Data#state{order = Order}).
    
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

update_order(State, Data) ->
    #state{last_floor = LastFloor, pos = Pos} = Data,
    Order = case test_backlog:get_order({Pos, State, LastFloor}) of 
        empty -> 
            Data#state.order;
        timeout -> 
            Data#state.order;
        NewOrder ->
            NewOrder
    end,
    Data#state{order = Order}.

deside_what_to_do(Data) ->
    LastFloor = Data#state.last_floor,
    case Data#state.order of
        {_Dir, LastFloor} ->
            enter_door_open_state(LastFloor, Data);
        empty ->
            {next_state, {stopped, idle}, Data};
        {_Dir, OrderedFloor} ->
            enter_moving_state(OrderedFloor, LastFloor, Data)
    end.

enter_door_open_state(Floor, Data) ->
    test_backlog:finnish_order(Floor),
    elevator_driver:set_button_light(int, Floor, off),
    elevator_driver:set_button_light(up, Floor, off),
    elevator_driver:set_button_light(down, Floor, off),
    elevator_driver:set_motor_dir(stop),
    elevator_driver:set_door_light(on),
    {next_state, {stopped, door_open}, Data#state{order = empty}, 
        [{state_timeout, Data#state.door_open_period, nothing}]}.

enter_moving_state(OrderedFloor, LastFloor, Data) ->
    Dir = direction(OrderedFloor, LastFloor),
    elevator_driver:set_motor_dir(Dir),
    {next_state, {moving, Dir}, Data,
    [{state_timeout, Data#state.traveling_timeout, nothing}]}.

direction(OrderedFloor, Floor) when OrderedFloor < Floor -> down;
direction(OrderedFloor, Floor) when OrderedFloor > Floor -> up.

get_env(Environment)->
    {ok,Value} = application:get_env(Environment),
    Value.