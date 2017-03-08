-module(environment_controller).
-include_lib("eunit/include/eunit.hrl").
-behaviour (gen_statem).
-behaviour(elevator_driver).
-define (NAME, environment_controller).

%supervisor 
-export([start_link/0, start_elevator/0]).

%Environment poller callbacks
-export([event_button_pressed/1, event_reached_new_floor/1]).

%Backlog 
-export([goto_floor/1, set_button_light/2]).

%Gen_statem callbacks
-export([init/1,callback_mode/0,terminate/3,code_change/4]).
-export([handle_event/4]).

-record(data, {dir, last_floor, ordered_floor, top_floor, door_open_period}).

callback_mode() ->
    handle_event_function.

start_link() ->
    gen_statem:start_link({local,?NAME}, ?MODULE, [], []).

start_elevator() ->
        elevator_driver:start_elevator(?MODULE, elevator).

get_top_floor()->
    {ok,NumberOfFloors} = application:get_env(number_of_floors),
    NumberOfFloors-1.

get_env(Environment)->
    {ok,Value} = application:get_env(Environment),
    Value.

event_button_pressed(Button) ->
    gen_statem:cast(?NAME, {button_pressed, Button}).

event_reached_new_floor(Floor) ->
    gen_statem:cast(?NAME, {reached_new_floor, Floor}).

goto_floor(Floor)->
    gen_statem:cast(?NAME, {goto_floor, Floor}).

set_button_light(Order,Value)->
    gen_statem:cast(?NAME, {set_button_light, Order, Value}).

init([]) ->
    process_flag(trap_exit, true),
    Data = #data{last_floor = unknown,
             ordered_floor = empty,
             top_floor = get_top_floor(),
             door_open_period = get_env(door_open_period)},
    io:format("Environment controller initialised ~n"),
    {ok, {stopped, idle}, Data}.

handle_event(cast, {button_pressed,{Button_type,Floor}}, _State, _Data) ->
    Order={Button_type, Floor},
    test_backlog:distribute_order(Order),
    elevator_driver:set_button_light(Button_type, Floor, on),
    keep_state_and_data;

handle_event(cast,{set_button_light,{Button_type,Floor},Value},_State,_Data) ->
    elevator_driver:set_button_light(Button_type,Floor,Value),
    keep_state_and_data;

handle_event(cast, {reached_new_floor, NewFloor}, test, Data) ->
    %io:format("Reached floor ~p~n",[NewFloor]),
    elevator_driver:set_floor_indicator(NewFloor),
    NewData = Data#data{last_floor = NewFloor},
    TopFloor = NewData#data.top_floor,
    case NewFloor of
    	TopFloor -> % Turn around
    	    elevator_driver:set_motor_dir(down);
    	0 -> % Turn around 
            elevator_driver:set_motor_dir(up);
    	_any_other_floor -> ok
    end,
    {keep_state, NewData};

handle_event(cast, {reached_new_floor, NewFloor}, State, Data) ->
    %io:format("Reached floor ~p~n",[NewFloor]),
    elevator_driver:set_floor_indicator(NewFloor),
    NewData = Data#data{last_floor = NewFloor},

    OrderedFloor = case test_backlog:get_order(NewFloor, NewData#data.dir) of 
        empty -> 
            NewData#data.ordered_floor;
        timeout -> 
            NewData#data.ordered_floor;
        NewOrder ->
            NewOrder
    end,
    io:format("Ordered floor =~p~n",[OrderedFloor]),
    case State of
        {stopped,_}->
            io:format("Is someone pushing me?!~n"),
            {keep_state,NewData};

        _any_state when OrderedFloor =:= empty ->
            io:format("Reached floor ~p in state ~p without an ordered_floor~n",
                [NewFloor,State]),
            {keep_state,NewData};

        {moving,_any_dir} when NewFloor =:= OrderedFloor ->
            prepare_door_open(OrderedFloor),
            NewNewData = NewData#data{ordered_floor = empty},
            {next_state, {stopped, door_open}, 
            NewNewData, [{state_timeout, Data#data.door_open_period, nothing}]};

        {moving, up} when OrderedFloor < NewFloor ->
            io:format("Going in wrong direction! ~n"),
            elevator_driver:set_motor_dir(down),
            {next_state,{moving, down}, NewData};

        {moving, down} when OrderedFloor > NewFloor ->
            io:format("Going in wrong direction! ~n"),
            elevator_driver:set_motor_dir(up),
            {next_state, {moving,up}, NewData};

        _ok ->
            {keep_state, NewData}
    end;

handle_event(state_timeout, _arg, {stopped, door_open}, Data) ->
    elevator_driver:set_door_light(off),
    LastFloor = Data#data.last_floor,
    OrderedFloor = case test_backlog:get_order(LastFloor, stop) of 
        empty -> 
            Data#data.ordered_floor;
        timeout -> 
            Data#data.ordered_floor;
        NewOrder ->
            NewOrder
    end,
    case OrderedFloor of
        empty ->
            {next_state, {stopped, idle}, Data};
        Floor when LastFloor < Floor->
            elevator_driver:set_motor_dir(up),
            {next_state, {moving, up}, Data};
        Floor when LastFloor > Floor->
            elevator_driver:set_motor_dir(down),
            {next_state, {moving, down}, Data};
        Floor when LastFloor =:= Floor ->
            prepare_door_open(OrderedFloor),
            NewData = Data#data{ordered_floor = empty},
            {next_state, {stopped, door_open},
            NewData, [{state_timeout, Data#data.door_open_period, nothing}]}
    end;

handle_event(cast, {goto_floor, test}, State, Data) ->
    case State of
        test ->
            elevator_driver:set_motor_dir(stop),
            io:format("state: ~p -> ~p~n",[State,idle]),
            {next_state,{stopped,idle},Data};
        _any_state -> 
            elevator_driver:set_motor_dir(up),
            io:format("state: ~p -> ~p~n",[State,test]),
            {next_state,test, Data}
    end;
            
handle_event(cast, {goto_floor, OrderedFloor}, State, Data) ->
    io:format("~nOrdered Floor = ~p~n",[OrderedFloor]),
    Last_floor = Data#data.last_floor,
    NewData = Data#data{ordered_floor = OrderedFloor},
    case State of
        {stopped, idle} when OrderedFloor < Last_floor->
            elevator_driver:set_motor_dir(down),
            io:format("state: ~p -> ~p~n",[State,{moving, down}]),
            {next_state, {moving, down}, NewData};

        {stopped, idle} when OrderedFloor > Last_floor->
            elevator_driver:set_motor_dir(up),
            io:format("state: ~p -> ~p~n",[State,{moving, up}]),
            {next_state, {moving, up}, NewData};

        {stopped, _any_state} when OrderedFloor =:= Last_floor->
            prepare_door_open(OrderedFloor),
            NewNewData = NewData#data{ordered_floor = empty},
            {next_state,{stopped, door_open}, NewNewData, 
            [{state_timeout, NewData#data.door_open_period, nothing}]};
        
        {stopped, door_open} ->
            {keep_state, NewData#data{ordered_floor = OrderedFloor}};

        _anything -> 
        io:format("Nothing matched in state ~p, ordered floor = ~p~n",[State, OrderedFloor]),
        keep_state_and_data
    end.

terminate(_Reason, _State, _Data) ->
    io:format("Terminating ~p!~n",[?MODULE]),
    io:format("done~n"),
    ok.
code_change(_Vsn, State, Data, _Extra) ->
    io:format("~n~nPerforming Code change in ~p!~n~n~n",[?MODULE]),
    {ok, State, Data}.

prepare_door_open(OrderedFloor) ->
    test_backlog:finnish_order(OrderedFloor),
    elevator_driver:set_button_light(internal, OrderedFloor, off),
    elevator_driver:set_button_light(up, OrderedFloor, off),
    elevator_driver:set_button_light(down, OrderedFloor, off),
    elevator_driver:set_motor_dir(stop),
    elevator_driver:set_door_light(on).

