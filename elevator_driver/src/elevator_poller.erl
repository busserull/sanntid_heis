-module(elevator_poller).

-behaviour (gen_statem).
-define (NAME, elevator_poller).

-export([start_link/1]).
-export([init/1,callback_mode/0,terminate/3,code_change/4]).
-export([polling/3]).

-record(data, {poll_period, button_list, last_floor, top_floor}).

callback_mode() -> state_functions.

get_env(Environment) -> 
    {ok, Value} = application:get_env(Environment),
    Value.

start_link(Environment) ->
    gen_statem:start_link({local,?NAME}, ?MODULE, Environment, []).

init(Environment) ->
    process_flag(trap_exit, true),
    elevator_driver:init_elevator(Environment),
    io:format("~p initialised.~n",[Environment]),
    %create a list of all the different buttons, with their value set to zero.
    TopFloor = get_env(number_of_floors) - 1,
    Data = #data{
    button_list = [{Floor,ButtonType,0} || Floor <- lists:seq(0, TopFloor), 
                                           ButtonType <- [up, down, internal]],
    poll_period = get_env(poll_period),
    last_floor = unknown,
    top_floor = TopFloor},

    io:format("Elevator poller initialised with poll period = ~p ms.~n",
        [Data#data.poll_period]),
    {ok, polling, Data, Data#data.poll_period}.

polling(timeout, _arg, Data) ->
    %io:format("I am now polling for the ~pth time!~n",[Count]),
    NewData = Data#data{
    button_list = 
    lists:map(fun create_event_if_button_pressed/1, Data#data.button_list),
    last_floor = 
    create_event_if_floor_changed(Data#data.last_floor, Data#data.top_floor)},
    {next_state, polling, NewData, Data#data.poll_period}.

create_event_if_button_pressed({Floor, Button, LastValue}) ->
    case elevator_driver:get_button_signal(Button, Floor) of
        LastValue -> %No state change
            {Floor, Button, LastValue};
        1 -> %Button pressed
            environment_controller:event_button_pressed({Button, Floor}),
            {Floor, Button, 1};
        0 -> %Button released
            {Floor, Button, 0};
        Undefined -> 
            io:format("Elevator poller undefined input: ~p~n",[Undefined])
    end.

create_event_if_floor_changed(LastFloor, TopFloor) ->
    CurrentFloor = elevator_driver:get_floor(),
    if
        CurrentFloor =:= LastFloor; CurrentFloor =:= 255 -> %No state change
            LastFloor;
        is_integer(CurrentFloor), CurrentFloor >= 0, CurrentFloor =< TopFloor ->
            environment_controller:event_reached_new_floor(CurrentFloor),
            CurrentFloor;
        true -> 
            io:format("Wrong return value from get_floor: ~p~n",[CurrentFloor]),
            {error,{undefined_floor,CurrentFloor}}
    end.

terminate(_Reason, _State, _Data) ->
    io:format("Terminating ~p!~n",[?MODULE]),
    io:format("done~n"),
    ok.
code_change(_Vsn, State, Data, _Extra) ->
    io:format("~n~nPerforming Code change in ~p!~n~n~n",[?MODULE]),
    {ok, State, Data}.