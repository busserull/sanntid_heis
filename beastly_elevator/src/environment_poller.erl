-module(environment_poller).

-behaviour (gen_statem).
-define (NAME, environment_poller).
-define (POLL_PERIOD,50). 

-export([start_link/0]).
-export([init/1,callback_mode/0,terminate/3,code_change/4]).
-export([polling/3]).

callback_mode() -> state_functions.

get_top_floor()->
    {ok,Number_of_floors} = application:get_env(number_of_floors),
    Number_of_floors-1.

get_env()->
    {ok,Environment} = application:get_env(environment),
    Environment.

start_link() ->
    gen_statem:start_link({local,?NAME}, ?MODULE, [], []).

init([]) ->
    process_flag(trap_exit, true),
    elevator_driver:init_elevator(get_env()),
    io:format("Elevator driver initialised in ~p mode.~n",[get_env()]),
    %create a list of all the different buttons, with their value set to zero.
    TopFloor = get_top_floor(),
    Button_list = [{Floor,Button,0} || Floor<-lists:seq(0,TopFloor), Button<-[up,down,internal]],
    io:format("Initialising environment poller~n",[]),
    {ok, polling, {Button_list,-1,0,TopFloor},?POLL_PERIOD}.

polling(timeout, _arg, {Button_list,Floor,Count,TopFloor}) ->
    %io:format("I am now polling for the ~pth time!~n",[Count]),
    New_button_list = lists:map(fun create_event_if_button_pressed/1, Button_list),
    New_floor = create_event_if_floor_changed(Floor,TopFloor),
    {next_state, polling, {New_button_list, New_floor,Count+1,TopFloor}, ?POLL_PERIOD}.

create_event_if_button_pressed({Floor,Button,Last_value}) ->
    case elevator_driver:get_button_signal(Button,Floor) of
        Last_value -> %No state change
            {Floor,Button,Last_value};
        1 -> %Button pressed
            environment_controller:event_button_pressed({Button,Floor}),
            {Floor,Button,1};
        0 -> %Button released
            {Floor,Button,0};
        Undefined -> io:format("Environment poller undefinded input: ~p~n",[Undefined])
    end.

create_event_if_floor_changed(Last_floor, TopFloor) ->
    Current_floor = elevator_driver:get_floor(),
    if
        Current_floor =:= Last_floor; Current_floor =:= 255 -> %No state change
            Last_floor;
        is_integer(Current_floor), Current_floor >= 0, Current_floor =< TopFloor -> %Reached new floor
            environment_controller:event_reached_new_floor(Current_floor),
            Current_floor;
        true -> 
            io:format("Wrong return value from get_floor: ~p~n",[Current_floor]),
            {error,{undefined_floor,Current_floor}}
    end.

terminate(_Reason, _State, _Data) ->
    io:format("Terminating ~p!~n",[?MODULE]),
    io:format("done~n"),
    ok.
code_change(_Vsn, State, Data, _Extra) ->
    io:format("~n~nPerforming Code change in ~p!~n~n~n",[?MODULE]),
    {ok, State, Data}.