-module(environment_controller).
-include_lib("eunit/include/eunit.hrl").
-behaviour (gen_statem).
-define (NAME, environment_controller).

-export([start_link/0]).
-export([event_button_pressed/1,
         event_reached_new_floor/1,
         goto_floor/1,
         set_button_light/2]).
-export([init/1,callback_mode/0,terminate/3,code_change/4]).
-export([handle_event/4]).

-record(data, {last_floor,ordered_floor,top_floor}).

callback_mode() ->
    handle_event_function.

start_link() ->
    gen_statem:start_link({local,?NAME}, ?MODULE, [], []).

get_top_floor()->
    {ok,Number_of_floors} = application:get_env(number_of_floors),
    Number_of_floors-1.

event_button_pressed(Button) ->
    gen_statem:cast(?NAME, {button_pressed,Button}).

event_reached_new_floor(Floor) ->
    gen_statem:cast(?NAME, {reached_new_floor,Floor}).

goto_floor(Floor)->
    gen_statem:cast(?NAME, {goto_floor,Floor}).

set_button_light(Order,Value)->
    gen_statem:cast(?NAME,{set_button_light,Order,Value}).

init([]) ->
    process_flag(trap_exit, true),
    Data = #data{last_floor = elevator_driver:get_floor(),
             ordered_floor = empty,
             top_floor = get_top_floor()},
    {ok, {stopped, idle}, Data}.

handle_event(cast, {button_pressed,{Button_type,Floor}}, _State, _Data) ->
    Order={{Button_type,Floor},0,0,0},
    order_distributer_distribute_order(Order),
    elevator_driver:set_button_light(Button_type,Floor,on),
    keep_state_and_data;

handle_event(cast,{set_button_light,{Button_type,Floor},Value},_State,_Data) ->
    elevator_driver:set_button_light(Button_type,Floor,Value),
    keep_state_and_data;

handle_event(cast, {reached_new_floor,New_floor}, test, Data = #data{top_floor = Top_floor}) ->
    io:format("Reached floor ~p~n",[New_floor]),
    elevator_driver:set_floor_indicator(New_floor),
    Data#data{last_floor = New_floor},
    case New_floor of
    	Top_floor -> % Turn around
    	    elevator_driver:set_motor_dir(down);
    	0 -> % Turn around
            elevator_driver:set_motor_dir(up);
    	_any_other_floor -> ok
    end,
    {keep_state, Data};

handle_event(cast, {reached_new_floor, New_floor}, State, Data = #data{ordered_floor = Ordered_floor}) ->
    io:format("Reached floor ~p~n",[New_floor]),
    elevator_driver:set_floor_indicator(New_floor),
    Data#data{last_floor = New_floor},
    case State of
        _any_state when Ordered_floor =:= empty ->
            io:format("Reached floor ~p in state ~p without an ordered_floor~n",[New_floor,State]),
            {keep_state,Data};

        {moving,_any_dir} when New_floor =:= Ordered_floor ->
            order_distributer_finish_order(Ordered_floor),
            Data#data{ordered_floor = empty},
            elevator_driver:set_motor_dir(stop),
            {next_state, {stopped, door_open}, Data, 5000}; %Create a event_timeout after 5 sec

        {moving, up} when Ordered_floor < New_floor ->
            io:format("Going in wrong direction! ~n"),
            elevator_driver:set_motor_dir(down),
            {next_state,{moving, down},Data};

        {moving, down} when Ordered_floor > New_floor ->
            io:format("Going in wrong direction! ~n"),
            elevator_driver:set_motor_dir(up),
            {next_state,{moving,up},Data};

        {stopped,_}->
            io:format("Is someone pushing me?!~n"),
            {keep_state,Data};

        _ok -> 
            {keep_state,Data}
    end;
            
handle_event(cast, {goto_floor,Ordered_floor}, State, Data = #data{last_floor = Last_floor}) ->
    case State of
        {stopped, idle} when Ordered_floor < Last_floor->
            elevator_driver:set_motor_dir(down),
            io:format("state: ~p -> ~p~n",[State,test]),
            {next_state,test, Data};

        {stopped, idle} when Ordered_floor > Last_floor->
            elevator_driver:set_motor_dir(up),
            io:format("state: ~p -> ~p~n",[State,test]),
            {next_state,test, Data};

        {stopped, idle} when Ordered_floor =:= Last_floor->
            keep_state_and_data;

        test ->
            elevator_driver:set_motor_dir(stop),
            io:format("state: ~p -> ~p~n",[State,idle]),
            {next_state,{stopped,idle},Data};
        _anything -> 
        io:format("Nothing matched in state ~p~n",[State]),
        keep_state_and_data
    end.



order_distributer_distribute_order(Order) ->
    io:format("order_distributer_distribute_order(~p).~n",[Order]).

order_distributer_finish_order(Order) ->
    io:format("order_distributer_finish_order(~p).~n",[Order]).

terminate(_Reason, _State, _Data) ->
    io:format("Terminating ~p!~n",[?MODULE]),
    io:format("done~n"),
    ok.
code_change(_Vsn, State, Data, _Extra) ->
    io:format("~n~nPerforming Code change in ~p!~n~n~n",[?MODULE]),
    {ok, State, Data}.

reached_new_floor_test(New_floor,Last_floor,Dir)->
    ?assert(Dir =/= stop),
    case Dir of
        up ->
            ?assert(New_floor > Last_floor);
        down ->
            ?assert(New_floor < Last_floor)
    end.