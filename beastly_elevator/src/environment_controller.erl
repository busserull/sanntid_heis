-module(environment_controller).
-include_lib("eunit/include/eunit.hrl").
-behaviour (gen_statem).
-define (NAME, environment_controller).

-export([start_link/0]).
-export([event_button_pressed/1,
         event_reached_new_floor/1,
         goto_floor/1,
         set_light/2]).
-export([init/1,callback_mode/0,terminate/3,code_change/4]).
-export([handle_event/4]).

callback_mode() ->
    handle_event_function.

start_link() ->
    gen_statem:start_link({local,?NAME}, ?MODULE, [], []).

get_top_floor()->
    {ok,Number_of_floors} = application:get_env(number_of_floors),
    Number_of_floors-1.
get_env()->
    {ok,Environment} = application:get_env(environment),
    Environment.

event_button_pressed(Button) ->
    gen_statem:cast(?NAME, {button_pressed,Button}).

event_reached_new_floor(Floor) ->
    gen_statem:cast(?NAME, {reached_new_floor,Floor}).

goto_floor(Floor)->
    gen_statem:cast(?NAME, {goto_floor,Floor}).

set_light(Order,Value)->
    gen_statem:cast(?NAME,{set_light,Order,Value}).

init([]) ->
    elevator_driver:init_elevator(get_env()),
    io:format("Elevator initialised in ~p mode.~n",[get_env()]),
    Data = #{last_floor => elevator_driver:get_floor(),
                    dir => stop,
                  order => {},
              top_floor => get_top_floor()},
    {ok, idle, Data}.

handle_event(cast, {button_pressed,{Button_type,Floor}}, _State, _Data) ->
    Order={{Button_type,Floor},0,0,0},
    order_distributer_distribute_order(Order),
    elevator_driver:set_light(Button_type,Floor,on),
    keep_state_and_data;

handle_event(cast,{set_light,{Button_type,Floor},Value},_State,_Data) ->
    elevator_driver:set_light(Button_type,Floor,Value);

handle_event(cast, {reached_new_floor,New_floor}, test, #{dir := Dir} = Data) ->
    io:format("Reached floor ~p~n",[New_floor]),
    elevator_driver:set_floor_indicator(New_floor),
    Data#{floor:=New_floor},
    case New_floor of
    	Top_floor -> % Turn around
    	    elevator_driver:set_motor_dir(down),
            %io:format("state: ~p -> ~p, data: ~p~n",[test,test,{New_floor,down}]),
            {keep_state, Data#{dir:= down}};
    	0 -> % Turn around
            elevator_driver:set_motor_dir(up),
            %io:format("state: ~p -> ~p, data: ~p~n",[test,test,{New_floor,up}]),
            {keep_state, Data#{dir:= up}};
    	_ -> 
            %io:format("state: ~p -> ~p, data: ~p~n",[test,test,{New_floor,Dir}]),
    	    {keep_state,Data}
    end;

handle_event(cast, {reached_new_floor,New_floor}, State,#{order:={_,Ordered_floor},order:= Order} = Data) ->
    io:format("Reached floor ~p~n",[New_floor]),
    elevator_driver:set_floor_indicator(New_floor),
    Data#{floor:=New_floor},
    case Ordered_floor of
        empty -> 
            elevator_driver:set_motor_dir(stop),
            {next_state, idle, Data#{dir:= stop}};
        New_floor ->
            elevator_driver:set_motor_dir(stop),
            order_distributer_finish_order(Order),
            {next_state,door_open,Data#{dir:= stop},5000};
        Floor when Ordered_floor < New_floor                              
    end,
    ok;
    

handle_event(cast, {goto_floor,Ordered_floor}, State, #{floor:= Last_floor} = Data) ->
    case State of
        idle when Ordered_floor < Last_floor->
            elevator_driver:set_motor_dir(down),
            io:format("state: ~p -> ~p~n",[State,test]),
            {next_state,test, Data#{dir:= down}};

        idle when Ordered_floor > Last_floor->
            elevator_driver:set_motor_dir(up),
            io:format("state: ~p -> ~p~n",[State,test]),
            {next_state,test, Data#{dir:= up}};

        idle when Ordered_floor =:= Last_floor->
            keep_state_and_data;

        test ->
            elevator_driver:set_motor_dir(stop),
            io:format("state: ~p -> ~p~n",[State,idle]),
            {next_state,idle,{Last_floor,Dir,Top_floor}}
    end.



order_distributer_distribute_order(Order) ->
    io:format("order_distributer_distribute_order(~p).~n",[Order]).

order_distributer_finish_order(Order) ->
    io:format("order_distributer_finish_order(~p).~n",[Order]).

terminate(_Reason, _State, _Data) ->
    ok.
code_change(_Vsn, State, Data, _Extra) ->
    {ok, State, Data}.

reached_new_floor_test(New_floor,Last_floor,Dir)->
    ?assert(Dir =/= stop),
    case Dir of
        up ->
            ?assert(New_floor > Last_floor);
        down ->
            ?assert(New_floor < Last_floor)
    end.