-module(elevator_driver).
-export([init_elevator/1,        %simulator/real
         set_motor_dir/1,         %up/down/stop
         set_light/3,            %up/down/internal , int , on/off
         get_floor/0,            %Return 0 index
         get_button_signal/2,     %up/down/internal , int
         set_door_light/1,        %on/off
         set_floor_indicator/1   % int
        ]).

-on_load(init/0).

-define(APPNAME, elevator_driver).
-define(LIBNAME, elevator_driver).

-define(NUMBER_OF_FLOORS,4).
-define(POLL_PERIOD, 50).
-define(BUTTTON_TYPES, [up,down,internal]).

init() ->
    SoName = case code:priv_dir(?APPNAME) of
        {error, bad_name} ->
            case filelib:is_dir(filename:join(["..", priv])) of
                true ->
                    filename:join(["..", priv, ?LIBNAME]);
                _ ->
                    filename:join([priv, ?LIBNAME])
            end;
        Dir ->
            filename:join(Dir, ?LIBNAME)
    end,
    erlang:load_nif(SoName, 0).



init_elevator(_ElevType) ->
    not_loaded(?LINE).

set_motor_dir(_Direction) ->
    not_loaded(?LINE).

set_light(_ButtonType,_Floor,_Value) ->
    not_loaded(?LINE).

get_floor() ->
    not_loaded(?LINE).

get_button_signal(_ButtonType,_Floor)->
    not_loaded(?LINE).

set_door_light(_Value)->
    not_loaded(?LINE).

set_floor_indicator(_floor)->
    not_loaded(?LINE).

not_loaded(Line) ->
    exit({not_loaded, [{module, ?MODULE}, {line, Line}]}).



