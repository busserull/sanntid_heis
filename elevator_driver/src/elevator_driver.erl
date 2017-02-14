-module(elevator_driver).
-export([initElevator/1,        %simulator/real
         setMotorDir/1,         %up/down/stop
         setLight/3,            %up/down/internal , int , on/off
         getFloor/0,            %Return 0 index
         getButtonSignal/2,     %up/down/internal , int
         setDoorLight/1,        %on/off
         setFloorIndicator/1   % int
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
    erlang:load_nif(SoName, 0),
    initElevator(simulator).



initElevator(_ElevType) ->
    not_loaded(?LINE).

setMotorDir(_Direction) ->
    not_loaded(?LINE).

setLight(_ButtonType,_Floor,_Value) ->
    not_loaded(?LINE).

getFloor() ->
    not_loaded(?LINE).

getButtonSignal(_ButtonType,_Floor)->
    not_loaded(?LINE).

setDoorLight(_Value)->
    not_loaded(?LINE).

setFloorIndicator(_floor)->
    not_loaded(?LINE).

not_loaded(Line) ->
    exit({not_loaded, [{module, ?MODULE}, {line, Line}]}).



