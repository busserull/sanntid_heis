-module(beastly_elevator_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    beastly_elevator_sup:start_link().

stop(_State) ->
    ok.
