-module(hnc_via_app).
-moduledoc false.

-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
	hnc_via_sup:start_link().

stop(_State) ->
	ok.
