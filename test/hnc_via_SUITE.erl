-module(hnc_via_SUITE).

-export([register_name/1,
	 unregister_name/1,
	 whereis_name/1,
	 send/1,
	 registered_names/1,
	 crash/1]).

-export([all/0,
	 init_per_testcase/2,
	 end_per_testcase/2]).

all() ->
	[register_name,
	 unregister_name,
	 whereis_name,
	 send,
	 registered_names,
	 crash].

init_per_testcase(_, Config) ->
	application:ensure_all_started(hnc_via),
	Config.

end_per_testcase(_, _) ->
	application:stop(hnc_via).

register_name(_) ->
	Pid1 = spawn_link(fun() -> receive stop -> ok end end),
	Pid2 = spawn_link(fun() -> receive stop -> ok end end),
	yes = hnc_via:register_name("foo", Pid1),
	no = hnc_via:register_name("foo", Pid1),
	no = hnc_via:register_name("foo", Pid2),
	yes = hnc_via:register_name("bar", Pid2),
	no = hnc_via:register_name("bar", Pid2),
	["bar", "foo"] = lists:sort(hnc_via:registered_names()),
	Pid1 ! stop,
	Pid2 ! stop,
	ok.

unregister_name(_) ->
	Pid1 = spawn_link(fun() -> receive stop -> ok end end),
	Pid2 = spawn_link(fun() -> receive stop -> ok end end),
	yes = hnc_via:register_name("foo", Pid1),
	yes = hnc_via:register_name("bar", Pid2),
	["bar", "foo"] = lists:sort(hnc_via:registered_names()),
	_ = hnc_via:unregister_name("baz"),
	["bar", "foo"] = lists:sort(hnc_via:registered_names()),
	_ = hnc_via:unregister_name("foo"),
	["bar"] = lists:sort(hnc_via:registered_names()),
	Pid1 ! stop,
	timer:sleep(100),
	["bar"] = lists:sort(hnc_via:registered_names()),
	Pid2 ! stop,
	timer:sleep(100),
	[] = hnc_via:registered_names(),
	ok.

whereis_name(_) ->
	undefined = hnc_via:whereis_name("foo"),
	undefined = hnc_via:whereis_name("bar"),
	undefined = hnc_via:whereis_name("baz"),
	Pid1 = spawn_link(fun() -> receive stop -> ok end end),
	Pid2 = spawn_link(fun() -> receive stop -> ok end end),
	yes = hnc_via:register_name("foo", Pid1),
	yes = hnc_via:register_name("bar", Pid2),
	["bar", "foo"] = lists:sort(hnc_via:registered_names()),
	Pid1 = hnc_via:whereis_name("foo"),
	Pid2 = hnc_via:whereis_name("bar"),
	undefined = hnc_via:whereis_name("baz"),
	Pid1 ! stop,
	timer:sleep(100),
	["bar"] = lists:sort(hnc_via:registered_names()),
	undefined = hnc_via:whereis_name("foo"),
	Pid2 = hnc_via:whereis_name("bar"),
	undefined = hnc_via:whereis_name("baz"),
	Pid2 ! stop,
	timer:sleep(100),
	[] = hnc_via:registered_names(),
	undefined = hnc_via:whereis_name("foo"),
	undefined = hnc_via:whereis_name("bar"),
	undefined = hnc_via:whereis_name("baz"),
	ok.

send(_) ->
	Self = self(),
	Pid1 = spawn_link(fun() -> receive stop -> ok; Msg -> Self ! {self(), Msg} end end),
	Pid2 = spawn_link(fun() -> receive stop -> ok; Msg -> Self ! {self(), Msg} end end),
	yes = hnc_via:register_name("foo", Pid1),
	yes = hnc_via:register_name("bar", Pid2),
	["bar", "foo"] = lists:sort(hnc_via:registered_names()),
	Pid1 = hnc_via:send("foo", foo),
	Pid2 = hnc_via:send("bar", bar),
	{'EXIT', {badarg, {"baz", baz}}} = catch hnc_via:send("baz", baz),
	receive {Pid1, foo} -> ok after 100 -> ct:fail({timeout, Pid1}) end,
	receive {Pid2, bar} -> ok after 100 -> ct:fail({timeout, Pid2}) end,
	Pid1 ! stop,
	Pid2 ! stop,
	ok.

registered_names(_) ->
	[] = hnc_via:registered_names(),
	Pid1 = spawn_link(fun() -> receive stop -> ok end end),
	Pid2 = spawn_link(fun() -> receive stop -> ok end end),
	yes = hnc_via:register_name("foo", Pid1),
	["foo"] = hnc_via:registered_names(),
	yes = hnc_via:register_name("bar", Pid2),
	["bar", "foo"] = lists:sort(hnc_via:registered_names()),
	_ = hnc_via:unregister_name("foo"),
	["bar"] = hnc_via:registered_names(),
	Pid2 ! stop,
	timer:sleep(100),
	[] = hnc_via:registered_names(),
	Pid1 ! stop,
	ok.

crash(_) ->
	Pid1 = spawn_link(fun() -> receive stop -> ok end end),
	Pid2 = spawn_link(fun() -> receive stop -> ok end end),
	yes = hnc_via:register_name("foo", Pid1),
	yes = hnc_via:register_name("bar", Pid2),
	["bar", "foo"] = lists:sort(hnc_via:registered_names()),
	exit(whereis(hnc_via), kill),
	timer:sleep(100),
	["bar", "foo"] = lists:sort(hnc_via:registered_names()),
	Pid1 ! stop,
	Pid2 ! stop,
	timer:sleep(100),
	[] = hnc_via:registered_names(),
	ok.
