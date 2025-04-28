# hnc-via

HNC-via is a small, light-weight registry for local processes. Other than
`erlang:register/2` and friends, which only allow registering a process
with an atom, HNC-via allows processes to be registered with arbitrary
terms.

About the same can be achieved with
[`global`](https://www.erlang.org/doc/apps/kernel/global) or
[`gproc`](https://github.com/uwiger/gproc), but they are rather heavy
tools, very complex and often overkill if all you want to deal with is
local processes.

HNC-via is mainly aimed to be used for process registration in supervision,
that is as a `via` module (hence the name), but it also exposes an API
so that it can be used directly, like `erlang:register/2` and friends.

## Usage for process registration (via-module)

### Example module:
```erlang
-module(hnc_via_example).

-behavior(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

-export([start/1]).
-export([read/1, inc/1, set/2]).

start(Name) ->
    application:ensure_all_started(hnc_via),
    gen_server:start({via, hnc_via, Name}, ?MODULE, [], []).

read(Name) ->
    gen_server:call({via, hnc_via, Name}, read).

inc(Name) ->
    gen_server:cast({via, hnc_via, Name}, inc).

set(Name, N) when is_integer(N) ->
    hnc_via:send(Name, {set, N}),
    ok.

init([]) ->
    {ok, 0}.

handle_call(read, _From, N) ->
    {reply, N, N};
handle_call(_, _, N) ->
    {noreply, N}.

handle_cast(inc, N) ->
    {noreply, N +1}.

handle_info({set, N}, _N) ->
    {noreply, N}.
```

### Example usage:

```erlang
1> hnc_via_example:start("foo").
{ok, <0.96.0>}

2> hnc_via_example:start("bar").
{ok, <0.98.0>}

3> hnc_via_example:read("foo").
0

4> hnc_via_example:read("bar").
0

5> hnc_via_example:inc("foo").
ok

6> hnc_via_example:read("foo").
1

7> hnc_via_example:read("bar").
0

8> hnc_via_example:set("bar", 9).
ok

9> hnc_via_example:read("foo").
1

10> hnc_via_example:read("bar").
9

11> hnc_via:whereis_name("foo").
<0.96.0>

12> hnc_via:whereis_name("bar").
<0.98.0>
```

## Direct usage

```erlang
1> application:ensure_all_started(hnc_via).
{ok, [hnc_via]}

2> hnc_via:register("baz", self()).
true

3> hnc_via:whereis("baz").
<0.89.0>

4> hnc_via:send("baz", asdf).
<0.89.0>

5> hnc_via:registered().
["baz"]

6> hnc_via:unregister("baz").
true

6> hnc_via:whereis("baz").
undefined

7> hnc_via:registered().
[]
```
