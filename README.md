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
