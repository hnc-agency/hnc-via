-module(hnc_via_sup).
-moduledoc false.

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	create_table(),
	{
		ok,
		{
			#{strategy => one_for_one,
			  intensity => 10,
			  period => 60},
	 		[#{id => hnc_reg,
			   start => {hnc_via, start_link, []}}
			]
		}
	}.

-if(?OTP_RELEASE >= 28).
create_table() ->
	hnc_via = ets:new(hnc_via, [named_table, set, protected, {read_concurrency, true}, {heir, self()}]).
-else.
create_table() ->
	hnc_via = ets:new(hnc_via, [named_table, set, protected, {read_concurrency, true}, {heir, self(), ignore}]).
-endif.
