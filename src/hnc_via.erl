-module(hnc_via).

-behavior(gen_server).

-export([start_link/0]).
-export([register_name/2, unregister_name/1, whereis_name/1, send/2]).
-export([registered_names/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(init_state, {pending = []}).
-record(state, {}).

-doc false.
-spec start_link() -> gen_server:start_ret().
start_link() ->
	{ok, Pid} = gen_server:start_link({local, ?MODULE}, ?MODULE, [], []),
	true = ets:give_away(?MODULE, Pid, self()),
	{ok, Pid}.

-doc """
Associates name `Name` with the given pid `Pid`.

If `Pid` is not the pid of a local process, the calling process exits
with reason `{badarg, {Name, Pid}}`.

The function returns `yes` if successful, `no` if it fails.\
For example, `no` is returned if an attempt is made to register
an already registered process or to register a process with a name
that is already in use.
""".
-spec register_name(Name, Pid) -> Result
	when Name :: term(),
	     Pid :: pid(),
	     Result :: 'yes' | 'no'.
register_name(Name, Pid) when is_pid(Pid), node() =:= node(Pid) ->
	gen_server:call(?MODULE, {register, Name, Pid}, infinity);
register_name(Name, Pid) ->
	exit({badarg, {Name, Pid}}).

-doc """
Unregisters the process currently registered under the given `Name`.
""".
-spec unregister_name(Name) -> _
	when Name :: term().
unregister_name(Name) ->
	gen_server:call(?MODULE, {unregister, Name}, infinity).

-doc """
Returns the pid with the registered name `Name`.

Returns `undefined` if the name is not registered.
""".
-spec whereis_name(Name) -> Result
	when Name :: term(),
	     Result :: pid() | 'undefined'.
whereis_name(Name) ->
	ets:lookup_element(?MODULE, {name, Name}, 2, undefined).

-doc """
Sends message `Msg` to the pid registered as `Name`.

If `Name` is not a registered name, the calling function exits
with reason `{badarg, {Name, Msg}}`.
""".
-spec send(Name, Msg) -> Pid
	when Name :: term(),
	     Msg :: term(),
	     Pid :: pid().
send(Name, Msg) ->
	case ets:lookup_element(?MODULE, {name, Name}, 2, undefined) of
		Pid when is_pid(Pid) ->
			Pid ! Msg,
			Pid;
		_ ->
			exit({badarg, {Name, Msg}})
	end.

-doc """
Returns a list of all registered names.
""".
-spec registered_names() -> [Name]
	when Name :: term().
registered_names() ->
	[Name || [Name] <- ets:match(?MODULE, {{name, '$1'}, '_', '_'})].


-doc false.
init([]) ->
	_ = process_flag(trap_exit, true),
	{ok, #init_state{pending = []}}.

-doc false.
handle_call({register, Name, Pid}, _From, State=#state{}) ->
	{reply, do_register(Name, Pid), State};
handle_call({register, _Name, _Pid} = Msg, From, State=#init_state{pending = Pending}) ->
	{noreply, State#init_state{pending = [{From, Msg} | Pending]}};
handle_call({unregister, Name}, _From, State=#state{}) ->
	{reply, do_unregister(Name), State};
handle_call({unregister, _Name} = Msg, From, State=#init_state{pending = Pending}) ->
	{noreply, State#init_state{pending = [{From, Msg} | Pending]}};
handle_call(_, _From, State) ->
	{noreply, State}.

-doc false.
handle_cast(_Msg, State) ->
	{noreply, State}.

-doc false.
handle_info({{'DOWN', Name}, Mon, process, Pid, _Reason}, State=#state{}) ->
	true = ets:delete_object(?MODULE, {{name, Name}, Pid, Mon}),
	true = ets:delete_object(?MODULE, {{pid, Pid}, Mon}),
	{noreply, State};
handle_info({'ETS-TRANSFER', ?MODULE, From, From}, State=#init_state{pending = Pending}) ->
	ok = init_from_table(),
	ok = process_pending(Pending),
	{noreply, State#state{}};
handle_info(_Msg, State) ->
	{noreply, State}.

-doc false.
terminate(_Reason, _State) ->
	ok.

-doc false.
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

init_from_table() ->
	true = ets:match_delete(?MODULE, {{pid, '_'}, '_'}),
	true = ets:safe_fixtable(?MODULE, true),
	ok = init_from_table_1(ets:match_object(?MODULE, {{name, '_'}, '_', '_'}, 10)),
	true = ets:safe_fixtable(?MODULE, false),
	ok.

init_from_table_1('$end_of_table') ->
	ok;
init_from_table_1({Data, Cont}) ->
	lists:foreach(
		fun({{name, Name}, Pid, _Mon}) ->
			Mon = do_monitor(Name, Pid),
			true = ets:update_element(?MODULE, {name, Name}, {3, Mon}),
			true = ets:insert_new(?MODULE, {{pid, Pid}, Mon})
		end,
		Data),
	init_from_table_1(ets:match_object(Cont)).

process_pending(Pending) ->
	process_pending_1(lists:reverse(Pending)).

process_pending_1([]) ->
	ok;
process_pending_1([{From, {register, Name, Pid}} | Pending]) ->
	gen_server:reply(From, do_register(Name, Pid)),
	process_pending_1(Pending);
process_pending_1([{From, {unregister, Name}} | Pending]) ->
	gen_server:reply(From, do_unregister(Name)),
	process_pending_1(Pending).

do_register(Name, Pid) ->
	case ets:insert_new(?MODULE, [{{pid, Pid}, undefined}, {{name, Name}, Pid, undefined}]) of
		true ->
			Mon = do_monitor(Name, Pid),
			true = ets:update_element(?MODULE, {name, Name}, {3, Mon}),
			true = ets:update_element(?MODULE, {pid, Pid}, {2, Mon}),
			yes;
		false ->
			no
	end.

do_unregister(Name) ->
	case ets:take(?MODULE, {name, Name}) of
		[] ->
			ok;
		[{{name, Name}, Pid, Mon}] ->
			true = ets:delete(?MODULE, {pid, Pid}),
			demonitor(Mon, [flush]),
			ok
	end.

-if(?OTP_RELEASE >= 28).
do_monitor(Name, Pid) ->
	monitor(process, Pid, [{tag, {'DOWN', Name}}, priority]).
-else.
do_monitor(Name, Pid) ->
	monitor(process, Pid, [{tag, {'DOWN', Name}}]).
-endif.

