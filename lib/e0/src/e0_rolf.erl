%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc Locker for e0.
%%% @copyright 2013 bwi.se
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(e0_rolf).

-behaviour(gen_server).

-export([ try_lock/1
        , try_lock/2
        , release_lock/1
        , start_link/0
        ]).

-export([ code_change/3
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , init/1
        , terminate/2
        ]).

%%%_* Api =====================================================================

-spec start_link() -> pid().
start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Tries to grab a lock outside of the gen_server.
%%      Must only be called in a controlled context, where the lock is
%%      guaranteed to be released.
-spec try_lock([e0:key()]) -> boolean().
try_lock(Keys) when is_list(Keys) ->
  do_try_lock(normalize_keys(Keys), self()).

%% @doc Tries to grab a lock via the gen_server.
%%      Tries to grab the lock at least Timeout milliseconds.
%%      The gen_server monitors the caller, and will release the lock
%%      would the caller crash.
-spec try_lock([e0:key()], non_neg_integer()) -> boolean().
try_lock(Keys, Timeout) when is_list(Keys), is_integer(Timeout), Timeout >= 0 ->
  gen_server:call(?MODULE, {try_lock, normalize_keys(Keys), Timeout}, infinity).

%% @doc releases a lock.
-spec release_lock([e0:key()]) -> ok.
release_lock(Keys) when is_list(Keys) ->
  gen_server:cast(?MODULE, {release_lock, normalize_keys(Keys), self()}),
  release_lock_int(normalize_keys(Keys)).

%%%_* Gen server ==============================================================
-record(s, {try_lock, monitors}).

init(_Opts) ->
  ets:new(?MODULE, [set, public, named_table, {write_concurrency, true}]),
  {ok, #s{ try_lock = []
         , monitors = []}}.

code_change(_, State, _) -> 
  {ok, State}.

terminate(_, _S) ->
  true = ets:delete(?MODULE),
  ok.

handle_call({try_lock, Keys, Timeout}, {Pid, _Tag}
           , #s{ monitors = M0
               , try_lock = TryLock0} = S) ->
  case lists:keyfind(Pid, 1, M0) of
    {Pid, {_Ref, _Keys}} ->
      %% A process may only have one monitored lock (for now).
      {reply, false, S};
    false ->
      case try_lock_and_monitor_timeout(Keys, Pid, M0, TryLock0, Timeout) of
        {true, M, TryLock} ->
          {reply, true, S#s{ monitors = M
                           , try_lock = TryLock}};
        {false, M, TryLock} when TryLock == TryLock0 ->
          %% No retry, return directly.
          {reply, false, S#s{ monitors = M
                            , try_lock = TryLock}};
        {false, M, TryLock} when TryLock0 == [] ->
          %% retry & need to start tick timer
          start_tick(),
          {noreply, S#s{ monitors = M
                       , try_lock = TryLock}};
        {false, M, TryLock} ->
          {noreply, S#s{ monitors = M
                       , try_lock = TryLock}}
      end
  end;
handle_call(_, _, State) ->
  {reply, unknown_handle_call, State}.

handle_cast({release_lock, ReleaseKeys, Pid},  #s{monitors = M0} = S) ->
  M = case lists:keyfind(Pid, 1, M0) of
        {Pid, {Ref, LockedKeys0}} ->
            case subtract_sorted_keys(ReleaseKeys, LockedKeys0) of
              [] ->
                erlang:demonitor(Ref),
                lists:keydelete(Pid, 1, M0);
              LockedKeys ->
                lists:keyreplace(Pid, 1, M0, {Pid, {Ref, LockedKeys}})
            end;
        false ->
          M0
      end,
  {noreply, S#s{monitors = M}};
handle_cast(_, S) ->
  {noreply, S}.

handle_info({'DOWN', _Ref, process, Pid, _Info}, #s{monitors = M0} = S) ->
  M = case lists:keyfind(Pid, 1, M0) of
        {Pid, {Ref, Keys}} ->
          erlang:demonitor(Ref),
          release_lock_int(Keys),
          lists:keydelete(Pid, 1, M0);
        false ->
          M0
      end,
  {noreply, S#s{monitors = M}};

handle_info(tick, #s{ try_lock = TryLock0
                    , monitors = M0} = S) ->
  case lists:foldl(fun({Keys, Pid, Ticks0}, {TryLock1, M1}) ->
                       case try_lock_and_monitor(Keys, Pid, M1) of
                         {true, M2} ->
                           gen_server:reply(Pid, true),
                           {TryLock1, M2};
                         {false, M2} ->
                           case Ticks0 - 1 of
                             Ticks when Ticks > 0 ->
                               {[{Keys, Pid, Ticks} | TryLock1], M2};
                             _Ticks ->
                               gen_server:reply(Pid, false),
                               {TryLock1, M2}
                           end
                       end
                   end
                  , {[], M0}
                  , TryLock0) of
    {[] = TryLock, M} ->
      {noreply, S#s{try_lock = TryLock
                   , monitors = M}};
    {TryLock, M} ->
      start_tick(),
      {noreply, S#s{try_lock = TryLock
                   , monitors = M}}
  end;
handle_info(_Msg, State) ->
  {noreply, State}.

%%%_* Internal ================================================================

normalize_keys(Keys) ->
  remove_dups(lists:sort(Keys), []).

remove_dups([K,K|Ks], Acc) -> 
  remove_dups([K|Ks], Acc);
remove_dups([K|Ks], Acc) ->
  remove_dups(Ks, [K|Acc]);
remove_dups([], Acc) ->
  lists:reverse(Acc).

subtract_sorted_keys(ReleaseKeys, LockedKeys) ->
  subtract_sorted_keys(ReleaseKeys, LockedKeys, []).

subtract_sorted_keys([R0|Rs], [R0|Ls], Acc) ->
  subtract_sorted_keys(Rs, Ls, Acc);
subtract_sorted_keys([R0|Rs], [L0|Ls], Acc) when R0 < L0 ->
  subtract_sorted_keys(Rs, [L0|Ls], Acc);
subtract_sorted_keys([R0|Rs], [L0|Ls], Acc) ->
  subtract_sorted_keys([R0|Rs], Ls, [L0|Acc]);
subtract_sorted_keys(_Rs, [], Acc) ->
  lists:reverse(Acc);
subtract_sorted_keys([], Ls, Acc) ->
  lists:reverse(Acc) ++ Ls.

tick_period() ->
  %% In milliseconds.
  100.

start_tick() ->
  erlang:send_after(tick_period(), self(), tick).

do_try_lock(Keys, Pid) ->
  case try_lock_int(Keys, Pid) of
    false ->
      case should_lock_more(Keys, Pid) of
        {true, []} ->
          %% We already had the lock(s).
          true;
        {true, KeysLeft} ->
          %% Need to try grab more lock(s).
          try_lock_int(KeysLeft, Pid);
        {false, _KeysLeft} ->
          %% Someone else have the lock(s).
          false
      end;
    true ->
      true
  end.

try_lock_and_monitor_timeout(Keys, Pid, M0, TryLock, Timeout) ->
  case try_lock_and_monitor(Keys, Pid, M0) of
    {true, M} ->
      {true, M, TryLock};      
    {false, M} when Timeout == 0 ->
      {false, M, TryLock};
    {false, M} ->
      Ticks = (Timeout div tick_period()) + 1,
      {false, M, [{Keys, Pid, Ticks} | TryLock]}
  end.

try_lock_and_monitor(Keys, Pid, M) ->
  case try_lock_int(Keys, Pid) of
    true ->
      Ref = monitor(process, Pid),
      {true, [{Pid, {Ref, Keys}} | M]};
    false ->
      {false, M}
  end.

try_lock_int(Keys, Pid) ->
  ets:insert_new(?MODULE, [{Key, Pid} || Key <- Keys]).
      
should_lock_more(Keys, Pid) ->
  lists:foldl(fun(_Key, {false, _KeysToTry}) ->
                  {false, []};
                 (Key, {true, KeysToTry}) ->
                  case ets:lookup(?MODULE, Key) of
                    [] ->
                      {true, [Key|KeysToTry]};
                    [{Key, Pid}] ->
                      {true, KeysToTry};
                    [{Key, _OtherPid}] ->
                      {false, []}
                  end
              end
             , {true, []}
             , Keys).

release_lock_int(Keys) ->
  lists:foreach( fun(Key) ->
                     ets:delete(?MODULE, Key)
                 end
               , Keys).

%%%_* Emacs ===================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
