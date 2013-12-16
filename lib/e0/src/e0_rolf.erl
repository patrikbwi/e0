%% Copyright (c) 2013, Patrik Winroth <patrik@bwi.se>
%%
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%% 
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
%%
%% @doc Locker for e0.
%% @end

-module(e0_rolf).

-behaviour(gen_server).

-export([ try_lock/1
        , try_lock/2
        , release_lock/1

        ]).

-export([ start_link/0
        , code_change/3
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
-spec try_lock([term()]) -> boolean().
try_lock(Locks) when is_list(Locks) ->
  do_try_lock(normalize_locks(Locks), self()).

%% @doc Tries to grab a lock via the gen_server.
%%      Tries to grab the lock at least Timeout milliseconds.
%%      The gen_server monitors the caller, and will release the lock
%%      would the caller crash.
-spec try_lock([term()], non_neg_integer()) -> boolean().
try_lock(Locks, Timeout) when is_list(Locks), is_integer(Timeout), Timeout >= 0 ->
  gen_server:call(?MODULE, {try_lock, normalize_locks(Locks), Timeout}, infinity).

%% @doc releases a lock.
-spec release_lock([term()]) -> ok.
release_lock(Locks) when is_list(Locks) ->
  gen_server:cast(?MODULE, {release_lock, normalize_locks(Locks), self()}),
  release_lock_int(normalize_locks(Locks)).

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

handle_call({try_lock, Locks, Timeout}, {Pid, Tag}
           , #s{ monitors = M0
               , try_lock = TryLock0} = S) ->
  case lists:keyfind(Pid, 1, M0) of
    {Pid, {_Ref, _Locks}} ->
      %% A process may only have one monitored lock (for now).
      {reply, false, S};
    false ->
      case try_lock_and_monitor_timeout(Locks, Pid, Tag, M0, TryLock0,Timeout) of
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

handle_cast({release_lock, ReleaseLocks, Pid},  #s{monitors = M0} = S) ->
  M = case lists:keyfind(Pid, 1, M0) of
        {Pid, {Ref, LockedLocks0}} ->
            case subtract_sorted_locks(ReleaseLocks, LockedLocks0) of
              [] ->
                erlang:demonitor(Ref),
                lists:keydelete(Pid, 1, M0);
              LockedLocks ->
                lists:keyreplace(Pid, 1, M0, {Pid, {Ref, LockedLocks}})
            end;
        false ->
          M0
      end,
  {noreply, S#s{monitors = M}};
handle_cast(_, S) ->
  {noreply, S}.

handle_info({'DOWN', _Ref, process, Pid, _Info}, #s{monitors = M0} = S) ->
  M = case lists:keyfind(Pid, 1, M0) of
        {Pid, {Ref, Locks}} ->
          erlang:demonitor(Ref),
          release_lock_int(Locks),
          lists:keydelete(Pid, 1, M0);
        false ->
          M0
      end,
  {noreply, S#s{monitors = M}};

handle_info(tick, #s{ try_lock = TryLock0
                    , monitors = M0} = S) ->
  case lists:foldl(fun({Locks, Pid, Tag, Ticks0}, {TryLock1, M1}) ->
                       case try_lock_and_monitor(Locks, Pid, M1) of
                         {true, M2} ->
                           gen_server:reply({Pid, Tag}, true),
                           {TryLock1, M2};
                         {false, M2} ->
                           case Ticks0 - 1 of
                             Ticks when Ticks > 0 ->
                               {[{Locks, Pid, Tag, Ticks} | TryLock1], M2};
                             _Ticks ->
                               gen_server:reply({Pid, Tag}, false),
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

normalize_locks(Locks) ->
  remove_dups(lists:sort(Locks), []).

remove_dups([K,K|Ks], Acc) -> 
  remove_dups([K|Ks], Acc);
remove_dups([K|Ks], Acc) ->
  remove_dups(Ks, [K|Acc]);
remove_dups([], Acc) ->
  lists:reverse(Acc).

subtract_sorted_locks(ReleaseLocks, LockedLocks) ->
  subtract_sorted_locks(ReleaseLocks, LockedLocks, []).

subtract_sorted_locks([R0|Rs], [R0|Ls], Acc) ->
  subtract_sorted_locks(Rs, Ls, Acc);
subtract_sorted_locks([R0|Rs], [L0|Ls], Acc) when R0 < L0 ->
  subtract_sorted_locks(Rs, [L0|Ls], Acc);
subtract_sorted_locks([R0|Rs], [L0|Ls], Acc) ->
  subtract_sorted_locks([R0|Rs], Ls, [L0|Acc]);
subtract_sorted_locks(_Rs, [], Acc) ->
  lists:reverse(Acc);
subtract_sorted_locks([], Ls, Acc) ->
  lists:reverse(Acc) ++ Ls.

tick_period() ->
  %% In milliseconds.
  100.

start_tick() ->
  erlang:send_after(tick_period(), self(), tick).

do_try_lock(Locks, Pid) ->
  case try_lock_int(Locks, Pid) of
    false ->
      case should_lock_more(Locks, Pid) of
        {true, []} ->
          %% We already had the lock(s).
          true;
        {true, LocksLeft} ->
          %% Need to try grab more lock(s).
          try_lock_int(LocksLeft, Pid);
        {false, _LocksLeft} ->
          %% Someone else have the lock(s).
          false
      end;
    true ->
      true
  end.

try_lock_and_monitor_timeout(Locks, Pid, Tag, M0, TryLock, Timeout) ->
  case try_lock_and_monitor(Locks, Pid, M0) of
    {true, M} ->
      {true, M, TryLock};      
    {false, M} when Timeout == 0 ->
      {false, M, TryLock};
    {false, M} ->
      Ticks = (Timeout div tick_period()) + 1,
      {false, M, [{Locks, Pid, Tag, Ticks} | TryLock]}
  end.

try_lock_and_monitor(Locks, Pid, M) ->
  case try_lock_int(Locks, Pid) of
    true ->
      Ref = monitor(process, Pid),
      {true, [{Pid, {Ref, Locks}} | M]};
    false ->
      {false, M}
  end.

try_lock_int(Locks, Pid) ->
  ets:insert_new(?MODULE, [{Lock, Pid} || Lock <- Locks]).
      
should_lock_more(Locks, Pid) ->
  lists:foldl(fun(_Lock, {false, _LocksToTry}) ->
                  {false, []};
                 (Lock, {true, LocksToTry}) ->
                  case ets:lookup(?MODULE, Lock) of
                    [] ->
                      {true, [Lock|LocksToTry]};
                    [{Lock, Pid}] ->
                      {true, LocksToTry};
                    [{Lock, _OtherPid}] ->
                      {false, []}
                  end
              end
             , {true, []}
             , Locks).

release_lock_int(Locks) ->
  lists:foreach( fun(Lock) ->
                     ets:delete(?MODULE, Lock)
                 end
               , Locks).

%%%_* Emacs ===================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
