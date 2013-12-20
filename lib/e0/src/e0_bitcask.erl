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
%% @doc Bitcask handler for e0. 
%%      Todo: keep raft_term and raft_n in memory with key, to be able
%%            to implement cheap optimistic transactions.
%% @end

-module(e0_bitcask).

-behaviour(gen_server).

-export([r/1,w/1,d/1]).

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

r(Ks) ->
 gen_server:call(?MODULE, {r, Ks}, infinity).

w(E0s) ->
 gen_server:call(?MODULE, {w, E0s}, infinity).

d(E0s) ->
 gen_server:call(?MODULE, {d, E0s}, infinity).

%%%_* Gen server ==============================================================
-record(s, {ref}).

init(_Opts) ->
  Ref = bitcask:open(bitcask_dir(), [{read_write, true}]),
  {ok, #s{ref = Ref}}.

code_change(_, S, _) -> 
  {ok, S}.

terminate(_, S) ->
  ok = bitcask:close(S#s.ref),
  ok.

handle_call({r, Ks}, _, S) ->
  Reply =
    lists:map(
      fun(K) ->
          case bitcask:get(S#s.ref, to_bitcask_key(K), 1, true) of
            {ok, V, E} ->
              E0 = e0_obj:from_binary(V),
              e0_obj:set_version(E0, E);
            Else ->
              Else
          end
      end, Ks),
  {reply, Reply, S};
handle_call({w, E0s}, _, S) ->
  Reply =
    case conflicts(S#s.ref, E0s) of
      [] ->
        %% TODO: insert RAFT append log here ...
        %% TODO: ... if it succeeds with consensus, go on and put in bitcask.
        Updated = os:timestamp(),
        lists:map(
          fun(E000) ->
              E00 = e0_obj:set_version(E000, undefined),
              E0 = e0_obj:set_updated(E00, Updated),
              bitcask:put( S#s.ref
                         , to_bitcask_key({e0_obj:box(E0), e0_obj:key(E0)})
                         , e0_obj:to_binary(E0))
          end, E0s);
      Conflicts ->
        {error, {conflicts, Conflicts}}
    end,
  {reply, Reply, S};
handle_call({d, E0s}, _, S) ->
  Reply =
    case conflicts(S#s.ref, E0s) of
      [] ->
        %% TODO: insert RAFT append log here ...
        %% TODO: ... if it succeeds with consensus, go on and put in bitcask.
        lists:map(
          fun(E0) ->
              bitcask:delete( S#s.ref
                            , to_bitcask_key({e0_obj:box(E0), e0_obj:key(E0)}))
          end, E0s);
      Conflicts ->
        {error, {conflicts, Conflicts}}
    end,
  {reply, Reply, S};
handle_call(_, _, S) ->
  {reply, unknown_handle_call, S}.

handle_cast(_, S) ->
  {noreply, S}.

handle_info(_Msg, State) ->
  {noreply, State}.

%%%_* Internal ================================================================
bitcask_dir() ->
  case code:priv_dir(e0) of
    {error, bad_name} ->
      case code:which(?MODULE) of
        Filename when is_list(Filename) ->
          filename:join([filename:dirname(Filename),"../priv", "bitcask"]);
        _ ->
          filename:join("../priv", "bitcask")
      end;
    Dir ->
      filename:join(Dir, "bitcask")
  end.

to_bitcask_key(K) ->
  term_to_binary(K).

conflicts(Ref, E0s) ->
  [E0 || E0 <- E0s, conflict(Ref, E0)].

conflict(Ref, E0) ->
  case bitcask:get_bitcask_entry( Ref
                                , to_bitcask_key({ e0_obj:box(E0)
                                                 , e0_obj:key(E0)})) of
    not_found ->
      false;
    {ok, E} ->
      E =/= e0_obj:version(E0)
  end.


%%%_* Emacs ===================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
