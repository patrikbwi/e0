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
%% @doc e0 API module.
%% @end

-module(e0).

-export([r4/2, r4/3, r/2, r/1, w/1, d/1]).

%%%_* Api =====================================================================

r4(BoxedKeys, Timeout) when is_list(BoxedKeys), Timeout >= 0 ->
  case e0_rolf:try_lock(BoxedKeys, Timeout) of
    true ->
      e0_bitcask:r(BoxedKeys);
    false ->
      {error, failed_locking}
  end;
r4({Box, Key}, Timeout) when is_binary(Box), is_binary(Key), Timeout >= 0 ->
  r4([{Box, Key}], Timeout).

r4(Box, Key, Timeout) when is_binary(Box), is_binary(Key), Timeout >= 0 ->
  r4([{Box, Key}], Timeout).

r(Box, Key) ->
  r([{Box, Key}]).

r(BoxedKeys) when is_list(BoxedKeys) ->
  e0_bitcask:r(BoxedKeys).

w(E0Objs) when is_list(E0Objs) ->
  Locks = locks(E0Objs),
  case e0_rolf:try_lock(Locks) of
    true ->
      try e0_bitcask:w(E0Objs)
      catch E -> E
      after e0_rolf:release_lock(Locks)
      end;
    false ->
      {error, failed_locking}
  end;
w(E0Obj) ->
  w([E0Obj]).

d(E0Objs) when is_list(E0Objs) ->
  Locks = locks(E0Objs),
  case e0_rolf:try_lock(Locks) of
    true ->
      try e0_bitcask:d(E0Objs)
      catch E -> E
      after e0_rolf:release_lock(Locks)
      end;
    false ->
      {error, failed_locking}
  end;
d(E0Obj) ->
  d([E0Obj]).

%%%_* Internal ================================================================
locks(E0Objs) ->
  [{e0_obj:box(E0Obj), e0_obj:key(E0Obj)} || E0Obj <- E0Objs].

%%%_* Emacs ===================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
