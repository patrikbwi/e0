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
%% @doc container for e0 data and metadata.
%% @end

-module(e0_obj).

-export_type([e0_obj/0, box/0, key/0, val/0]).

-type box() :: binary().
-type key() :: binary().
-type val() :: term().
-type raft_term() :: non_neg_integer().
-type raft_n() :: non_neg_integer().

-record(e0_obj, { box :: box()
                , key :: key()
                , val :: val()
                , created :: erlang:timestamp()
                , updated :: erlang:timestamp()
                , raft_term :: raft_term()
                , raft_n :: raft_n()
                }).

-opaque e0_obj() :: #e0_obj{}.

-export([new/3]).
-export([box/1, key/1, val/1, created/1, updated/1]).
-export([set_val/2, set_updated/2]).
-export([raft_term/1, set_raft_term/2, raft_n/1, set_raft_n/2]).
-export([to_binary/1, from_binary/1]).

%%%_* Api =====================================================================

%% @doc Constructor for new e0 objects.
-spec new(Box::box(), Key::key(), Val::val()) -> e0_obj().
new(Box, Key, Val) when is_binary(Box), is_binary(Key) ->
  case size(Key) > max_key_size() of
    true ->
      {error, key_too_large};
    false ->
      #e0_obj{ box = Box
             , key = Key
             , val = Val
             , created = os:timestamp()}
  end.

%% @doc  Return the box for this e0_obj.
-spec box(e0_obj()) -> box().
box(#e0_obj{box=Box}) -> 
  Box.

%% @doc  Return the key for this e0_obj.
-spec key(e0_obj()) -> key().
key(#e0_obj{key=Key}) -> 
  Key.

%% @doc  Return the value for this e0_obj.
-spec val(e0_obj()) -> val().
val(#e0_obj{val=Val}) -> 
  Val.

%% @doc  Set the value for this e0_obj.
-spec set_val(e0_obj(), val()) -> e0_obj().
set_val(#e0_obj{}=E0, Val) -> 
  E0#e0_obj{val = Val}.

%% @doc  Return the created timestamp for this e0_obj.
-spec created(e0_obj()) -> erlang:timestamp().
created(#e0_obj{created=Created}) -> 
  Created.

%% @doc  Return the updated timestamp for this e0_obj.
-spec updated(e0_obj()) -> erlang:timestamp().
updated(#e0_obj{updated=Updated}) -> 
  Updated.

%% @doc  Set the updated timestamp for this e0_obj.
-spec set_updated(e0_obj(), os:timestamp()) -> e0_obj().
set_updated(#e0_obj{}=E0, Updated) -> 
  E0#e0_obj{updated = Updated}.

%% @doc Return the raft_term for this e0_obj.
-spec raft_term(e0_obj()) -> raft_term().
raft_term(#e0_obj{raft_term=RT}) -> 
  RT.

%% @doc Set the raft_term for this e0_obj.
-spec set_raft_term(e0_obj(), raft_term()) -> e0_obj().
set_raft_term(#e0_obj{}=E0, RT) -> 
  E0#e0_obj{raft_term = RT}.

%% @doc Return the raft_term for this e0_obj.
-spec raft_n(e0_obj()) -> raft_n().
raft_n(#e0_obj{raft_n=Rn}) -> 
  Rn.

%% @doc Set the raft_n for this e0_obj.
-spec set_raft_n(e0_obj(), raft_n()) -> e0_obj().
set_raft_n(#e0_obj{}=E0, Rn) -> 
  E0#e0_obj{raft_n = Rn}.

%% @doc Convert an e0 object to binary form
-spec to_binary(e0_obj()) -> binary().
to_binary(E0Obj) ->
  term_to_binary(E0Obj).

%% @doc Convert an e0 binary to a e0 object.
-spec from_binary(binary()) -> e0_obj().
from_binary(E0Bin) ->
  binary_to_term(E0Bin).

%%%_* Internal ================================================================
max_key_size() ->
  65536.

%%%_* Emacs ===================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
