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
%% @doc e0 client module.
%% @end

-module(e0_client).

-export([r/1, w/1, d/1]).

%%%_* Api =====================================================================
r(BoxedKeys) ->
  io:format("e0_client: ~p\n", [BoxedKeys]).

w(E0Objs) ->
  io:format("e0_client: ~p\n", [E0Objs]).

d(E0Objs) ->
  io:format("e0_client: ~p\n", [E0Objs]).

%%%_* Internal ================================================================

%%%_* Emacs ===================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
