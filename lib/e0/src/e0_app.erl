-module(e0_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    case e0_sup:start_link() of
        {ok, Pid} ->
            ok = riak_core:register([{vnode_module, e0_vnode}]),
            ok = riak_core_ring_events:add_guarded_handler(e0_ring_event_handler, []),
            ok = riak_core_node_watcher_events:add_guarded_handler(e0_node_event_handler, []),
            ok = riak_core_node_watcher:service_up(e0, self()),
            {ok, Pid};
        {error, Reason} ->
            {error, Reason}
    end.

stop(_State) ->
    ok.

%%%_* Emacs ===================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
