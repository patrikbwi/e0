-module(e0_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init(_Args) ->
    VMaster = {e0_vnode_master,
               {riak_core_vnode_master, start_link, [e0_vnode]},
               permanent, 5000, worker, [riak_core_vnode_master]},
    E0Rolf = {e0_rolf,
              {e0_rolf, start_link, []},
              permanent, 5000, worker, [e0_rolf]},
    E0Bitcask = {e0_bitcask,
              {e0_bitcask, start_link, []},
              permanent, 5000, worker, [e0_bitcask]},
    {ok,
     {{one_for_all, 60, 60},
      [ VMaster
      , E0Rolf
      , E0Bitcask]}}.
