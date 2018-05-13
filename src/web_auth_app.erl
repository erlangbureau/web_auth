-module(web_auth_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    ok = start_workers_pool(),
    web_auth_sup:start_link().

stop(_State) ->
    ok.

start_workers_pool() ->
    Services = application:get_env(web_auth, services, []),
    PoolSize = application:get_env(web_auth, connection_pool_size, 1),
    PoolOpts = [{pool_size, PoolSize}, {worker, web_auth_worker}],
    ok = octopus:start_pool(web_auth_pool, PoolOpts, [Services]).
