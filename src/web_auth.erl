-module(web_auth).
-export([start/0, stop/0]).
-export([user_signin/1, get_userinfo/2]).

start() ->
    application:ensure_all_started(?MODULE).

stop() ->
    application:stop(?MODULE).

user_signin(Service) ->
    web_auth_worker:user_signin(Service).

get_userinfo(Service, TokenCode) ->
    web_auth_worker:get_userinfo(Service, TokenCode).
