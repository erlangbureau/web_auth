-module(web_auth_worker).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1]).
-export([user_signin/1, get_userinfo/2]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(Services) ->
    gen_server:start_link(?MODULE, Services, []).

user_signin(Service) ->
    Fun = fun(Worker) ->
        gen_server:call(Worker, {signin, Service})
    end,
    octopus:perform(web_auth_pool, Fun).

get_userinfo(Service, AuthCode) ->
    Fun = fun(Worker) ->
        gen_server:call(Worker, {userinfo, Service, AuthCode})
    end,
    octopus:perform(web_auth_pool, Fun).
%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Services) ->
    State = connect_to_api(Services),
    {ok, State}.

handle_call({signin, Service}, _From, State) ->
    Result = case maps:find(Service, State) of
        {ok, Opts} ->
            Params = maps:get(params, Opts, []),
            EncodedUri = web_auth_encoder:uri(Params),
            AuthApiConn = maps:get(authapi_connection, Opts),
            IsAuthApiConn = maps:get(is_authapi_connected, Opts),
            request({EncodedUri, [], AuthApiConn}, IsAuthApiConn);
        error ->
            ServiceBin = atom_to_binary(Service, utf8),
            {error, <<"Not found settings for ", ServiceBin/binary, " service">>}
    end, 
    {reply, Result, State};
handle_call({userinfo, Service, AuthCode}, _From, State) ->
    Opts = maps:get(Service, State), 
    Params = maps:get(params, Opts, []),
    MainApiConn = maps:get(mainapi_connection, Opts),
    IsMainApiConn = maps:get(is_mainapi_connected, Opts),
    TokenPath = proplists:get_value(token_path, Params, <<>>),
    EncodedBody = web_auth_encoder:body(AuthCode, Params),
    Header = [{<<"Content-Type">>, <<"application/x-www-form-urlencoded">>}],
    Req = {TokenPath, EncodedBody, Header, MainApiConn},
    Result = case request(Req, IsMainApiConn) of
        {ok, Json} ->
            EncodedJson = jsx:decode(Json),
            Token = proplists:get_value(<<"access_token">>, EncodedJson, <<>>),
            UserInfoPath = proplists:get_value(userinfo_path, Params),
            userinfo({Token, UserInfoPath, MainApiConn}, IsMainApiConn);
        {error, Reason} ->
            {error, Reason}
    end,
    {reply, Result, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
connect_to_api(Services) ->
    Fun = fun({Service, Params}, Acc) ->
        AuthApi = proplists:get_value(auth_api, Params),
        MainApi = proplists:get_value(main_api, Params),
        {AuthStatus, AuthResultConn} = connect(AuthApi),
        {MainStatus, MainResultConn} = connect(MainApi),
        Acc#{Service => #{is_authapi_connected  => AuthStatus =:= ok,
                          authapi_connection    => AuthResultConn,
                          is_mainapi_connected  => MainStatus =:= ok,
                          mainapi_connection    => MainResultConn,
                          params                => Params
            }                        
        }
    end,
    lists:foldl(Fun, #{}, Services).

connect(API) ->
    ApiHost = unicode:characters_to_list(API),
    case gun:open(ApiHost, 443) of
        {ok, Pid} -> {ok, Pid};
        {error, Reason} -> {error, Reason}
    end.

userinfo({AccessToken, UserInfoPath, MainApiConn}, IsMainApiConn) ->
    AuthHeader = [{<<"Authorization">>, <<"Bearer ", AccessToken/binary>>}],
    case request({UserInfoPath, AuthHeader, MainApiConn}, IsMainApiConn) of
        {200, _Header, Body}-> {ok, Body};
        {_, _Header, Body}  -> {error, Body};
        {error, Reason}     -> {error, Reason}
    end.

request({Path, Header, Connection}, true) ->
    StreamRef = gun:get(Connection, Path, Header),
    case gun:await(Connection, StreamRef) of
        {response, fin, 302, Headers} ->
            {302, Headers, []};
        {response, nofin, HttpCode, Headers} ->
            {ok, Body} = gun:await_body(Connection, StreamRef),
            {HttpCode, Headers, Body};
        {error, Reason} -> {error, Reason}
    end;
request({Path, Body, Header, Connection}, true) ->
    StreamRef = gun:post(Connection, Path, Header, Body),
    case gun:await(Connection, StreamRef) of
        {response, fin, _Status, _Headers} ->
            {error, <<"Get token is failed">>};
        {response, nofin, _Status, _Headers} ->
            gun:await_body(Connection, StreamRef);
        {error, Reason} -> {error, Reason}
    end;
request(_, false) ->
    {error, <<"Connection is lost, try againe later">>}.
