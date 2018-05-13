-module(web_auth_encoder).
-export([body/2, uri/1]).

body(AuthCode, Params) ->
    RedirectUri = proplists:get_value(redirect_uri, Params, <<>>),
    ClientId = proplists:get_value(client_id, Params, <<>>),
    ClientSecret = proplists:get_value(client_secret, Params, <<>>),
    Body = [{<<"code">>, AuthCode},
            {<<"client_id">>, ClientId},
            {<<"client_secret">>, ClientSecret},
            {<<"redirect_uri">>, RedirectUri},
            {<<"grant_type">>, <<"authorization_code">>}],
    encode_args(Body).

uri(Params) ->
    Id = proplists:get_value(client_id, Params, <<>>),
    AuthPath = proplists:get_value(auth_path, Params, <<>>),
    MainApi = proplists:get_value(main_api, Params, <<>>),
    UserInfo = proplists:get_value(userinfo_email, Params, <<>>),
    RedirectUri = proplists:get_value(redirect_uri, Params, <<>>),
    EncodedRedirectUri = cow_qs:urlencode(RedirectUri),
    ScopeUri = <<"https://", MainApi/binary, UserInfo/binary>>,
    Scope = cow_qs:urlencode(ScopeUri),
    Args = [{<<"scope">>, Scope}, 
            {<<"access_type">>, <<"offline">>}, 
            {<<"include_granted_scopes">>, <<"true">>},
            {<<"redirect_uri">>, EncodedRedirectUri},
            {<<"response_type">>, <<"code">>},
            {<<"client_id">>, Id}],
    <<AuthPath/binary, "?", (encode_args(Args))/binary>>.

encode_args(Args) ->
    lists:foldl(fun
        ({Field, Value}, <<>>) -> 
            <<Field/binary, "=", Value/binary>>;
        ({Field, Value}, Acc) ->
            <<Acc/binary, "&", Field/binary, "=", Value/binary>>
    end, <<>>, Args).
