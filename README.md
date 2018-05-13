# web_auth
## Description

User authentication through web services

## Getting started
1. Add as a dependency in your project
2. Add web_auth in **your_project.app.src** file in tuple **applications**:
```erl
    {applications, [
        kernel,
        stdlib,
        web_auth
    ]},
```
3. Application config must include some parametrs of web service:
```erl
{services, [
    {google, [
        {client_id, <<"your client ID obtained from the Google API Console">>},
        {client_secret, <<"your client secret obtained from the Google API Console">>},
        {main_api, <<"www.googleapis.com">>},
        {auth_api, <<"accounts.google.com">>},
        {auth_path, <<"/o/oauth2/v2/auth">>},
        {token_path, <<"/oauth2/v4/token">>},
        {userinfo_path, <<"/oauth2/v1/userinfo">>},
        {userinfo_email, <<"/auth/userinfo.email">>},
        {redirect_uri, <<"http://localhost:8080/google_auth">>} %% your callback URI
    ]}
]}
```

## Usage

example:
```erl
1> web_auth:user_signin(google).
{302,
 [{<<"content-type">>,<<"application/binary">>},
  {<<"location">>,
   <<"https://accounts.google.com/ServiceLogin?passive=1209600"...>>},
  {<<"p3p">>,
   <<"CP=\"This is not a P3P policy! See g.co/p3phelp for more info.\"">>},
  {<<"content-language">>,<<"en-US">>},
  {<<"content-security-policy">>,
   <<"script-src 'report-sample' 'nonce-VZtnPk/pHw/xI4/YaPgLHae '"...>>},
  {<<"date">>,<<"Sun, 13 May 2018 18:31:59 GMT">>},
  {<<"server">>,<<"ESF">>},
  {<<"content-length">>,<<"0">>},
  {<<"x-xss-protection">>,<<"1; mode=block">>},
  {<<"x-frame-options">>,<<"SAMEORIGIN">>},
  {<<"set-cookie">>,
   <<"NID=130=Ou2uW8HJ7NW8NppQ6R3YJzM6egxjymOMVShDerGGDUXgZRa_"...>>},
  {<<"alt-svc">>,
   <<"hq=\":443\"; ma=2592000; quic=51303433; quic=51303432;"...>>}],
 []}

2> web_auth:get_userinfo(google, <<"4/AABpqgdpI5kx3aogJo0pFOSGNJ09h">>).
{ok, <<"{\n \"id\": \"104078575209304872818\",\n \"email\": \"erlangbureau@gmail.com\",\n \"verified_email\": true,\n \"name\": \""...>>}
```
