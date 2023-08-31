-module(liqpay_post).

-export([post/2, post/3, post/4]).

-define(ALERT_TIMEOUT, 3000).


% API
-spec post(list(), list()) -> {ok, binary()} | {error, binary()}.
post(URL, Data)->
    post(URL, Data, []).

-spec( post(list(), list(), list()) -> {ok, binary()} | {error, binary()} ).
post(URL, Data, Headers)->
    post(URL, Data, Headers, [{timeout, 50000}]).

-spec( post(list(), list(), list(), list()) -> {ok, binary()} | {error, binary()} ).
post(URL, Data, Headers, HTTPOptions)->
    request(post, URL, Data, Headers, HTTPOptions).




%%%%%%%%%%% LOCAL
-spec( request(atom(), list(), list(), list(), list()) -> {ok, binary(), float()} | {ok, integer(), float()} ).
request(Method, URL, BodyCli, HeadersCli, HTTPOptions0)->

    HTTPOptions = [{connect_timeout, 5000} | HTTPOptions0],
    Start  = erlang:timestamp(),
    Result = request_run(Method, URL, HeadersCli, BodyCli, HTTPOptions),
    Time   = trunc( timer:now_diff(erlang:timestamp(), Start)/1000),

    Res =
    case Result of
        {ok, 200, Body} ->

                {ok, Body, Time};

        {ok, Code, _Body} ->

                {error, Code, Time};

        {error, Error}  ->

                {throw, Error, Time}
    end,

    Res.







% подключаемся к серверу
-spec( request_run(atom(), list(), list(), list(), list()) -> {ok, integer(), binary()} | {error, term()} ).
request_run(post, URL, Headers, Body, HTTPOptions)->
    ContentType = "application/x-www-form-urlencoded",
    ReqBody = uri_string:compose_query(Body),

    Res = httpc:request(post, {to_list(URL), Headers, ContentType, to_list(ReqBody)}, HTTPOptions, [{body_format, binary}]),
    case Res of
        {ok, {{_,Code, _}, _, Response}} ->

                    {ok, Code, Response};

        _Other   ->
                    {error, _Other}
    end.






to_list(null)->
    "";
to_list(undefined)->
    "";
to_list(Value) when is_list(Value)->
    Value;
to_list(Value) when is_binary(Value)->
    binary_to_list(Value);
to_list(Value) when is_integer(Value)->
    integer_to_list(Value);
to_list(Value) when is_float(Value)->
    [AmountList] = io_lib:format("~.2f", [Value]),
    AmountList.
