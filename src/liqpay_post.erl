-module(liqpay_post).

-export([post/2, post/3, post/4]).

-define(ALERT_TIMEOUT, 3000).


% API
post(URL, Data)->
    post(URL, Data, []).
post(URL, Data, Headers)->
    post(URL, Data, Headers, [{timeout, 50000}]).
post(URL, Data, Headers, HTTPOptions)->
    request(post, URL, Data, Headers, HTTPOptions).




%%%%%%%%%%% LOCAL

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
request_run(post, URL, Headers, Body, HTTPOptions)->
    ContentType = "application/x-www-form-urlencoded",
    ReqBody = compose_body(Body),
    
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



       
%  compose_body   
compose_body(Args) ->
    lists:concat(
        lists:foldl(
            fun (Rec, []) -> [Rec]; (Rec, Ac) -> [Rec, "&" | Ac] end,
            [],
            [to_list(K) ++ "=" ++ url_encode2(to_list(V)) || {K, V} <- Args]
        )
    ).






%% encode url params
url_encode2(T) when is_binary(T)->
    url_encode2( binary_to_list(T) ) ;
url_encode2([H|T]) ->
    if
        H >= $a, $z >= H ->
            [H|url_encode2(T)];
        H >= $A, $Z >= H ->
            [H|url_encode2(T)];
        H >= $0, $9 >= H ->
            [H|url_encode2(T)];
        H == $_; H == $.; H == $-; H == $/; H == $: -> % FIXME: more..
            [H|url_encode2(T)];
        true ->
            case integer_to_hex(H) of
                [X, Y] ->
                    [$%, X, Y | url_encode2(T)];
                [X] ->
                    [$%, $0, X | url_encode2(T)]
            end
     end;

url_encode2([]) -> [].



  
integer_to_hex(I) ->
    case catch erlang:integer_to_list(I, 16) of
        {'EXIT', _} ->
            old_integer_to_hex(I);
        Int ->
            Int
    end.    
old_integer_to_hex(I) when I<10 ->
    integer_to_list(I);
old_integer_to_hex(I) when I<16 ->
    [I-10+$A];
old_integer_to_hex(I) when I>=16 ->
    N = trunc(I/16),
    old_integer_to_hex(N) ++ old_integer_to_hex(I rem 16).