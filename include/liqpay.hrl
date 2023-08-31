-define(LIQPAY_HOST, "https://www.liqpay.ua").
-define(LIQPAY_URL_API, ?LIQPAY_HOST ++ "/api/").
-define(LIQPAY_URL_CNB, ?LIQPAY_HOST ++ "/api/3/checkout").


-record(liqpay, {
    public_key,
    private_key,
    json_fun_encode = fun(X)-> jsx:encode(X) end,
    json_fun_decode = fun(X)-> jsx:decode(X, [return_maps]) end
}).

