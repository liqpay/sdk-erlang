-define(LIQPAY_URL_API, "https://www.liqpay.com/api/").
-define(LIQPAY_URL_CNB, "https://www.liqpay.com/api/3/checkout").
-define(LIQPAY_URL_BUTTON, "//static.liqpay.com/buttons/").


-record(liqpay, {

    public_key,
    private_key,
    json_fun_encode = fun(X)-> jsx:encode(X) end,
    json_fun_decode = fun(X)-> jsx:decode(X) end

}).

