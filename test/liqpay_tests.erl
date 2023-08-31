-module(liqpay_tests).
-include_lib("eunit/include/eunit.hrl").
-include("liqpay.hrl").

%% Mock functions
mock_json_fun_encode(Params) -> jsx:encode(Params).
mock_json_fun_decode(Params) -> jsx:decode(Params, [return_maps]).

%% Tests for init/2 function
init_two_args_test() ->
    Lp = liqpay:init(<<"public_key">>, <<"private_key">>),
    ?assertEqual(<<"public_key">>, Lp#liqpay.public_key),
    ?assertEqual(<<"private_key">>, Lp#liqpay.private_key).

%% Tests for init/4 function
init_four_args_test() ->
    Lp = liqpay:init(<<"public_key">>, <<"private_key">>, fun mock_json_fun_encode/1, fun mock_json_fun_decode/1),
    ?assertEqual(<<"public_key">>, Lp#liqpay.public_key),
    ?assertEqual(<<"private_key">>, Lp#liqpay.private_key),
    ?assertEqual(fun mock_json_fun_encode/1, Lp#liqpay.json_fun_encode),
    ?assertEqual(fun mock_json_fun_decode/1, Lp#liqpay.json_fun_decode).

