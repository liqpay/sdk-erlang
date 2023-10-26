-module(liqpay_api_tests).

-include_lib("eunit/include/eunit.hrl").
-include("liqpay.hrl").

% Mock functions
mock_json_fun_encode(Params) -> jsx:encode(Params).
mock_json_fun_decode(Params) -> Params.
mock_post(_, _) -> {ok, <<"mock_response">>, []}.

%% Tests for request function

missing_version_test() ->
    Lp = #liqpay{public_key = <<"public_key">>, private_key = <<"private_key">>, json_fun_encode = fun mock_json_fun_encode/1, json_fun_decode = fun mock_json_fun_decode/1},
    Params = #{<<"some_key">> => <<"some_value">>},
    ?assertException(error, {badarg, version}, liqpay_api:request("path", Params, Lp)).

missing_action_test() ->
    Lp = #liqpay{public_key = <<"public_key">>, private_key = <<"private_key">>, json_fun_encode = fun mock_json_fun_encode/1, json_fun_decode = fun mock_json_fun_decode/1},
    Params = #{<<"version">> => <<"3">>},
    ?assertException(error, {badarg, action}, liqpay_api:request("path", Params, Lp)).

all_params_present_test() ->
    meck:new(liqpay_post, [unstick, passthrough]),
    meck:expect(liqpay_post, post, fun mock_post/2),
    Lp = #liqpay{public_key = <<"public_key">>, private_key = <<"private_key">>, json_fun_encode = fun mock_json_fun_encode/1, json_fun_decode = fun mock_json_fun_decode/1},
    Params = #{<<"version">> => 3, <<"action">> => <<"some_value">>},
    ?assertEqual(<<"mock_response">>, liqpay_api:request("path", Params, Lp)),
    meck:unload(liqpay_post).
