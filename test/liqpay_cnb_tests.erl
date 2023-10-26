-module(liqpay_cnb_tests).

%% API
-export([]).

-include_lib("eunit/include/eunit.hrl").
-include("liqpay.hrl").

% Mock the json_fun_encode function to just return its input
mock_json_fun_encode(Params) -> Params.

% Test when all required parameters are present
all_params_present_test() ->
    Lp = #liqpay{public_key = <<"public_key_123">>, json_fun_encode = fun mock_json_fun_encode/1},
    Params = #{<<"version">> => 3, <<"action">> => <<"pay">>, <<"amount">> => 100, <<"currency">> => <<"USD">>, <<"description">> => <<"Test">>, <<"order_id">> => <<"order_123">>},
    Expected = #{<<"public_key">> => <<"public_key_123">>, <<"version">> => 3, <<"action">> => <<"pay">>, <<"amount">> => 100, <<"currency">> => <<"USD">>, <<"description">> => <<"Test">>, <<"order_id">> => <<"order_123">>},
    ?assertEqual(Expected, liqpay_cnb:params(Lp, Params)).

% Test when each required parameter is missing
missing_version_test() ->
    Lp = #liqpay{public_key = <<"public_key_123">>, json_fun_encode = fun mock_json_fun_encode/1},
    Params = #{<<"action">> => <<"pay">>, <<"amount">> => 100, <<"currency">> => <<"USD">>, <<"description">> => <<"Test">>, <<"order_id">> => <<"order_123">>},
    ?assertException(error, {badarg, version}, liqpay_cnb:params(Lp, Params)).

missing_action_test() ->
    Lp = #liqpay{public_key = <<"public_key_123">>, json_fun_encode = fun mock_json_fun_encode/1},
    Params = #{<<"version">> => 3, <<"amount">> => 100, <<"currency">> => <<"USD">>, <<"description">> => <<"Test">>, <<"order_id">> => <<"order_123">>},
    ?assertException(error, {badarg, action}, liqpay_cnb:params(Lp, Params)).

missing_amount_test() ->
    Lp = #liqpay{public_key = <<"public_key_123">>, json_fun_encode = fun mock_json_fun_encode/1},
    Params = #{<<"version">> => 3, <<"action">> => <<"pay">>, <<"currency">> => <<"USD">>, <<"description">> => <<"Test">>, <<"order_id">> => <<"order_123">>},
    ?assertException(error, {badarg, amount}, liqpay_cnb:params(Lp, Params)).

missing_currency_test() ->
    Lp = #liqpay{public_key = <<"public_key_123">>, json_fun_encode = fun mock_json_fun_encode/1},
    Params = #{<<"version">> => 3, <<"action">> => <<"pay">>, <<"amount">> => 100, <<"description">> => <<"Test">>, <<"order_id">> => <<"order_123">>},
    ?assertException(error, {badarg, currency}, liqpay_cnb:params(Lp, Params)).

missing_description_test() ->
    Lp = #liqpay{public_key = <<"public_key_123">>, json_fun_encode = fun mock_json_fun_encode/1},
    Params = #{<<"version">> => 3, <<"action">> => <<"pay">>, <<"amount">> => 100, <<"currency">> => <<"USD">>, <<"order_id">> => <<"order_123">>},
    ?assertException(error, {badarg, description}, liqpay_cnb:params(Lp, Params)).

% Test when the public key is already present in Params
public_key_already_present_test() ->
    Lp = #liqpay{public_key = <<"public_key_123">>, json_fun_encode = fun mock_json_fun_encode/1},
    Params = #{<<"public_key">> => <<"public_key_456">>, <<"version">> => 3, <<"action">> => <<"pay">>, <<"amount">> => 100, <<"currency">> => <<"USD">>, <<"description">> => <<"Test">>, <<"order_id">> => <<"order_123">>},
    Expected = #{<<"public_key">> => <<"public_key_456">>, <<"version">> => 3, <<"action">> => <<"pay">>, <<"amount">> => 100, <<"currency">> => <<"USD">>, <<"description">> => <<"Test">>, <<"order_id">> => <<"order_123">>},
    ?assertEqual(Expected, liqpay_cnb:params(Lp, Params)).

% Test when all required parameters are present and default language is used
default_language_test() ->
    Lp = #liqpay{public_key = <<"public_key">>, private_key = <<"private_key">>},
    Params = #{<<"version">> => 3, <<"action">> => <<"pay">>, <<"amount">> => 100, <<"currency">> => <<"USD">>, <<"description">> => <<"Test">>, <<"order_id">> => <<"order_123">>},
    ActualForm = liqpay_cnb:form(Lp, Params),
    ?assert(string:find(binary_to_list(ActualForm), binary_to_list(<<"Cплатити"/utf8>>)) /= nomatch).

specific_language_test() ->
    Lp = #liqpay{public_key = <<"public_key">>, private_key = <<"private_key">>},
    Params = #{<<"version">> => 3, <<"action">> => <<"pay">>, <<"amount">> => 100, <<"currency">> => <<"USD">>, <<"description">> => <<"Test">>, <<"order_id">> => <<"order_123">>, <<"language">> => <<"en">>},
    ActualForm = liqpay_cnb:form(Lp, Params),
    ?assert(string:find(binary_to_list(ActualForm), "label=\"Pay\"") /= nomatch).
