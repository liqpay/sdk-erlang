-module(liqpay_cnb).

-include("liqpay.hrl").

% init
-export([form/2]).
-export([signature/2]).
-export([str_to_sign/1]).
-export([params/2]).

-define(BUTTON_TEXTS, #{<<"ru">> => <<"Оплатить"/utf8>>, <<"uk">> => <<"Cплатити"/utf8>>, <<"en">> => <<"Pay">>}).

%%% API

-spec form(Lp :: liqpay:liqpay(), map()) -> binary().
form(Lp, Params) ->

    Language = maps:get(<<"language">>, Params, <<"uk">>),
	ButtonText = maps:get(Language, ?BUTTON_TEXTS, <<"Cплатити"/utf8>>),
    JsonData = base64:encode(params(Lp, Params)),
    Sign = signature(Lp, Params),
    Url = list_to_binary(?LIQPAY_URL_CNB),
    <<
        "<form id=\"liqpay_form\" method=\"POST\" action=\"", Url/binary, "\">\n",
        "\t<input type=\"hidden\" name=\"data\" value=\"", JsonData/binary, "\" />\n",
        "\t<input type=\"hidden\" name=\"signature\" value=\"", Sign/binary, "\" />\n",
        "\t<script type=\"text/javascript\" src=\"https://static.liqpay.ua/libjs/sdk_button.js\"></script>\n",
        "\t<sdk-button label=\"", ButtonText/binary, "\" background=\"#77CC5D\" onClick=\"submit()\"></sdk-button>\n",
        "</form>\n"
    >>.





-spec signature(Lp, map()) -> binary() when Lp :: liqpay:liqpay().
signature(Lp = #liqpay{private_key = PrivateKey}, Params) ->

    JsonData = base64:encode(params(Lp, Params)),
    str_to_sign(<<PrivateKey/binary, JsonData/binary, PrivateKey/binary>>).




-spec str_to_sign(list()) -> binary().
str_to_sign(Str) ->
    base64:encode(crypto:hash(sha, Str)).




params(Lp = #liqpay{public_key = PublicKey}, Params) ->
    FunEncode = Lp#liqpay.json_fun_encode,
    Params2 =
        case maps:get(<<"public_key">>, Params, undefined) of
            undefined ->
                maps:merge(#{<<"public_key">> => PublicKey}, Params);
            _ ->
                Params
        end,
    %% validate params
    case maps:get(<<"version">>, Params, undefined) of
        undefined -> error({badarg, version});
        _ -> ok
    end,
    case maps:get(<<"action">>, Params, undefined) of
        undefined -> error({badarg, action});
        _ -> ok
    end,
    case maps:get(<<"amount">>, Params, undefined) of
        undefined -> error({badarg, amount});
        _ -> ok
    end,
    case maps:get(<<"currency">>, Params, undefined) of
        undefined -> error({badarg, currency});
        _ -> ok
    end,
    case maps:get(<<"description">>, Params, undefined) of
        undefined -> error({badarg, description});
        _ -> ok
    end,

    FunEncode(Params2).
