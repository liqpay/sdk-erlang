-module(liqpay_cnb).

-include("liqpay.hrl").

% init
-export([form/2]).
-export([signature/2]).
-export([str_to_sign/1]).



%%% API

-spec form(Lp, map()) -> binary() when Lp::liqpay:liqpay().
form(Lp, Params)->

	Action   = maps:get(<<"action">>, Params, <<"pay">>),
	Language = maps:get(<<"language">>, Params, <<"ru">>),

	BtnNameFull = 
	case Action of
		<<"paydonate">> -> <<"d1", Language/binary, ".png">>;
		_               -> <<"p1", Language/binary, ".png">>
	end,
	JsonData = base64:encode( params(Lp, Params) ),
	Sign     = signature(Lp, Params),
	Url = ?LIQPAY_URL_CNB,
	<<
	"<form id=\"liqpay_form\" method=\"POST\" action=\"", Url, "\">\n",
		"\t<input type=\"hidden\" name=\"data\" value=\"", JsonData/binary, "\" />\n",	
		"\t<input type=\"hidden\" name=\"signature\" value=\"", Sign/binary, "\" />\n",
		"\t<input type=\"image\" src=\"", ?LIQPAY_URL_BUTTON,  BtnNameFull/binary, "\" name=\"btn_text\" class=\"liqpay_pay_button\" />\n",
	"</form>\n"
	>>.





-spec signature(Lp, map()) -> binary() when Lp::liqpay:liqpay().
signature(Lp = #liqpay{private_key = PrivateKey}, Params)->
	
  	JsonData  = base64:encode( params(Lp, Params) ),
	str_to_sign(<<PrivateKey/binary, JsonData/binary, PrivateKey/binary>>).




-spec str_to_sign(list()) -> binary().
str_to_sign(Str)->
	base64:encode( crypto:hash(sha, Str) ).




params(Lp = #liqpay{public_key = PublicKey}, Params)->
	FunEncode = Lp#liqpay.json_fun_encode,
	Params2 = 
	case maps:get(<<"public_key">>, Params, undefined) of
		undefined ->
			[{<<"public_key">>, PublicKey} | Params];
		_  ->
			Params
	end,
	%% validate params
	case maps:get(<<"version">>, Params, undefined) of
		undefined -> error({badarg, version});
		_         -> ok 			
	end,
	case maps:get(<<"amount">>, Params, undefined) of
		undefined -> error({badarg, amount});
		_         -> ok 			
	end,
	case maps:get(<<"currency">>, Params, undefined) of
		undefined -> error({badarg, currency});
		_         -> ok 			
	end,
	case maps:get(<<"description">>, Params, undefined) of
		undefined -> error({badarg, description});
		_         -> ok 			
	end,

	FunEncode(Params2).