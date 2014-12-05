-module(liqpay_cnb).

-include("liqpay.hrl").

% init
-export([form/2]).
-export([signature/2]).
-export([str_to_sign/1]).



%%% API

-spec form(Lp, list()) -> binary() when Lp::liqpay:liqpay().
form(Lp, Params)->

	Type = proplists:get_value(<<"type">>, Params),
	Language = proplists:get_value(<<"language">>, Params, <<"ru">>),

	BtnNameFull = 
	case Type of
		<<"donate">> -> <<"d1", Language/binary, ".png">>;
		_            -> <<"p1", Language/binary, ".png">>
	end,
	JsonData = base64:encode( params(Lp, Params) ),
	Sign     = signature(Lp, Params),
	<<
	"<form id=\"liqpay_form\" method=\"POST\" action=\"", ?LIQPAY_URL_CNB, "\">\n",
		"\t<input type=\"hidden\" name=\"data\" value=\"", JsonData/binary, "\" />\n",	
		"\t<input type=\"hidden\" name=\"signature\" value=\"", Sign/binary, "\" />\n",
		"\t<input type=\"image\" src=\"", ?LIQPAY_URL_BUTTON,  BtnNameFull/binary, "\" name=\"btn_text\" class=\"liqpay_pay_button\" />\n",
	"</form>\n"
	>>.





-spec signature(Lp, list()) -> binary() when Lp::liqpay:liqpay().
signature(Lp = #liqpay{private_key = PrivateKey}, Params)->
	
  	JsonData  = base64:encode( params(Lp, Params) ),
	str_to_sign(<<PrivateKey/binary, JsonData/binary, PrivateKey/binary>>).




-spec str_to_sign(list()) -> binary().
str_to_sign(Str)->
	base64:encode( crypto:hash(sha, Str) ).




params(Lp = #liqpay{public_key = PublicKey}, Params)->
	FunEncode = Lp#liqpay.json_fun_encode,
	Params2 = 
	case proplists:get_value(<<"public_key">>, Params) of
		undefined ->
			[{<<"public_key">>, PublicKey} | Params];
		_  ->
			Params
	end,
	%% validate params
	case proplists:get_value(<<"version">>, Params) of
		undefined -> error({badarg, version});
		_         -> ok 			
	end,
	case proplists:get_value(<<"amount">>, Params) of
		undefined -> error({badarg, amount});
		_         -> ok 			
	end,
	case proplists:get_value(<<"currency">>, Params) of
		undefined -> error({badarg, currency});
		_         -> ok 			
	end,
	case proplists:get_value(<<"description">>, Params) of
		undefined -> error({badarg, description});
		_         -> ok 			
	end,

	FunEncode(Params2).