-module(liqpay_cnb).

-include("liqpay.hrl").

% init
-export([form/2]).
-export([signature/2]).



%%% API

-spec form(Lp, list()) -> list() when Lp::liqpay:liqpay().
form(Lp, Params)->

	StrFields = 
	lists:foldl(fun({Key, Val}, Acc)-> 		
		
		Acc ++ "\t<input type=\"hidden\" name=\""++ to_list(Key) ++"\" value=\""++ to_list(Val) ++"\" />\n"		

	end, "", Params),

	Type = proplists:get_value(type, Params),
	Language = proplists:get_value(language, Params, "ru"),

	BtnNameFull = 
	case Type of
		"donate" -> "d1" ++ Language ++ ".png";
		_        -> "p1" ++ Language ++ ".png"
	end,

	"<form id=\"liqpay_form\" method=\"POST\" action=\"" ++ ?LIQPAY_URL_CNB ++ "\">\n"
		++ StrFields ++ 
		"\t<input type=\"hidden\" name=\"signature\" value=\"" ++ signature(Lp, Params) ++ "\" />\n"
		"\t<input type=\"image\" src=\"" ++ ?LIQPAY_URL_BUTTON ++ BtnNameFull ++ "\" name=\"btn_text\" class=\"liqpay_pay_button\" />\n"
	"</form>\n".





-spec signature(Lp, list()) -> list() when Lp::liqpay:liqpay().
signature(#liqpay{public_key = PublicKey, private_key = PrivateKey}, Params)->

	Str = 
		to_list_sign( PrivateKey ) ++
		to_list_sign( proplists:get_value(amount, Params) ) ++
		to_list_sign( proplists:get_value(currency, Params) ) ++
		to_list_sign( PublicKey ) ++
		to_list_sign( proplists:get_value(order_id, Params) ) ++
		to_list_sign( proplists:get_value(type, Params) ) ++
		to_list_sign( proplists:get_value(description, Params) ) ++
		to_list_sign( proplists:get_value(result_url, Params) ) ++
		to_list_sign( proplists:get_value(server_url, Params) ),

	to_list( base64:encode( crypto:hash(sha, Str) ) ).






%%%%% LOCAL

to_list_sign(undefined)-> "";
to_list_sign(Value)->
	to_list(Value).


to_list(Value) when is_list(Value)->
    Value;
to_list(Value) when is_binary(Value)->
    binary_to_list(Value);
to_list(Value) when is_integer(Value)->
    integer_to_list(Value);
to_list(Value) when is_atom(Value)->
    atom_to_list(Value);
to_list(Value) when is_float(Value)->
    [Str] = io_lib:format("~.2f", [Value]),
    Str.