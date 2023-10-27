-module(liqpay_api).

-include("liqpay.hrl").

% init
-export([request/3]).



%%% API
-spec(request(list(), map()|list(), Lp :: liqpay:liqpay()) -> map()).
request(Path, Params, Lp) when is_list(Params) ->
  request(Path, maps:from_list(Params), Lp);
request(Path, Params, Lp)->

	PrivKey   = Lp#liqpay.private_key,
	PubKey    = Lp#liqpay.public_key,
  FunEncode = Lp#liqpay.json_fun_encode,
  FunDecode = Lp#liqpay.json_fun_decode,


  %% validate params
  case maps:get(<<"version">>, Params, undefined) of
    undefined -> error({badarg, version});
    _         -> ok
  end,
  case maps:get(<<"action">>, Params, undefined) of
    undefined -> error({badarg, action});
    _         -> ok
  end,

  Params2   = Params#{<<"public_key">> => PubKey},
  JsonData  = base64:encode( FunEncode(Params2) ),
  Str       = <<PrivKey/binary, JsonData/binary, PrivKey/binary>>,
  Signature = base64:encode( crypto:hash(sha, Str) ),

  Data = [
    {"data", JsonData},
    {"signature", Signature}
  ],

  Res = liqpay_post:post(?LIQPAY_URL_API ++ Path, Data),

  case Res of
    {ok, Response, _} ->
        FunDecode(Response);
    _ ->
        Res
  end.
