-module(liqpay_api).

-include("liqpay.hrl").

% init
-export([request/3]).



%%% API

request(Path, Params, Lp)->

	PrivKey   = Lp#liqpay.private_key,
	PubKey    = Lp#liqpay.public_key,
  FunEncode = Lp#liqpay.json_fun_encode,
  FunDecode = Lp#liqpay.json_fun_decode,


  %% validate params
  case proplists:get_value(<<"version">>, Params) of
    undefined -> error({badarg, version});
    _         -> ok       
  end,

  Params2   = [{<<"public_key">>, PubKey} | Params],
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