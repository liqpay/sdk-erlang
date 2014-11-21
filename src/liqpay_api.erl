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

  Params2   = [{<<"public_key">>, PubKey} | Params],
  Params3   = 
  lists:map(fun({Key, Val})-> 
    {to_binary(Key), to_binary(Val)}
  end, Params2),
	JsonData  = FunEncode(Params3),

    Signature = base64:encode( crypto:hash(sha, <<(to_binary(PrivKey))/binary, JsonData/binary, (to_binary(PrivKey))/binary>>) ),
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




to_binary(null)->
    <<>>;
to_binary(undefined)->
    <<>>;
to_binary(Value) when is_atom(Value)->
    atom_to_binary(Value, unicode);
to_binary(Value) when is_binary(Value)->
    Value;        
to_binary(Value) when is_list(Value)->
    list_to_binary(Value);        
to_binary(Value) when is_integer(Value)->
    integer_to_binary(Value);        
to_binary(Value) when is_float(Value)->
    [AmountList] = io_lib:format("~.2f", [Value]),
    list_to_binary(AmountList).