-module(liqpay).

-include("liqpay.hrl").

% init
-export([init/2, init/4]).

-export([api/3]).
% cnb API
-export([cnb_form/2]).
-export([cnb_signature/2]).
-export([str_to_sign/1]).


-opaque liqpay() :: #liqpay{}.
-export_type([liqpay/0]).



-spec init(binary(), binary()) -> liqpay().
init(PublicKey, PrivateKey)->
    #liqpay{
        public_key  = PublicKey,
        private_key = PrivateKey
    }.

-spec init(binary(), binary(), fun((map()) -> binary()), fun((binary()) -> map())) -> liqpay().
init(PublicKey, PrivateKey, Encoder, Decoder)->
    #liqpay{
        public_key  = PublicKey,
        private_key = PrivateKey,
        json_fun_encode = Encoder,
    	json_fun_decode = Decoder
    }.



-spec api(list(), map(), Lp) -> map() when Lp::liqpay().
api(Path, Params, Lp)->
	liqpay_api:request(Path, Params, Lp).




-spec cnb_form(Lp | map(), map() | Lp) -> binary() when Lp::liqpay().
cnb_form(Params, Lp) when is_record(Lp, liqpay) ->
  liqpay_cnb:form(Lp, Params);
cnb_form(Lp, Params)->
	liqpay_cnb:form(Lp, Params).



-spec cnb_signature(Lp, map()) -> binary() when Lp::liqpay().
cnb_signature(Lp, Params)->
	liqpay_cnb:signature(Lp, Params).



-spec str_to_sign(binary()) -> binary().
str_to_sign(Str)->
	liqpay_cnb:str_to_sign(Str).