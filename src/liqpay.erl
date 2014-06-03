-module(liqpay).

-version("1.0").

-include("liqpay.hrl").

% init
-export([init/2]).

-export([api/3]).
% cnb API
-export([cnb_form/2]).
-export([cnb_signature/2]).
-export([str_to_sign/1]).


-opaque liqpay() :: #liqpay{}.
-export_type([liqpay/0]).



-spec init(list(), list()) -> liqpay().
init(PublicKey, PrivateKey)->
    #liqpay{
        public_key  = PublicKey,
        private_key = PrivateKey
    }.



-spec api(list(), list(), Lp) -> list() when Lp::liqpay().
api(Path, Params, Lp)->
	liqpay_api:request(Path, Params, Lp).




-spec cnb_form(Lp, list()) -> list() when Lp::liqpay().
cnb_form(Lp, Params)->
	liqpay_cnb:form(Lp, Params).



-spec cnb_signature(Lp, list()) -> list() when Lp::liqpay().
cnb_signature(Lp, Params)->
	liqpay_cnb:signature(Lp, Params).



-spec str_to_sign(list()) -> list().
str_to_sign(Str)->
	liqpay_cnb:str_to_sign(Str).