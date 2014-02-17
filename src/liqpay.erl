-module(liqpay).

-version("1.0").

-include("liqpay.hrl").

% init
-export([init/2]).

% cnb API
-export([cnb_form/2]).
-export([cnb_signature/2]).


-opaque liqpay() :: #liqpay{}.
-export_type([liqpay/0]).



-spec init(list(), list()) -> liqpay().
init(PublicKey, PrivateKey)->
    #liqpay{
        public_key  = PublicKey,
        private_key = PrivateKey
    }.



-spec cnb_form(Lp, list()) -> list() when Lp::liqpay().
cnb_form(Lp, Params)->
	liqpay_cnb:form(Lp, Params).



-spec cnb_signature(Lp, list()) -> list() when Lp::liqpay().
cnb_signature(Lp, Params)->
	liqpay_cnb:signature(Lp, Params).