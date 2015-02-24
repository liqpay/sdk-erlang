-module(liqpay_app).

-export([start/2, stop/1]).

-export([start/0]).

-ignore_xref([{start,0}]).

start()->
  application:start(inets),
  ssl:start(),
  application:start(liqpay).


start(_Type, _Arg) ->
  liqpay_sup:start_link().

stop(_State)->
  ok.
