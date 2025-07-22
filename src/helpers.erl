-module(helpers).
-export([print/1, print/2, random_id/0]).

print(X) ->
  io:format("~p\n", X).

print(X, Y) ->
  io:format("~p ~p\n", X, Y).

random_id() ->
  rand:seed(exsplus, os:timestamp()),
  10000 + rand:uniform(90000) - 1.

