-module(decoded_json).

-export([
  find/2,
  get/2,
  is_set/2]).

find(Key, {Attrs}) ->
  case lists:keyfind(Key, 1, Attrs) of
    false -> error;
    Value -> {ok, element(2, Value)}
  end.

get(Key, Decoded) ->
  {ok, Value} = find(Key, Decoded),
  Value.

is_set(Key, {Attrs}) ->
  lists:member({Key, true}, Attrs).