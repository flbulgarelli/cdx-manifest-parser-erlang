-module(decoded_json_tests).
-include_lib("eunit/include/eunit.hrl").

get_defined_test() ->
  Decoded = jiffy:decode(<<"{\"foo\": \"bar\"}">>),
  ?assertEqual(<<"bar">>, decoded_json:get(<<"foo">>, Decoded)).

find_defined_test() ->
  Decoded = jiffy:decode(<<"{\"foo\": \"bar\"}">>),
  ?assertEqual({ok, <<"bar">>}, decoded_json:find(<<"foo">>, Decoded)).

find_undefined_test() ->
  Decoded = jiffy:decode(<<"{\"foo\": \"bar\"}">>),
  ?assertEqual(error, decoded_json:find(<<"baz">>, Decoded)).

is_set_test() ->
  Decoded = jiffy:decode(<<"{\"foo\": true}">>),
  ?assertEqual(true, decoded_json:is_set(<<"foo">>, Decoded)).

is_set_false_test() ->
  Decoded = jiffy:decode(<<"{\"foo\": false}">>),
  ?assertEqual(false, decoded_json:is_set(<<"foo">>, Decoded)).

is_set_undefined_test() ->
  Decoded = jiffy:decode(<<"{\"foo\": false}">>),
  ?assertEqual(false, decoded_json:is_set(<<"bar">>, Decoded)).