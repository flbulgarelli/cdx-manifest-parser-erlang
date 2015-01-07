-module(manifest_tests).
-include_lib("eunit/include/eunit.hrl").

apply_manifest_test() ->
  Manifest = { manifest, {}, [
    { field_mapping, "foo", {beginning_of, "patient_information.age", "month"}, {indexed, string} } ]},
  Event = [
      {month, "jan_foo_bar"},
      {baz, bar}],
  Fields = manifest:apply_to(Manifest, Event),
  ?assertEqual([{field, foo, "jan", {indexed, string}}], Fields).

apply_mapping_test() ->
  Mapping =  [{ field_mapping, "foo", {beginning_of, "patient_information.age", "month"}, {indexed, string} }],
  Event = [
      {month, "jan_foo_bar"},
      {baz, bar}],
  Fields = manifest:apply_mapping_to(Mapping, Event),
  ?assertEqual([{field, foo, "jan", {indexed, string}}], Fields).

apply_field_mapping_test() ->
  Field = manifest:apply_field_mapping_to(
    { field_mapping, "foo", {beginning_of, "patient_information.age", "month"}, {indexed, string} },
    [ {month, "jan_foo_bar"}, {baz, bar} ]),
  ?assertEqual({field, foo, "jan", {indexed, string}}, Field).

extract_value_beginning_of_test() ->
  Value = manifest:extract_value(
    {beginning_of, "patient_information.age", "month"},
    [ {month, "jan_foo_bar"}, {baz, bar}]),
  ?assertEqual("jan", Value).

extract_value_lookup_test() ->
  Value = manifest:extract_value(
    {lookup, <<"patient_information.age">>},
    {[{<<"patient_information">>,{[{<<"age">>,21}]}}]}),
  ?assertEqual(21, Value).