-module(manifest_tests).
-include_lib("eunit/include/eunit.hrl").

apply_manifest_test() ->
  Manifest = { manifest, {}, [
    { field_mapping, <<"foo">>, {lookup, <<"patient_information.age">>}, {indexed, string} } ]},
  Event = {[{<<"patient_information">>,{[{<<"age">>,21}]}}]},
  Fields = manifest:apply_to(Manifest, Event),
  ?assertEqual([{field, <<"foo">>, 21, {indexed, string}}], Fields).

apply_mapping_test() ->
  Mapping =  [{ field_mapping, <<"foo">>, {lookup, <<"patient_information.age">>}, {indexed, string} }],
  Event = {[{<<"patient_information">>,{[{<<"age">>,21}]}}]},
  Fields = manifest:apply_mapping_to(Mapping, Event),
  ?assertEqual([{field, <<"foo">>, 21, {indexed, string}}], Fields).

apply_field_mapping_test() ->
  Field = manifest:apply_field_mapping_to(
    { field_mapping, <<"foo">>, {lookup, <<"patient_information.age">>}, {indexed, string} },
    {[{<<"patient_information">>,{[{<<"age">>,21}]}}]}),
  ?assertEqual({field, <<"foo">>, 21, {indexed, string}}, Field).

extract_value_beginning_of_month_test() ->
  Value = manifest:extract_value(
    {beginning_of, <<"patient_information.bith">>, month},
    {[{<<"patient_information">>,{[{<<"birth">>,"1993-4-24"}]}}]}),
  ?assertEqual("4", Value).

extract_value_beginning_of_year_test() ->
  Value = manifest:extract_value(
    {beginning_of, <<"patient_information.bith">>, year},
    {[{<<"patient_information">>,{[{<<"birth">>,"1993-4-24"}]}}]}),
  ?assertEqual("1993", Value).

extract_value_lookup_test() ->
  Value = manifest:extract_value(
    {lookup, <<"patient_information.age">>},
    {[{<<"patient_information">>,{[{<<"age">>,21}]}}]}),
  ?assertEqual(21, Value).