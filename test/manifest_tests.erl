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

apply_mapping_with_missing_data_test() ->
  Mapping =  [
    { field_mapping, <<"foo">>, {lookup, <<"patient_information.age">>},  {indexed, string} },
    { field_mapping, <<"bar">>, {lookup, <<"patient_information.name">>}, {indexed, string} }],
  Event = {[{<<"patient_information">>,{[{<<"age">>,21}]}}]},
  Fields = manifest:apply_mapping_to(Mapping, Event),
  ?assertEqual([{field, <<"foo">>, 21, {indexed, string}}], Fields).

apply_mapping_with_extra_data_test() ->
  Mapping =  [{ field_mapping, <<"bar">>, {lookup, <<"patient_information.name">>}, {indexed, string} }],
  Event = {[
    {<<"patient_information">>,{[
      {<<"age">>,21},
      {<<"name">>,<<"jon doe">>}
    ]}}]},
  Fields = manifest:apply_mapping_to(Mapping, Event),
  ?assertEqual([{field, <<"bar">>, <<"jon doe">>, {indexed, string}}], Fields).

apply_field_mapping_test() ->
  Field = manifest:apply_field_mapping_to(
    { field_mapping, <<"foo">>, {lookup, <<"patient_information.age">>}, {indexed, string} },
    {[{<<"patient_information">>,{[{<<"age">>,21}]}}]}),
  ?assertEqual({field, <<"foo">>, 21, {indexed, string}}, Field).


extract_value_beginning_of_day_test() ->
  Value = manifest:extract_value(
    {beginning_of, <<"patient_information.birth">>, day},
    {[{<<"patient_information">>,{[{<<"birth">>,<<"1993-04-24T02:25:12">>}]}}]}),
  ?assertEqual({1993, 04, 24}, Value).

extract_value_beginning_of_month_test() ->
  Value = manifest:extract_value(
    {beginning_of, <<"patient_information.birth">>, month},
    {[{<<"patient_information">>,{[{<<"birth">>,<<"1993-04-24T02:25:12">>}]}}]}),
  ?assertEqual({1993, 04, 01}, Value).

extract_value_beginning_of_year_test() ->
  Value = manifest:extract_value(
    {beginning_of, <<"patient_information.birth">>, year},
    {[{<<"patient_information">>,{[{<<"birth">>,<<"1993-04-24T02:25:12">>}]}}]}),
  ?assertEqual({1993, 01, 01}, Value).

extract_value_lookup_test() ->
  Value = manifest:extract_value(
    {lookup, <<"patient_information.age">>},
    {[{<<"patient_information">>,{[{<<"age">>,21}]}}]}),
  ?assertEqual(21, Value).

extract_value_lookup_error_test() ->
  Path = <<"name">>,
  Event = {[]},
  ?assertThrow({undefined_path, Path, Event}, manifest:extract_value({lookup, Path}, Event)).