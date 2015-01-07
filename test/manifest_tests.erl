-module(manifest_tests).
-include_lib("eunit/include/eunit.hrl").

manifed_from_json_test() ->
  Manifest = manifest:parse(<<"{
    \"metadata\": {},
    \"field_mapping\": [{
      \"target_field\": \"foo\",
      \"type\": \"string\",
      \"core\": true,
      \"indexed\": true,
      \"pii\": false }]}">>),
  ?assertEqual({ manifest, {}, [
    { field_mapping, "foo", {beginning_of, "patient_information.age", "month"}, {indexed, string} }]}, Manifest).

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

extract_value_test() ->
  Value = manifest:extract_value(
    {beginning_of, "patient_information.age", "month"},
    [ {month, "jan_foo_bar"}, {baz, bar}]),
  ?assertEqual("jan", Value).

field_type_for_core_indexed_test() ->
  Visibility = manifest:parse_field_visibility([
    {core, true},
    {indexed, true},
    {pii, false}]),
  ?assertEqual(indexed, Visibility).

parse_field_visibility_for_core_pii_test() ->
  Visibility = manifest:parse_field_visibility([
    {core, true},
    {indexed, false},
    {pii, true}]),
  ?assertEqual(pii, Visibility).

parse_field_visibility_for_non_core_pii_test() ->
  Visibility = manifest:parse_field_visibility([
    {core, false},
    {indexed, false},
    {pii, true}]),
  ?assertEqual(pii, Visibility).

parse_field_visibility_for_non_core_custom_test() ->
  Visibility = manifest:parse_field_visibility([
    {core, false},
    {indexed, false},
    {pii, false}]),
  ?assertEqual(indexed, Visibility).

