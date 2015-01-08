-module(manifest_parser_tests).
-include_lib("eunit/include/eunit.hrl").

manifest_from_json_integration_test() ->
  Manifest = manifest_parser:parse(<<"{
    \"metadata\": {},
    \"field_mapping\": [{
      \"target_field\": \"foo\",
      \"type\": \"string\",
      \"core\": true,
      \"indexed\": true,
      \"pii\": false,
      \"source\": {
        \"lookup\": \"foo.bar\"
      }}, {
      \"target_field\": \"fux\",
      \"type\": \"integer\",
      \"core\": false,
      \"indexed\": false,
      \"pii\": true,
      \"source\": {
        \"lookup\": \"foo.fux\"
      }}, {
      \"target_field\": \"foobar\",
      \"type\": \"integer\",
      \"core\": false,
      \"indexed\": false,
      \"pii\": false,
      \"source\": {
        \"beginning_of\": [{\"path\":\"foo.baz\"}, \"month\"]
      }}]
    }">>),
  Event = jiffy:decode(<<"{
    \"foo\": {
      \"bar\":\"hello\",
      \"fux\": 3,
      \"baz\": \"1990-01-30T00:00:00\"
    }}">>),
  Result = manifest:apply_to(Manifest, Event),
  ?assertEqual([
    {field, <<"foo">>,    <<"hello">>, {indexed, string }},
    {field, <<"fux">>,    3,           {pii,     integer}},
    {field, <<"foobar">>, 1,           {custom,  integer}}], Result).

manifest_from_json_test() ->
  Manifest = manifest_parser:parse(<<"{
    \"metadata\": {},
    \"field_mapping\": [{
      \"target_field\": \"foo\",
      \"type\": \"string\",
      \"core\": true,
      \"indexed\": true,
      \"pii\": false,
      \"source\": {
        \"lookup\": \"bar\"
      }}]}">>),
  ?assertEqual({ manifest, {}, [
    { field_mapping, <<"foo">>, {lookup, <<"bar">>}, {indexed, string} }]}, Manifest).

parse_decoded_field_visibility_core_indexed_test() ->
  Visibility = manifest_parser:parse_decoded_field_visibility({[
    {<<"core">>, true},
    {<<"indexed">>, true},
    {<<"pii">>, false}]}),
  ?assertEqual(indexed, Visibility).

parse_decoded_field_visibility_core_pii_test() ->
  Visibility = manifest_parser:parse_decoded_field_visibility({[
    {<<"core">>, true},
    {<<"indexed">>, false},
    {<<"pii">>, true}]}),
  ?assertEqual(pii, Visibility).

parse_decoded_field_visibility_non_core_pii_test() ->
  Visibility = manifest_parser:parse_decoded_field_visibility({[
    {<<"core">>, false},
    {<<"indexed">>, false},
    {<<"pii">>, true}]}),
  ?assertEqual(pii, Visibility).

parse_decoded_field_visibility_non_core_custom_test() ->
  Visibility = manifest_parser:parse_decoded_field_visibility({[
    {<<"core">>, false},
    {<<"indexed">>, false},
    {<<"pii">>, false}]}),
  ?assertEqual(custom, Visibility).

parse_decoded_field_mapping_test() ->
  DecodedFieldMapping = {[
    {<<"target_field">>,<<"foo">>},
    {<<"type">>,<<"string">>},
    {<<"core">>,true},
    {<<"indexed">>,true},
    {<<"pii">>,false},
    {<<"source">>,{[{<<"lookup">>,<<"bar">>}]}}
  ]},
  FieldMapping = manifest_parser:parse_decoded_field_mapping(DecodedFieldMapping),
  ?assertEqual({field_mapping, <<"foo">>, {lookup, <<"bar">>}, {indexed, string}}, FieldMapping).

parse_decoded_source_lookup_test() ->
  DecodedSource = {[{<<"lookup">>,<<"bar">>}]},
  Source = manifest_parser:parse_decoded_source(DecodedSource),
  ?assertEqual({lookup, <<"bar">>}, Source).


parse_decoded_source_beginning_of_test() ->
  DecodedSource = {[{<<"beginning_of">>,[{[{<<"path">>,<<"patient_information.age">>}]}, <<"month">>]}]},
  Source = manifest_parser:parse_decoded_source(DecodedSource),
  ?assertEqual({beginning_of, <<"patient_information.age">>, month}, Source).
