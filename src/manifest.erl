-module(manifest).

-export([
  parse/1,
  apply_to/2,
  parse_decoded_mapping/1,
  parse_decoded_field_mapping/1,
  parse_decoded_source/1,
  apply_mapping_to/2,
  parse_decoded_field_visibility/1,
  apply_field_mapping_to/2,
  extract_value/2]).

mapping({manifest, _, Mapping}) -> Mapping.

parse(ManifestJson) ->
  parse_decoded(jiffy:decode(ManifestJson)).

parse_decoded(DecodedManifest) ->
  Mapping = parse_decoded_mapping(lists:keyfind(<<"field_mapping">>, 1, DecodedManifest)),
  {manifest, {}, Mapping}.

parse_decoded_mapping({ _, DecodedFieldMappings }) ->
  lists:map(fun(DecodedFieldMapping) ->
    parse_decoded_field_mapping(DecodedFieldMapping) end,
  DecodedFieldMappings).

get(Key, {Attrs}) ->
  element(2, lists:keyfind(Key, 1, Attrs)).

parse_decoded_field_mapping(DecodedFieldMapping) ->
  Target = binary:bin_to_list(get(<<"target_field">>, DecodedFieldMapping)),
  Type = case get(<<"type">>, DecodedFieldMapping) of
          <<"string">> -> string;
          <<"integer">> -> interger;
          <<"enum">> -> enum
        end,
  Visibility = parse_decoded_field_visibility(DecodedFieldMapping),
  Source = parse_decoded_source(get(<<"source">>, DecodedFieldMapping)),
  {field_mapping, Target, Source, {Visibility, Type}}.

parse_decoded_source({[{<<"lookup">>,DecodedPath}]}) ->
  {lookup, binary:bin_to_list(DecodedPath) };
parse_decoded_source({[{<<"beginning_of">>,[{[{<<"path">>,DecodedPath}]}, DecodedPeriod]}]}) ->
  Period = case DecodedPeriod of
    <<"year">> -> year;
    <<"month">> -> month
  end,
  {beginning_of, binary:bin_to_list(DecodedPath), Period }.

apply_to(Manifest, Event) ->
  apply_field_mapping_to(mapping(Manifest), Event).

apply_mapping_to(Mapping, Event) ->
  lists:map(fun(FieldMapping) ->
      apply_field_mapping_to(FieldMapping, Event)
    end,
    Mapping).

parse_decoded_field_visibility({Attrs}) ->
  case {lists:member({<<"core">>,    <<"true">>}, Attrs),
        lists:member({<<"pii">>,     <<"true">>}, Attrs),
        lists:member({<<"indexed">>, <<"true">>}, Attrs)} of
    {true, true,  _   } -> pii;
    {true,    _,  _   } -> indexed;
    {   _, true,  _   } -> pii;
    {   _,    _,  true} -> indexed;
    _                   -> custom
  end.

apply_field_mapping_to({field_mapping, Target, Source, Signature}, Event) ->
 {field, ok, extract_value(Source, Event), Signature }.

extract_value({lookup, Path}, Event) ->
  jsonpath:search(Path, Event);
extract_value({beginning_of, _, _}, Event) -> ok;
extract_value({strip, _, _}, Event) -> ok;
extract_value({concat, _, _}, Event) -> ok;
extract_value({substring, _, _}, Event) -> ok;
extract_value({milliseconds_between, _, _}, Event) -> ok;
extract_value({convert_time, _, _}, Event) -> ok.

%% manifest: {manifest, Metadata, Mapping }
%% Mapping: [ FieldMapping ]
%% FieldMapping : { field_mapping, Target, Source, Signature }
%% Signature : {Visibility, Type}
%% Visibility : custom | indexed | pii
%% Type : string | integer | boolean
%% Source :

%% field : { field, Name, Value, Signature  }