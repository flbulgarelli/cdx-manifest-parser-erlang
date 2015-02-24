-module(manifest_parser).

%% manifest_parser: manifest_parser library's entry point.

-export([
  parse/1,
  parse_decoded_mapping/1,
  parse_decoded_field_mapping/1,
  parse_decoded_source/1,
  parse_decoded_field_visibility/1]).

parse(ManifestJson) ->
  parse_decoded(jiffy:decode(ManifestJson)).

parse_decoded(DecodedManifest) ->
  Mapping = parse_decoded_mapping(decoded_json:get(<<"field_mapping">>, DecodedManifest)),
  {manifest, {}, Mapping}.

parse_decoded_mapping(DecodedFieldMappings) ->
  lists:map(fun(DecodedFieldMapping) ->
    parse_decoded_field_mapping(DecodedFieldMapping) end,
  DecodedFieldMappings).

parse_decoded_field_mapping(DecodedFieldMapping) ->
  Target = decoded_json:get(<<"target_field">>, DecodedFieldMapping),
  Type = case decoded_json:get(<<"type">>, DecodedFieldMapping) of
          <<"string">> -> string;
          <<"integer">> -> integer;
          <<"long">> -> long;
          <<"float">> -> float;
          <<"double">> -> double;
          <<"date">> -> date;
          <<"location">> -> location;
          <<"boolean">> -> boolean;
          <<"enum">> -> enum
        end,
  Visibility = parse_decoded_field_visibility(DecodedFieldMapping),
  Source = parse_decoded_source(decoded_json:get(<<"source">>, DecodedFieldMapping)),
  {field_mapping, Target, Source, {Visibility, Type}}.

parse_decoded_field_visibility(DecodedFieldMapping) ->
  case {decoded_json:is_set(<<"core">>,    DecodedFieldMapping),
        decoded_json:is_set(<<"pii">>,     DecodedFieldMapping),
        decoded_json:is_set(<<"indexed">>, DecodedFieldMapping)} of
    {true, true,  _   } -> pii;
    {true,    _,  _   } -> indexed;
    {   _, true,  _   } -> pii;
    {   _,    _,  true} -> indexed;
    _                   -> custom
  end.

parse_decoded_source({[{<<"lookup">>,Path}]}) ->
  {lookup, Path };
parse_decoded_source({[{<<"beginning_of">>,[{[{<<"lookup">>,Path}]}, DecodedPeriod]}]}) ->
  Period = case DecodedPeriod of
    <<"day">> -> day;
    <<"year">> -> year;
    <<"month">> -> month
  end,
  {beginning_of, Path, Period }.