-module(manifest_parser).

%% manifest_parser: manifest_parser library's entry point.

-export([
  parse/1,
  parse_decoded_mapping/1,
  parse_decoded_field_mapping/1]).

-ifdef(TEST).
-export([
  parse_decoded_source/1,
  parse_decoded_field_visibility/1]).
-endif.

parse(ManifestJson) ->
  parse_decoded(jiffy:decode(ManifestJson)).

parse_decoded(Manifest) ->
  Mapping = parse_decoded_mapping(decoded_json:get(<<"field_mapping">>, Manifest)),
  {manifest, {}, Mapping}.

parse_decoded_mapping(FieldMappings) ->
  lists:map(fun(FieldMapping) ->
    parse_decoded_field_mapping(FieldMapping) end,
  FieldMappings).

parse_decoded_field_mapping(FieldMapping) ->
  Target = decoded_json:get(<<"target_field">>, FieldMapping),
  Type = case decoded_json:get(<<"type">>, FieldMapping) of
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
  Visibility = parse_decoded_field_visibility(FieldMapping),
  Source = parse_decoded_source(decoded_json:get(<<"source">>, FieldMapping)),
  {field_mapping, Target, Source, {Visibility, Type}}.

parse_decoded_field_visibility(FieldMapping) ->
  case {decoded_json:is_set(<<"core">>,    FieldMapping),
        decoded_json:is_set(<<"pii">>,     FieldMapping),
        decoded_json:is_set(<<"indexed">>, FieldMapping)} of
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