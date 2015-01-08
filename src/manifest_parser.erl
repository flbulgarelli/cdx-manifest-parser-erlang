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

parse_decoded({Attrs}) ->
  Mapping = parse_decoded_mapping(lists:keyfind(<<"field_mapping">>, 1, Attrs)),
  {manifest, {}, Mapping}.

parse_decoded_mapping({ _, DecodedFieldMappings }) ->
  lists:map(fun(DecodedFieldMapping) ->
    parse_decoded_field_mapping(DecodedFieldMapping) end,
  DecodedFieldMappings).

parse_decoded_field_mapping(DecodedFieldMapping) ->
  Target = get(<<"target_field">>, DecodedFieldMapping),
  Type = case get(<<"type">>, DecodedFieldMapping) of
          <<"string">> -> string;
          <<"integer">> -> integer;
          <<"enum">> -> enum
        end,
  Visibility = parse_decoded_field_visibility(DecodedFieldMapping),
  Source = parse_decoded_source(get(<<"source">>, DecodedFieldMapping)),
  {field_mapping, Target, Source, {Visibility, Type}}.

parse_decoded_field_visibility({Attrs}) ->
  case {lists:member({<<"core">>,    true}, Attrs),
        lists:member({<<"pii">>,     true}, Attrs),
        lists:member({<<"indexed">>, true}, Attrs)} of
    {true, true,  _   } -> pii;
    {true,    _,  _   } -> indexed;
    {   _, true,  _   } -> pii;
    {   _,    _,  true} -> indexed;
    _                   -> custom
  end.

parse_decoded_source({[{<<"lookup">>,Path}]}) ->
  {lookup, Path };
parse_decoded_source({[{<<"beginning_of">>,[{[{<<"path">>,Path}]}, DecodedPeriod]}]}) ->
  Period = case DecodedPeriod of
    <<"year">> -> year;
    <<"month">> -> month
  end,
  {beginning_of, Path, Period }.


get(Key, {Attrs}) ->
  element(2, lists:keyfind(Key, 1, Attrs)).