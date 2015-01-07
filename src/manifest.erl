-module(manifest).

-export([
  apply_to/2,
  apply_mapping_to/2,
  apply_field_mapping_to/2,
  extract_value/2]).

mapping({manifest, _, Mapping}) -> Mapping.

apply_to(Manifest, Event) ->
  apply_field_mapping_to(mapping(Manifest), Event).

apply_mapping_to(Mapping, Event) ->
  lists:map(fun(FieldMapping) ->
      apply_field_mapping_to(FieldMapping, Event)
    end,
    Mapping).

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