-module(manifest).

-export([
  apply_to/2,
  apply_mapping_to/2,
  apply_field_mapping_to/2,
  extract_value/2]).

mapping({manifest, _, Mapping}) -> Mapping.

apply_to(Manifest, Event) ->
  apply_mapping_to(mapping(Manifest), Event).

apply_mapping_to(Mapping, Event) ->
  lists:map(fun(FieldMapping) ->
      apply_field_mapping_to(FieldMapping, Event)
    end,
    Mapping).

apply_field_mapping_to({field_mapping, Target, Source, Signature}, Event) ->
 {field, Target, extract_value(Source, Event), Signature }.

extract_value({lookup, Path}, Event) ->
  lookup_raw(Path, Event);
extract_value({beginning_of, Path, TimeUnit}, Event) ->
  beginning_of(lookup_date(Path, Event), TimeUnit);
extract_value({strip, _, _}, _Event) -> ok;
extract_value({concat, _, _}, _Event) -> ok;
extract_value({substring, _, _}, _Event) -> ok;
extract_value({milliseconds_between, _, _}, _Event) -> ok;
extract_value({convert_time, _, _}, _Event) -> ok.

lookup_raw(Path, Event)    ->
  case jsonpath:search(Path, Event) of
    undefined -> error({undefined_path, Path, Event});
    X -> X
  end.
lookup_string(Path, Event) ->
  binary:bin_to_list(lookup_raw(Path, Event)).
lookup_date(Path, Event)   ->
  ec_date:parse(lookup_string(Path, Event)).

beginning_of({{_, _,   Day}, _}, day) -> Day;
beginning_of({{_, Month, _}, _}, month) -> Month;
beginning_of({{Year, _,  _}, _},  year) -> Year.