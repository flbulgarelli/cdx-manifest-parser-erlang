-module(manifest).

-export([
  mapping/1,
  apply_to/2,
  apply_mapping_to/2,
  apply_field_mapping_to/2,
  extract_value/2]).

-export_type([
  manifest/0,
  field/0]).

-type manifest() :: {manifest, metadata(), mapping()}.
-type metadata() :: any().
-type mapping()  :: [field_mapping()].
-type field_mapping() :: { field_mapping, target(), source(), signature() }.
-type target()   :: field_name().
-type source()   :: {lookup, path()}
                  | {beginning_of, path(), time_unit() }.
-type signature() :: {visibility(), type(), extensions()}.
-type field_name() :: binary().
-type path()      :: binary().
-type time_unit() :: day | month | year.
-type visibility() :: custom | indexed | pii.
-type type() :: string
              | integer
              | long
              | float
              | double
              | date
              | location
              | boolean
              | enum.
-type extensions() :: [any()].

-type field() :: {field, field_name(), Value::any(), signature()}.

-type event() :: any(). %%Actualy jiffy json

-spec mapping(manifest()) -> mapping().
mapping({manifest, _, Mapping}) -> Mapping.

-spec apply_to(manifest(), event()) -> [field()].
apply_to(Manifest, Event) ->
  apply_mapping_to(mapping(Manifest), Event).

-spec apply_mapping_to(mapping(), event()) -> [field()].
apply_mapping_to(Mapping, Event) ->
  lists:flatmap(fun(FieldMapping) ->
      case catch apply_field_mapping_to(FieldMapping, Event) of
        {undefined_path, _, _} -> [];
        Value = {field, _, _, _} -> [Value]
      end
    end,
    Mapping).

apply_field_mapping_to({field_mapping, Target, Source, Signature}, Event) ->
 {field, Target, extract_value(Source, Event), Signature }.

extract_value({lookup, Path}, Event) ->
  lookup_raw(Path, Event);
extract_value({beginning_of, Path, TimeUnit}, Event) ->
  transform_json_date(lookup_raw(Path, Event), fun(Date) -> beginning_of(Date, TimeUnit) end);
extract_value({strip, _, _}, _Event) -> ok;
extract_value({concat, _, _}, _Event) -> ok;
extract_value({substring, _, _}, _Event) -> ok;
extract_value({milliseconds_between, _, _}, _Event) -> ok;
extract_value({convert_time, _, _}, _Event) -> ok.

lookup_raw(Path, Event)    ->
  case jsonpath:search(Path, Event) of
    undefined -> throw({undefined_path, Path, Event});
    X -> X
  end.

beginning_of({Date, _}, TimeUnit) -> blank_time(truncate_date(Date, TimeUnit)).

truncate_date(Date, day) -> Date;
truncate_date({Year, Month, _}, month) -> {Year, Month, 1};
truncate_date({Year, _,  _},  year) -> {Year, 1, 1}.

blank_time(Date) -> {Date, {0, 0, 0}}.

transform_json_date(RawJsonDate, Transformation) ->
  transform_bin_string(RawJsonDate, fun(JsonDate) ->
    ec_date:format("Y-m-dTh:i:s", Transformation(ec_date:parse(JsonDate)))
  end).
transform_bin_string(Raw, Transformation) ->
  binary:list_to_bin(Transformation(binary:bin_to_list(Raw))).