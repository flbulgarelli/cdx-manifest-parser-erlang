

apply_manifest_test() ->
  Fields = manifest:apply_to(
    {{}, [[
      {target_field, "foo"}, 
      {type, string}, 
      {core, true}, 
      {indexed, true}, 
      {pii, false}, 
      {source,  
       {beginning_of, "patient_information.age", "month"}
      }]]},
    [
      {month, "jan_foo_bar"}, 
      {baz, bar}]),
  ?assertEqual([{foo, {indexed, "jan"}}], Fields).


apply_field_test() ->
  Field = manifest:apply_field_to([
      {target_field, "foo"}, 
      {type, string}, 
      {core, true}, 
      {indexed, true}, 
      {pii, false}, 
      {source,  
       {beginning_of, "patient_information.age", "month"}
      }],
    [
      {month, "jan_foo_bar"}, 
      {baz, bar}]),
  ?assertEqual({foo, {indexed, "jan"}}, Field).

extract_value_test() ->
  Value = extract_value(
    {beginning_of, "patient_information.age", "month"}, 
    [ {month, "jan_foo_bar"}, {baz, bar}]),
  ?assertEqual("jan", Value).


field_type_for_core_indexed_test() -> 
  FieldType = field_type_for([
    {core, true}, 
    {indexed, true}, 
    {pii, false}]),
  ?assertEqual(indexed, FieldType).

field_type_for_core_pii_test() -> 
  FieldType = field_type_for([
    {core, true}, 
    {indexed, false}, 
    {pii, true}]),
  ?assertEqual(pii, FieldType).

field_type_for_non_core_pii_test() -> 
  FieldType = field_type_for([
    {core, false}, 
    {indexed, false}, 
    {pii, true}]),
  ?assertEqual(pii, FieldType).

field_type_for_non_core_custom_test() -> 
  FieldType = field_type_for([
    {core, false}, 
    {indexed, false}, 
    {pii, false}]),
  ?assertEqual(indexed, FieldType).

