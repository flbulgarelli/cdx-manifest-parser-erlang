-module(manifest).

-export([parse_field_visibility/1, 
  apply_field_mapping_to/2, 
  extract_value/2]).


parse_field_visibility(RawManifest) ->
  case {lists:member({core, true}, RawManifest), 
        lists:member({pii, true}, RawManifest),
        lists:member({indexed, true}, RawManifest)} of
    {true, true,  _   } -> pii;
    {true,    _,  _   } -> indexed;
    {   _, true,  _   } -> pii;
    {   _,    _,  true} -> indexed;
    _                   -> custom
  end.


apply_field_mapping_to(FieldMapping, Event) ->
 ok.

extract_value(Source, Event) -> ok. 

  
%% manifest: {manifest, Metadata, Mapping }
%% Mapping: [ FieldMapping ]
%% FieldMapping : { field_mapping, Target, Source, Signature }
%% Signature : {Visibility, Type}
%% Visibility : custom | indexed | pii
%% Type : string | integer | boolean
%% Source : 

%% field : { field, Name, Value, Signature  }