-module(manifest).

-export([apply/2]).


field_value({pii, Value}) -> Value;
field_value({indexed, Value}) -> Value;
field_value({custom, Value}) -> Value.

field_type({pii, _}) -> pii;
field_type({indexed, _}) -> indexed;
field_type({custom, _}) -> custom.

field_type_for(Visibility) ->


def hash_key
    if @field["core"]
      return :pii if Event.pii?(@target_field)
      :indexed
    else
      return :pii if @field["pii"]
      return :indexed if @field["indexed"]
      :custom
    end
  end

  
%% manifest: 
%%{{...metadata...},{...mapping...}} 
%% Mapping: [ FieldMapping ]
%% FieldMapping : { field_mapping, Target, Type, Visibility, Source  }
%% Visibility : custom | indexed | pii
%% Source : 
