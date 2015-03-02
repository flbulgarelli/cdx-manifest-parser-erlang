[![Build Status](https://travis-ci.org/instedd/cdx-manifest-parser.svg?branch=master)](https://travis-ci.org/instedd/cdx-manifest-parser)


CDX Manifest Parser
===================

This repository contains a simple library for parsing and processing [CDX Manifests](http://dxapi.org/#/manifest) in the Erlang language.

# Core Types

The core type of this library is the manifest, which is a 3-tuple with the following format:

```erlang
Manifest :: {manifest, Metadata, Mapping}
Metadata :: {} (*) See below
Mapping :: [ FieldMapping ]
FieldMapping :: { field_mapping, Target, Source, Signature }
Signature :: {Visibility, Type, Extensions}
Visibility :: custom | indexed | pii
Type :: string
        | integer
        | long
        | float
        | double
        | date
        | location
        | boolean
        | enum
Source :: {lookup, Path } |
          {beginning_of, Path, TimeUnit }
Path :: String
TimeUnit :: day | month | year
Extensions :: [ UserDefinedExtensions ]
```

# Processing Manifests

Manifests are proceesed using the `manifest:apply_to/2` function, which expects a manifest and an event. The event is a JSON encoded as Erlang term using [Jiffy](https://github.com/davisp/jiffy) data format.

```erlang
> manifest:apply_to(
    {manifest, {}, [ { field_mapping, <<"foo">>, {lookup, <<"patient_information.age">>}, {indexed, string} }]},
    {[{<<"patient_information">>,{[{<<"age">>,21}]}}]}).
[{field,<<"foo">>,21,{indexed,string}}]
```

`manifest:apply_to/2` returns a list of processed fields according to the manifest. A field is a 4-tuple that has the following format:

```erlang
 Field :: {field, Name, Value, Signature}
 Signature :: ...as described previously...
```

`manifest:apply_to/2` will fail under some scenarios, raising the following errors:
  * `{undefined_path, Path, Event}` if the path does not point to a valid value within the given event

# Parsing Manifests

In order to parse a manifest from a JSON string, you need to create a parser, using `manifest_parser:new()`. Then use `manifest_parser:parse/2`, which expects a JSON string and returns a manifest. 

```erlang
> Parser = manifest_parser:new(),
> manifest_parser:parse(Parser, <<"{
    \"metadata\": {},
    \"field_mapping\": [{
      \"target_field\": \"foo\",
      \"type\": \"string\",
      \"core\": true,
      \"indexed\": true,
      \"pii\": false,
      \"source\": {
        \"lookup\": \"bar\"
      }}]}">>).
{manifest,{},
          [{field_mapping,<<"foo">>,
                          {lookup,<<"bar">>},
                          {indexed,string}}]}
```

## Using Extensions

Sometimes you would like to add to the signature of your fields information that is not cdx-compliant. That is why we support signature extensions. 

Extensions are simply funcitions that take the mapping decoded json - encoded in jiffy format -, and return a list with zero or more elements you want to extract. Example:

```erlang
> Parser = manifest_parser:new([fun(Decoded) -> [decoded_json:get(<<"x-foo">>, Decoded)] end]),
> Manifest = manifest_parser:parse(Parser, <<"{
    \"metadata\": {},
    \"field_mapping\": [{
      \"target_field\": \"foo\",
      \"type\": \"string\",
      \"core\": true,
      \"indexed\": true,
      \"pii\": false,
      \"x-foo\": 10,
      \"source\": {
        \"lookup\": \"bar\"
      }}]}">>).
{manifest,{},
          [{field_mapping,<<"foo">>,
                          {lookup,<<"bar">>},
                          {indexed,string,[10]}}]}
```

# Full Sample

```erlang
> Parser = manifest_parser:new(),
> Manifest = manifest_parser:parse(Parser, <<"{
    \"metadata\": {},
    \"field_mapping\": [{
      \"target_field\": \"foo\",
      \"type\": \"string\",
      \"core\": true,
      \"indexed\": true,
      \"pii\": false,
      \"source\": {
        \"lookup\": \"foo.bar\"
      }}, {
      \"target_field\": \"fux\",
      \"type\": \"integer\",
      \"core\": false,
      \"indexed\": false,
      \"pii\": true,
      \"source\": {
        \"lookup\": \"foo.fux\"
      }}, {
      \"target_field\": \"foobar\",
      \"type\": \"integer\",
      \"core\": false,
      \"indexed\": false,
      \"pii\": false,
      \"source\": {
        \"beginning_of\": [{\"path\":\"foo.baz\"}, \"month\"]
      }}]
    }">>).
{manifest,{},
          [{field_mapping,<<"foo">>,
                          {lookup,<<"foo.bar">>},
                          {indexed,string}},
           {field_mapping,<<"fux">>,
                          {lookup,<<"foo.fux">>},
                          {pii,integer}},
           {field_mapping,<<"foobar">>,
                          {beginning_of,<<"foo.baz">>,month},
                          {custom,integer}}]}

> Event = jiffy:decode(<<"{
    \"foo\": {
      \"bar\":\"hello\",
      \"fux\": 3,
      \"baz\": \"1990-01-30T00:00:00\"
    }}">>).
{[{<<"foo">>,
   {[{<<"bar">>,<<"hello">>},
     {<<"fux">>,3},
     {<<"baz">>,<<"1990-01-30T00:00:00">>}]}}]}

> Result = manifest:apply_to(Manifest, Event).
[{field,<<"foo">>,<<"hello">>,{indexed,string}},
 {field,<<"fux">>,3,{pii,integer}},
 {field,<<"foobar">>,ok,{custom,integer}}]

```


# Compliance with DXAPI

This library has limited support of CDX Manifests:
* Metadata is not being parsed
* The following mapping attributes are not supported:
  * ``options``
  * ``valid_values``
* Only ``lookup`` and ``beginning_of`` sources are supported
* Timezones are not supported when parsing dates. 
