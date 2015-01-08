CDX Manifest Parser
===================

This repository contains a simple library for parsing and processing [CDX Manifests](http://dxapi.org/#/manifest) in the Erlang language.

# Parsing Manifests

```erlang
```

# Processing Manifests

The manifest parser expects Events to be complaint with [Jiffy](https://github.com/davisp/jiffy) encoding format of JSON into Erlang terms.

```erlang
```

# Full Sample

```erlang

> Manifest = manifest_parser:parse(<<"{
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
