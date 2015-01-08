CDX Manifest Parser
===================

This repository contains a simple library for parsing and processing [CDX Manifests](http://dxapi.org/#/manifest) in the Erlang language.

# Parsing Manifests

```erlang
```

# Processing Manifests

Manifests are proceesed using the `manifest:apply_to/2` function, which expects a manifest, as returned by `manifest_parser:parse/1`, and an event. The event is a JSON encoded as Erlang term using [Jiffy](https://github.com/davisp/jiffy) data format.

```erlang
> manifest:apply_to(
    {manifest, {}, [ { field_mapping, <<"foo">>, {lookup, <<"patient_information.age">>}, {indexed, string} }]},
    {[{<<"patient_information">>,{[{<<"age">>,21}]}}]}).
[{field,<<"foo">>,21,{indexed,string}}]
```

`manifest:apply_to/2` returns a list of processed fields according to the manifest. A field is a 4-tuple that has the following format:

```erlang
{field, Name, Value, Signature}
```

...where signature has the same format as described previously.

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
* Timezones are not supported when parsing dates. 
