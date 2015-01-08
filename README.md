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

# Compliance with DXAPI

This library has limited support of CDX Manifests:
* Metadata is not being parsed
* The following mapping attributes are not supported:
  * ``options``
  * ``valid_values``
* Only ``lookup`` and ``beginning_of`` sources are supported
