# purescript-metajelo
DOM-based parsing tools and types for
[metajelo](https://github.com/labordynamicsinstitute/metajelo) XML

# Documentation

API docs are available [on Pursuit](https://pursuit.purescript.org/packages/purescript-metajelo).

# Buidling

Specify the desired (typically latest) schema version in the SCHEMA_VERSION file;
this should be part of the URL for the version file located in the official
metajelo repository: e.g., `v0.8` for:

```
https://github.com/labordynamicsinstitute/metajelo/blob/master/schema/v0.8/reproMetadata.xml
```

## Docker

* First build the image: `./build-docker.sh`

* Run a command , e.g. `./psc.sh npm run prod`. This will run `npm run prod` the
command in the container with the CWD mounted and then exit. Alternatively if
you want to issue multiple commands in the container quickly, you can run
`./psc.sh bash`.

