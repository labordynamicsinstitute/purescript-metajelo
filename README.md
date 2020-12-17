# purescript-metajelo
DOM-based parsing tools and types for
[metajelo](https://github.com/labordynamicsinstitute/metajelo) XML

# Documentation

API docs are available [on Pursuit](https://pursuit.purescript.org/packages/purescript-metajelo).

# Buidling

The build depends on the Metajelo schema (xsd file) to generate some of
the source code.
Specify the desired (typically latest) schema version in
the `SCHEMA_VERSION` file;
this should be part of the URL for the version file located in the official
metajelo repository: e.g., `v0.8` for:

```
https://raw.githubusercontent.com/labordynamicsinstitute/metajelo/master/schema/v0.8/reproMetadata.xml
```

For details on how the build works, see the [template README](metajelo-templates/README.md).

## Docker

* First build the image: `./build-docker.sh`

* Run a command , e.g. `./psc.sh npm run prod`. This will run `npm run prod` the
command in the container with the CWD mounted and then exit. Alternatively if
you want to issue multiple commands in the container quickly, you can run
`./psc.sh bash`.

