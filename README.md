# purescript-metajelo
DOM-based parsing tools and types for
[metajelo](https://github.com/labordynamicsinstitute/metajelo) XML

# Documentation

API docs are available [on Pursuit](https://pursuit.purescript.org/packages/purescript-metajelo).

# Buidling

## Docker

* First build the image: `./build-docker.sh`

* Run a command , e.g. `./psc.sh npm run build`. This will run `npm run build`
the command in the container with the CWD mounted and then exit. Alternatively
if you want to issue multiple commands in the container quickly, you can run
`./psc.sh bash`.

## Shell environment

   1. git clone git://github.com/kaitanie/nix-purescript-example-project
   2. cd nix-purescript-example-project
   3. nix-shell

   Purescript, psc-package, and such from the pinned nixpkgs version should now
   be available in the `nix-shell` environment and can be used normally.

* Known issues

** Currently the version of Pulp from nixpkgs throws exception when used

   The relevant issue is here: https://github.com/NixOS/nixpkgs/issues/40406

   Currently the workaround is to use `npm install -g pulp` and use that one
   instead of the nixpkgs version.
