## Building

### Nix

First, load the necessary build tools:

```
$ nix-shell -p haskellPackages.hpack cabal-install cabal2nix
```

Then, build the project

```
$ hpack
$ cabal2nix --shell . > package-shell.nix
$ nix-shell package-shell.nix
$ cabal build
```

# Updating other dependencies

ZIO is currently not in nixpkgs; to update it to the latest on master:

```
cabal2nix https://github.com/bbarker/haskell-zio.git > zio.nix
```

Comment out the line `prePatch = "hpack";` in `zio.nix`, as it seems
to cause the build to fail.


Note: you can use haskellPackages.callPackage on the output of
`cabal2nix cabal://package-version` and put it in an override for the
nixpkgs haskell package set