## Building

### Nix

#### Updating cabal build file

```
$ nix-shell -p haskellPackages.hpack cabal2nix
$ hpack
$ cabal2nix --shell . > package-shell.nix
```

#### Build the project

Note, if things go poorly, it is always worth trying
`cabal --enable-nix clean` or maybe `cabal clean` as well.


At this point, you can exit the shell used to generate
the `.cabal` and `.nix` file if you haven't already.

First, make cabal-install available if it isnt, then
run the build.

```
$ nix-shell -p cabal-install
$ nix-shell shell.nix --run "cabal build"
```

#### Running

```
nix-shell shell.nix --run "cabal exec metajelo-templates"
```