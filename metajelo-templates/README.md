## Building

### Nix

#### Updating cabal build file

```
$ nix-shell -p haskellPackages.hpack cabal2nix
$ hpack
$ cabal2nix --shell . > shell.nix
```

#### Build the project

Note, if things go poorly, it is always worth trying
`cabal --enable-nix clean` or maybe `cabal clean` as well.


At this point, you can exit the shell used to generate 

```
$ nix-shell -p cabal-install
$ cabal --enable-nix build
```
