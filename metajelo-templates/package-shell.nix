{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:
let
  config = {
    packageOverrides = pkgs: rec {
      haskellPackages = pkgs.haskellPackages.override {
        overrides = haskellPackagesNew: haskellPackagesOld: rec {
          zio = haskellPackagesNew.callPackage ./zio.nix { };
        };
      };
    };
  };

  pkgs = import <nixpkgs> { inherit config; };

  # inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, bytestring, directory, exceptions
      , hpack, path, path-io, stdenv, turtle, zio
      }:
      mkDerivation {
        pname = "metajelo-templates";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        libraryToolDepends = [ hpack ];
        executableHaskellDepends = [
          base bytestring directory exceptions path path-io turtle zio
        ];
        prePatch = "hpack";
        homepage = "https://github.com/githubuser/metajelo-templates#readme";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
