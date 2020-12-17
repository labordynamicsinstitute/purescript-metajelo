{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, bytestring, containers, directory
      , exceptions, hpack, http-conduit, path, path-io, safe-exceptions
      , stdenv, string-interpolate, text, xml-conduit, xml-types, zio
      }:
      mkDerivation {
        pname = "metajelo-templates";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        libraryToolDepends = [ hpack ];
        executableHaskellDepends = [
          base bytestring containers directory exceptions http-conduit path
          path-io safe-exceptions string-interpolate text xml-conduit
          xml-types zio
        ];
        prePatch = "hpack";
        homepage = "https://github.com/labordynamicsinstitute/metajelo-ui#readme";
        license = stdenv.lib.licenses.mpl20;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
