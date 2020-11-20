{ mkDerivation, base, fetchgit, hpack, mtl, stdenv, transformers
, unexceptionalio, unexceptionalio-trans
}:
mkDerivation {
  pname = "zio";
  version = "0.1.0.0";
  src = fetchgit {
    url = "https://github.com/bbarker/haskell-zio.git";
    sha256 = "1aaqypk592hwv2b1s4jyl6yg2yar2h6s03bi5w4qzw63m45qhazp";
    rev = "78c8656861e2491aaf6bdcd292ef26d3cf20ced9";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [
    base mtl transformers unexceptionalio unexceptionalio-trans
  ];
  libraryToolDepends = [ hpack ];
  testHaskellDepends = [
    base mtl transformers unexceptionalio unexceptionalio-trans
  ];
  # prePatch = "hpack";
  homepage = "https://github.com/githubuser/haskell-zio#readme";
  license = stdenv.lib.licenses.mpl20;
}
