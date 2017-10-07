{ mkDerivation, ansi-wl-pprint, base, bytestring, Cabal
, composition-prelude, containers, directory, file-embed, hspec
, hspec-megaparsec, http-client, megaparsec, microlens, MonadRandom
, mtl, optparse-applicative, process, random-shuffle
, recursion-schemes, stdenv, tar, template-haskell, text, zlib
}:
mkDerivation {
  pname = "madlang";
  version = "2.4.2.1";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  setupHaskellDepends = [ base Cabal directory file-embed process ];
  libraryHaskellDepends = [
    ansi-wl-pprint base bytestring composition-prelude containers
    directory file-embed http-client megaparsec microlens MonadRandom
    mtl optparse-applicative random-shuffle recursion-schemes tar
    template-haskell text zlib
  ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [
    base hspec hspec-megaparsec megaparsec mtl text
  ];
  homepage = "https://hub.darcs.net/vmchale/madlang";
  description = "Randomized templating language DSL";
  license = stdenv.lib.licenses.bsd3;
}
