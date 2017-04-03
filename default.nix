{ mkDerivation, ansi-wl-pprint, base, composition, containers
, directory, hspec, hspec-megaparsec, lens, megaparsec, mtl
, mwc-random, optparse-applicative, stdenv, text, tibetan-utils
}:
mkDerivation {
  pname = "madlang";
  version = "2.1.0.1";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    ansi-wl-pprint base composition containers directory lens
    megaparsec mtl mwc-random optparse-applicative text tibetan-utils
  ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [
    base hspec hspec-megaparsec megaparsec mtl text
  ];
  homepage = "https://github.com/vmchale/madlang#readme";
  description = "Randomized templating language DSL";
  license = stdenv.lib.licenses.bsd3;
}
