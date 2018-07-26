{ mkDerivation, base, Cabal, hedgehog, inline-java, jni, jvm
, stdenv, template-haskell, text
}:
mkDerivation {
  pname = "hedgehog-inline-java-testing";
  version = "0.1";
  src = ./.;
  setupHaskellDepends = [ base Cabal inline-java ];
  libraryHaskellDepends = [
    base inline-java jni jvm template-haskell text
  ];
  testHaskellDepends = [
    base hedgehog inline-java jni jvm template-haskell text
  ];
  description = "Example of using Hedgehog's model-based state machine testing together with inline-java";
  license = stdenv.lib.licenses.bsd3;
}
