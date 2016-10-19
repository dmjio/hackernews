{ mkDerivation, aeson, attoparsec, base, base-compat, bytestring
, case-insensitive, directory, doctest, filemanip, filepath, hspec
, http-api-data, http-media, http-types, mmorph, mtl, network-uri
, QuickCheck, quickcheck-instances, stdenv, string-conversions
, text, url, vault
}:
mkDerivation {
  pname = "servant";
  version = "0.9.0.1";
  sha256 = "0km3vbbvmxk0f11g703n9p53pn78198r0czcyxps3xl4bryajzwr";
  libraryHaskellDepends = [
    aeson attoparsec base base-compat bytestring case-insensitive
    http-api-data http-media http-types mmorph mtl network-uri
    string-conversions text vault
  ];
  testHaskellDepends = [
    aeson attoparsec base base-compat bytestring directory doctest
    filemanip filepath hspec QuickCheck quickcheck-instances
    string-conversions text url
  ];
  homepage = "http://haskell-servant.readthedocs.org/";
  description = "A family of combinators for defining webservices APIs";
  license = stdenv.lib.licenses.bsd3;
}
