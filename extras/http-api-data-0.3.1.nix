{ mkDerivation, base, bytestring, containers, directory, doctest
, filepath, hashable, hspec, HUnit, QuickCheck
, quickcheck-instances, stdenv, text, time, time-locale-compat
, unordered-containers, uri-bytestring, uuid, uuid-types
}:
mkDerivation {
  pname = "http-api-data";
  version = "0.3.1";
  sha256 = "1iwlmnv0xkqm925pjwazff86ji496b7b9wzzg21aqr70mabniaym";
  libraryHaskellDepends = [
    base bytestring containers hashable text time time-locale-compat
    unordered-containers uri-bytestring uuid-types
  ];
  testHaskellDepends = [
    base bytestring directory doctest filepath hspec HUnit QuickCheck
    quickcheck-instances text time unordered-containers uuid
  ];
  doCheck = false;
  homepage = "http://github.com/fizruk/http-api-data";
  description = "Converting to/from HTTP API data like URL pieces, headers and query parameters";
  license = stdenv.lib.licenses.bsd3;
}
