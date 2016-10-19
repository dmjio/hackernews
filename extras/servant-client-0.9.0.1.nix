{ mkDerivation, aeson, attoparsec, base, base64-bytestring
, bytestring, deepseq, exceptions, hspec, http-api-data
, http-client, http-client-tls, http-media, http-types, HUnit, mtl
, network, network-uri, QuickCheck, safe, servant, servant-server
, stdenv, string-conversions, text, transformers
, transformers-compat, wai, warp
}:
mkDerivation {
  pname = "servant-client";
  version = "0.9.0.1";
  sha256 = "1s33hmd6xjyrqv3bjjc1ymwbqx08hkap720pcbm7pxlv61a2x5ix";
  libraryHaskellDepends = [
    aeson attoparsec base base64-bytestring bytestring exceptions
    http-api-data http-client http-client-tls http-media http-types mtl
    network-uri safe servant string-conversions text transformers
    transformers-compat
  ];
  testHaskellDepends = [
    aeson base bytestring deepseq hspec http-api-data http-client
    http-media http-types HUnit network QuickCheck servant
    servant-server text transformers transformers-compat wai warp
  ];
  doCheck = false;
  homepage = "http://haskell-servant.readthedocs.org/";
  description = "automatical derivation of querying functions for servant webservices";
  license = stdenv.lib.licenses.bsd3;
}
