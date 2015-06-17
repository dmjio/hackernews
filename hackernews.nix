{ mkDerivation, aeson, attoparsec, base, bytestring, either
, HsOpenSSL, hspec, http-streams, io-streams, stdenv, text, time
, transformers
}:
mkDerivation {
  pname = "hackernews";
  version = "0.5.0.0";
  src = ./.;
  buildDepends = [
    aeson attoparsec base bytestring either HsOpenSSL http-streams
    io-streams text time transformers
  ];
  testDepends = [ base hspec transformers ];
  description = "API for Hacker News";
  license = stdenv.lib.licenses.mit;
}
