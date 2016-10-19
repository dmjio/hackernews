{ mkDerivation, base, directory, filepath, hspec-meta, stdenv }:
mkDerivation {
  pname = "hspec-discover";
  version = "2.3.2";
  sha256 = "0pz3izwdicvg2p5ahqjd5msxv4x5iwa2y30y0jwhnza13nwwjdpx";
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base directory filepath ];
  executableHaskellDepends = [ base directory filepath ];
  testHaskellDepends = [ base directory filepath hspec-meta ];
  doCheck = false;
  homepage = "http://hspec.github.io/";
  description = "Automatically discover and run Hspec tests";
  license = stdenv.lib.licenses.mit;
}
