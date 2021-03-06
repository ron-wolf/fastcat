with import <nixpkgs>{};

stdenv.mkDerivation {
  name = "ac-0.1.3";
  src = fetchgit {
    url = "https://github.com/vmchale/fastcat";
    rev = "5031c47631ea5a31d4eb8a584c8226a698453125";
    sha256 = "d2757a58a29c721f65a9677f803d5f51aa2b099fd0b2ab61f97a3a65dd01fce8";
  };
  outputs = [ "out" ];
  installPhase = ''
    atscc src/ac.dats -o $out -O3
  '';
  buildInputs = [ ats ];
}
