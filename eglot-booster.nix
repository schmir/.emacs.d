{
  lib,
  fetchFromGitHub,
  emacs,
  trivialBuild,
}:
trivialBuild rec {
  pname = "eglot-booster";
  version = "0.1.0";

  src = fetchFromGitHub {
    owner = "jdtsmith";
    repo = "eglot-booster";
    rev = "cab7803c4f0adc7fff9da6680f90110674bb7a22";
    hash = "sha256-xUBQrQpw+JZxcqT1fy/8C2tjKwa7sLFHXamBm45Fa4Y=";
  };

  meta = with lib; {
    homepage = "https://github.com/jdtsmith/eglot-booster";
    license = licenses.gpl3;
    inherit (emacs.meta) platforms;
  };
}
