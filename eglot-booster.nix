{
  lib,
  fetchFromGitHub,
  emacs,
  trivialBuild,
}:
trivialBuild rec {
  pname = "eglot-booster";
  version = "0.0.2";

  src = fetchFromGitHub {
    owner = "jdtsmith";
    repo = "eglot-booster";
    rev = "3f9159a8b7fe87e2f01280a2c4c98ca6dab49d13";
    hash = "sha256-yRZci0foZUw2Thx1SwSoY0iPf2DmkAnRp6U+rdx1Bas=";
  };

  meta = with lib; {
    homepage = "https://github.com/jdtsmith/eglot-booster";
    license = licenses.gpl3;
    inherit (emacs.meta) platforms;
  };
}
