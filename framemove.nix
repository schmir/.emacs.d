{
  lib,
  fetchFromGitHub,
  emacs,
  trivialBuild,
}:
trivialBuild rec {
  pname = "framemove";
  version = "0.10";

  src = fetchFromGitHub {
    owner = "emacsmirror";
    repo = "framemove";
    rev = "0faa8a4937f398e4971fc877b1c294100506b645";
    hash = "sha256-9A0lg6wlTlOQ6jNMldt7h6ddpF5e83YHoJHSzULHnfw=";
  };

  meta = with lib; {
    homepage = "https://github.com/emacsmirror/framemove";
    license = licenses.gpl3;
    inherit (emacs.meta) platforms;
  };
}
