{
  lib,
  fetchFromGitHub,
  emacs,
  trivialBuild,
}:
trivialBuild rec {
  pname = "fm-ruff";
  version = "0-unstable-2026-10-18";

  src = fetchFromGitHub {
    owner = "schmir";
    repo = "fm-ruff";
    rev = "92b0263f3fd9ab0cb34374573910386879a74a2e";
    hash = "sha256-ZDRJg0vcDFoZ+lPbXWNtMbtxFTA3fBe0A2I+LP+fVRk=";
  };

  meta = with lib; {
    homepage = "https://github.com/schmir/fm-ruff";
    inherit (emacs.meta) platforms;
  };
}
