{
  lib,
  emacs,
  melpaBuild,
}:
melpaBuild rec {
  pname = "fm-ruff";
  version = "0.${builtins.substring 0 8 src.lastModifiedDate}";

  src = builtins.fetchTree {
    type = "github";
    owner = "schmir";
    repo = "fm-ruff";
    rev = "92b0263f3fd9ab0cb34374573910386879a74a2e";
    narHash = "sha256-ZDRJg0vcDFoZ+lPbXWNtMbtxFTA3fBe0A2I+LP+fVRk=";
  };

  meta = with lib; {
    homepage = "https://github.com/schmir/fm-ruff";
    inherit (emacs.meta) platforms;
  };
}
