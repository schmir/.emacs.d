{
  lib,
  fetchFromGitHub,
  emacs,
  trivialBuild,
  age,
  f,
  with-editor,
}:
trivialBuild rec {
  pname = "passage";
  version = "0-unstable-2026-10-18";

  src = fetchFromGitHub {
    owner = "anticomputer";
    repo = "passage.el";
    rev = "e7385ecbb6aa857a799424d7838bd93624fa2d9c";
    hash = "sha256-x73D5BXdDgIMQFSatv/4v5Ld9NdQx4cANblNs6RarzE=";
  };

  packageRequires = [
    age
    f
    with-editor
  ];

  meta = with lib; {
    homepage = "https://github.com/anticomputer/passage.el";
    inherit (emacs.meta) platforms;
  };
}
