{
  lib,
  emacs,
  melpaBuild,
  age,
  f,
  with-editor,
}:
melpaBuild rec {
  pname = "passage";
  version = "0.${builtins.substring 0 8 src.lastModifiedDate}";

  src = builtins.fetchTree {
    type = "github";
    owner = "anticomputer";
    repo = "passage.el";
    rev = "e7385ecbb6aa857a799424d7838bd93624fa2d9c";
    narHash = "sha256-x73D5BXdDgIMQFSatv/4v5Ld9NdQx4cANblNs6RarzE=";
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
