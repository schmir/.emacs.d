{
  lib,
  emacs,
  melpaBuild,
}:
melpaBuild rec {
  pname = "setup";
  version = "1.5.0.${builtins.substring 0 8 src.lastModifiedDate}";

  src = builtins.fetchTree {
    type = "git";
    url = "https://codeberg.org/pkal/setup.el.git";
    rev = "de9b86c6f29cf9888e911924ffc4ce47312320c3";
    narHash = "sha256-lLeKsLFf1+Bjk6ETq+0Nncw6DPOVxE49cAexdLAr/5U=";
  };

  meta = with lib; {
    homepage = "https://codeberg.org/pkal/setup.el";
    license = licenses.gpl3Plus;
    inherit (emacs.meta) platforms;
  };
}
