;; Elpaca calls an external emacs process to compile packages. It concatenates the
;; invocation-directory and invocation-name to determine the path to the emacs executable.  On nix
;; with the emacsWithPackages package, this is different to what would be found on PATH.  This
;; leads to problems during the build process. We help elpaca here a bit to find the right
;; executable.
(when (string-prefix-p "/nix/store" invocation-directory)
  (setq
   original-invocation-directory invocation-directory
   invocation-directory (expand-file-name "bin/" "~/.nix-profile/")))

(load-file (expand-file-name "install-elpaca.el" user-emacs-directory))

;; activate packages installed as part of the emacsWithPackages package
(package-activate-all)
(when (featurep 'vterm-autoloads)
  (message "init.el: adding vterm to elpaca-ignored-dependencies")
  (add-to-list 'elpaca-ignored-dependencies 'vterm))

;; Install use-package support
(elpaca elpaca-use-package
  ;; Enable :elpaca use-package keyword.
  (elpaca-use-package-mode)
  ;; Assume :elpaca t unless otherwise specified.
  (setq use-package-verbose t)
  (setq elpaca-use-package-by-default t))

(elpaca no-littering
  (require 'no-littering)
  (setq custom-file (no-littering-expand-etc-file-name "custom.el")))

(elpaca diminish
  (require 'diminish))

(load-file (expand-file-name "elpaca-update-seq.el" user-emacs-directory))

;; Block until current queue processed.
(elpaca-wait)
