;;; setup-setup --- setup.el and package.el setup     -*- lexical-binding: t -*-

;;; Code:

;;; Initialize package.el
(progn
  ;; make sure to set this before we call (package-initialize). Otherwise site-lisp will bail out
  ;; with an error when we try to set the value.
  (setopt package-user-dir (expand-file-name "var/elpa-packages" user-emacs-directory)
          package-gnupghome-dir (expand-file-name "var/elpa-gnupg" user-emacs-directory)
          package-archives
          '(("melpa" . "https://melpa.org/packages/")
            ("nongnu" . "https://elpa.nongnu.org/nongnu/")
            ("gnu" . "https://elpa.gnu.org/packages/")))
  (package-initialize))

;;; Install no-littering
(progn
  (setq custom-file (expand-file-name "etc/custom.el" user-emacs-directory))
  (sup-package-install 'no-littering)
  (no-littering-theme-backups))

;;; Install setup.el
(progn
  (sup-package-install '(setup :url "https://codeberg.org/pkal/setup.el.git"))
  (require 'setup)

  (setup-define :package
    (lambda (package)
      `(sup-package-install ',package))
    :documentation "Install PACKAGE if it hasn't been installed yet.
The first PACKAGE can be used to deduce the feature context."
    :repeatable t
    :shorthand (lambda (form)
                 (let ((pkg-desc (cadr form)))
                   (if (consp pkg-desc)
                       (car pkg-desc)
                     pkg-desc)))))

;; use-package: Useful for trying out packages with copy-paste install instructions
;; let's keep use-package as it's useful when trying out package, so we can copy and paste the
;; install instructions.
(setup (:package use-package)
  ;; Let imenu see `use-package' declarations
  (setopt use-package-enable-imenu-support t
          use-package-always-ensure t)
  (require 'use-package-ensure))

(provide 'setup-setup)
;;; setup-setup.el ends here
