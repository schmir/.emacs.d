(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
	     '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

(defvar prelude-packages
  '(clojure-mode clojure-cheatsheet paredit bm boxquote key-chord lua-mode magit git-messenger diminish
		 highlight-symbol framemove rainbow-mode jedi
		 whole-line-or-region python-pep8 smex ssh-config-mode
		 textile-mode yaml-mode elscreen quack auto-complete
		 escreen smart-tab gist w3m php-mode
		 pkgbuild-mode markdown-mode textile-mode nrepl deft
		 rainbow-delimiters shell-pop)
  "A list of packages to ensure are installed at launch.")

(defun prelude-packages-installed-p ()
  (loop for p in prelude-packages
	when (not (package-installed-p p)) do (return nil)
	finally (return t)))

(unless (prelude-packages-installed-p)
  ;; check for new packages (package versions)
  (message "%s" "Emacs is now refreshing its package database...")
  (package-refresh-contents)
  (message "%s" " done.")
  ;; install the missing packages
  (dolist (p prelude-packages)
    (when (not (package-installed-p p))
      (package-install p))))

(provide 'setup-package)
