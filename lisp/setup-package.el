(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/") t)
;; (add-to-list 'package-archives
;;	     '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

(defvar prelude-packages '(use-package pkg-info clojure-mode clojure-mode-extra-font-locking slamhound
  paredit smartparens bm boxquote key-chord lua-mode magit git-messenger diminish
  highlight-symbol framemove rainbow-mode jedi whole-line-or-region
  ssh-config-mode textile-mode yaml-mode company auto-complete
  smart-tab gist w3m php-mode markdown-mode textile-mode cider ac-cider
  deft rainbow-delimiters shell-pop projectile ack-and-a-half flycheck
  nginx-mode batch-mode ace-jump-mode bbdb anzu misc-cmds sequential-command)
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


;; avoids having to modify this file when i use emacs somewhere where i don't
;; have particular extensions
(defun require-try (&rest args)
  "require symbols, load-library strings, fail silently if some aren't
   available"
  (let (lib)
    (condition-case err
	(mapc (lambda (e)
		(setq lib e)
		(cond
		 ((stringp e) (load-library e))
		 ((symbolp e) (require e)))) args)
      (file-error
       (progn (message "Couldn't load extension: %s: %S" lib err) nil)))))

(unless (require-try 'diminish)
  (defun diminish (mode)
    t))

(provide 'setup-package)
