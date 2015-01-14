(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/") t)
;; (add-to-list 'package-archives
;;	     '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

(defvar prelude-packages '(use-package pkg-info slamhound
  smartparens boxquote diminish
  framemove rainbow-mode jedi whole-line-or-region
  textile-mode yaml-mode auto-complete
  smart-tab gist w3m php-mode
  projectile flycheck
  nginx-mode batch-mode bbdb anzu)
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
