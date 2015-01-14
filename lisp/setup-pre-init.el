(setq vendor-dir (concat dotfiles-dir "vendor/"))
(setq custom-file (concat dotfiles-dir "custom.el"))
(setq my-private-file "~/.private.el")
;; (setq generated-autoload-file (concat dotfiles-dir "loaddefs.el"))
(setq home-dir (getenv "HOME"))
(setq custom-theme-directory (concat dotfiles-dir "themes/"))

(defun add-load-path (path)
  (add-to-list 'load-path path))

(add-load-path vendor-dir)
(add-load-path (concat vendor-dir "emacs-w3m"))
(add-load-path (concat vendor-dir "bbdb"))
(add-load-path (concat vendor-dir "auto-complete"))

(if (file-exists-p custom-file)
    (load custom-file))

(if (file-exists-p my-private-file)
    (load my-private-file))

;; (if (file-exists-p generated-autoload-file)
;;     (load-file generated-autoload-file))

;;; compat methods



;; emacs 24 doesn't have this anymore
(defun make-local-hook (hook)
  (if (local-variable-p hook)
      nil
    (or (boundp hook) (set hook nil))
    (make-local-variable hook)
    (set hook (list t)))
  hook)
(make-obsolete 'make-local-hook "not necessary any more." "21.1")

;; (defun schmir-loaddefs ()
;;   (interactive)
;;   (message "generating %s" generated-autoload-file)
;;   (update-directory-autoloads dotfiles-dir vendor-dir))

;; (if (not (file-exists-p generated-autoload-file))
;;       (schmir-loaddefs))
;; (load-file generated-autoload-file)


;; (defun schmir-recompile ()
;;   (interactive)
;;   (byte-recompile-directory vendor-dir 0))

(if (eq system-type 'windows-nt)
    (setq w32-pass-apps-to-system nil
	  w32-apps-modifier 'hyper))


(provide 'setup-pre-init)
