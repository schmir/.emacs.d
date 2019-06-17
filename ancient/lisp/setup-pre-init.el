(setq vendor-dir (concat dotfiles-dir "vendor/"))
(setq custom-file (concat dotfiles-dir "custom.el"))
(setq my-private-file "~/.private.el")
(setq home-dir (getenv "HOME"))

(defun add-load-path (path)
  (add-to-list 'load-path path))

(add-load-path vendor-dir)

(if (file-exists-p custom-file)
    (load custom-file))

(if (file-exists-p my-private-file)
    (load my-private-file))

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


(if (eq system-type 'windows-nt)
    (setq w32-pass-apps-to-system nil
	  w32-apps-modifier 'hyper))


(provide 'setup-pre-init)
