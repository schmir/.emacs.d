;;; setup-theme --- setup theme     -*- lexical-binding: t -*-

;;; Code:

;; Consider all themes safe to load
(setq custom-safe-themes t)

(defun my/load-theme-via-hook (theme)
  (letrec ((*load-theme* (lambda (frame)
                           (select-frame frame)
                           (load-theme theme t)
                           (remove-hook 'after-make-frame-functions *load-theme*))))
    (add-hook 'after-make-frame-functions *load-theme*)))

(defun my/load-theme (theme)
  (if (daemonp)
      (my/load-theme-via-hook theme)
    (load-theme theme t)))

(use-package leuven-theme)
(use-package gruvbox-theme)

(use-package spacemacs-theme
  ;; :config (my/load-theme 'spacemacs-dark)
  :demand t)

(use-package ef-themes
  :config
  ;;(my/load-theme 'ef-melissa-dark)
  (my/load-theme 'ef-bio)
  :demand t)

(use-package zenburn-theme)
(use-package anti-zenburn-theme)
(use-package kaolin-themes)
(use-package zerodark-theme)

(provide 'setup-theme)
;;; setup-theme.el ends here
