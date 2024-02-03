;;; setup-theme --- setup theme     -*- lexical-binding: t -*-

;;; Code:


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
  :demand t
  :config (my/load-theme 'spacemacs-dark))

(use-package zenburn-theme)
(use-package anti-zenburn-theme)
(use-package kaolin-themes)
(use-package zerodark-theme)

(provide 'setup-theme)
;;; setup-theme.el ends here
