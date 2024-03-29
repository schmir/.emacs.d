;;; setup-theme --- setup theme     -*- lexical-binding: t -*-

;;; Code:

;; Consider all themes safe to load
(setopt custom-safe-themes t)

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

;; load a theme unless we have customized one
(add-hook 'elpaca-after-init-hook
          (lambda()
            (when (not custom-enabled-themes)
              (message "setup-theme.el: loading default theme")
              (my/load-theme 'ef-day))))

(dolist (th '(leuven-theme gruvbox-theme spacemacs-theme ef-themes zenburn-theme anti-zenburn-theme kaolin-themes zerodark-theme))
  (eval `(elpaca ,th)))

(provide 'setup-theme)
;;; setup-theme.el ends here
