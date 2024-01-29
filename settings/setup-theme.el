;;; setup-theme --- setup theme     -*- lexical-binding: t -*-

;;; Code:
(use-package leuven-theme)
(use-package gruvbox-theme)
(use-package spacemacs-theme)
(use-package zenburn-theme)
(use-package anti-zenburn-theme)
;; (use-package omtose-phellack-theme)
(use-package kaolin-themes)
(use-package zerodark-theme)

(if (version< emacs-version "28")
    (use-package modus-themes))

(elpaca-wait)

(load-theme 'spacemacs-light t)

;;(load-theme 'zerodark t)
;;(zerodark-setup-modeline-format)

;; (load-theme 'spacemacs-dark t)
;; 
;; (load-theme 'omtose-darker t)
;; (load-theme 'zenburn t)
;; (load-theme 'leuven t)
;;(add-to-list 'default-frame-alist '(mouse-color . "gold2"))
;; (load-theme 'modus-operandi t)

;; (load-theme 'gruvbox t)

(provide 'setup-theme)
;;; setup-theme.el ends here
