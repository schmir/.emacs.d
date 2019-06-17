


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(font-lock-warning-face ((t (:foreground "dark orange" :weight bold))))
 '(gnus-signature ((t (:slant italic :height 0.85))))
 '(markup-meta-face ((t (:stipple nil :foreground "gray50" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 90 :width normal :foundry "unknown" :family "Monospace"))))
 '(markup-meta-hide-face ((t (:inherit markup-meta-face :foreground "gray45"))))
 '(my-title-face ((t (:background "gray10"))))
 '(quack-threesemi-semi-face ((((class color) (background light)) (:background "#c0ffff"))))
 '(rosi-field-face ((t (:inherit font-lock-preprocessor-face :foreground "turquoise"))))
 '(rosi-perform-face ((t (:inherit font-lock-warning-face :foreground "pale green"))))
 '(rst-level-1-face ((t (:background "grey30"))) t)
 '(rst-level-2-face ((t (:background "grey30"))) t)
 '(rst-level-3-face ((t (:background "grey30"))) t)
 '(semantic-decoration-on-unknown-includes ((((class color) (background light)) (:underline t))) t)
 '(visible-mark-face ((((class color) (background light)) (:background "grey80" :box (:line-width 2 :color "grey75" :style released-button)))) t)
 '(which-func ((t (:inherit mode-line :foreground "orange red"))))
 '(yaoddmuse-heading ((t (:foreground "dark green"))) t)
 '(yaoddmuse-link ((t (:foreground "DarkOrange1"))) t)
 '(yaoddmuse-source-code ((t (:foreground "red"))) t))



(if (executable-find "urlopen")
    (setq browse-url-generic-program "urlopen"
	  browse-url-browser-function 'browse-url-generic))

;; (browse-url "http://web.de")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cljr-auto-clean-ns nil)
 '(cljr-auto-sort-ns nil)
 '(cljr-eagerly-build-asts-on-startup nil)
 '(custom-safe-themes
   (quote
    ("3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "e9368e158b8980d1de681c0c844ac6c5f2fb1080" "43a89a6e83e08c522fd6bb8a93aecbc3e8a869bc" "144a8ef16b453268ba4c8557b7a01e237d92180d" "a43c13ca965294ae5447cbd6574af2a07c98cd63" "7a0245d27fcc823646fb777b79871dc916e2d91b" default)))
 '(git-commit-summary-max-length 72)
 '(shell-pop-shell-type (quote ("eshell" "*eshell*" (lambda nil (eshell))))))
