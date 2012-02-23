


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(gnus-signature ((t (:slant italic :height 0.85))))
 '(my-title-face ((t (:background "gray10"))))
 '(quack-threesemi-semi-face ((((class color) (background light)) (:background "#c0ffff"))))
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
 '(custom-safe-themes (quote ("e9368e158b8980d1de681c0c844ac6c5f2fb1080" "43a89a6e83e08c522fd6bb8a93aecbc3e8a869bc" "144a8ef16b453268ba4c8557b7a01e237d92180d" "a43c13ca965294ae5447cbd6574af2a07c98cd63" "7a0245d27fcc823646fb777b79871dc916e2d91b" default))))
