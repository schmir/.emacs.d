


(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(mode-line ((t (:background "orange" :foreground "black" :inverse-video nil :box (:line-width -1 :style released-button)))))
 '(quack-threesemi-semi-face ((((class color) (background light)) (:background "#c0ffff"))))
 '(semantic-decoration-on-unknown-includes ((((class color) (background light)) (:underline t))))
 '(visible-mark-face ((((class color) (background light)) (:background "grey80" :box (:line-width 2 :color "grey75" :style released-button)))))
 '(yaoddmuse-heading ((t (:foreground "dark green"))))
 '(yaoddmuse-link ((t (:foreground "DarkOrange1"))))
 '(yaoddmuse-source-code ((t (:foreground "red")))))



(if (executable-find "urlopen")
    (setq browse-url-generic-program "urlopen"
	  browse-url-browser-function 'browse-url-generic))

;; (browse-url "http://web.de")
