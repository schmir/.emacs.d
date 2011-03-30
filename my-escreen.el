;; 
(require 'escreen)
(escreen-install)

;; add C-\ l to list screens with emphase for current one
(defun escreen-get-active-screen-numbers-with-emphasis ()
  "what the name says"
  (interactive)
  (let ((escreens (escreen-get-active-screen-numbers))
        (emphased ""))

    (dolist (s escreens)
      (setq emphased
            (concat emphased (if (= escreen-current-screen-number s)
                                 (propertize (number-to-string s)
                                             ;;'face 'custom-variable-tag) " ")
                                             ;; 'face 'info-title-3)
					     'face 'font-lock-warning-face)
			       ;;'face 'secondary-selection)
                               (number-to-string s))
                    " ")))
    (message "escreen: active screens: %s" emphased)))

(global-set-key (kbd "C-\\ l") 'escreen-get-active-screen-numbers-with-emphasis)

(defun dim:escreen-goto-last-screen ()
  (interactive)
  (escreen-goto-last-screen)
  (escreen-get-active-screen-numbers-with-emphasis))

(defun dim:escreen-goto-prev-screen (&optional n)
  (interactive "p")
  (escreen-goto-prev-screen n)
  (escreen-get-active-screen-numbers-with-emphasis))

(defun dim:escreen-goto-next-screen (&optional n)
  (interactive "p")
  (escreen-goto-next-screen n)
  (escreen-get-active-screen-numbers-with-emphasis))

(define-key escreen-map escreen-prefix-char 'dim:escreen-goto-last-screen)

(global-set-key '[M-right] (quote dim:escreen-goto-next-screen))
(global-set-key '[M-left] (quote dim:escreen-goto-prev-screen))

(global-set-key (kbd "C-\\ DEL") 'dim:escreen-goto-prev-screen)
(global-set-key (kbd "C-\\ SPC") 'dim:escreen-goto-next-screen)

;; (global-set-key '[S-mouse-4] 'dim:escreen-goto-prev-screen)
;; (global-set-key '[S-mouse-5] 'dim:escreen-goto-next-screen)

;; Oh, and as I'm in the terms in emacs part of universe (rather than using emacs -nw in some terminal emulator, but loosing sync between X clipbloard and emacs selection), I had to add this too:

;; add support for C-\ from terms
(require 'term)
(define-key term-raw-map escreen-prefix-char escreen-map)
(define-key term-raw-map (kbd "M-[") 'dim:escreen-goto-prev-screen)
(define-key term-raw-map (kbd "M-]") 'dim:escreen-goto-next-screen)

(provide 'my-escreen)
