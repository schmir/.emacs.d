(require 'ido)



(setq ido-execute-command-cache nil)

;; (add-hook 'ido-setup-hook
;;	    (lambda ()
;;	      (setq ido-enable-flex-matching t)
;;	      ;;(global-set-key "\M-x" 'ido-execute-command)
;;	      ))

(ido-mode t)  ; use 'buffer rather than t to use only buffer switching
(ido-everywhere t)

(add-to-list 'ido-ignore-buffers "\\.ido\\.last")

(setq ido-enable-flex-matching t
      ido-use-filename-at-point nil
      ido-use-virtual-buffers t
      ido-auto-merge-work-directories-length 0
      ido-max-window-height 10
      ido-max-file-prompt-width 1400
      ido-max-prospects 32
      ido-max-directory-size 300000))

(provide 'setup-ido)
