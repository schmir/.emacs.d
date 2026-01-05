;;; setup-dired --- configure dired     -*- lexical-binding: t -*-

;;; Code:

;; (dired "~/repos/lulu/upp/tools/platform/")
(setup dired (:package diredfl dired-open dired-subtree peep-dired)

       (autoload 'dired-open-xdg "dired-open")
       (with-eval-after-load 'dired-x
         (add-to-list 'dired-omit-extensions ".zwc")
         (setopt dired-omit-verbose nil
                 dired-omit-files (rx (or (seq bol (? ".") "#")         ;; emacs autosave files
                                          (seq bol "." (not (any "."))) ;; dot-files
                                          (seq "~" eol)                 ;; backup-files
                                          ))))
       
       (add-hook 'dired-mode-hook #'dired-hide-details-mode)
       (add-hook 'dired-mode-hook #'hl-line-mode)
       (add-hook 'dired-mode-hook #'dired-omit-mode)

       (setopt dired-recursive-copies 'always
               dired-recursive-deletes 'always
               dired-clean-confirm-killing-deleted-buffers nil
               dired-kill-when-opening-new-dired-buffer t
               dired-auto-revert-buffer t
               delete-by-moving-to-trash nil
               dired-dwim-target t
               dired-hide-details-hide-symlink-targets nil)

       (setopt ls-lisp-dirs-first t
               ls-lisp-ignore-case t
               ls-lisp-use-insert-directory-program nil)
       (require 'ls-lisp)

       (with-eval-after-load 'dired
         (put 'dired-find-alternate-file 'disabled nil)
         (keymap-set dired-mode-map "<tab>" #'dired-subtree-toggle)
         (keymap-set dired-mode-map "O" #'dired-open-xdg)
         (keymap-set dired-mode-map "." #'dired-omit-mode)
         (diredfl-global-mode)))


(provide 'setup-dired)
;;; setup-dired.el ends here
