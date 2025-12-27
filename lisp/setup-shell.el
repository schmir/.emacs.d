;;; setup-shell.el --- Shell and terminal configuration  -*- lexical-binding: t -*-

;;; Code:

;; zoxide: Smart directory tracking and jumping
(setup (:and (executable-find "zoxide")
             (:package zoxide))
  (defun my/zoxide-add
      ()
    (zoxide-add)
    (when-let* ((proj (project-current))
                (root (project-root proj))
                (path (expand-file-name root)))
      (message "add project-root %s to zoxide" path)
      (zoxide-add path)))

  (add-hook 'find-file-hook #'my/zoxide-add))

;; envrc: Direnv integration for per-directory environments
(setup (:and (executable-find "direnv") (:package envrc))
  (require 'envrc)
  (keymap-set envrc-mode-map "C-c e" #'envrc-command-map)
  (add-hook 'after-init-hook #'envrc-global-mode))

;; vterm: Full-featured terminal emulator using libvterm
;; apt install libvterm-dev libvterm-bin libtool-bin cmake
;; dnf install libvterm-devel libtool cmake
(setup (:package vterm)
  (setq vterm-max-scrollback 10000
        vterm-shell (executable-find "zsh"))
  (with-eval-after-load 'vterm
    (add-to-list 'vterm-eval-cmds '("find-file-other-window" find-file-other-window)))
  (:hook #'compilation-shell-minor-mode)
  (:bind "C-t" #'shell-pop)
  ;; :after shell-pop
  )

;; shell-pop: Toggle a shell window with C-t
(setup (:package shell-pop)
  (keymap-global-set "C-t" #'shell-pop)
  (setq shell-pop-shell-type '("vterm" "*vterm*" #'vterm)
        shell-pop-term-shell (executable-find "zsh")
        shell-pop-window-size 40))

;; eat: Terminal emulation for eshell, handles curses applications
(setup (:package eat)
  (add-hook 'eshell-load-hook #'eat-eshell-mode)
  ;; (add-hook 'eshell-load-hook #'eat-eshell-visual-command-mode)
  (setq eshell-visual-commands '()))

;; eshell: Emacs shell with zoxide integration and custom z command
(setup eshell
  (defun my/insert-compile-command
      ()
    (interactive)
    (goto-char (max-char))
    (insert compile-command))

  (defvar my/consult-dir-source-eshell
    `(:name "Eshell"
            :narrow ?e
            :category file
            :face consult-file
            :enabled ,(lambda () (and (boundp 'eshell-last-dir-ring)
                                      eshell-last-dir-ring))
            :items ,(lambda()
                      (delete-dups
                       (mapcar 'abbreviate-file-name
                               (ring-elements eshell-last-dir-ring)))))
    "Eshell directory source for `consult-dir--pick'.")

  (with-eval-after-load 'consult-dir
    (add-to-list 'consult-dir-sources my/consult-dir-source-eshell))

  (defun my/zoxide-eshell-directory-changed
      ()
    (zoxide-add default-directory))

  (with-eval-after-load 'eshell
    (require 'esh-mode)

    (add-hook 'eshell-mode-hook #'my/zoxide-eshell-directory-changed)
    (add-hook 'eshell-mode-hook #'hack-dir-local-variables-non-file-buffer)
    (keymap-set eshell-mode-map "<f9>" #'my/insert-compile-command)
    (add-hook 'eshell-directory-change-hook #'my/zoxide-eshell-directory-changed)
    (add-hook 'eshell-directory-change-hook #'hack-dir-local-variables-non-file-buffer))


  (defun my/zoxide-query
      (s)
    (let ((r (zoxide-run nil "query" s)))
      (if (stringp r)
          (car (split-string r "\n" t))
        nil)))

  (autoload 'consult-dir--pick "consult-dir")
  (defun eshell/z (&optional regexp)
    "Navigate to a previously visited directory in eshell, or to
any directory proferred by `consult-dir'."
    (let ((dir (if regexp
                   (or (my/zoxide-query regexp)
                       (eshell-find-previous-directory regexp))
                 (substring-no-properties
                  (consult-dir--pick "Switch directory: ")))))
      (eshell/cd dir))))

(provide 'setup-shell)
;;; setup-shell.el ends here
