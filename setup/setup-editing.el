;;; setup-editing.el --- Editing-related packages   -*- lexical-binding: t -*-

;; apheleia: Auto-format code on save without moving point
(setup (:package apheleia)
  (apheleia-global-mode +1)
  (keymap-global-set "C-c b" #'apheleia-format-buffer)
  (with-eval-after-load 'apheleia
    ;; apheleia currently does not configure a formatter for nix-ts-mode
    ;; see https://github.com/radian-software/apheleia/issues/298
    (setq apheleia-remote-algorithm 'local)
    (add-to-list 'apheleia-mode-alist '(nix-ts-mode . nixfmt))
    (when (executable-find "zprint")
      (setf (alist-get 'zprint apheleia-formatters) '("zprint"))
      (add-to-list 'apheleia-mode-alist '(clojure-mode . zprint))
      (add-to-list 'apheleia-mode-alist '(clojure-ts-mode . zprint)))

    (setf (alist-get 'blackzim apheleia-formatters)
          '("blackzim"))

    (setf (alist-get 'latexindent apheleia-formatters)
          '("latexindent" "--logfile=/dev/null" "-y" "defaultIndent: \"    \""))

    ;; toml
    (progn
      (setf (alist-get 'taplo apheleia-formatters)
            '("taplo" "format" "-"))
      (add-to-list 'apheleia-mode-alist '(conf-toml-mode . taplo)))

    (when (executable-find "ruff")
      (add-to-list 'apheleia-mode-alist '(python-mode . (ruff-isort ruff)))
      (add-to-list 'apheleia-mode-alist '(python-ts-mode . (ruff-isort ruff))))
    (add-to-list 'apheleia-mode-alist '(sh-mode . shfmt))
    (add-to-list 'apheleia-mode-alist '(markdown-mode . prettier))
    (add-to-list 'apheleia-mode-alist '(solidity-mode . prettier))))

;; easy-kill: Better kill-ring-save with expandable selection
(setup (:package easy-kill)
  (keymap-global-set "<remap> <kill-ring-save>" #'easy-kill))

;; smartscan: Jump between symbols with M-n/M-p
(setup (:package smartscan)
  (:hook-into prog-mode-hook))

;; puni: Structured editing with slurp/barf/splice
(setup (:package puni)
  (electric-pair-mode +1)
  (puni-global-mode)
  (add-hook 'term-mode-hook #'puni-disable-puni-mode)
  (keymap-global-set "M-<right>"   #'puni-slurp-forward)
  (keymap-global-set "M-<left>"    #'puni-barf-forward)
  (keymap-global-set "M-<up>"      #'puni-splice)
  (keymap-global-set "C-S-<right>" #'puni-forward-sexp)
  (keymap-global-set "C-S-<left>"  #'puni-backward-sexp))

;; undo-fu: Simple undo/redo while keeping access to the full non-linear undo history.
;; undo-fu-session: Save & restore undo/redo information between sessions.
;; vundo:Visualize the undo tree (just call M-x vundo)
(setup (:package undo-fu undo-fu-session vundo)
  ;; Increase internal undo limits
  ;; See https://codeberg.org/ideasman42/emacs-undo-fu#undo-limits
  (:option undo-limit 67108864          ; 64mb.
           undo-strong-limit 100663296  ; 96mb.
           undo-outer-limit 1006632960) ; 960mb.
  (:option undo-fu-allow-undo-in-region t)
  (keymap-global-set "C-z" #'undo-fu-only-undo)
  (keymap-global-set "C-S-z"  #'undo-fu-only-redo)
  (undo-fu-session-global-mode))

;; selected: Keybindings active only when region is selected
(setup (:package selected expreg)
  (selected-global-mode)
  (:with-map selected-keymap
    (:bind "b" #'boxquote-region
           "g" #'selected-off
           "q" #'fill-paragraph
           "c" #'kill-ring-save
           "x" #'kill-region
           "s" #'sort-lines
           "S" #'my/sort-words-in-region
           "<" #'my/shift-left
           "," #'my/shift-left
           ">" #'my/shift-right
           "." #'my/shift-right
           ";" #'comment-dwim
           "l" #'git-link
           (kbd "SPC") #'expreg-expand
           "z" (lambda()
                 (interactive)
                 (let ((deactivate-mark nil))
                   (undo))))))

;; cwc: Run whitespace-cleanup only for changed lines
(setup cwc
  (my/run-when-display-initialized
   (lambda()
     (message "init.el: initializing cwc-global-mode")
     (cwc-global-mode +1))))

;; super-save: Auto-save buffers on focus loss
(setup (:package super-save)
  (super-save-mode +1))

(provide 'setup-editing)

;;; setup-editing.el ends here
