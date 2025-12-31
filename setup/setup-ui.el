;;; setup-ui --- configure ui setttings     -*- lexical-binding: t -*-

;;; Code:

;; themes: Load and configure color themes
(setup (:package anti-zenburn-theme doom-themes ef-themes gruvbox-theme kaolin-themes leuven-theme
                 spacemacs-theme zenburn-theme zerodark-theme)
  ;; Consider all themes safe to load
  (setq custom-safe-themes t)
  (when (file-exists-p custom-file)
    (message "init.el: loading custom file %s" custom-file)
    (load custom-file))
  ;; load a theme unless we have customized one
  (when (not custom-enabled-themes)
    (message "setup-theme.el: loading default theme")
    (my/load-theme 'ef-day)))

;; emacs: Mouse and cursor behavior settings
(setup emacs
  ;; mouse avoidance mode is buggy, see
  ;; https://groups.google.com/g/gnu.emacs.help/c/W_1VhwJrelE
  ;; (mouse-avoidance-mode 'banish)
  (setq make-pointer-invisible t
        mouse-highlight 1)

  (setq mouse-yank-at-point t)
  ;; when on a tab, make the cursor the tab length
  (setq-default x-stretch-cursor t))


;; pixel-scroll: Smooth scrolling with momentum
(setup (:and (fboundp #'pixel-scroll-precision-mode)
             pixel-scroll-precision-mode)
  (setq pixel-scroll-precision-interpolate-page t
        ;; pixel-scroll-precision-large-scroll-height 5
        pixel-scroll-precision-use-momentum t)
  ;; (global-set-key [remap mwheel-scroll] 'pixel-scroll-precision)
  (pixel-scroll-precision-mode))

;; show-paren: Highlight matching parentheses
(setup show-paren
  (setq show-paren-style 'parenthesis
        show-paren-context-when-offscreen t)
  (show-paren-mode t))

;; display-fill-column-indicator: Show vertical line at fill-column
(setup display-fill-column-indicator-mode
  (setq-default fill-column 76)
  (add-hook 'prog-mode-hook (lambda()
                              (setq-local fill-column 99)))
  (:hook-into prog-mode text-mode))

;; hl-line-mode: Highlight current line except when region active
(setup hl-line-mode
  (defun my/hl-line-highlight-unless-region-active
      ()
    (if (region-active-p)
        nil
      (cons (line-beginning-position) (line-beginning-position 2))))
  (setq hl-line-range-function #'my/hl-line-highlight-unless-region-active)
  (:hook-into prog-mode text-mode))

;; page-break-lines: display ugly ^L page breaks as tidy horizontal lines
(setup (:package page-break-lines)
  (global-page-break-lines-mode))

;; idle-highlight-mode: Highlight the symbol at the cursor when idle
(setup (:package idle-highlight-mode)
  (:option idle-highlight-before-point t
           idle-highlight-exclude-point t
           idle-highlight-idle-time 0.2)
  (idle-highlight-global-mode))


;; framemove: Navigate between frames with windmove keys
(setup (:package (framemove :url "https://github.com/emacsmirror/framemove"))
  (require 'framemove)
  (windmove-default-keybindings)
  (setq framemove-hook-into-windmove t)
  ;; let me use windmove keybindings even in org-mode
  (setq org-replace-disputed-keys t))

;; display-buffer-alist: Configure window placement for shells and REPLs
(setup emacs
  (setq display-buffer-alist
        '(("e?shell*\\|ielm\\|eat\\|compilation\\|vterm\\|Help\\*\\(?:<[[:digit:]]+>\\)?\\'"
           (display-buffer-reuse-window
            display-buffer-in-side-window)
           (reusable-frames . visible)
           (side . bottom)
           (window-height . 0.4))
          ("\\`\\*cider-repl\\|.*.clj"
           (display-buffer-reuse-window
            display-buffer-pop-up-window)
           (reusable-frames . t)
           (inhibit-switch-frames . nil))))

  (defun lunaryorn-quit-bottom-side-windows ()
    "Quit side windows of the current frame."
    (interactive)
    (dolist (window (window-at-side-list))
      (quit-window nil window)))

  (keymap-global-set "C-c q" #'lunaryorn-quit-bottom-side-windows)
  (keymap-global-set "C-c C-q" #'lunaryorn-quit-bottom-side-windows))

;; pulsar: Pulse highlight line after navigation
(setup (:package pulsar)
  (setq pulsar-pulse t
        pulsar-delay 0.045
        pulsar-iterations 10
        pulsar-face 'pulsar-magenta
        pulsar-highlight-face 'pulsar-yellow)
  (pulsar-global-mode 1))

;; text-scaling: Keybindings for adjusting font size
(setup text-scaling
  (keymap-global-set "C--"  #'text-scale-decrease)
  (keymap-global-set "C-="  #'text-scale-increase)
  (setq global-text-scale-adjust-resizes-frames nil))

;; zoom: Auto-resize windows to golden ratio
(setup (:package zoom)
  ;; (zoom-mode)
  )

;; which-key: Show available keybindings in popup
(setup (:package which-key)
  (which-key-mode))

;; minions: consolidate minor modes in modeline into a single menu
(setup (:package minions)
  (minions-mode))

(provide 'setup-ui)
;;; setup-ui.el ends here
