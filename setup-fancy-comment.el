;;; nice comment highlighting
(defconst my-title-face 'my-title-face)
(defface my-title-face
  '((((class color) (background light))
     (:background "DarkSeaGreen1" :foreground "grey25"))
    (((class color) (background dark))
     (:background "DarkGrey")))
  "Face used for titles.")

(defun my-generate-highlight-keywords (comment-rx)
  `((,(concat "^\\([ \t]*"
	      comment-rx
	      "[ \t]*"
	      "\\("
	      "[^\r\n]*"
	      "\\)"
	      "\r?\n?\\)")
     1 my-title-face prepend)))

(defun my-nice-comment-slash ()
  (interactive)
  (font-lock-add-keywords nil (my-generate-highlight-keywords "///"))
  (font-lock-add-keywords nil (my-generate-highlight-keywords "// ---")))

(defun my-nice-comment-hash-mark ()
  (interactive)
  (font-lock-add-keywords nil (my-generate-highlight-keywords "# --"))
  (font-lock-add-keywords nil (my-generate-highlight-keywords "# =="))
  (font-lock-add-keywords nil (my-generate-highlight-keywords "###")))

(defun my-nice-comment-semicolon ()
  (interactive)
  (font-lock-add-keywords nil (my-generate-highlight-keywords "\\(;;;\\|;--\\|;==\\)"))
  (font-lock-add-keywords nil (my-generate-highlight-keywords ";; --")))

(defun my-control-l ()
  (interactive)
  (font-lock-add-keywords nil (my-generate-highlight-keywords "")))

(add-hook 'emacs-lisp-mode-hook 'my-nice-comment-semicolon)
(add-hook 'python-mode-hook 'my-nice-comment-hash-mark)
(add-hook 'c-mode-hook 'my-nice-comment-slash)
(add-hook 'c++-mode-hook 'my-nice-comment-slash)
(add-hook 'sh-mode-hook 'my-nice-comment-hash-mark)
(add-hook 'text-mode-hook 'my-control-l)
(add-hook 'conf-mode-hook 'my-nice-comment-hash-mark)

(provide 'setup-fancy-comment)
