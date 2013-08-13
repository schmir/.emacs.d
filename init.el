;; -*- mode: emacs-lisp; coding: utf-8 -*-
(require 'cl)

(setq dotfiles-dir
      (file-name-directory
       (or (buffer-file-name) load-file-name)))
(setq vendor-dir (concat dotfiles-dir "vendor/"))
(setq abbrev-file-name (concat dotfiles-dir "abbrevs_defs"))
(setq custom-file (concat dotfiles-dir "custom.el"))
(setq my-private-file "~/.private.el")
(setq generated-autoload-file (concat dotfiles-dir "loaddefs.el"))
(setq home-dir (getenv "HOME"))
(setq custom-theme-directory (concat dotfiles-dir "themes/"))
(setq gnus-init-file (concat home-dir "/.gnus-init.el"))

(load-theme 'sinburn t)

(if (file-exists-p custom-file)
    (load custom-file))

(if (file-exists-p my-private-file)
    (load my-private-file))

(if (file-exists-p generated-autoload-file)
    (load-file generated-autoload-file))


(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
	     '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

(defvar prelude-packages
  '(clojure-mode clojure-cheatsheet paredit bm boxquote key-chord lua-mode magit diminish
		 highlight-symbol framemove rainbow-mode jedi
		 whole-line-or-region python-pep8 smex ssh-config-mode
		 textile-mode yaml-mode elscreen quack auto-complete
		 escreen smart-tab gist w3m php-mode
		 pkgbuild-mode markdown-mode textile-mode nrepl deft
		 rainbow-delimiters)
  "A list of packages to ensure are installed at launch.")

;; fix indentation of cond expressions
(put 'cond 'clojure-backtracking-indent '(2 4 2 4 2 4 2 4 2 4 2 4 2 4 2 4 2 4 2 4 2 4 2 4 2 4 2 4 2 4))

(defun prelude-packages-installed-p ()
  (loop for p in prelude-packages
	when (not (package-installed-p p)) do (return nil)
	finally (return t)))

(unless (prelude-packages-installed-p)
  ;; check for new packages (package versions)
  (message "%s" "Emacs is now refreshing its package database...")
  (package-refresh-contents)
  (message "%s" " done.")
  ;; install the missing packages
  (dolist (p prelude-packages)
    (when (not (package-installed-p p))
      (package-install p))))


;; autoloads
(autoload 'ac-ropemacs-setup "auto-complete-python" "setup autocomplete with ropemacs" t)
(autoload 'rst-mode "rst" "mode for editing reStructuredText documents" t)
(autoload 'flymake-mode "flymake" "flymake mode" t)
(autoload 'php-mode "php-mode" "PHP editing mode." t)
(autoload 'gid "id-utils" t)
(autoload 'pymacs-load "pymacs" nil t)
(autoload 'sgml-quote "sgml-mode"
  "Quote SGML text in region START ... END.
Only &, < and > are quoted, the rest is left untouched.
With prefix argument UNQUOTEP, unquote the region." t)
(defalias 'html-quote 'sgml-quote)

(autoload 'git-grep "git-grep" "Run git grep" t)
(defalias 'gg 'git-grep)

;;; compat methods

;; avoids having to modify this file when i use emacs somewhere where i don't
;; have particular extensions
(defun require-try (&rest args)
  "require symbols, load-library strings, fail silently if some aren't
   available"
  (let (lib)
    (condition-case err
	(mapc (lambda (e)
		(setq lib e)
		(cond
		 ((stringp e) (load-library e))
		 ((symbolp e) (require e)))) args)
      (file-error
       (progn (message "Couldn't load extension: %s: %S" lib err) nil)))))


;; emacs 24 doesn't have this anymore
(defun make-local-hook (hook)
  (if (local-variable-p hook)
      nil
    (or (boundp hook) (set hook nil))
    (make-local-variable hook)
    (set hook (list t)))
  hook)
(make-obsolete 'make-local-hook "not necessary any more." "21.1")


(defun schmir-loaddefs ()
  (interactive)
  (message "generating %s" generated-autoload-file) 
  (update-directory-autoloads dotfiles-dir vendor-dir))

(if (not (file-exists-p generated-autoload-file))
      (schmir-loaddefs))
(load-file generated-autoload-file)


(defun schmir-recompile ()
  (interactive)
  (byte-recompile-directory vendor-dir 0))


(defun add-load-path (path)
  (add-to-list 'load-path path))


(add-load-path dotfiles-dir)
(add-load-path vendor-dir)
(add-load-path (concat vendor-dir "emacs-w3m"))
(add-load-path (concat vendor-dir "bbdb"))
(add-load-path (concat vendor-dir "auto-complete"))
(add-load-path (concat vendor-dir "gnus/lisp"))

(unless (require-try 'diminish)
  (defun diminish (mode)
    t))

(if (require-try 'highlight-symbol)
    (diminish 'highlight-symbol-mode)
  (defun highlight-symbol-mode (&optional arg)
    t))


(defun untabify-buffer ()
  (interactive)
  (save-excursion
    (untabify (point-min) (point-max))))


(defun maybe-untabify ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (if (search-forward "\t" nil t)
	(if (y-or-n-p "Buffer contains tabs. Replace with spaces? ")
	    (untabify-buffer)))))

(defadvice save-buffers-kill-emacs (around emacs-die-hard activate)
  "really???"
  (if (or
       (not server-process)
       (string= "kill emacs" (read-from-minibuffer "to quit emacs type: 'kill emacs':")))
      ad-do-it))


(defadvice python-shift-left (around no-deactivate-mark activate)
  "keep region active"
  ad-do-it
  (setq deactivate-mark nil))

(defadvice python-shift-right (around no-deactivate-mark activate)
  "keep region active"
  ad-do-it
  (setq deactivate-mark nil))

(defadvice bm-buffer-save  (around no-message activate)
  "be quiet when saving bookmarks"
  (flet ((message ())) ad-do-it))

(defun schmir-setup-bm()
  (interactive)

  ;; Restoring bookmarks when on file find.
  (add-hook 'find-file-hooks 'bm-buffer-restore)

  ;; Saving bookmark data on killing a buffer
  (add-hook 'kill-buffer-hook 'bm-buffer-save)

  ;; Saving the repository to file when on exit.
  ;; kill-buffer-hook is not called when emacs is killed, so we
  ;; must save all bookmarks first.
  (add-hook 'kill-emacs-hook '(lambda nil
				(bm-buffer-save-all)
				(bm-repository-save)))

  ;; Update bookmark repository when saving the file.
  (add-hook 'after-save-hook 'bm-buffer-save)

  ;; Restore bookmarks when buffer is reverted.
  (add-hook 'after-revert-hook 'bm-buffer-restore)

  (setq bm-highlight-style 'bm-highlight-only-fringe
	bm-recenter 1
	bm-wrap-immediately nil
	bm-buffer-persistence t
	bm-restore-repository-on-load t)
  (require 'bm))

(when (require-try 'bm)
  (schmir-setup-bm))


(autoload 'pkgbuild-mode "pkgbuild-mode.el" "PKGBUILD mode." t)
(setq auto-mode-alist (append '(("/PKGBUILD$" . pkgbuild-mode))
			      auto-mode-alist))


(when (require-try 'schmir-fun)
  (schmir-maybe-server))


(defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
  "Prevent annoying \"Active processes exist\" query when you quit Emacs."
  (flet ((process-list ())) ad-do-it))


(defun schmir-isearch-beginning ()
  "Use with isearch hook to end search at first char of match."
  (when isearch-forward (goto-char isearch-other-end)))

;; -- from http://trey-jackson.blogspot.com/2010/04/emacs-tip-36-abort-minibuffer-when.html
(defun stop-using-minibuffer ()
  "kill the minibuffer"
  (when (>= (recursion-depth) 1)
    (abort-recursive-edit)))
(add-hook 'mouse-leave-buffer-hook 'stop-using-minibuffer)


(defun schmir-basic-setup ()
  (interactive)
  (tool-bar-mode -1)
  (menu-bar-mode 1)
  (if (>= emacs-major-version 22)
      (progn
	(savehist-mode 1) ;; keep track of minibuffer commands
	(size-indication-mode 1) ;; show file size
	))
  (global-rainbow-delimiters-mode 1)
  (global-auto-revert-mode 1) ;; re-read buffers from disk unless they're `dirty'
  (display-time-mode 1)
  (auto-compression-mode t) ; allow loading of compressed (e.g. gzipped) files
  (global-font-lock-mode t)

  (setq magit-omit-untracked-dir-contents t)
  (setq gist-view-gist t)
  (put 'narrow-to-region 'disabled nil)
  (setq line-move-visual nil) ;; what did they think ?

  (setq highlight-symbol-idle-delay 0.3)

  (recentf-mode t)
  (setq recentf-max-saved-items 200)

  (require 'which-func)
  (add-to-list 'which-func-modes 'org-mode)
  (which-func-mode 1)

  ;; higlight changes in documents
  (global-highlight-changes-mode t)
  (setq highlight-changes-visibility-initial-state nil); initially, hide#
  (global-set-key (kbd "<f7>") 'highlight-changes-visible-mode)
  ;; shift -pgup/pgdown jump to the previous/next change
  (global-set-key (kbd "<S-prior>") 'highlight-changes-next-change)
  (global-set-key (kbd "<S-next>")  'highlight-changes-previous-change)



  (ansi-color-for-comint-mode-on)
  (add-hook 'comint-output-filter-functions 'comint-watch-for-password-prompt)

  (setq dired-recursive-deletes 'always)

  (mouse-wheel-mode 1)
  (auto-image-file-mode 1)
  (global-cwarn-mode 1)

  (global-hl-line-mode 1)   ;; highlight line where cursor is
  ;; (set-face-background 'hl-line "#eeeeee")

  (column-number-mode 1)
  (show-paren-mode 1)
  (put 'overwrite-mode 'disabled nil)
  (setq mark-even-if-inactive t)
  (transient-mark-mode 1)

  (setq cua-enable-cua-keys nil)
  (setq cua-highlight-region-shift-only t) ;; no transient mark mode
  (setq cua-toggle-set-mark nil) ;; original set-mark behavior, i.e. no transient-mark-mode
  ;; (cua-mode t)

  ;; Drive out the mouse when it's too near to the cursor.
  (mouse-avoidance-mode 'exile)
  (setq mouse-avoidance-threshold 10
	mouse-avoidance-nudge-dist 20
	mouse-avoidance-nudge-var 5)




  ;; show pathnames for buffers with same name
  (require 'uniquify)
  (setq uniquify-buffer-name-style 'reverse
	uniquify-separator "/"
	uniquify-after-kill-buffer-p t ; rename after killing uniquified
	uniquify-ignore-buffers-re "^\\*") ; don't muck with special buffers



  (setq visible-bell 1
	require-final-newline t
	display-time-24hr-format t
	inhibit-startup-message t)

  (setq scroll-margin 2
	scroll-step 0
	scroll-conservatively 10000
	scroll-preserve-screen-position 1)

  (require-try 'smooth-scrolling)
  (setq smooth-scroll-margin 3)

  (setq save-abbrevs t
	default-abbrev-mode t)
  (abbrev-mode 1)

  (if (file-exists-p abbrev-file-name)
      (quietly-read-abbrev-file))

  (setq change-major-mode-with-file-name t
	;; Filename completion ignores these.
	completion-ignored-extensions (append completion-ignored-extensions
					      '(".pyc" ".o" ".so" ".os" ".cmi" ".cmx"))
	backward-delete-char-untabify-method 'nil	;; don´t untabify, just delete one char
	font-lock-maximum-decoration t			;; maximum decoration
	next-line-add-newlines nil			;; don´t add newlines when trying to move cursor behind eof
	show-paren-style 'expression
	compilation-scroll-output t
	default-indicate-empty-lines t
	line-number-display-limit-width 100000
	kill-whole-line t				;; make kill-line at beginning of line kill the whole line
	woman-use-own-frame nil				;; don't create new frame for manpages
	vc-handled-backends nil
	vc-follow-symlinks t				;; follow symlinks and don't ask
	enable-recursive-minibuffers t
	)


  ;; prevent emacs from asking for coding-system...
  (set-language-environment "utf-8")

  ;; when on a tab, make the cursor the tab length
  (setq-default x-stretch-cursor t)

  ;; place cursor on same buffer position between editing sessions
  (setq-default save-place t
		save-place-file (concat dotfiles-dir "emacs-places"))
  (require 'saveplace)

  ;; automatically chmod +x when the file has shebang "#!"
  (add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

  ;; (add-hook 'server-switch-hook
  ;;               (lambda nil
  ;;                 (let ((server-buf (current-buffer)))
  ;;                   (bury-buffer)
  ;;                   (switch-to-buffer-other-frame server-buf))))
  (add-hook 'server-done-hook 'delete-frame)



  ;; Turn on time-stamp updating. Timestamp must be in first 8 lines of file and look like:
  ;; Time-stamp: <>
  (add-hook 'write-file-hooks 'time-stamp)
  (add-hook 'before-save-hook 'time-stamp)

  ;; Always end searches at the beginning of the matching expression.
  (add-hook 'isearch-mode-end-hook 'schmir-isearch-beginning)



  (eval-after-load "multi-term"
    '(progn
       (multi-term-keystroke-setup)))

  ;; (clrhash tramp-cache-data)

  (when (require-try 'tramp)
    (add-to-list 'tramp-default-method-alist
		 '("\\`localhost\\'" "\\`root\\'" "ssh"))

    (if (boundp 'tramp-remote-path)
	(progn
	  (add-to-list 'tramp-remote-path "~/bin")
	  (add-to-list 'tramp-remote-path "~/local/bin")
	  (add-to-list 'tramp-remote-path "~/rc/bin")))

    (setq tramp-default-method "ssh"))

  ;; switch windows with shift-(up/down/left/right)
  (require-try 'framemove)
  (setq framemove-hook-into-windmove t)
  (windmove-default-keybindings 'shift))


(schmir-basic-setup)
(require 'evimodeline)
(add-hook 'find-file-hook 'evimodeline-find-file-hook)

(require 'eproject)
(define-project-type generic-hg (generic) (look-for ".hg/00changelog.i")
  :irrelevant-files ("^[.]" "^[#]" ".hg/"))

(defun examine-project ()
  (interactive)
  (message "examine project %s" (eproject-root))
  (if (file-exists-p (concat (eproject-root) "SConstruct"))
      (setq compile-command (concat "cd " (eproject-root) "; scons")))
  (if (file-exists-p (concat (eproject-root) "Makefile"))
      (setq compile-command "make")))


(add-hook 'generic-git-project-file-visit-hook 'examine-project)
(add-hook 'generic-hg-project-file-visit-hook 'examine-project)



(defun flymake-elisp-init ()
  (let* ((temp-file   (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
         (local-file  (file-relative-name
                       temp-file
                       (file-name-directory buffer-file-name))))
    (list "elisplint" (list local-file))))

(defun schmir-elisp-setup ()
  ;; (push '("\\.el$" flymake-elisp-init) flymake-allowed-file-name-masks)
  ;; (add-hook 'emacs-lisp-mode-hook 'flymake-mode)
)

(defun schmir-setup-c-mode-common ()
  (local-set-key [(tab)] 'smart-tab)
  (highlight-symbol-mode 1)
  (setq indent-tabs-mode nil
	c-hungry-delete-key t)
  (make-local-variable 'local-write-file-hooks))


(defun schmir-c-setup ()
  (require 'cc-vars)
  ;; *.h files are C++
  (add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
  (add-to-list 'c-default-style '(c-mode . "python-new"))
  (add-to-list 'c-default-style '(c++-mode . "python-new"))
  (add-hook 'c-mode-common-hook
	    'schmir-setup-c-mode-common)
  (c-add-style
   "python-new"
   '((indent-tabs-mode . nil)
     (fill-column      . 78)
     (c-basic-offset   . 4)
     (c-offsets-alist  . ((substatement-open . 0)
			  (inextern-lang . 0)
			  (arglist-intro . +)
			  (case-label . +)
			  (innamespace . 0)
			  (knr-argdecl-intro . +)))
     (c-hanging-braces-alist . ((brace-list-open)
				(brace-list-intro)
				(brace-list-close)
				(brace-entry-open)
				(substatement-open after)
				(block-close . c-snug-do-while)))
     (c-block-comment-prefix . "* "))))

;; Change backup behavior to save in a directory, not in a miscellany
;; of files all over the place.
(setq backup-by-copying t      ; don't clobber symlinks
      backup-directory-alist
      '(("." . "~/.saves"))    ; don't litter my fs tree
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t       ; use versioned backups
      frame-title-format `(,"%b    ---   ", (user-login-name) "@" ,(system-name))
      )

;; get rid of yes-or-no questions - y or n is enough
(defalias 'yes-or-no-p 'y-or-n-p)

(defface completion-setup-directory-face  '((t (:foreground "dark orange")))
  "Face to use for directories."
  :group 'color-file-completion)

(defcustom color-file-completion-always t "If true, always turn on regexps in
completion buffers."
  :group 'color-file-completion
  :type 'boolean)

(defun completion-setup-directory-face()
  "When we are completing a filename, highlight directories."
  (interactive)
  ;;if this is completing a filename... highlight faces...
  (when (or color-file-completion-always
            (eq minibuffer-completion-table 'read-file-name-internal))
    (let((font-lock-verbose nil))
      (font-lock-mode 1)
      (font-lock-add-keywords nil '(("[^ \n]+/" 0 'completion-setup-directory-face keep)))
      (font-lock-fontify-buffer))))

(add-hook 'completion-list-mode-hook 'completion-setup-directory-face)

(defun schmir-setup-org-mode()
  ;; Make windmove work in org-mode:

  (add-hook 'org-shiftup-final-hook 'windmove-up)
  (add-hook 'org-shiftleft-final-hook 'windmove-left)
  (add-hook 'org-shiftdown-final-hook 'windmove-down)
  (add-hook 'org-shiftright-final-hook 'windmove-right))

(add-hook 'org-mode-hook 'schmir-setup-org-mode)
(setq org-replace-disputed-keys t
      org-startup-truncated nil)


(defun schmir-setup-ido ()
  (require 'ido)

  (add-hook
   'ido-setup-hook
   (lambda ()
     ;; Go straight home
     (define-key ido-file-completion-map
       (kbd "~")
       (lambda ()
	 (interactive)
	 (if (looking-back "/")
	     (insert "~/")
	   (call-interactively 'self-insert-command))))))


  (setq ido-execute-command-cache nil)

  ;; (add-hook 'ido-setup-hook
  ;; 	    (lambda ()
  ;; 	      (setq ido-enable-flex-matching t)
  ;; 	      ;;(global-set-key "\M-x" 'ido-execute-command)
  ;; 	      ))

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

(schmir-setup-ido)

(defun schmir-setup-hippie-expand ()
  (require 'hippie-exp)
  (defun try-complete-abbrev (old)
    (if (expand-abbrev) t nil))

  (setq hippie-expand-try-functions-list
	'(try-complete-abbrev
	  try-expand-dabbrev-visible
	  try-expand-dabbrev
	  try-expand-dabbrev-all-buffers
	  try-expand-dabbrev-from-kill
	  try-complete-file-name-partially
	  try-complete-file-name
	  try-expand-all-abbrevs
	  try-expand-list
	  try-expand-line
	  try-complete-lisp-symbol-partially
	  try-complete-lisp-symbol)))

(schmir-setup-hippie-expand)
(defun schmir-change-abbrev (&optional args)
  (setq local-abbrev-table
	(if (python-in-string/comment)
	    text-mode-abbrev-table
	  python-mode-abbrev-table)))


(unless (require-try 'smart-operator)
  (defun smart-operator-insert (c &optional dummy)
    (insert c)))

(defun schmir-python-smart-comma()
  (interactive)
  (if (python-in-string/comment)
      (insert ",")
    (smart-operator-insert "," t)))

(defun schmir-python-smart-equal()
  (interactive)
  (if (python-in-string/comment)
      (insert "=")
    (smart-operator-insert "=")))

(defun schmir-python-hook ()
  (interactive)
  (when (require-try 'unicode-symbols)
    (substitute-patterns-with-unicode
     (list
      (cons "\\<\\(lambda\\)\\>" 'lambda))))

  (jedi:setup)
  (eproject-maybe-turn-on)   ;; make sure the eproject-hook has run

  (add-hook 'find-file-hooks 'maybe-untabify 'nil 1)
  (setq py-smart-indentation 1
	indent-tabs-mode nil)
  (modify-syntax-entry ?_ "w") ;; make _ part of words.

  (highlight-symbol-mode 1)

  ;;(highlight-phrase "[Ss]elf" (quote bold))
  (local-set-key (kbd "RET") 'newline-and-indent)
  (local-set-key (kbd "M-]") 'python-mark-block)
  (local-set-key (kbd "C-h n") 'schmir-pyhelp)
  ;; (local-set-key (kbd ",") 'schmir-python-smart-comma)

  (local-set-key [C-S-left]  '(lambda()
				(interactive)
				(shift-region -4)))
  (local-set-key [C-S-right] '(lambda()
				(interactive)
				(shift-region 4)))


  ;; (if (fboundp 'py-shift-region-left)
  ;;     (progn
  ;;	(local-set-key [C-S-right] 'py-shift-region-right)
  ;;	(local-set-key [C-S-left] 'py-shift-region-left))
  ;;   (local-set-key [C-S-left]  'python-shift-left)
  ;;   (local-set-key [C-S-right] 'python-shift-right))

  ;; (local-set-key (kbd "=") 'schmir-python-smart-equal)
  (local-set-key [(tab)] 'smart-tab)

  (let ((use-flymake 't)
	(use-ropemacs 't))
    (if (file-remote-p (buffer-file-name))
	(setq use-flymake nil
	      use-ropemacs nil))
    (if (eq system-type 'windows-nt)
	(setq use-ropemacs nil))   ;; it's too slow on windows

    ;; (if use-ropemacs
    ;;	(progn
    ;;	  (ac-ropemacs-require)
    ;;	  (if ac-ropemacs-loaded
    ;;	      (progn
    ;;		(setq ac-sources (append (list 'ac-source-ropemacs) ac-sources))
    ;;		(ropemacs-mode 1)
    ;;		(let* ((root			(or eproject-root (concat home-dir "/")))
    ;;		       (rope-project-root	(concat root ".ropeproject")))
    ;;		      (unless (file-exists-p rope-project-root)
    ;;			(make-directory rope-project-root))
    ;;		      (rope-open-project root))

    ;;		(setq ropemacs-enable-autoimport t)))))

    (if use-flymake
	(flymake-mode 1)))

  ;; (add-hook 'pre-abbrev-expand-hook 'schmir-change-abbrev nil t)
  )


(defun schmir-python-setup ()
  ;; (autoload 'python-mode "python" "Python editing mode." t)

  (unless (require-try 'python-mode)
    (require 'python))


  (add-hook 'python-mode-hook
	    'schmir-python-hook)

  (add-to-list 'auto-mode-alist
	       '("\\wscript$\\|\\SConstruct$\\|\\SConscript$\\|\\.py$\\|\\.jy$\\|\\.py\\.cov$" . python-mode))

  (loop for i in '("jython" "pypy" "python" "python2" "python2.4" "python2.5" "python2.6" "python2.7" "python3" "python3.0" "python3.1" "python3.2" "python3.3")
	do (add-to-list 'interpreter-mode-alist `(,i . python-mode)))


  ;; highlight self in python-mode
  (font-lock-add-keywords 'python-mode '(("\\<\\(self\\)" 1 font-lock-builtin-face)))

  (setq py-XXX-tag-face font-lock-warning-face)

  (if (fboundp 'cython-mode)
      (add-to-list 'auto-mode-alist '("\\.\\(pyx\\|pxi\\|pxd\\)$" . cython-mode))))


(defun schmir-misc-setup()
  (autoload 'fm-start "fm" "follow mode for compilation like buffers")

  ;; (add-hook 'occur-mode-hook 'fm-start)
  ;; (add-hook 'compilation-mode-hook 'fm-start)

  ;; (require 'yaoddmuse)
  ;; (add-to-list 'yaoddmuse-wikis '("mine" "http://systemexit.de/w/wiki.cgi" utf-8 "uihnscuskc=1;"))
  ;; (setq yaoddmuse-default-wiki "mine")

  (add-hook 'emacs-lisp-mode-hook
	    '(lambda()
	       (local-set-key [(tab)] 'smart-tab)
	       (highlight-symbol-mode 1)))

  (add-to-list 'auto-mode-alist
	       '("\\.md$\\|\\.markdown$" . markdown-mode))


  (add-to-list 'auto-mode-alist '(".ssh/config\\'"  . ssh-config-mode))
  (add-to-list 'auto-mode-alist '("sshd?_config\\'" . ssh-config-mode))

  (add-to-list 'auto-mode-alist '("\\PKGBUILD$\\|\\.sh$" . sh-mode))
  (add-to-list 'auto-mode-alist '("\\.php$" . php-mode))
  (add-to-list 'auto-mode-alist '("\\.wsdl$" . sgml-mode))


  (add-to-list 'auto-mode-alist '("\\.pas$\\|\\.dpr" .  delphi-mode))


  (add-to-list 'auto-mode-alist '("\\.js$" . espresso-mode))
  (add-to-list 'auto-mode-alist '("\\.json$" . espresso-mode))
  (add-hook 'espresso-mode-hook
	    '(lambda()
	       (local-set-key [(tab)] 'smart-tab)
	       (highlight-symbol-mode 1)
	       (setq c-basic-offset 2)
	       ))

  (add-hook 'javascript-mode-hook
	    '(lambda()
	       (local-set-key [(tab)] 'smart-tab)
	       (highlight-symbol-mode 1)
	       (setq c-basic-offset 2)
	       ))
  (add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))

  (add-hook 'rst-mode-hook 'auto-fill-mode)

)

;; --- lua
(defun schmir-lua-setup()
  (add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
  (add-hook 'lua-mode-hook
	    '(lambda()
	       (local-set-key [(tab)] 'smart-tab)
	       (highlight-symbol-mode 1)
	       (setq lua-indent-level 4)
	       (flymake-mode)))
)

(defun schmir-erlang-setup ()
  (add-load-path "/usr/lib/erlang/lib/tools-2.6.4/emacs/")

  (setq erlang-root-dir (concat home-dir "/local/lib/erlang"))
  (setq exec-path (cons (concat home-dir "/local/bin/") exec-path))

  (when (require-try 'erlang-start)

    (when (require-try 'distel)
      (distel-setup))

    (setq inferior-erlang-machine-options '("-sname" "emacs"))
    (add-hook 'erlang-mode-hook
	      '(lambda()
		 (local-set-key [(tab)] 'smart-tab)
		 (highlight-symbol-mode 1)
		 (flymake-mode))))
)

(defun schmir-setup-modes()
  (schmir-python-setup)
  (schmir-lua-setup)
  (schmir-erlang-setup)
  (schmir-c-setup)
  (schmir-misc-setup)
  (schmir-erlang-setup)
  (schmir-elisp-setup)
  (require-try 'schmir-flymake)
)
(schmir-setup-modes)

(defun schmir-setup-frame (frame)
  "Set display parameters for the current frame"
  (select-frame frame)
  (if (window-system frame)
      (progn
	;; (set-background-color "#003344")
	;; (set-foreground-color "white")
	(set-cursor-color "red"))))

(add-hook 'after-make-frame-functions 'schmir-setup-frame)
(schmir-setup-frame (selected-frame))

(if (fboundp 'schmir-hl-fixme)
    (mapc 'schmir-hl-fixme
	  '(erlang-mode python-mode c-mode c++-mode emacs-lisp-mode listp-mode js2-mode)))


;; (define-key compilation-mode-map "q" 'kill-this-buffer)

;; (swapchars "0" ")")
;; (swapchars "1" "!")
;; (swapchars "2" "@")
;; (swapchars "3" "#")
;; (swapchars "4" "$")
;; (swapchars "5" "%")
;; (swapchars "6" "^")
;; (swapchars "7" "&")
;; (swapchars "8" "*")
;; (swapchars "9" "(")



(add-hook 'gnus-startup-hook 'bbdb-insinuate-gnus)

(setq smart-tab-using-hippie-expand 't)
(global-set-key (kbd "M-[") 'ido-goto-symbol)

(global-set-key (kbd "<C-f8>") 'bm-toggle)
(global-set-key (kbd "<f8>")   'bm-next)
(global-set-key (kbd "<S-f8>") 'bm-previous)

(global-set-key [(control f1)] 'highlight-symbol-at-point)
(global-set-key [f1] 'highlight-symbol-next)
(global-set-key [(shift f1)] 'highlight-symbol-prev)
(global-set-key [(meta f1)] 'highlight-symbol-query-replace)

(global-set-key [f5] 'git-grep)

(global-set-key (kbd "\e\el") 'goto-line)
(global-set-key (kbd "\e\ea") 'mark-whole-buffer)
(global-set-key (kbd "\e\em") 'manual-entry)
(global-set-key (kbd "\e\es") 'm-shell-command)

(when (require-try 'compile-dwim)
  (global-set-key (quote [f9]) 'compile-dwim)
  (global-set-key "c" 'compile-dwim))


(global-set-key (quote [S-iso-lefttab]) 'tab-to-tab-stop)
(global-set-key (quote [C-tab]) 'hippie-expand)
;; (global-set-key "" (quote comment-region))

(global-set-key (kbd "C-o") 'delete-blank-lines)
(global-set-key (kbd "C-x k") 'kill-this-buffer)

(global-set-key [C-S-right] 'shift-right)
(global-set-key [C-S-left] 'shift-left)

(global-set-key (kbd "C-x C-r") 'find-alternative-file-with-sudo)

;; (global-unset-key (kbd "C-x C-c"))
(global-set-key (kbd "C-x C-c") 'save-buffers-kill-terminal)

(global-set-key (kbd "M-:") 'align-regexp)
(global-set-key (kbd "M-b") 'save-buffers-kill-terminal)
(global-set-key (kbd "M-RET") 'fullscreen)


(global-set-key (quote [S-return]) 'open-line-below)
(global-set-key (kbd "M-\"") 'comment-dwim)

(eval-after-load "rcirc"
  '(progn
    (message "loading schmir-irc")
    (require 'schmir-irc)))


(setq irc-screen-number nil)
(defun goto-irc-screen ()
  (interactive)
  (if (not irc-screen-number)
      (progn
	(escreen-create-screen)
	(setq irc-screen-number escreen-current-screen-number)
	;; as i don't do this by default in escreen-create-screen
	(delete-other-windows))
    (escreen-goto-screen irc-screen-number)))

(defun toggle-irc-screen ()
  (interactive)
  (if (eq irc-screen-number escreen-current-screen-number)
      (escreen-goto-last-screen)
    (goto-irc-screen))
  (escreen-get-active-screen-numbers-with-emphasis))

(defun my-irc-next-active()
  (interactive)
  (if (fboundp 'rcirc-next-active-buffer)
      (progn
	(if rcirc-activity
	    (progn
	      (goto-irc-screen)
	      (rcirc-next-active-buffer nil)
	      (escreen-get-active-screen-numbers-with-emphasis))
	  (if (or
	       (eq irc-screen-number escreen-current-screen-number)
	       (if (eq last-command 'my-irc-next-active)
		   t
		 (message "press key again in order to return to irc screen")
		 nil))
	      (toggle-irc-screen))))
    (goto-irc-screen)
    (my-irc)))

(global-set-key [f12] 'my-irc-next-active)

(global-set-key [mouse-3] 'imenu)

(when (require-try 'key-chord)
  (setq key-chord-two-keys-delay 0.05)

  (key-chord-mode 1)

  (key-chord-define-global "yu" 'whole-line-or-region-yank)
  (key-chord-define-global "yy" 'yank-pop)
  (key-chord-define-global "ui"     "\M-w") ;; copy
  (key-chord-define-global "io"     "\C-w") ;; cut

  (key-chord-define-global "nm"     'ido-switch-buffer)

  (key-chord-define-global "m,"     'ido-find-file)
  (key-chord-define-global "./"     'save-buffer)

  (key-chord-define-global ":\""    'comment-dwim)
  (key-chord-define-global ";\""    'comment-dwim)
  (key-chord-define-global ";'"    'comment-dwim)

  (key-chord-define-global "hj"     'toggle-windows-split)
  (key-chord-define-global "jk"     'flip-window)
  (key-chord-define-global "kl"     'other-frame))


(setq suggest-key-bindings t)

(when (require-try 'misc-cmds)
  (define-key ctl-x-map [home] 'mark-buffer-before-point)
  (define-key ctl-x-map [end]  'mark-buffer-after-point))

;;  Suggested key bindings:
;;
;;   (define-key ctl-x-map [home] 'mark-buffer-before-point)
;;   (define-key ctl-x-map [end]  'mark-buffer-after-point)
;;   (define-key ctl-x-map "w"    'region-to-file)
;;   (global-set-key [C-S-f1]     'region-to-buffer)
;;   (global-set-key [C-S-backspace] 'region-to-file)
;;   (global-set-key [home]       'backward-line-text)
;;   (substitute-key-definition   'kill-buffer
;;                                'kill-buffer-and-its-windows global-map)
;;   (substitute-key-definition   'move-beginning-of-line 'beginning-of-line+ global-map)
;;   (substitute-key-definition   'end-of-line 'end-of-line+ global-map)
;;   (substitute-key-definition   'recenter 'recenter-top-bottom global-map)

;; http://www.emacswiki.org/emacs-ru/WholeLineOrRegion
;; This minor mode allows functions to operate on the current line if
;; they would normally operate on a region and region is currently
;; undefined.

(when (require-try 'whole-line-or-region)
  (add-to-list 'whole-line-or-region-extensions-alist '(comment-dwim whole-line-or-region-comment-dwim nil))
  (whole-line-or-region-mode t)
  (diminish 'whole-line-or-region-mode)
  (defalias 'whole-line-or-region-kill-region 'schmir-whole-line-or-region-kill-region))


;; "funky stuff" ;; proceed with caution

(global-set-key (kbd "C-z") 'undo)
;; (when (require-try 'redo+)
;;   (global-set-key (kbd "C-S-z") 'redo))


(setq my-key-pairs
      '((?! ?1) (?@ ?2) (?# ?3) (?$ ?4) (?% ?5)
        (?^ ?6) (?& ?7) (?* ?8) (?( ?9) (?) ?0)
        (?- ?_) (?\" ?') (?{ ?[) (?} ?])         ; (?| ?\\)
        ))

(defun my-key-swap (key-pairs)
  (if (eq key-pairs nil)
      (message "Keyboard zapped!! Shift-F10 to restore!")
      (progn
	(keyboard-translate (caar key-pairs)  (cadar key-pairs))
	(keyboard-translate (cadar key-pairs) (caar key-pairs))
        (my-key-swap (cdr key-pairs))
        )
    ))


;; (my-key-swap my-key-pairs)
;; (my-key-restore my-key-pairs)

(defun my-key-restore (key-pairs)
  (if (eq key-pairs nil)
      (message "Keyboard restored!! F10 to Zap!")
      (progn
        (keyboard-translate (caar key-pairs)  (caar key-pairs))
        (keyboard-translate (cadar key-pairs) (cadar key-pairs))
        (my-key-restore (cdr key-pairs))
        )
    ))

(when (require-try 'sequential-command)
  (define-sequential-command my-home
    back-to-indentation
    ;; beginning-of-line
    beginning-of-buffer
    seq-return)

  (define-sequential-command my-end
    end-of-line
    end-of-buffer
    seq-return)

  (global-set-key (quote [home]) 'my-home)
  (global-set-key (quote [end]) 'my-end))

;; (local-set-key "." 'electric-dot-and-dash-dot)
;; (local-set-key ",," 'electric-dot-and-dash-dash)

(when (require-try 'repeatable)
  (repeatable-command-advice next-buffer)
  (repeatable-command-advice exchange-point-and-mark))
  ;; (repeatable-command-advice undo))




(setq isearch-allow-scroll t)
(define-key isearch-mode-map [next] 'isearch-repeat-forward)
(define-key isearch-mode-map [prior] 'isearch-repeat-backward)
(global-set-key [C-next] 'isearch-repeat-forward)
(global-set-key [C-prior] 'isearch-repeat-backward)

(define-key isearch-mode-map (kbd "C-o")
  (lambda ()
    (interactive)
    (let ((case-fold-search isearch-case-fold-search))
      (occur (if isearch-regexp isearch-string
	       (regexp-quote isearch-string))))))


;; TAB expands even during isearch (Ctrl-S)
(define-key isearch-mode-map [tab] 'isearch-yank-word)
(setq lazy-highlight-cleanup nil) ;; keep search results highlighted



(global-set-key (kbd "<mouse-3>") 'mouse-buffer-menu)

(global-set-key (kbd "C-j")
		'(lambda()
		   (interactive)
		   (delete-indentation 1)))

(global-set-key (kbd "C-S-j")
		'(lambda()
		   (interactive)
		   (delete-indentation)))
(global-set-key "\M-p" 'goto-line)

(global-set-key [C-backspace] 'backward-kill-word)
(defadvice yes-or-no-p (around no-query-compilation-always-kill activate)
  "make `compile' always kill existing compilation."
  (if (string-match "A compilation process is running; kill it\\?"
		     prompt)
      (setq ad-return-value t)
    ad-do-it))

(setq compilation-ask-about-save nil)
(setq ps-lpr-command "gtklp"
      ps-lpr-switches '("-X")
      ps-landscape-mode nil
      ps-number-of-columns 1
      ps-n-up-printing 2
      ps-n-up-border-p nil
      ps-line-number t
      ps-line-number-step 'zebra
      ps-left-margin 14
      ps-right-margin 14
      ps-top-margin 14
      ps-bottom-margin 14
      ps-zebra-stripes t
      ps-zebra-stripe-height 10
      ps-zebra-color 0.98
      ps-paper-type 'a4)

(put 'downcase-region 'disabled nil)

;;; scheme
(setq quack-pretty-lambda-p 't)
(require-try 'quack)

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
;; (add-hook 'scheme-mode-hook 'my-nice-comment-semicolon)
(add-hook 'python-mode-hook 'my-nice-comment-hash-mark)
(add-hook 'c-mode-hook 'my-nice-comment-slash)
(add-hook 'c++-mode-hook 'my-nice-comment-slash)
(add-hook 'sh-mode-hook 'my-nice-comment-hash-mark)
(add-hook 'text-mode-hook 'my-control-l)
(add-hook 'conf-mode-hook 'my-nice-comment-hash-mark)

;;; mail
;; (setq message-send-mail-function 'smtpmail-send-it
;;       send-mail-function 'smtpmail-send-it
;;       smtpmail-smtp-server "mail"
;;       smtpmail-local-domain nil
;;       smtpmail-debug-info t)

(setq send-mail-function 'message-send-mail-with-sendmail
      message-send-mail-function 'message-send-mail-with-sendmail)

;; we substitute sendmail with msmtp
(setq sendmail-program "/usr/bin/msmtp"
      mail-specify-envelope-from t
      mail-envelope-from 'header)
(setq message-sendmail-envelope-from 'header)

(put 'set-goal-column 'disabled nil)
(define-prefix-command 'nabla-map)
(global-set-key '[8711] 'nabla-map)
(define-key nabla-map (kbd "a") 'execute-extended-command)
(define-key nabla-map (kbd "x") 'exchange-point-and-mark)


(define-key global-map (kbd "C-M-<left>") 'shrink-window-horizontally)
(define-key global-map (kbd "C-M-<right>") 'enlarge-window-horizontally)
(define-key global-map (kbd "C-M-<up>") 'enlarge-window)
(define-key global-map (kbd "C-M-<down>") 'shrink-window)

(defun toggle-windows-split()
  "Switch back and forth between one window and whatever split of windows we might have in the frame. The idea is to maximize the current buffer, while being able to go back to the previous split of windows in the frame simply by calling this command again."
  (interactive)
  (if (not (window-minibuffer-p (selected-window)))
      (progn
	(if (< 1 (count-windows))
	    (progn
	      (window-configuration-to-register ?u)
	      (delete-other-windows))
	  (jump-to-register ?u)))))

(defun dedicate-window()
  (interactive)
  (let* ((window    (selected-window))
	 (dedicated (window-dedicated-p window)))
    (set-window-dedicated-p window (not dedicated))
    (message "Window %sdedicated to %s"
	     (if dedicated "no longer " "")
	     (buffer-name))))

(global-set-key (quote [8711 100]) (quote dedicate-window))

;;; escreen/gnus interaction
(when (require-try 'my-escreen)
  (defadvice gnus-group-exit (after remove-screen (&rest args) activate)
    (escreen-kill-screen)
    (setq gnus-screen-number nil))

  ;; i'm in the habit of quitting when i don't really need to
  (add-hook 'gnus-group-mode-hook
	    (lambda ()
	      (local-set-key (kbd "q") 'escreen-goto-last-screen)
	      (local-set-key (kbd "Q") 'gnus-group-exit)))
  (global-set-key [f11] 'my-switch-to-gnus))

(setq gnus-screen-number nil)
(defun my-switch-to-gnus()
  (interactive)
  (if (or (not (fboundp 'gnus-alive-p))
	  (not (gnus-alive-p))
	  (not gnus-screen-number))
      (progn
	(escreen-create-screen)
	(setq gnus-screen-number escreen-current-screen-number)
	;; as i don't do this by default in escreen-create-screen
	(delete-other-windows)
	(gnus))

    ;; if we're not in a gnus buffer, just switch to our gnus screen, thus
    ;; returning us to where we were previously. otherwise determine what we
    ;; should switch to
    (if (eq escreen-current-screen-number gnus-screen-number)
	(escreen-goto-last-screen)
      (escreen-goto-screen gnus-screen-number)
      (switch-to-buffer "*Group*")
      (gnus-group-get-new-news)))
  (escreen-get-active-screen-numbers-with-emphasis))


;; quit gnus properly instead of leaving auto-save files around
(defadvice save-buffers-kill-emacs (before quit-gnus (&rest args) activate)
  (let (buf)
    (when (and (fboundp 'gnus-alive-p)
	       (gnus-alive-p)
	       (bufferp (setq buf (get-buffer "*Group*"))))
      (with-current-buffer buf
	(gnus-group-exit)))))




(blink-cursor-mode 1)

(defun my-set-compile-command()
  "If first line contains #!/usr/... set compile-command to the file itself"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (if (and (looking-at "#! */")
	     (file-executable-p buffer-file-name)
	     (not (eq (variable-binding-locus 'compile-command) (current-buffer))))
	(progn
	  (make-local-variable 'compile-command)
	  (setq compile-command buffer-file-name)))))

(add-hook 'find-file-hook 'my-set-compile-command)


(defun update-last-modified()
  (interactive)
  (save-excursion
    (let ((case-replace t)
	  (case-fold-search t))
      (goto-char (point-min))
       (while (re-search-forward
	      "\\(Last[ -]\\(changed\\|modified\\):\\) [1-9].*"
	      nil t)

	(replace-match
	 (concat "\\1 "
		 (format-time-string "%Y-%m-%d %H:%M:%S")
		 " by "
		 (user-login-name)
		 )
	 nil nil)))))

(add-hook 'write-file-hooks 'update-last-modified)
(add-to-list 'auto-mode-alist '("\\.ml\\w?" . tuareg-mode))

(when (require-try 'cwc)
  (global-highlight-changes-mode t)
  (setq highlight-changes-visibility-initial-state nil)
  (add-to-list 'whitespace-style 'trailing)
  (add-hook 'before-save-hook 'changed-whitespace-cleanup))


(defun show-trailing-whitespace()
  (interactive)
  (setq show-trailing-whitespace (not show-trailing-whitespace))
  (message (if show-trailing-whitespace
	       "show-trailing-whitespace enabled"
	       "show-trailing-whitespace disabled")))
;; (show-trailing-whitespace)



(setq exec-abbrev-cmd-file "~/.emacs.d/exec-abbrev-cmd.dat")
(when (require-try 'exec-abbrev-cmd)
  (exec-abbrev-cmd-mode 1)
  (global-set-key (kbd "M-x") 'exec-abbrev-cmd))


;;; enable smerge mode
(defun sm-try-smerge ()
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^<<<<<<< " nil t)
      (smerge-mode 1))))
(add-hook 'find-file-hook 'sm-try-smerge t)


(setq python-pep8-options '("--repeat"))
(require 'help-mode)
(when (require-try 'auto-complete-config)
  (ac-config-default)
  (add-to-list 'ac-modes 'message-mode)
  (add-to-list 'ac-modes 'cython-mode)
  (add-to-list 'ac-dictionary-directories (concat dotfiles-dir "vendor/auto-complete/dict")))


(require 'grep)
(setq search-all-buffers-ignored-files '(".bbdb" ".newsrc-dribble"))

(defun search-all-buffers (regexp prefix)
  "Searches file-visiting buffers for occurence of REGEXP.  With
prefix > 1 (i.e., if you type C-u \\[search-all-buffers]),
searches all buffers."
  (interactive (list (grep-read-regexp)
		     current-prefix-arg))
  (message "Regexp is %s; prefix is %s" regexp prefix)
  (multi-occur
   (if (member prefix '(4 (4)))
       (buffer-list)
     (remove-if
      (lambda (b) (some (lambda (rx) (string-match rx  (file-name-nondirectory (buffer-file-name b)))) search-all-buffers-ignored-files))
      (remove-if-not 'buffer-file-name (buffer-list))))

   regexp))

(global-set-key [f7] 'search-all-buffers)

(eval-after-load "magit"
  '(progn
     (define-key magit-status-mode-map (kbd "q") 'magit-quit-session)
     (if (eq system-type 'windows-nt)
       (setq magit-git-executable (executable-find "git")))))
;; full screen magit-status

(defadvice magit-status (around magit-fullscreen activate)
  (window-configuration-to-register :magit-fullscreen)
  ad-do-it
  (delete-other-windows))

(defun magit-quit-session ()
  "Restores the previous window configuration and kills the magit buffer"
  (interactive)
  (kill-buffer)
  (jump-to-register :magit-fullscreen))



(defun sort-words (reverse beg end)
  "Sort words in region alphabetically, in REVERSE if negative.
    Prefixed with negative \\[universal-argument], sorts in reverse.

    The variable `sort-fold-case' determines whether alphabetic case
    affects the sort order.

    See `sort-regexp-fields'."
  (interactive "*P\nr")
  (sort-regexp-fields reverse "\\w+" "\\&" beg end))

(defun my-gnus-summary-view-html-alternative-in-mozilla ()
      "Display the HTML part of the current multipart/alternative MIME message
    in mozilla."
      (interactive)
      (save-current-buffer
	(gnus-summary-show-article)
	(set-buffer gnus-article-buffer)
	(let ((file (make-temp-file "html-message-" nil ".html"))
	      (handle (nth 3 (assq 1 gnus-article-mime-handle-alist))))
	  (mm-save-part-to-file handle file)
	  (browse-url-firefox (concat "file://" file)))))


(message "initialization complete")
