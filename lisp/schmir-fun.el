;; -*- mode: emacs-lisp-*-
;;

;; autoloads
(autoload 'sgml-quote "sgml-mode"
  "Quote SGML text in region START ... END.
Only &, < and > are quoted, the rest is left untouched.
With prefix argument UNQUOTEP, unquote the region." t)
(defalias 'html-quote 'sgml-quote)

(defun remove-dos-eol ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))

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

(defun show-trailing-whitespace()
  (interactive)
  (setq show-trailing-whitespace (not show-trailing-whitespace))
  (message (if show-trailing-whitespace
	       "show-trailing-whitespace enabled"
	       "show-trailing-whitespace disabled")))
;; (show-trailing-whitespace)

(defun wipe-right-whitespace ()
  "White-space (blanks and tabs) right of point are removed"
  (interactive)
  (while (looking-at "[ \t]")
    (delete-char 1)))

(defun delete-whole-line (&optional arg)
  "delete whole line"
  (interactive)
  (beginning-of-line)
  (let ((kill-whole-line 't))
    (kill-line))) 



(defvar saved-frame-parameters 'nil)
(defun toggle-fullscreen-window ()
  (interactive)
  (if (frame-parameter nil 'fullscreen)
      (progn
	(modify-frame-parameters nil saved-frame-parameters)
	(set-frame-parameter nil 'fullscreen 'nil))
    (progn
      (setq saved-frame-parameters (cdr (frame-parameters)))
      (set-frame-parameter nil 'fullscreen 'fullboth))))


(defun fullscreen()
  (interactive)
  (if (fboundp 'mac-toggle-full-frame)
      (mac-toggle-full-frame)
    (toggle-fullscreen-window)))

(defun indent-buffer ()
  "Indents the current buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((lines (count-lines (point-min) (point-max)))
          (current (line-number-at-pos)))
      (while (<= current lines)
        (indent-according-to-mode)
        (forward-line)
        (incf current)))))

(defun copy-line (arg)
  "Copy lines (as many as prefix argument) in the kill ring"
  (interactive "p")
  (kill-ring-save (line-beginning-position)
                  (line-beginning-position (+ 1 arg)))
  (message "%d line%s copied" arg (if (= 1 arg) "" "s")))

(defun open-line-above ()
  "Open a line above the line the point is at.
Then move to that line and indent accordning to mode"
  (interactive)
  (move-beginning-of-line 1)
  (newline)
  (previous-line)
  (indent-according-to-mode))

(defun open-line-below ()
  "Open a line below the line the point is at.
Then move to that line and indent accordning to mode"
  (interactive)
  (move-end-of-line 1)
  (newline)
  (indent-according-to-mode))

;; flip to the last-visited buffer
(defun flip-window () "Flip this window" (interactive)
  (switch-to-buffer (other-buffer)))

(defun pydoc (man-args)
    (interactive "sPydoc: ")
    (require 'man)
    (let ((manual-program "pydoc"))
      (man man-args)))

(defun read-shell-file-command (command)
  "Prompt for shell COMMAND, using current buffer's file as default arg.
If buffer is not associated with a file, you are prompted for a file.
COMMAND is a symbol."
  (let ((file (or (buffer-file-name) (read-file-name "File: "))))
    (setq file (and file (file-name-nondirectory file)))
    (setq command (format "%s  " command)) ; Convert to string.
    (read-from-minibuffer
     "" (cons (concat command (and file (concat " " file)))
              (length command)))))

(defun chmod (cmd)
  "Execute Unix command `chmod'.  Current buffer's file is default arg.
CMD is the command to execute (interactively, `chmod')."
  (interactive (list (read-shell-file-command 'chmod)))
  (shell-command cmd))

(defun chgrp (cmd)
  "Execute Unix command `chgrp'.  Current buffer's file is default arg.
CMD is the command to execute (interactively, `chgrp')."
  (interactive (list (read-shell-file-command 'chgrp)))
  (shell-command cmd))

(defun chown (cmd)
  "Execute Unix command `chown'.  Current buffer's file is default arg.
CMD is the command to execute (interactively, `chown')."
  (interactive (list (read-shell-file-command 'chown)))
  (shell-command cmd))

(defun svn (cmd)
  "Execute Unix command `svn'.  Current buffer's file is default arg.
CMD is the command to execute (interactively, `svn')."
  (interactive (list (read-shell-file-command 'svn)))
  (shell-command cmd))

(defun hg  (cmd)
  "Execute Unix command `hg'.  Current buffer's file is default arg.
CMD is the command to execute (interactively, `hg')."
  (interactive (list (read-shell-file-command 'hg)))
  (shell-command cmd))

(defun search-google ()
  "Prompt for a query in the minibuffer, launch the web browser and query google."
  (interactive)
  (let ((search (read-from-minibuffer "Google Search: " (word-around-point))))
    (browse-url (concat "http://www.google.com/search?q=" search))))

(defun move-line (&optional n)
  "Move current line N (1) lines up/down leaving point in place."
  (interactive "p")
  (when (null n) 
    (setq n 1)) 
  (let ((col (current-column)))
    (beginning-of-line)
    (next-line 1)
    (transpose-lines n)
    (previous-line 1)
    (forward-char col))) 

(defun move-line-up (n)
  "Moves current line N (1) lines up leaving point in place."
  (interactive "p")
  (move-line (if (null n) -1 (- n))))

(defun move-line-down (n)
  "Moves current line N (1) lines down leaving point in place."
  (interactive "p")
  (move-line (if (null n) 1 n)))
;; Shift the selected region right if distance is postive, left if
;; negative

(defun shift-region (distance)
  (let ((mark (mark)))
    (save-excursion
      (indent-rigidly (region-beginning) (region-end) distance)
      (push-mark mark t t)
      ;; Tell the command loop not to deactivate the mark
      ;; for transient mark mode
      (setq deactivate-mark nil))))

(defun shift-right ()
  (interactive)
  (shift-region 1))

(defun shift-left ()
  (interactive)
  (shift-region -1))

(defun find-alternative-file-with-sudo ()
  (interactive)
  (when buffer-file-name
    (let ((my-point (point)))
      (find-alternate-file
       (concat "/root@localhost:"
	       buffer-file-name))
      (goto-char my-point))
    (message buffer-file-name)))


(defun swapchars (a b)
  (interactive)
  (lexical-let ((a a)(b b))
    (global-set-key a #'(lambda () (interactive) (insert b)))
    (global-set-key b #'(lambda () (interactive) (insert a)))
))


(defmacro define-buffer-visitor (visitor-name buffer-name command)
  `(defun ,visitor-name ()
     (interactive)
     (if (get-buffer ,buffer-name)
	 (if (equal ,buffer-name (buffer-name))
	     (bury-buffer)
	   (switch-to-buffer ,buffer-name))
	   
       (call-interactively ,command))))

(defun ido-goto-symbol ()
  "Will update the imenu index and then use ido to select a symbol to navigate to"
  (interactive)
  (require 'ido)
  (require 'imenu)
  (imenu--make-index-alist)
  (let ((name-and-pos '())
	(symbol-names '()))
    (flet ((addsymbols (symbol-list)
		       (when (listp symbol-list)
			 (dolist (symbol symbol-list)
			   (let ((name nil) (position nil))
			     (cond
			      ((and (listp symbol) (imenu--subalist-p symbol))
			       (addsymbols symbol))
			      
			      ((listp symbol)
			       (setq name (car symbol))
			       (setq position (cdr symbol)))
			      
			      ((stringp symbol)
			       (setq name symbol)
			       (setq position (get-text-property 1 'org-imenu-marker symbol))))
			     
			     (unless (or (null position) (null name))
			       (add-to-list 'symbol-names name)
			       (add-to-list 'name-and-pos (cons name position))))))))
      (addsymbols imenu--index-alist))
    (let* ((selected-symbol (ido-completing-read "Symbol? " symbol-names))
	   (position (cdr (assoc selected-symbol name-and-pos))))
      (goto-char position))))

;;;
;; Smart Tab

(defvar smart-tab-using-hippie-expand nil
  "turn this on if you want to use hippie-expand completion.")

;; (global-set-key [(tab)] 'smart-tab)
(defun smart-tab (prefix)
  "Needs `transient-mark-mode' to be on. This smart tab is
minibuffer compliant: it acts as usual in the minibuffer.

In all other buffers: if PREFIX is \\[universal-argument], calls
`smart-indent'. Else if point is at the end of a symbol,
expands it. Else calls `smart-indent'."
  (interactive "P")
  (if (minibufferp)
      (minibuffer-complete)
    (if (smart-tab-must-expand prefix)
        (if smart-tab-using-hippie-expand
            (hippie-expand nil)
          (dabbrev-expand nil))
      (smart-indent))))

(defun smart-indent ()
  "Indents region if mark is active, or current line otherwise."
  (interactive)
  (if mark-active
      (indent-region (region-beginning)
                     (region-end))
    (indent-for-tab-command)))

(defun smart-tab-must-expand (&optional prefix)
  "If PREFIX is \\[universal-argument], answers no.
Otherwise, analyses point position and answers."
  (unless (or (consp prefix)
              mark-active)
    (looking-at "\\_>")))


(defun indent-or-expand (arg)
  "Either indent according to mode, or expand the word preceding
point."
  (interactive "*P")
  (if snippet
      (snippet-next-field)

    (if (and
	 (or (bobp) (= ?w (char-syntax (char-before))))
       (or (eobp) (not (= ?w (char-syntax (char-after))))))
	(hippie-expand arg)
      (indent-according-to-mode))))

(defun ido-execute-command ()
   (interactive)
   (call-interactively
    (intern
     (ido-completing-read
      "M-x "
      (progn
        (unless ido-execute-command-cache
          (mapatoms (lambda (s)
                      (when (commandp s)
                        (setq ido-execute-command-cache
                              (cons (format "%S" s) ido-execute-command-cache))))))
        ido-execute-command-cache)))))
(defun filter-special-name(name)
  (if (or
       (string= (substring name 0 1) " ")
       (string= (file-name-nondirectory name) ".ido.last")
       )
      'nil
    name))

(defun rs-ido-recentf-switch-buffer ()
  "switch to buffer or recently opened file"
  (interactive)
  (let* ((all-files (delete 'nil (append
		     (mapcar 'buffer-file-name (buffer-list))
		     recentf-list
		     (mapcar (lambda(x)
					    (if (buffer-file-name x)
						'nil
					      (filter-special-name (buffer-name x)))) 
					  (buffer-list))
		     )))
	 (file-assoc-list (mapcar (lambda (x) (cons (file-name-nondirectory x) x)) all-files))
	 (filename-list (remove-duplicates (mapcar 'car file-assoc-list) :test 'string=))
	 (ido-make-buffer-list-hook
	  (lambda ()
	    (setq ido-temp-list filename-list)))
	 (filename (ido-read-buffer "(recent) buffer: "))
	 (result-list (delq nil (mapcar (lambda (x) (if (string= (car x) filename) (cdr x))) file-assoc-list)))
	 (result-length (length result-list)))
    (if (get-buffer filename)
	(switch-to-buffer (get-buffer filename))
      (find-file 
       (cond 
	((= result-length 0) filename)
	((= result-length 1) (car result-list))
	( t
	  (let ( (ido-make-buffer-list-hook
		  (lambda ()
		    (setq ido-temp-list result-list))))
	    (ido-read-buffer (format "%d matches:" result-length))))
	)))))

(defun steve-ido-choose-from-recentf ()
  "Use ido to select a recently opened file from the `recentf-list'"
  (interactive)
  (find-file (ido-completing-read "Open file: " recentf-list nil t)))


(defun schmir-define-custom-modes()
  (interactive)
  (define-generic-mode 'xmodmap-mode
    '(?!)
    '("add" "clear" "keycode" "keysym" "remove" "pointer")
    nil
    '("[xX]modmap\\(rc\\)?\\'")
    nil
    "Simple mode for xmodmap files.")

  (define-generic-mode 'keymap-mode
    '(?#)
    '("include" "keymaps" "keycode" )
    nil
    '("\\.map$\\|keymap\\(rc\\)?\\'")
    nil
    "Simple mode for keymap files."))

(defun schmir-split-or-unsplit-window()
  (interactive)
  (if (> (count-windows) 1)
      (delete-other-windows)
    (split-window-vertically)))

(defun schmir-hl-fixme (mode)
  "highlight FIXME, XXX, TODO in mode"
  (interactive)
  (font-lock-add-keywords mode
			  '(("\\<\\(fixme\\|FIXME\\|HACK\\|XXX\\|TODO\\|assert\\)" 1 font-lock-warning-face prepend))))

(defun pyflakes ()
  (interactive)
  (compile (format "pyflakes %s" (buffer-file-name))))

(defun m-shell-command ()
    "Launch a shell command."
    (interactive)
    (let ((command (read-string "Command: ")))
      (shell-command (concat command " &") (concat "*" command "*"))))

(defun schmir-whole-line-or-region-kill-region (prefix)
  (interactive "*p")
  (if mark-active
      (whole-line-or-region-call-with-region 'kill-region prefix t)
    (save-excursion 
      (beginning-of-line)
      (if (looking-at "[ \t]*$")
	  (delete-blank-lines)
	(whole-line-or-region-call-with-region 'kill-region prefix t)))))

;; http://atomized.org/2008/07/emacs-open-a-shell-in-the-current-directory/
(defun schmir-shell-here ()
  "Open a shell in `default-directory'."
  (interactive)
  (let ((dir (expand-file-name default-directory))
        (buf (or (get-buffer "*shell*") (shell))))
    (if (not (string= (buffer-name) "*shell*"))
        (switch-to-buffer-other-window buf))

    (comint-send-string (get-buffer-process buf)
			(concat "cd \"" dir "\"\recho\r"))
    (setq list-buffers-directory dir)))

;; http://hjiang.net/archives/
(defun smart-split ()
  "Split the frame into 80-column sub-windows, and make sure no window has
   fewer than 80 columns."
  (interactive)
  (defun smart-split-helper (w)
    "Helper function to split a given window into two, the first of which has 
     80 columns."
    (if (> (window-width w) (* 2 81))
    (let ((w2 (split-window w 82 t)))
      (smart-split-helper w2))))
  (smart-split-helper nil))

(defun bury-buffer-other-window ()
  (interactive)
  (if (one-window-p t)
      (split-window-vertically))
  (save-selected-window
    (other-window 1)
    (bury-buffer)))


(defun schmir-kill-line-backward ()
  "Kill text between the beginning of the line to the cursor position.
If there's no text, delete the previous line ending."
  (interactive)
  (if (looking-back "\n")
      (delete-char -1)
    (kill-line 0)))


(require 'url)
(defun schmir-pyhelp ()
  "search google for python docs"
  (interactive)
  (browse-url   
   (format "http://google.com/search?q=%s+site:docs.python.org+inurl:library&btnI"
	   (url-hexify-string (thing-at-point 'symbol)))))


;; Move to match if on (){}[] when pressing %, otherwise insert %.
(defun schmir-match-paren (arg)
  "Go to the matching paren if on a paren; otherwise insert %."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
	((looking-at "\\s\)") (forward-char 1) (backward-list 1))
	((looking-at "\\s\{") (forward-list 1) (backward-char 1))
	((looking-at "\\s\}") (forward-char 1) (backward-list 1))
	((looking-at "\\s[") (forward-list 1) (backward-char 1))
	((looking-at "\\s]") (forward-char 1) (backward-list 1))
	(t (self-insert-command (or arg 1)))))

;; (global-set-key "%" 'schmir-match-paren)

(defun google-region (beg end)
  "Google the selected region."
  (interactive "r")
  (browse-url (concat "http://www.google.com/search?ie=utf-8&oe=utf-8&q=" (buffer-substring beg end))))

(defun sort-words (reverse beg end)
  "Sort words in region alphabetically, in REVERSE if negative.
    Prefixed with negative \\[universal-argument], sorts in reverse.

    The variable `sort-fold-case' determines whether alphabetic case
    affects the sort order.

    See `sort-regexp-fields'."
  (interactive "*P\nr")
  (sort-regexp-fields reverse "\\w+" "\\&" beg end))

(defun rs/replace (src dst)
  (goto-char (point-min))
  (let ((case-fold-search nil))
    (while (re-search-forward src nil t)
      (replace-match dst t nil))))

(defun rs/replace-umlauts ()
  (interactive)
  (save-excursion
    (save-restriction
      (narrow-to-region (region-beginning) (region-end))
      (rs/replace "Ä" "Ae")
      (rs/replace "ä" "ae")
      (rs/replace "Ü" "Ue")
      (rs/replace "ü" "ue")
      (rs/replace "Ö" "Oe")
      (rs/replace "ö" "oe"))))


(provide 'schmir-fun)
