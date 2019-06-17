;; next function from: https://github.com/purcell/emacs.d/blob/master/site-lisp/flymake-ruby/flymake-ruby.el
;; Not provided by flymake itself, curiously
(defun flymake-create-temp-in-system-tempdir (filename prefix)
  (let ((tmpfile (make-temp-file (or prefix "flymake-") nil (concat "." (file-name-extension (or (buffer-name) ""))))))
    ;; (message "using %s" tmpfile)
    tmpfile))

(defun flymake-erlang-init ()
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
		     'flymake-create-temp-inplace))
	 (local-file (file-relative-name
		      temp-file
		      (file-name-directory buffer-file-name))))
    (list "eflymake" (list local-file))))


(defun flymake-pyflakes-init()
  (list "pyflakes"
	(list (flymake-init-create-temp-buffer-copy 'flymake-create-temp-in-system-tempdir))))


(defun flymake-lua-init ()
  "Invoke luac with '-p' to get syntax checking"
  (list "luac"
	(list "-p" (flymake-init-create-temp-buffer-copy 'flymake-create-temp-in-system-tempdir))))


(eval-after-load "flymake"
  '(progn
     (global-set-key (quote [f6]) (quote flymake-goto-next-error))

     (defun flymake-report-fatal-status (status warning)
       "Display a warning and switch flymake mode off."
       (message "Flymake: %s" warning)
       (flymake-log 0 "buffer %s fatal status %s, warning %s"
		    (buffer-name) status warning))

     (if (executable-find "eflymake")
	 (add-to-list 'flymake-allowed-file-name-masks
		      '("\\.erl\\'" flymake-erlang-init))
       (run-at-time 2 nil (lambda () (message "WARNING: eflymake not found. cannot enable flymake for erlang"))))

     
     (if (executable-find "pyflakes")
	 (add-to-list 'flymake-allowed-file-name-masks
		      '("\\.py\\'" flymake-pyflakes-init))
       (run-at-time 2 nil (lambda() (message "WARNING: pyflakes not found. cannot enable flymake for python"))))


     (push '("\\.lua\\'" flymake-lua-init) flymake-allowed-file-name-masks)
     (push '("^.*luac[0-9.]*\\(.exe\\)?: *\\(.*\\):\\([0-9]+\\): \\(.*\\)$" 2 3 nil 4)
	   flymake-err-line-patterns)

     (setq flymake-no-changes-timeout 4.0)))

;;flymake-ler(file line type text &optional full-file)
(defun show-fly-err-at-point ()
  "If the cursor is sitting on a flymake error, display the
message in the minibuffer"
  (interactive)
  (let ((line-no (line-number-at-pos)))
    (dolist (elem flymake-err-info)
      (if (eq (car elem) line-no)
	  (let ((err (car (second elem))))
	    (message "%s" (fly-pyflake-determine-message err)))))))

(defun fly-pyflake-determine-message (err)
  "pyflake is flakey if it has compile problems, this adjusts the
message to display, so there is one ;)"
  (cond ((not (or (eq major-mode 'Python) (eq major-mode 'python-mode) t)))
	((null (flymake-ler-file err))
	 ;; normal message do your thing
	 (flymake-ler-text err))
	(t ;; could not compile err
	 (format "compile error, problem on line %s" (flymake-ler-line err)))))

(defadvice flymake-goto-next-error (after display-message activate compile)
  "Display the error in the mini-buffer rather than having to mouse over it"
  (show-fly-err-at-point))

(defadvice flymake-goto-prev-error (after display-message activate compile)
  "Display the error in the mini-buffer rather than having to mouse over it"
  (show-fly-err-at-point))

(defadvice flymake-mode (before post-command-stuff activate compile)
  "Add functionality to the post command hook so that if the
cursor is sitting on a flymake error the error information is
displayed in the minibuffer (rather than having to mouse over
it)"
  (set (make-local-variable 'post-command-hook)
       (cons 'show-fly-err-at-point post-command-hook))) 


(provide 'schmir-flymake)
