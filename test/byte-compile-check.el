;;; byte-compile-check.el --- Fail on byte-compile warnings  -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Byte-compile lisp/ and setup/ and exit non-zero if anything warns.
;;
;; Run with: just check
;; Or manually: emacs --batch -l test/byte-compile-check.el
;;
;; early-init.el silences the byte compiler for interactive use, which is
;; how several obsolete settings went unnoticed for a while.  This check
;; turns the warnings back on so they surface somewhere.
;;
;; Categories that a lazily-configured init cannot avoid are disabled:
;; package variables and functions are legitimately unknown at compile
;; time because the packages load later.
;;
;; The configuration is loaded first, and not only so that `setup' is
;; defined: a function is reported as obsolete only once the library
;; defining it has been loaded, since that is what carries the marker.
;; Checking against a bare Emacs would silently miss those.

;;; Code:

(require 'bytecomp)

;; Load the configuration so that `setup' and its macros are defined.
(load (expand-file-name "init.el" user-emacs-directory))

;; prism colorizes using the current theme and fails without a display.
(advice-add 'prism-mode :override #'ignore)

;; Undo the startup-time silencing from early-init.el.
(setq byte-compile-warnings '(not free-vars unresolved noruntime
                                  docstrings docstrings-wide
                                  docstrings-non-ascii-quotes)
      byte-compile-verbose nil
      warning-minimum-level :warning
      warning-suppress-log-types nil
      warning-suppress-types nil)

(defvar bcc-directories '("lisp" "setup")
  "Directories below `user-emacs-directory' to byte-compile.")

(defun bcc-files ()
  "Return the Elisp files to check, skipping generated ones."
  (let (files)
    (dolist (dir bcc-directories (nreverse files))
      (dolist (file (directory-files
                     (expand-file-name dir user-emacs-directory) t "\\.el\\'"))
        (unless (equal (file-name-nondirectory file) "loaddefs.el")
          (push file files))))))

(defun bcc-run ()
  "Byte-compile the configuration, returning the compile log as a string.
Compiled output goes to a temporary directory so that no .elc file is
written next to the sources, where a stale one could later be loaded in
preference to the .el file."
  (let* ((tmpdir (make-temp-file "bcc" t))
         (byte-compile-dest-file-function
          (lambda (source)
            (expand-file-name (concat (file-name-nondirectory source) "c")
                              tmpdir))))
    (unwind-protect
        (progn
          (dolist (file (bcc-files))
            (byte-compile-file file))
          (with-current-buffer (get-buffer-create byte-compile-log-buffer)
            (buffer-string)))
      (delete-directory tmpdir t))))

(let* ((log (bcc-run))
       (problems (seq-filter (lambda (line)
                               (string-match-p "\\(Warning\\|Error\\):" line))
                             (split-string log "\n" t))))
  (if (null problems)
      (message "byte-compile-check: %d files, clean" (length (bcc-files)))
    (message "%s" log)
    (message "byte-compile-check: %d problem(s)" (length problems))
    (kill-emacs 1)))

;;; byte-compile-check.el ends here
