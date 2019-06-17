;; Copyright (c) 2010 David Anderson <dave@natulte.net>
;;
;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use, copy,
;; modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.
;;
;;
;; Vim modeline support for Emacs. If a loaded file doesn't have Emacs
;; file-local variable declarations (the -*- ... -*- thing), this
;; looks for a Vim modeline definition, and applies some of the
;; whitelisted modes in Emacs terms (mostly things to do with
;; whitespace handling). To enable, add to your config:
;;
;; (add-hook 'find-file-hook 'evimodeline-find-file-hook)
;;

(defun evimodeline-find-file-hook ()
  (when (not (evimodeline-has-emacs-modeline))
    (let ((modeline (evimodeline-extract-modeline)))
      (when modeline
        (mapc 'evimodeline-apply-mode modeline)))))

(defun evimodeline-has-emacs-modeline ()
  (save-excursion
    (beginning-of-buffer)
    (search-forward-regexp "-\\*-[^\n]+-\\*-" (point-max) t)))

(defun evimodeline-apply-mode (mode)
  (let ((m (car mode))
        (v (cdr mode)))
    (cond
     ((or (equal m "textwidth") (equal m "tw"))
      (set (make-local-variable 'fill-column) (string-to-number v)))
     ((or (equal m "tabstop") (equal m "ts"))
      (set (make-local-variable 'tab-width) (string-to-number v)))
     ((or (equal m "shiftwidth") (equal m "sw"))
      (set (make-local-variable 'c-basic-offset) (string-to-number v)))
     ((or (equal m "expandtab") (equal m "et"))
      (set (make-local-variable 'indent-tabs-mode) nil))
     ((or (equal m "noexpandtab") (equal m "noet"))
      (set (make-local-variable 'indent-tabs-mode) t))
     ((or (equal m "readonly") (equal m "ro"))
      (toggle-read-only 1))
     (t (message "unknown setting %s" (car mode))))))

(defun evimodeline-extract-modeline ()
  (save-excursion
    (beginning-of-buffer)
    (when (search-forward-regexp "\\(vi\\|vim\\|ex\\):" (point-max) t)
      (skip-chars-forward " \t")
      (let ((modeline
             (if (looking-at "set? ")
                 ; compat-style modeline
                 (progn
                   (forward-char 4)
                   (skip-chars-forward " \t")
                   (let ((start (point)))
                     (end-of-line)
                     (search-backward ":" start t)
                     (buffer-substring-no-properties start (point))))
               ; new-style modeline
               (let ((start (point)))
                 (end-of-line)
                 (buffer-substring-no-properties start (point))))))
        (mapcar 'evimodeline-split-equals
                (split-string modeline ":\\|[ \t]+"))))))

(defun evimodeline-split-equals (s)
  (string-match "\\([^=]+\\)\\(?:=\\(.*\\)\\)?" s)
  (if (or (/= (match-beginning 0) 0)
          (/= (match-end 0) (length s)))
      nil
    (if (match-string 2 s)
        (cons (match-string 1 s) (match-string 2 s))
      (cons s nil))))

(provide 'evimodeline)
