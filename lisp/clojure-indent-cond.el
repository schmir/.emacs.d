;; fix indentation of cond expressions -*- lexical-binding: t -*-
;; see https://github.com/clojure-emacs/clojure-mode/issues/337

(require 'clojure-mode)

;;;###autoload
(defun schmir/indent-cond (indent-point state)
  "Indent cond statement."
  (goto-char (elt state 1))
  (let ((pos -1)
        (base-col (current-column)))
    (forward-char 1)
    ;; `forward-sexp' will error if indent-point is after
    ;; the last sexp in the current sexp.
    (condition-case nil
        (while (and (<= (point) indent-point)
                    (not (eobp)))
          (clojure-forward-logical-sexp 1)
          (cl-incf pos))
      ;; If indent-point is _after_ the last sexp in the
      ;; current sexp, we detect that by catching the
      ;; `scan-error'. In that case, we should return the
      ;; indentation as if there were an extra sexp at point.
      (scan-error (cl-incf pos)))
    (+ base-col (if (cl-evenp pos) 4 2))))

(provide 'clojure-indent-cond)
;;; clojure-indent-cond ends here
