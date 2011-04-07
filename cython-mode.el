;; last modified: 2011-04-07 12:09:24 by ralf

(if (not (fboundp 'python-mode))
    (require 'python-mode))

;;;###autoload
(define-derived-mode cython-mode python-mode "Cython"
  (font-lock-add-keywords
   nil
   `((,(concat "\\<\\(NULL"
               "\\|c\\(def\\|har\\|typedef\\)"
               "\\|e\\(num\\|xtern\\)"
	       "\\|float"
	       "\\|cimport"
	       "\\|cppclass"
	       "\\|new"
	       "\\|in\\(clude\\|t\\)"
               "\\|object\\|public\\|struct\\|type\\|union\\|void"
               "\\)\\>")
      1 font-lock-keyword-face t))))

(provide 'cython-mode)
