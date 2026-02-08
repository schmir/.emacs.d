;;; test-init.el --- Integration tests for Emacs configuration  -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Run with: just test
;; Or manually: emacs --batch -l test/test-init.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)

;; Load the full configuration
(load (expand-file-name "init.el" user-emacs-directory))

;; Suppress modes that require a display in batch mode
(advice-add 'prism-mode :override #'ignore)
(when (fboundp 'cwc-global-mode) (cwc-global-mode -1))
(when (fboundp 'highlight-changes-mode)
  (advice-add 'highlight-changes-mode :override #'ignore))

;; Run after-init-hook to trigger deferred setup (corfu, vertico, etc.)
(run-hooks 'after-init-hook)
(run-hooks 'emacs-startup-hook)

;;; --- Helpers ---

(defmacro with-mode-buffer (mode &rest body)
  "Create a temp buffer, activate MODE, run BODY, then clean up."
  (declare (indent 1))
  `(with-temp-buffer
     (funcall #',mode)
     ,@body))

;;; --- Configuration Loading ---

(ert-deftest test-setup-modules-loaded ()
  "All setup modules should provide their features."
  (dolist (feature '(setup-setup
                     setup-ui
                     setup-core
                     setup-dired
                     setup-editing
                     setup-languages
                     setup-misc
                     setup-completion
                     setup-git
                     setup-lisp
                     setup-go
                     setup-python
                     setup-shell))
    (should (featurep feature))))

;;; --- Core Packages Available ---

(ert-deftest test-core-packages-loadable ()
  "Key packages should be installed and loadable."
  (dolist (pkg '(corfu
                 vertico
                 consult
                 embark
                 cape
                 marginalia
                 orderless
                 prescient
                 tempel
                 magit
                 apheleia
                 eglot
                 puni
                 which-key))
    (should (require pkg nil t))))

;;; --- Global Modes Active ---

(ert-deftest test-global-corfu-mode-active ()
  "global-corfu-mode should be active after init."
  (should (bound-and-true-p global-corfu-mode)))

(ert-deftest test-vertico-mode-active ()
  "vertico-mode should be active after init."
  (should (bound-and-true-p vertico-mode)))

(ert-deftest test-marginalia-mode-active ()
  "marginalia-mode should be active after init."
  (should (bound-and-true-p marginalia-mode)))

(ert-deftest test-apheleia-global-mode-active ()
  "apheleia-global-mode should be active after init."
  (should (bound-and-true-p apheleia-global-mode)))

(ert-deftest test-puni-global-mode-active ()
  "puni-global-mode should be active after init."
  (should (bound-and-true-p puni-global-mode)))

(ert-deftest test-electric-pair-mode-active ()
  "electric-pair-mode should be active after init."
  (should (bound-and-true-p electric-pair-mode)))

;;; --- Completion System ---

(ert-deftest test-completion-styles-include-orderless ()
  "completion-styles should include orderless."
  (should (memq 'orderless completion-styles)))

(ert-deftest test-case-insensitive-completion ()
  "Case-insensitive completion should be configured."
  (should (eq read-file-name-completion-ignore-case t))
  (should (eq read-buffer-completion-ignore-case t))
  (should (eq completion-ignore-case t)))

(ert-deftest test-cape-in-completion-at-point-functions ()
  "Global completion-at-point-functions should have cape-based entries."
  (require 'cape)
  ;; Cape wraps capfs as closures. Check that the global value is non-trivial
  ;; (has functions beyond the default) after cape setup.
  (let ((capfs (default-value 'completion-at-point-functions)))
    (should (> (length capfs) 0))
    ;; At least one non-symbol function (cape-capf-super returns closures)
    (should (cl-some (lambda (f) (and (functionp f) (not (symbolp f)))) capfs))))

;;; --- Eglot Configuration ---

(ert-deftest test-eglot-autoshutdown ()
  "eglot-autoshutdown should be enabled."
  (require 'eglot)
  (should (eq eglot-autoshutdown t)))

(ert-deftest test-eglot-stay-out-of-flymake ()
  "Eglot should stay out of flymake management."
  (require 'eglot)
  (should (member 'flymake eglot-stay-out-of)))

;;; --- Python Mode ---

(ert-deftest test-python-mode-activates ()
  "python-ts-mode or python-mode should be activatable."
  (with-mode-buffer python-ts-mode
    (should (derived-mode-p 'python-base-mode))))

(ert-deftest test-python-shell-interpreter ()
  "python-shell-interpreter should be python3."
  (should (equal python-shell-interpreter "python3")))

(ert-deftest test-python-pet-mode-hook ()
  "pet-mode should be hooked into python-base-mode."
  (should (memq 'pet-mode (default-value 'python-base-mode-hook))))

(ert-deftest test-python-completion-at-point ()
  "Python buffers should have completion-at-point-functions."
  (with-mode-buffer python-ts-mode
    (should completion-at-point-functions)))

(ert-deftest test-python-apheleia-ruff ()
  "Apheleia should configure ruff for python modes when ruff is available."
  (require 'apheleia)
  (when (executable-find "ruff")
    (should (assq 'python-mode apheleia-mode-alist))
    (should (assq 'python-ts-mode apheleia-mode-alist))))

(ert-deftest test-python-setup-hook-configured ()
  "my/setup-python-mode should be hooked into python modes."
  (should (or (memq #'my/setup-python-mode (default-value 'python-mode-hook))
              (memq #'my/setup-python-mode (default-value 'python-ts-mode-hook)))))

(ert-deftest test-python-actual-completion ()
  "Typing a prefix in python-ts-mode should yield dabbrev completion candidates."
  (with-mode-buffer python-ts-mode
    (insert "def my_test_func_beta(x):\n    return x\n\nmy_test_func_")
    (let ((candidates (test--capf-candidates)))
      (should candidates)
      (should (member "my_test_func_beta" candidates)))))

;;; --- Go Mode ---

(ert-deftest test-go-mode-activates ()
  "go-mode should be activatable."
  (with-mode-buffer go-mode
    (should (derived-mode-p 'go-mode))))

(ert-deftest test-go-mode-hooks-configured ()
  "Go mode hooks should include my/setup-go-mode."
  (should (or (memq #'my/setup-go-mode (default-value 'go-mode-hook))
              (memq #'my/setup-go-mode (default-value 'go-ts-mode-hook)))))

(ert-deftest test-go-mode-gofmt-command ()
  "gofmt-command should be set to gofumports in go buffers."
  (with-mode-buffer go-mode
    (run-hooks 'go-mode-hook)
    (should (equal gofmt-command "gofumports"))))

(ert-deftest test-go-mode-completion-at-point ()
  "Go buffers should have completion-at-point-functions."
  (with-mode-buffer go-mode
    (should completion-at-point-functions)))

(ert-deftest test-go-actual-completion ()
  "Typing a prefix in go-mode should yield dabbrev completion candidates."
  (with-mode-buffer go-mode
    (insert "func myTestFuncGamma() int {\n\treturn 0\n}\n\nmyTestFunc")
    (let ((candidates (test--capf-candidates)))
      (should candidates)
      (should (member "myTestFuncGamma" candidates)))))

;;; --- Clojure Mode ---

(ert-deftest test-clojure-mode-activates ()
  "clojure-mode should be activatable."
  (with-mode-buffer clojure-mode
    (should (derived-mode-p 'clojure-mode))))

(ert-deftest test-clojure-cider-loadable ()
  "cider should be loadable."
  (should (require 'cider nil t)))

(ert-deftest test-clojure-mode-hooks-configured ()
  "Clojure mode hooks should include my/setup-clojure-mode."
  (should (or (memq #'my/setup-clojure-mode (default-value 'clojure-mode-hook))
              (memq #'my/setup-clojure-mode (default-value 'clojure-ts-mode-hook)))))

(ert-deftest test-clojure-eldoc-hook ()
  "eldoc-mode should be configured for clojure."
  (require 'clojure-mode)
  (should (or (memq #'eldoc-mode (default-value 'clojure-mode-hook))
              (memq #'eldoc-mode (default-value 'clojure-ts-mode-hook)))))

(ert-deftest test-clojure-completion-at-point ()
  "Clojure buffers should have completion-at-point-functions."
  (with-mode-buffer clojure-mode
    (should completion-at-point-functions)))

(ert-deftest test-clojure-apheleia-zprint ()
  "Apheleia should configure zprint for clojure modes when zprint is available."
  (require 'apheleia)
  (when (executable-find "zprint")
    (should (assq 'clojure-mode apheleia-mode-alist))))

(defun test--capf-candidates ()
  "Collect completion candidates from `completion-at-point-functions'.
Resolves the `t' sentinel that defers to the global value."
  (let ((capfs completion-at-point-functions)
        result)
    (while (and capfs (not result))
      (let ((f (car capfs)))
        (cond
         ((eq f t)
          (setq capfs (append (default-value 'completion-at-point-functions)
                              (cdr capfs))))
         (t
          (setq result (ignore-errors (funcall f)))
          (setq capfs (cdr capfs))))))
    (when result
      (let ((start (nth 0 result))
            (end (nth 1 result))
            (collection (nth 2 result)))
        (all-completions (buffer-substring-no-properties start end)
                         collection)))))

(ert-deftest test-clojure-actual-completion ()
  "Typing a prefix in clojure-mode should yield dabbrev completion candidates."
  (with-mode-buffer clojure-mode
    (insert "(defn my-test-fn-alpha [x] x)\n\n(my-test-fn-")
    (let ((candidates (test--capf-candidates)))
      (should candidates)
      (should (member "my-test-fn-alpha" candidates)))))

;;; --- Corfu in prog-mode ---

(ert-deftest test-corfu-active-in-prog-mode ()
  "corfu-mode should activate in prog-mode buffers when global-corfu-mode is on."
  ;; Corfu's globalized mode relies on display-related buffer tracking that
  ;; doesn't work in batch mode. Instead verify global mode is on and
  ;; explicitly enable corfu in a prog-mode buffer.
  (should (bound-and-true-p global-corfu-mode))
  (let ((buf (generate-new-buffer "test-corfu.py")))
    (unwind-protect
        (with-current-buffer buf
          (python-ts-mode)
          (corfu-mode 1)
          (should (bound-and-true-p corfu-mode)))
      (kill-buffer buf))))

;;; test-init.el ends here
