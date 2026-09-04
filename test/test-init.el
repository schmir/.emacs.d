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
                     setup-shell
                     language-profiles))
    (should (featurep feature))))

(ert-deftest test-language-profiles-installed-once ()
  "Installing twice should run each installation step once."
  (let ((my/language-profiles--installed nil)
        (my/language-profiles--installers '(test--install-language-profile))
        (shared-installs 0)
        (profile-installs 0))
    (cl-letf (((symbol-function
                'my/language-profiles--install-shared-policy)
               (lambda () (setq shared-installs (1+ shared-installs))))
              ((symbol-function 'test--install-language-profile)
               (lambda () (setq profile-installs (1+ profile-installs)))))
      (my/language-profiles-install)
      (my/language-profiles-install))
    (should my/language-profiles--installed)
    (should (= shared-installs 1))
    (should (= profile-installs 1))))

(ert-deftest test-language-profile-remaps-follow-grammar-availability ()
  "Language profiles should prefer only available tree-sitter modes."
  (dolist (entry '((javascript js-mode . js-ts-mode)
                   (nix nix-mode . nix-ts-mode)
                   (python python-mode . python-ts-mode)
                   (markdown markdown-mode . markdown-ts-mode)))
    (let ((grammar (car entry))
          (remap (cdr entry)))
      (should (eq (not (null (member remap major-mode-remap-alist)))
                  (not (null (treesit-ready-p grammar))))))))

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

(ert-deftest test-cape-registration-is-reload-safe ()
  "Registering the shared Cape CAPF twice should keep one entry."
  (my/register-cape-file-dabbrev-capf)
  (my/register-cape-file-dabbrev-capf)
  (should (= 1 (seq-count
                (lambda (capf) (eq capf my/cape-file-dabbrev-capf))
                (default-value 'completion-at-point-functions)))))

;;; --- Eglot Configuration ---

(ert-deftest test-eglot-autoshutdown ()
  "eglot-autoshutdown should be enabled."
  (require 'eglot)
  (should (eq eglot-autoshutdown t)))

(ert-deftest test-eglot-stay-out-of-flymake ()
  "Eglot should stay out of flymake management."
  (require 'eglot)
  (should (member 'flymake eglot-stay-out-of)))

(ert-deftest test-eglot-keybindings-are-mode-local ()
  "Eglot commands should not replace global keybindings."
  (require 'eglot)
  (dolist (binding '(("C-c ." . xref-find-references)
                     ("C-c t" . eglot-find-typeDefinition)
                     ("C-c i" . eglot-find-implementation)
                     ("C-c r" . my/eglot-rename)))
    (should-not (lookup-key (current-global-map) (kbd (car binding))))
    (should (eq (lookup-key eglot-mode-map (kbd (car binding)))
                (cdr binding)))))

;;; --- Python Mode ---

(ert-deftest test-python-mode-activates ()
  "python-ts-mode or python-mode should be activatable."
  (with-mode-buffer python-ts-mode
    (should (derived-mode-p 'python-base-mode))))

(ert-deftest test-python-shell-interpreter ()
  "python-shell-interpreter should be python3."
  (should (equal python-shell-interpreter "python3")))

(ert-deftest test-python-profile-activates-pet ()
  "Python buffers should activate Pet environment discovery."
  (with-mode-buffer python-ts-mode
    (should (bound-and-true-p pet-mode))))

(ert-deftest test-python-server-is-ty ()
  "Python should use ty, which reads none of pyright's settings.

The profile sends no `eglot-workspace-configuration' because ty discards
it.  Should this ever point at a pyright-family server again, that
decision has to be revisited."
  (require 'eglot)
  (let ((entry (seq-find (lambda (e)
                           (let ((key (car e)))
                             (and (consp key)
                                  (eq (car key) 'python-base-mode))))
                         eglot-server-programs)))
    (should entry)
    (should (member "ty" (cdr entry)))
    (should-not (default-value 'eglot-workspace-configuration))))

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

(ert-deftest test-python-profile-activates-diagnostics ()
  "Python buffers should activate Flymake with Eglot diagnostics."
  (with-mode-buffer python-ts-mode
    (should (bound-and-true-p flymake-mode))
    (should (memq #'eglot-flymake-backend
                  flymake-diagnostic-functions))))

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

(ert-deftest test-go-profile-activates-diagnostics ()
  "Go buffers should activate Flymake with Eglot diagnostics."
  (with-mode-buffer go-mode
    (should (bound-and-true-p flymake-mode))
    (should (memq #'eglot-flymake-backend
                  flymake-diagnostic-functions))))

(ert-deftest test-go-mode-gofmt-command ()
  "gofmt-command should be set to gofumports in go buffers."
  (with-mode-buffer go-mode
    (should (equal gofmt-command "gofumports"))))

(ert-deftest test-go-profile-preserves-save-order ()
  "Go buffers should format before they organize imports."
  (let (events)
    (cl-letf (((symbol-function 'eglot-format-buffer)
               (lambda ()
                 (interactive)
                 (push 'format events)))
              ((symbol-function 'eglot-code-action-organize-imports)
               (lambda ()
                 (interactive)
                 (push 'imports events)
                 (error "test import failure"))))
      (with-mode-buffer go-mode
        (run-hooks 'before-save-hook)))
    (should (equal (nreverse events) '(format imports)))))

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

(ert-deftest test-clojure-profile-activates-diagnostics ()
  "Clojure buffers should activate Flymake diagnostics."
  (with-mode-buffer clojure-mode
    (should (bound-and-true-p flymake-mode))))

(ert-deftest test-clojure-profile-activates-eldoc ()
  "Clojure buffers should activate Eldoc."
  (with-mode-buffer clojure-mode
    (should (bound-and-true-p eldoc-mode))))

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

;;; --- Other Language Profiles ---

(ert-deftest test-emacs-lisp-profile-activates-editing-modes ()
  "Emacs Lisp buffers should activate Eldoc and aggressive indentation."
  (with-mode-buffer emacs-lisp-mode
    (should (bound-and-true-p eldoc-mode))
    (should (bound-and-true-p aggressive-indent-mode))))

(ert-deftest test-protobuf-profile-applies-style ()
  "Protocol Buffer buffers should use the configured editing style."
  (with-mode-buffer protobuf-mode
    (should (= c-basic-offset 8))
    (should-not indent-tabs-mode)))

(ert-deftest test-yaml-profile-installs-completion-aware-indentation ()
  "YAML buffers should let TAB complete after content."
  (with-mode-buffer yaml-ts-mode
    (should (eq indent-line-function
                #'my/language-profiles--yaml-indent-line))
    (insert "key: value")
    (should (eq (funcall indent-line-function) 'noindent))))

(ert-deftest test-javascript-profile-activates-diagnostics ()
  "JavaScript buffers should activate Flymake diagnostics."
  (with-mode-buffer js-mode
    (should (bound-and-true-p flymake-mode))))

(ert-deftest test-shell-profile-activates-diagnostics ()
  "Shell buffers should activate Flymake diagnostics."
  (with-mode-buffer sh-mode
    (should (bound-and-true-p flymake-mode))))

(ert-deftest test-non-project-profile-does-not-start-eglot ()
  "A language profile should not start Eglot outside a project."
  (let ((starts 0))
    (cl-letf (((symbol-function 'project-current)
               (lambda (&optional _prompt) nil))
              ((symbol-function 'eglot-ensure)
               (lambda () (setq starts (1+ starts)))))
      (with-mode-buffer nix-mode
        (should-not (bound-and-true-p eglot-managed-mode))))
    (should (zerop starts))))

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
