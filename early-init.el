;;; early-init.el     -*- lexical-binding: t; -*-

;; emacs 27.1 reads early-init.el first

(defconst my/start-time (current-time))

(setq gc-cons-threshold most-positive-fixnum) ;; will be reverted with the next hook
(add-hook 'emacs-startup-hook
          `(lambda ()
             (if (fboundp #'gcmh-mode)
                 (gcmh-mode 1)
               (setq gc-cons-threshold 8000000))
             (garbage-collect)
             (message "Load time %.06f"
                      (float-time (time-since my/start-time)))) t)

(setq comp-deferred-compilation t)  ;; asynchrounous native compilation

;; increase some internal limits related to elisp execution
(setq load-prefer-newer t
      max-specpdl-size 5000
      max-lisp-eval-depth 6000)

(setq package-enable-at-startup nil)

(setq byte-compile-warnings '(not obsolete))
(setq warning-suppress-log-types '((comp) (bytecomp)))
(setq native-comp-async-report-warnings-errors 'silent)

;; get rid of visual clutter
(setq inhibit-splash-screen t
      initial-scratch-message nil)

(if (fboundp 'menu-bar-mode)
    (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode)
    (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode)
    (scroll-bar-mode -1))
(if (fboundp 'horizontal-scroll-bar-mode)
    (horizontal-scroll-bar-mode -1))

(defun radian--advice-disable-x-resource-application ()
  "Disable `x-apply-session-resources'.
Now, `x-apply-session-resources' normally gets called before
reading the init-file. However if we do our initialization in the
early init-file, before that function gets called, then it may
override some important things like the cursor color. So we just
disable it, since there's no real reason to respect X
resources.")

(advice-add #'x-apply-session-resources :override
            #'radian--advice-disable-x-resource-application)

(setq straight-check-for-modifications '(check-on-save find-when-checking))

;; bootstrap straight.el
;; https://github.com/raxod502/straight.el/blob/develop/README.md#getting-started
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq default-frame-alist '(;; (fullscreen . maximized)

                            ;; You can turn off scroll bars by uncommenting these lines:
                            ;; (vertical-scroll-bars . nil)
                            ;; (horizontal-scroll-bars . nil)

                            ;; Determine with (face-attribute 'default :background)
                            ;; Setting the face in here prevents flashes of
                            ;; color as the theme gets activated
                            (background-color . "#fbf8ef")
                            (ns-appearance . dark)
                            (ns-transparent-titlebar . t)))


(provide 'early-init)
;;; early-init.el ends here
