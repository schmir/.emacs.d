;;; early-init.el     -*- lexical-binding: t; -*-

;; emacs 27.1 reads early-init.el first

(setq debug-on-error t)

(defconst my/start-time (current-time))
(when (and (native-comp-available-p)
           (fboundp 'startup-redirect-eln-cache))
  (startup-redirect-eln-cache
   (convert-standard-filename
    (expand-file-name  "var/eln-cache/" user-emacs-directory))))

;; Improves startup time, we reset this later
(defvar default-file-name-handler-alist file-name-handler-alist)

(setq file-name-handler-alist nil
      vc-handled-backends '(Git) ;; need Git for package-vc-install
      gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 1)

(defun my/finish-init ()
  (setq file-name-handler-alist default-file-name-handler-alist
        vc-handled-backends '(Git)
        gc-cons-percentage 0.1
        gc-cons-threshold 100000000)
  (if (fboundp #'gcmh-mode)
      (gcmh-mode 1))
  (garbage-collect)
  (setq debug-on-error nil)
  (let ((m (format "init.el: load time %.06f" (float-time (time-since my/start-time)))))
    (run-with-timer 1.0 nil (lambda () (message m)))))

(add-hook 'emacs-startup-hook #'my/finish-init)

;; increase some internal limits related to elisp execution
(setq load-prefer-newer t
      max-specpdl-size 5000
      max-lisp-eval-depth 6000)

(setq package-enable-at-startup nil)

(setq byte-compile-warnings '(not obsolete)
      warning-suppress-log-types '((comp) (bytecomp))
      native-comp-async-report-warnings-errors 'silent)

;; get rid of visual clutter
(progn
  (setq inhibit-splash-screen t
        initial-scratch-message nil
        frame-inhibit-implied-resize t)
  (if (fboundp 'menu-bar-mode)
      (menu-bar-mode -1))
  (if (fboundp 'tool-bar-mode)
      (tool-bar-mode -1))
  (if (fboundp 'scroll-bar-mode)
      (scroll-bar-mode -1))
  (if (fboundp 'horizontal-scroll-bar-mode)
      (horizontal-scroll-bar-mode -1)))

(modify-all-frames-parameters
 '((width . 130)
   (height . 60)))

;; Disable X resources handling
(advice-add #'x-apply-session-resources :override #'ignore)

(provide 'early-init)
;;; early-init.el ends here
