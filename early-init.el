;;; early-init.el     -*- lexical-binding: t; -*-

;;; Debugging

(setq debug-on-error t)

;;; Startup time measurement

(defun display-startup-time ()
  "Display the startup time and number of garbage collections."
  (message "Emacs init loaded in %.2f seconds (Full emacs-startup: %.2fs) with %d garbage collections."
           (float-time (time-subtract after-init-time before-init-time))
           (time-to-seconds (time-since before-init-time))
           gcs-done))
(add-hook 'emacs-startup-hook #'display-startup-time 100)

;;; Native compilation

(when (and (native-comp-available-p)
           (fboundp 'startup-redirect-eln-cache))
  (startup-redirect-eln-cache
   (convert-standard-filename
    (expand-file-name  "var/eln-cache/" user-emacs-directory))))

;;; Startup performance
;; Improves startup time, we reset this later
(defvar default-file-name-handler-alist file-name-handler-alist)
(set-default-toplevel-value 'file-name-handler-alist nil)
(setq vc-handled-backends '(Git) ;; need Git for package-vc-install
      gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 1.0)

(defun my/finish-init ()
  (set-default-toplevel-value
   'file-name-handler-alist
   ;; Merge instead of overwrite to preserve any changes made since startup.
   (delete-dups (append file-name-handler-alist
                        default-file-name-handler-alist)))

  (setq vc-handled-backends '(Git)
        gc-cons-percentage 0.1
        gc-cons-threshold 100000000)
  (if (fboundp #'gcmh-mode)
      (gcmh-mode 1))
  (garbage-collect)
  (setq debug-on-error nil))

(add-hook 'emacs-startup-hook #'my/finish-init)

;;; Elisp execution limits

(setq load-prefer-newer t
      max-specpdl-size 5000
      max-lisp-eval-depth 6000)

;;; Package initialization

(setq package-enable-at-startup nil)

;;; Suppress warnings

(setq byte-compile-warnings '(not obsolete)
      warning-suppress-log-types '((comp) (bytecomp))
      native-comp-async-report-warnings-errors 'silent)

;;; UI cleanup
;; get rid of visual clutter
(setq inhibit-startup-screen t
      inhibit-startup-echo-area-message user-login-name
      initial-scratch-message nil
      frame-inhibit-implied-resize t)
(dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode horizontal-scroll-bar-mode))
  (when (fboundp mode)
    (funcall mode -1)))

;;; Bidirectional text
;; Disable bidirectional text scanning for a modest performance boost.
(setq-default bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right)

;; Give up some bidirectional functionality for slightly faster re-display.
(setq bidi-inhibit-bpa t)

;;; Frame configuration

(modify-all-frames-parameters
 '((width . 130)
   (height . 56)))

;;; X resources
;; Disable X resources handling
(advice-add #'x-apply-session-resources :override #'ignore)

(provide 'early-init)
;;; early-init.el ends here
