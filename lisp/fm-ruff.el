;;; fm-ruff.el --- Flymake backend for python using ruff with json output  -*- lexical-binding: t -*-

;; Author: Ralf Schmitt
;; Version: 0.0.1

;; Package-Requires: ((emacs "28.1") (seq "2.24"))

;; Homepage: https://github.com/schmir/fm-ruff

;; This file is not part of GNU Emacs

;; SPDX-License-Identifier: GPL-3.0-or-later

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Modelled after https://www.gnu.org/software/emacs/manual/html_node/flymake/An-annotated-example-backend.html

;;; Code:

(require 'seq)
(defvar-local fm-ruff--flymake-proc nil)

;;;###autoload
(defun fm-ruff-flymake (report-fn &rest _args)

  ;; Not having a ruff interpreter is a serious problem which should cause
  ;; the backend to disable itself, so an error is signaled.
  ;;
  (unless (executable-find "ruff")
    (error "Cannot find a suitable ruff"))
  (unless (json-available-p)
    (error "json support missing in emacs"))
  ;; If a live process launched in an earlier check was found, that
  ;; process is killed.  When that process's sentinel eventually runs,
  ;; it will notice its obsoletion, since it have since reset
  ;; `fm-ruff--flymake-proc' to a different value
  ;;
  (when (process-live-p fm-ruff--flymake-proc)
    (kill-process fm-ruff--flymake-proc))


  ;; Save the current buffer, the narrowing restriction, remove any
  ;; narrowing restriction.
  ;;
  (let ((source (current-buffer))
        (filename (or (buffer-file-name) "stdin")))
    (save-restriction
      (widen)
      ;; Reset the `fm-ruff--flymake-proc' process to a new process
      ;; calling the ruff tool.
      ;;
      (setq
       fm-ruff--flymake-proc
       (make-process
        :name "fm-ruff" :noquery t :connection-type 'pipe
        ;; Make output go to a temporary buffer.
        ;;
        :buffer (generate-new-buffer " *fm-ruff*")
        :command `("ruff" "check" "--quiet" "--output-format=json"
                   ,(format "--stdin-filename=%s" filename)
                   "-")
        :sentinel
        (lambda (proc _event)
          ;; Check that the process has indeed exited, as it might
          ;; be simply suspended.
          ;;
          (when (memq (process-status proc) '(exit signal))
            (unwind-protect
                ;; Only proceed if `proc' is the same as
                ;; `fm-ruff--flymake-proc', which indicates that
                ;; `proc' is not an obsolete process.
                ;;

                (if (with-current-buffer source (eq proc fm-ruff--flymake-proc))
                    (with-current-buffer (process-buffer proc)
                      (goto-char (point-min))
                      (let* ((warnings (json-parse-buffer))
                             (res (seq-map (lambda (w)
                                             (flymake-make-diagnostic source
                                                                      (cons (gethash "row" (gethash "location" w))
                                                                            (gethash "column" (gethash "location" w)))
                                                                      (cons (gethash "row" (gethash "end_location" w))
                                                                            (gethash "column" (gethash "end_location" w)))
                                                                      :error
                                                                      (format "ruff: %s %s" (gethash "code" w) (gethash "message" w))))
                                           warnings)))
                        (funcall report-fn res)))
                  (flymake-log :warning "Canceling obsolete check %s"
                               proc))
              ;; Cleanup the temporary buffer used to hold the
              ;; check's output.
              ;;
              (kill-buffer (process-buffer proc)))))))
      ;; Send the buffer contents to the process's stdin, followed by
      ;; an EOF.
      ;;
      (process-send-region fm-ruff--flymake-proc (point-min) (point-max))
      (process-send-eof fm-ruff--flymake-proc))))


;;;###autoload
(defun fm-ruff-setup ()
  (interactive)
  (when (or t (derived-mode-p 'python-base-mode))
    (add-hook 'flymake-diagnostic-functions #'fm-ruff-flymake nil t)))


(provide 'fm-ruff)
;;; fm-ruff.el ends here
