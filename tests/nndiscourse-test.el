;;; nndiscourse-test.el --- Test utilities for nndiscourse  -*- lexical-binding: t; coding: utf-8 -*-

;; The following is a derivative work of
;; https://github.com/millejoh/emacs-ipython-notebook
;; licensed under GNU General Public License v3.0.

(custom-set-default 'gnus-home-directory (concat default-directory "tests"))
(custom-set-default 'message-directory (concat default-directory "tests/Mail"))
(custom-set-variables
 '(auto-revert-verbose nil)
 '(auto-revert-stop-on-user-input nil)
 '(gnus-batch-mode t)
 '(gnus-use-dribble-file nil)
 '(gnus-read-newsrc-file nil)
 '(gnus-save-killed-list nil)
 '(gnus-save-newsrc-file nil)
 '(gnus-secondary-select-methods (quote ((nndiscourse "meta.discourse.org"))))
 '(gnus-select-method (quote (nnnil)))
 '(gnus-message-highlight-citation nil)
 '(gnus-verbose 8)
 '(gnus-large-ephemeral-newsgroup 4000)
 '(gnus-large-newsgroup 4000)
 '(gnus-interactive-exit (quote quiet)))

(require 'nndiscourse)
(require 'ert)
(require 'message)

(setq ert-runner-profile nil)
(mapc (lambda (key-params)
        (when (string-match-p (car key-params) "nndiscourse")
          (let ((params (cdr key-params)))
            (setq params (assq-delete-all 'gnus-thread-sort-functions params))
            (setcdr key-params params))))
      gnus-parameters)

(defun nndiscourse-test-wait-for (predicate &optional predargs ms interval continue)
  "Wait until PREDICATE function returns non-`nil'.
  PREDARGS is argument list for the PREDICATE function.
  MS is milliseconds to wait.  INTERVAL is polling interval in milliseconds."
  (let* ((int (or interval (if ms (max 300 (/ ms 10)) 300)))
         (count (max 1 (if ms (truncate (/ ms int)) 25))))
    (unless (or (cl-loop repeat count
                         when (apply predicate predargs)
                         return t
                         do (sleep-for (/ int 1000.0)))
                continue)
      (error "Timeout: %s" predicate))))

;; if yes-or-no-p isn't specially overridden, make it always "yes"
(let ((original-yes-or-no-p (symbol-function 'yes-or-no-p)))
  (add-function :around (symbol-function 'message-cancel-news)
                (lambda (f &rest args)
                  (if (not (eq (symbol-function 'yes-or-no-p) original-yes-or-no-p))
                      (apply f args)
                    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _args) t)))
                      (apply f args))))))

(provide 'nndiscourse-test)
