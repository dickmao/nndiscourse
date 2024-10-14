;;; -*- lexical-binding: t; coding: utf-8 -*-

;; (defsubst dir-up (x)
;;   "Replica of f-parent without using f.el.

;; I should just use f.el since ecukes loads it anyway."
;;   (file-name-directory (directory-file-name x)))

;; (let ((root-path (car (last (-iterate 'dir-up load-file-name 4)))))
;;   (add-to-list 'load-path (concat root-path "lisp"))
;;   (add-to-list 'load-path (concat root-path "tests")))

(require 'ecukes)
(require 'espuds)

(add-to-list 'load-path (f-expand "lisp" (ecukes-project-path)))
(add-to-list 'load-path (f-expand "tests" (ecukes-project-path)))

(require 'nndiscourse-test)

(defvar incoming-iteration 0 "Used in filter-args advice of `nndiscourse--incoming'.")

(defmacro if-demote (demote &rest forms)
  (declare (debug t) (indent 1))
  `(if ,demote
       (with-demoted-errors "demoted: %s"
         ,@forms)
     ,@forms))

(defun cleanup ()
  (let ((quick-file (concat (or (bound-and-true-p gnus-newsrc-file)
				(bound-and-true-p gnus-current-startup-file))
			    ".eld")))
    (when (file-exists-p quick-file)
      (message "Deleting %s" quick-file)
      (delete-file quick-file))))

(defun save-log (buffer-or-name file-name)
  "from tkf/emacs-ipython-notebook ein:testing-save-buffer."
  (when (and buffer-or-name (get-buffer buffer-or-name) file-name)
    (with-current-buffer buffer-or-name
      (let ((coding-system-for-write 'raw-text))
        (write-region (point-min) (point-max) file-name)))))

(defvar scenario-recording-alist '((touched nil)))
(defvar scenario-recording-p t)

(Setup
 (add-function
  :around (symbol-function 'nndiscourse-rpc-request)
  (lambda (f server method &rest method-args)
    (let ((sig (intern (mapconcat (apply-partially #'format "%s")
                                  (cons method method-args) "-"))))
      (if scenario-recording-p
          (let ((result (apply f server method method-args)))
            (prog1 result
              (gnus-score-set sig
                              (append (gnus-score-get sig scenario-recording-alist)
                                      (list result))
                              scenario-recording-alist)))
        (let* ((values (gnus-score-get sig scenario-recording-alist))
               (result (pop values)))
          (gnus-score-set sig values scenario-recording-alist)
          (or result (error "nndiscourse-rpc-request: could not playback %s" sig))))))))

(defmacro with-scenario (scenario &rest body)
  (declare (indent defun))
  `(let* ((name (ecukes-scenario-name ,scenario))
          (filename (f-expand (replace-regexp-in-string "\\s-+" "-" name)
                              (f-expand "tests/recordings" (ecukes-project-path)))))
     ,@body))

(Before
 (dolist (server (mapcar #'car nndiscourse-processes))
   (setf (nndiscourse-by-server server :last-scan-time) 0))
 (setq ecukes-reporter-before-scenario-hook
       (lambda (scenario)
         (with-scenario scenario
           (setq scenario-recording-p (not (file-exists-p filename)))
           (setq scenario-recording-alist
                 (if scenario-recording-p
                     '((touched nil))
                   (with-temp-buffer
                     (let ((coding-system-for-read score-mode-coding-system))
                       (insert-file-contents filename))
                     (goto-char (point-min))
                     (read (current-buffer))))))))
 (setq ecukes-reporter-after-scenario-hook
       (lambda (scenario)
         (with-scenario scenario
           (when scenario-recording-p
             (setq scenario-recording-alist
                   (assq-delete-all 'touched scenario-recording-alist))
             (gnus-make-directory (file-name-directory filename))
             (with-temp-buffer
               (gnus-prin1 scenario-recording-alist)
               (let ((coding-system-for-write score-mode-coding-system))
                 (gnus-write-buffer filename)))))
         (setq scenario-recording-alist '((touched nil)))
         (setq scenario-recording-p t))))

(After
 )

(Teardown
 (cleanup)
)

(Fail
 (if noninteractive
     (with-demoted-errors "demote: %s"
       (Teardown))
   (backtrace)
   (keyboard-quit))) ;; useful to prevent emacs from quitting
