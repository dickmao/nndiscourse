;;; nndiscourse.el --- Gnus backend for Discourse  -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2019 The Authors of nndiscourse.el

;; Authors: dickmao <github id: dickmao>
;; Version: 0.1.0
;; Keywords: news
;; URL: https://github.com/dickmao/nndiscourse
;; Package-Requires: ((emacs "25.1") (dash "2.16") (dash-functional "1.2.0") (anaphora "1.0.4") (rbenv "0.0.3"))

;; This file is NOT part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with nndiscourse.el.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; A Gnus backend for Discourse.

;;; Code:

(require 'nnoo)
(require 'gnus)
(require 'gnus-start)
(require 'gnus-art)
(require 'gnus-sum)
(require 'gnus-msg)
(require 'gnus-cite)
(require 'gnus-srvr)
(require 'gnus-cache)
(require 'gnus-bcklg)
(require 'gnus-score)
(require 'mm-url)
(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'dash)
(require 'dash-functional)
(require 'anaphora)
(require 'json-rpc)
(require 'rbenv)

(nnoo-declare nndiscourse)

(nnoo-define-basics nndiscourse)

(defvoo nndiscourse-scheme "https"
  "URI scheme for address.")

(defvoo nndiscourse-charset "utf-8"
  "Charset for address.")

(defcustom nndiscourse-render-post t
  "If non-nil, follow link upon `gnus-summary-select-article'.
Otherwise, just display link."
  :type 'boolean
  :group 'nndiscourse)

(defcustom nndiscourse-public-keyfile (expand-file-name "~/.ssh/id_rsa.pub")
  "Location of rsa private key."
  :type '(file :must-match t)
  :group 'nndiscourse)

(defcustom nndiscourse-localhost "127.0.0.1"
  "Some users keep their browser in a separate domain."
  :type 'string
  :group 'nndiscourse)

(defvar nndiscourse-status-string "" "Out-of-band message.")

(defvar nndiscourse--last-id nil "Keep track of where we are.")

(defvar nndiscourse--debug-request-posts nil "Keep track of ids to re-request for testing.")

(defvar nndiscourse--last-scan-time (- (truncate (float-time)) 100)
  "Don't scan more than once every few seconds.")

(defmacro nndiscourse--callback (result &optional callback)
  "Set RESULT to return value of CALLBACK."
  `(cl-function (lambda (&rest args &key data &allow-other-keys)
                  (setq ,result (if ,callback
                                    (apply ,callback args)
                                  data)))))

(cl-defstruct (nndiscourse-proc-info)
  "port and elisp process"
  port process)

(defvar nndiscourse-processes nil
  "Association list of ( server-name-qua-url . nndiscourse-proc-info ).")

(defsubst nndiscourse--gethash (string hashtable &optional dflt)
  "Get corresponding value of STRING from HASHTABLE, or DFLT if undefined.

Starting in emacs-src commit c1b63af, Gnus moved from obarrays to normal hashtables."
  (declare (indent defun))
  (unless (stringp string)
    (setq string (format "%s" string)))
  (if (fboundp 'gnus-gethash)
      (let ((sym (intern-soft string hashtable)))
        (if (or (null sym) (not (boundp sym))) dflt (symbol-value sym)))
    (gethash string hashtable dflt)))

(defsubst nndiscourse-good-server (server)
  "SERVER needs to be a non-zero length string."
  (or (and (stringp server) (not (zerop (length server))))
      (prog1 nil (backtrace))))

(defsubst nndiscourse--replace-hash (string func hashtable)
  "Set value of STRING to FUNC applied to existing STRING value in HASHTABLE.

Starting in emacs-src commit c1b63af, Gnus moved from obarrays to normal hashtables."
  (declare (indent defun))
  (unless (stringp string)
    (setq string (prin1-to-string string)))
  (let* ((capture (nndiscourse--gethash string hashtable))
         (replace-with (funcall func capture)))
    (if (fboundp 'gnus-sethash)
       (set (intern string hashtable) replace-with)
      (puthash string replace-with hashtable))))

(defmacro nndiscourse--sethash (string value hashtable)
  "Set corresponding value of STRING to VALUE in HASHTABLE.

Starting in emacs-src commit c1b63af, Gnus moved from obarrays to normal hashtables."
  (declare (indent defun))
  `(,(if (fboundp 'gnus-sethash)
         'gnus-sethash
       'puthash)
    (format "%s" ,string) ,value ,hashtable))

(defmacro nndiscourse--maphash (func table)
  "Map FUNC taking key and value over TABLE, return nil.

Starting in emacs-src commit c1b63af, Gnus moved from obarrays to normal hashtables."
  (declare (indent nil))
  `(,(if (fboundp 'gnus-gethash-safe)
         'mapatoms
       'maphash)
    ,(if (fboundp 'gnus-gethash-safe)
         `(lambda (k) (funcall
                       (apply-partially
                        ,func
                        (symbol-name k) (gnus-gethash-safe k ,table))))
       func)
    ,table))

(defvar nndiscourse-summary-voting-map
  (let ((map (make-sparse-keymap)))
    map)
  "Voting map.")

(defvar nndiscourse-summary-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "r" 'gnus-summary-followup)
    (define-prefix-command 'nndiscourse-summary-voting-map)
    (define-key map "R" 'nndiscourse-summary-voting-map)
    (define-key nndiscourse-summary-voting-map "0" 'nndiscourse-novote)
    (define-key nndiscourse-summary-voting-map "-" 'nndiscourse-downvote)
    (define-key nndiscourse-summary-voting-map "=" 'nndiscourse-upvote)
    (define-key nndiscourse-summary-voting-map "+" 'nndiscourse-upvote)
    map))

(defvar nndiscourse-article-mode-map
  (copy-keymap nndiscourse-summary-mode-map)) ;; how does Gnus do this?

(define-minor-mode nndiscourse-article-mode
  "Minor mode for nndiscourse articles.  Disallow `gnus-article-reply-with-original'.

\\{gnus-article-mode-map}
"
  :lighter " Discourse"
  :keymap nndiscourse-article-mode-map)

(define-minor-mode nndiscourse-summary-mode
  "Disallow \"reply\" commands in `gnus-summary-mode-map'.

\\{nndiscourse-summary-mode-map}
"
  :lighter " Discourse"
  :keymap nndiscourse-summary-mode-map)

(defsubst nndiscourse--server-buffer-name (server)
  "Arbitrary proc buffer name for SERVER."
  (when (nndiscourse-good-server server)
    (format " *%s*" server)))

(defsubst nndiscourse--server-buffer (server &optional create)
  "Get proc buffer for SERVER.  Create if necessary if CREATE."
  (when (nndiscourse-good-server server)
    (let ((name (nndiscourse--server-buffer-name server)))
      (if create
          (get-buffer-create name)
        (get-buffer name)))))

(defvar-local nndiscourse--headers-hashtb (gnus-make-hashtable)
  "Group -> headers.  Buffer-local to individual servers' proc buffer.")

(defsubst nndiscourse-get-headers (server group)
  "List headers for SERVER GROUP."
  (and (buffer-live-p (nndiscourse--server-buffer server))
       (with-current-buffer (nndiscourse--server-buffer server)
         (nndiscourse--gethash group nndiscourse--headers-hashtb))))

(defmacro nndiscourse-set-headers (server group new-headers)
  "Assign headers for SERVER GROUP to NEW-HEADERS."
  (declare (indent defun))
  `(with-current-buffer (nndiscourse--server-buffer ,server)
     (nndiscourse--sethash ,group ,new-headers nndiscourse--headers-hashtb)))

(defvar-local nndiscourse--refs-hashtb (gnus-make-hashtable)
  "Id -> parent.  Buffer-local to individual servers' proc buffer.")

(defun nndiscourse-get-ref (server id)
  "Amongst SERVER refs, return list of descending ancestors for ID."
  (declare (indent defun))
  (with-current-buffer (nndiscourse--server-buffer server)
    (cl-loop for prev-id = id then cur-id
             for cur-id = (nndiscourse--gethash prev-id nndiscourse--refs-hashtb)
             until (not cur-id)
             collect cur-id into rresult
             finally return (nreverse rresult))))

(defmacro nndiscourse-set-ref (server id parent-id)
  "Amongst SERVER refs, associate ID to PARENT-ID."
  `(with-current-buffer (nndiscourse--server-buffer ,server)
     (nndiscourse--sethash ,id ,parent-id nndiscourse--refs-hashtb)))

(defvar-local nndiscourse--categories-hashtb (gnus-make-hashtable)
  "Category id -> group.  Buffer-local to individual servers' proc buffer.")

(defun nndiscourse-get-category (server category-id)
  "Amongst SERVER categories, return group for CATEGORY-ID."
  (declare (indent defun))
  (with-current-buffer (nndiscourse--server-buffer server)
    (nndiscourse--gethash (format "%s" category-id) nndiscourse--categories-hashtb)))

(defmacro nndiscourse-set-category (server category-id group)
  "Amongst SERVER categories, associate CATEGORY-ID to GROUP."
  (declare (indent defun))
  `(with-current-buffer (nndiscourse--server-buffer ,server)
     (nndiscourse--sethash ,category-id ,group nndiscourse--categories-hashtb)))

(defmacro nndiscourse--with-mutex (mtx &rest body)
  "If capable of threading, lock with MTX and execute BODY."
  (declare (indent defun))
  (if (fboundp 'with-mutex)
      `(with-mutex ,mtx ,@body)
    `(progn ,@body)))

(defvar nndiscourse--mutex-rpc-request (when (fboundp 'make-mutex)
                                         (make-mutex "nndiscourse--mutex-rpc-request"))
  "Only one jsonrpc output buffer, so avoid two requests using at the same time.")

(declare-function set-process-thread "process" t t) ;; emacs-25

(defun nndiscourse-rpc-request (server method &rest args)
  "Make jsonrpc call to SERVER invoking METHOD ARGS.

nnreddit had just one jsonrpyc process using stdio pipe for IPC.
jsonrpyc could not assume HTTP.

The jimson library does assume HTTP, so we follow `json-rpc' SOP.
This means two processes, one jimson process, which we administer,
and one `json-rpc' network pipe which json-rpc.el administers.

Process stays the same, but the `json-rpc' connection (a cheap struct) gets
reinstantiated with every call.

Return response of METHOD ARGS of type `json-object-type' or nil if failure."
  (when (and (nndiscourse-good-server server) (nndiscourse-server-opened server))
    (condition-case err
        (let* ((port (nndiscourse-proc-info-port
                      (cdr (assoc server nndiscourse-processes))))
               (connection (json-rpc-connect nndiscourse-localhost port)))
          (when-let ((threads-p (fboundp 'set-process-thread))
                     (proc (json-rpc-process connection)))
            (set-process-thread proc nil))
          (nndiscourse--with-mutex nndiscourse--mutex-rpc-request
            (gnus-message 7 "nndiscourse-rpc-request: send %s %s" method
                          (mapconcat (lambda (s) (format "%s" s)) args " "))
            (json-rpc connection method args)))
      (error (prog1 nil
               (gnus-message 3 "nndiscourse-rpc-request: %s" (error-message-string err)))))))

(defsubst nndiscourse--gate (&optional group)
  "Apply our minor modes only when the following conditions hold for GROUP."
  (unless group
    (setq group gnus-newsgroup-name))
  (and (stringp group)
       (listp (gnus-group-method group))
       (eq 'nndiscourse (car (gnus-group-method group)))))

(deffoo nndiscourse-request-close ()
  (nndiscourse-close-server)
  t)

(deffoo nndiscourse-request-type (_group &optional _article)
  'news)

(deffoo nndiscourse-server-opened (&optional server)
  (nndiscourse--server-buffer server))

(deffoo nndiscourse-status-message (&optional _server)
  "")

(defun nndiscourse--initialize ()
  "Run `bundle install` if necessary."
  (let ((default-directory
          (expand-file-name "nndiscourse"
                            (file-name-directory
                             (or (locate-library "nndiscourse")
                                 default-directory))))
        (bundle-exec (executable-find "bundle")))
    (unless bundle-exec
      (error "`nndiscourse--initialize': nndiscourse requires bundler"))
    (unless (file-exists-p (expand-file-name "vendor"))
      (let ((bundle-buffer (get-buffer-create "*nndiscourse: bundle install*")))
        (if (zerop (apply #'call-process bundle-exec nil
                          (cons bundle-buffer (list t))
                          nil (split-string "install --deployment --without development")))
            (kill-buffer bundle-buffer)
          (switch-to-buffer bundle-buffer)
          (error "`nndiscourse--initialize': bundle install failed"))))))

(deffoo nndiscourse-open-server (server &optional defs)
  "Retrieve the Jimson process for SERVER.

I am counting on `gnus-check-server` in `gnus-read-active-file-1' in
`gnus-get-unread-articles' to open server upon install."
  (when (nndiscourse-good-server server)
    (let ((original-global-rbenv-mode global-rbenv-mode))
      (unless global-rbenv-mode
        (let (rbenv-show-active-ruby-in-modeline)
          (global-rbenv-mode)))
      (unwind-protect
          (progn
            (when defs ;; defs should be non-nil when called from `gnus-open-server'
              (nndiscourse--initialize))
            (nnoo-change-server 'nndiscourse server defs)
            (let* ((proc-buf (nndiscourse--server-buffer server t))
                   (proc (get-buffer-process proc-buf)))
              (if (process-live-p proc)
                  proc
                (let* ((free-port (with-temp-buffer
                                    (let ((proc (make-network-process
                                                 :name "free-port"
                                                 :noquery t
                                                 :host nndiscourse-localhost
                                                 :buffer (current-buffer)
                                                 :server t
                                                 :stop t
                                                 :service t)))
                                      (prog1 (process-contact proc :service)
                                        (delete-process proc)))))
                       (ruby-command (split-string (format "%s exec thor cli:serve %s://%s -p %s"
                                                           (executable-find "bundle")
                                                           nndiscourse-scheme
                                                           server
                                                           free-port)))
                       (stderr-buffer (get-buffer-create (format " *%s-stderr*" server))))
                  (with-current-buffer stderr-buffer
                    (add-hook 'after-change-functions
                              (apply-partially 'nndiscourse--message-user server)
                              nil t))
                  (nndiscourse-register-process
                    free-port
                    (let ((default-directory
                            (expand-file-name "nndiscourse"
                                              (file-name-directory
                                               (or (locate-library "nndiscourse")
                                                   default-directory)))))
                      (let ((new-proc (make-process :name server
                                                    :buffer proc-buf
                                                    :command ruby-command
                                                    :noquery t
                                                    :sentinel #'nndiscourse-sentinel
                                                    :stderr stderr-buffer)))
                        (cl-loop repeat 10
                                 until (condition-case nil
                                           (prog1 t
                                             (delete-process
                                              (make-network-process :name "test-port"
                                                                    :noquery t
                                                                    :host nndiscourse-localhost
                                                                    :service free-port
                                                                    :buffer nil
                                                                    :stop t)))
                                         (file-error nil))
                                 do (accept-process-output new-proc 0.3))
                        new-proc)))))))
        (unless original-global-rbenv-mode
          (global-rbenv-mode -1))))))

(defun nndiscourse-alist-get (key alist &optional default remove testfn)
  "Replicated library function for emacs-25.

Same argument meanings for KEY ALIST DEFAULT REMOVE and TESTFN."
  (ignore remove)
  (let ((x (if (not testfn)
               (assq key alist)
             (assoc key alist))))
    (if x (cdr x) default)))

(gv-define-expander nndiscourse-alist-get
  (lambda (do key alist &optional default remove testfn)
    (macroexp-let2 macroexp-copyable-p k key
      (gv-letplace (getter setter) alist
        (macroexp-let2 nil p `(if (and ,testfn (not (eq ,testfn 'eq)))
                                  (assoc ,k ,getter)
                                (assq ,k ,getter))
          (funcall do (if (null default) `(cdr ,p)
                        `(if ,p (cdr ,p) ,default))
                   (lambda (v)
                     (macroexp-let2 nil v v
                       (let ((set-exp
                              `(if ,p (setcdr ,p ,v)
                                 ,(funcall setter
                                           `(cons (setq ,p (cons ,k ,v))
                                                  ,getter)))))
                         `(progn
                            ,(cond
                             ((null remove) set-exp)
                             ((or (eql v default)
                                  (and (eq (car-safe v) 'quote)
                                       (eq (car-safe default) 'quote)
                                       (eql (cadr v) (cadr default))))
                              `(if ,p ,(funcall setter `(delq ,p ,getter))))
                             (t
                              `(cond
                                ((not (eql ,default ,v)) ,set-exp)
                                (,p ,(funcall setter
                                              `(delq ,p ,getter))))))
                            ,v))))))))))

(defun nndiscourse-register-process (port proc)
  "Register PORT and PROC with a server-name-qua-url.
Return PROC if success, nil otherwise."
  (declare (indent defun))
  (nndiscourse-deregister-process (process-name proc))
  (if (process-live-p proc)
      (prog1 proc
        (gnus-message 5 "nndiscourse-register-process: registering %s"
                      (process-name proc))
        (setf (nndiscourse-alist-get (process-name proc) nndiscourse-processes
                                     nil nil #'equal)
              (make-nndiscourse-proc-info :port port :process proc)))
    (prog1 nil
      (gnus-message 3 "`nndiscourse-register-process': dead process %s"
                    (process-name proc))
      (nndiscourse-deregister-process (process-name proc)))))

(defun nndiscourse-deregister-process (server)
  "Disavow any knowledge of SERVER's process."
  (awhen (nndiscourse-alist-get server nndiscourse-processes nil nil #'equal)
    (let ((proc (nndiscourse-proc-info-process it)))
      (gnus-message 5 "`nndiscourse-deregister-process': deregistering %s %s pid=%s"
                    server (process-name proc) (process-id proc))
      (delete-process proc)))
  (setf (nndiscourse-alist-get server nndiscourse-processes nil nil #'equal) nil))

(deffoo nndiscourse-close-server (&optional server _defs)
  "Patterning after nnimap.el."
  (awhen (nndiscourse--server-buffer server)
    (kill-buffer it))
  (when (nnoo-change-server 'nndiscourse server nil)
    (nnoo-close-server 'nndiscourse server)
    t))

(deffoo nndiscourse-close-group (_group &optional _server)
  t)

(defmacro nndiscourse--with-group (server group &rest body)
  "If `gnus-newsgroup-name' is null, recreate it based on SERVER.
Disambiguate GROUP if it's empty.
Then execute BODY."
  (declare (debug (form &rest form))
           (indent defun))
  `(let* ((server (or ,server (nnoo-current-server 'nndiscourse)))
          (group (or ,group (gnus-group-real-name gnus-newsgroup-name)))
          (gnus-newsgroup-name (or gnus-newsgroup-name
                                   (gnus-group-full-name
                                    group (cons 'nndiscourse (list server))))))
     ,@body))

(defsubst nndiscourse--first-article-number (server group)
  "Get article-number qua id of first article of SERVER GROUP."
  (plist-get (car (nndiscourse-get-headers server group)) :id))

(defsubst nndiscourse--last-article-number (server group)
  "Get article-number qua id of last article of SERVER GROUP."
  (plist-get (car (last (nndiscourse-get-headers server group))) :id))

(defun nndiscourse--get-header (server group article-number)
  "Amongst SERVER GROUP headers, binary search ARTICLE-NUMBER."
  (declare (indent defun))
  (let ((headers (nndiscourse-get-headers server group)))
    (cl-flet ((id-of (k) (plist-get (elt headers k) :id)))
      (cl-do* ((x article-number)
               (l 0 (if dir (1+ m) l))
               (r (length headers) (if dir r m))
               (m (/ (- r l) 2) (+ m (* (if dir 1 -1) (max 1 (/ (- r l) 2)))))
               (dir (> x (id-of m)) (> x (id-of m))))
          ((or (<= (- r l) 1) (= x (id-of m)))
           (and (< m (length headers)) (>= m 0) (= x (id-of m)) (elt headers m)))))))

(defun nndiscourse--massage (body)
  "Precede each quoted line of BODY broken by `shr-fill-line' with '>'."
  (with-temp-buffer
    (insert body)
    (mm-url-decode-entities)
    (cl-loop initially (goto-char (point-min))
             until (and (null (re-search-forward "\\(^>\\( .*?\\)\\)<p>" nil t))
                        (null (re-search-forward "\\(<p>\\s-*>\\( .*?\\)\\)<p>" nil t)))
             do (let* ((start (match-beginning 1))
                       (end (match-end 1))
                       (matched (match-string 2)))
                  (perform-replace
                   ".*"
                   (concat "<p>\n"
                           (with-temp-buffer
                             (insert matched)
                             (fill-region (point-min) (point-max))
                             (insert
                              (prog1
                                  (cl-subseq (replace-regexp-in-string
                                              "\n" "<br>\n> " (concat "\n" (buffer-string)))
                                             5)
                                (erase-buffer)))
                             (buffer-string))
                           "\n")
                   nil t nil nil nil start end)))
    (buffer-string)))

(defsubst nndiscourse--citation-wrap (author body)
  "Cite AUTHOR using `gnus-message-cite-prefix-regexp' before displaying BODY.

Originally written by Paul Issartel."
  (with-temp-buffer
    (insert body)
    (mm-url-remove-markup)
    (mm-url-decode-entities)
    (fill-region (point-min) (point-max))
    (let* ((trimmed-1 (replace-regexp-in-string "\\(\\s-\\|\n\\)+$" "" (buffer-string)))
           (trimmed (replace-regexp-in-string "^\\(\\s-\\|\n\\)+" "" trimmed-1)))
      (concat author " wrote:<p>\n"
              "<pre>\n"
              (cl-subseq (replace-regexp-in-string "\n" "\n> " (concat "\n" trimmed)) 1)
              "\n</pre><p>"))))

(defun nndiscourse-add-entry (hashtb e field)
  "Add to HASHTB a lookup consisting of entry E's id to its FIELD."
  (nndiscourse--sethash (plist-get e :id) (plist-get e field) hashtb))

(defsubst nndiscourse--summary-exit ()
  "Call `gnus-summary-exit' without the hackery."
  (remove-function (symbol-function 'gnus-summary-exit)
                   (symbol-function 'nndiscourse--score-pending))
  (gnus-summary-exit)
  (add-function :after (symbol-function 'gnus-summary-exit)
                (symbol-function 'nndiscourse--score-pending)))

(deffoo nndiscourse-request-group-scan (group &optional server info)
  "M-g from *Group* calls this."
  (nndiscourse--with-group server group
    (gnus-message 5 "nndiscourse-request-group-scan: scanning %s..." group)
    (nndiscourse-request-scan nil server)
    (gnus-get-unread-articles-in-group
     (or info (gnus-get-info gnus-newsgroup-name))
     (gnus-active (gnus-info-group info)))
    (gnus-message 5 "nndiscourse-request-group-scan: scanning %s...done" group))
  t)

;; gnus-group-select-group
;;   gnus-group-read-group
;;     gnus-summary-read-group
;;       gnus-summary-read-group-1
;;         gnus-summary-setup-buffer
;;           sets gnus-newsgroup-name
;;         gnus-select-newsgroup
;;           gnus-request-group
;;             nndiscourse-request-group
(deffoo nndiscourse-request-group (group &optional server _fast _info)
  (nndiscourse--with-group server group
    (let* ((num-headers (length (nndiscourse-get-headers server group)))
           (status (format "211 %d %d %d %s" num-headers
                           (aif (nndiscourse--first-article-number server group) it 1)
                           (aif (nndiscourse--last-article-number server group) it 0)
                           group)))
      (gnus-message 7 "nndiscourse-request-group: %s" status)
      (nnheader-insert "%s\n" status))
    t))

(defun nndiscourse--request-item (id server)
  "Retrieve ID from SERVER as a property list."
  (push id nndiscourse--debug-request-posts)
  (let* ((port (nndiscourse-proc-info-port (cdr (assoc server nndiscourse-processes))))
         (conn (json-rpc-connect nndiscourse-localhost port))
         (utf-decoder (lambda (x)
                        (decode-coding-string (with-temp-buffer
                                                (set-buffer-multibyte nil)
                                                (insert x)
                                                (buffer-string))
                                              'utf-8))))
    (add-function :filter-return (symbol-function 'json-read-string) utf-decoder)
    (unwind-protect
        (condition-case err  (json-rpc conn "get_post" id)
          (error (gnus-message 3 "nndiscourse--request-item: %s" (error-message-string err))
                 nil))
      (remove-function (symbol-function 'json-read-string) utf-decoder))))

(defun nndiscourse-get-categories (server)
  "Query SERVER /categories.json."
  (seq-filter (lambda (x) (eq json-false (plist-get x :read_restricted)))
              (let ((cats (funcall #'nndiscourse-rpc-request server "categories")))
                (if (seqp cats) cats nil))))

(cl-defun nndiscourse-get-topics (server slug &key (page 0))
  "Query SERVER /c/SLUG/l/latest.json, optionally for PAGE."
  (funcall #'nndiscourse-rpc-request server
           "category_latest_topics"
           :category_slug slug :page page))

(cl-defun nndiscourse-get-posts (server &key (before 0))
  "Query SERVER /posts.json for posts before BEFORE."
  (plist-get (let ((result (funcall #'nndiscourse-rpc-request server
                                    "posts" :before before)))
               (if (listp result) result nil)) :latest_posts))

(defun nndiscourse--number-to-header (server group topic-id post-number)
  "O(n) search for SERVER GROUP TOPIC-ID POST-NUMBER in headers."
  (declare (indent defun))
  (-when-let* ((headers (nndiscourse-get-headers server group))
               (found (seq-position
                       headers (cons topic-id post-number)
                       (lambda (plst loc)
                         (cl-destructuring-bind (topic-id* . post-number*) loc
                           (and (= topic-id* (plist-get plst :topic_id))
                                (= post-number* (plist-get plst :post_number))))))))
    (elt headers found)))

(defsubst nndiscourse-hash-count (table-or-obarray)
  "Return number items in TABLE-OR-OBARRAY."
  (let ((result 0))
    (nndiscourse--maphash (lambda (&rest _args) (cl-incf result)) table-or-obarray)
    result))

(defun nndiscourse--incoming (server)
  "Drink from the SERVER firehose."
  (interactive)
  (setq nndiscourse--debug-request-posts nil)
  (when (zerop (nndiscourse-hash-count nndiscourse--categories-hashtb))
    (nndiscourse-request-list server))
  (cl-loop
   with new-posts
   for page-bottom = 1 then (plist-get (elt posts (1- (length posts))) :id)
   for posts = (nndiscourse-get-posts server :before (1- page-bottom))
   until (null posts)
   do (unless nndiscourse--last-id
        (setq nndiscourse--last-id
              (1- (plist-get (elt posts (1- (length posts))) :id))))
   do (cl-do* ((k 0 (1+ k))
               (plst (and (< k (length posts)) (elt posts k))
                     (and (< k (length posts)) (elt posts k))))
          ((or (null plst)
               (<= (plist-get plst :id) nndiscourse--last-id)))
        (push plst new-posts))
   until (<= (1- (plist-get (elt posts (1- (length posts))) :id))
             nndiscourse--last-id)
   finally
   (let ((counts (gnus-make-hashtable)))
     (dolist (plst new-posts)
       (setq nndiscourse--last-id (plist-get plst :id))
       (-when-let* ((not-deleted (not (plist-get plst :deleted_at)))
                    (type (plist-get plst :post_type))
                    (category-id (plist-get plst :category_id))
                    (group (nndiscourse-get-category server category-id))
                    (full-group (gnus-group-full-name
                                 group
                                 (cons 'nndiscourse (list server)))))
         (when-let ((parent-number (plist-get plst :reply_to_post_number)))
           (nndiscourse-set-ref server
                                (plist-get plst :id)
                                (plist-get (nndiscourse--number-to-header
                                             server group (plist-get plst :topic_id)
                                             parent-number)
                                           :id)))
         (nndiscourse--replace-hash type (lambda (x) (1+ (or x 0))) counts)
         (if-let ((info (gnus-get-info full-group)))
             (progn
               (unless (gnus-info-read info)
                 (setf (gnus-info-read info)
                       (gnus-range-normalize `(1 . ,(1- (plist-get plst :id))))))
               (-when-let* ((last-number (nndiscourse--last-article-number server group))
                            (next-number (plist-get plst :id))
                            (gap `(,(1+ last-number) . ,(1- next-number))))
                 (when (<= (car gap) (cdr gap))
                   (setf (gnus-info-read info)
                         (gnus-range-add (gnus-info-read info) (gnus-range-normalize gap)))
                   (setf (alist-get 'unexist (gnus-info-marks info))
                         (gnus-range-add (alist-get 'unexist (gnus-info-marks info))
                                         (gnus-range-normalize gap))))))
           (gnus-message 3 "nndiscourse--incoming: cannot update read for %s" group))
         (nndiscourse-set-headers server group
           (nconc (nndiscourse-get-headers server group) (list plst)))))
     (gnus-message
      5 (concat "nndiscourse--incoming: "
                (format "last-id: %s, " nndiscourse--last-id)
                (let ((result ""))
                  (nndiscourse--maphash
                   (lambda (key value)
                     (setq result (concat result (format "type=%s +%s " key value))))
                   counts)
                  result))))))

(deffoo nndiscourse-request-scan (&optional _group server)
  (when (nndiscourse-good-server server)
    (if (> 2 (- (truncate (float-time)) nndiscourse--last-scan-time))
        (gnus-message 7 "nndiscourse-request-scan: last scanned at %s"
                      (current-time-string nndiscourse--last-scan-time))
      (cl-destructuring-bind (seconds num-gc seconds-gc)
          (benchmark-run (nndiscourse--incoming server))
        (setq nndiscourse--last-scan-time (truncate (float-time)))
        (gnus-message 5 (concat "nndiscourse-request-scan: Took %s seconds,"
                                " with %s gc runs taking %s seconds")
                      seconds num-gc seconds-gc)))))

(defsubst nndiscourse--make-message-id (id)
  "Construct a valid Gnus message id from ID."
  (format "<%s@discourse.org>" id))

(defsubst nndiscourse--make-references (server id)
  "For SERVER, construct a space delimited string of message ancestors of ID."
  (mapconcat (lambda (ref) (nndiscourse--make-message-id ref))
             (nndiscourse-get-ref server id) " "))

(defsubst nndiscourse--make-header (server group article-number)
  "Construct mail headers from article header.
For SERVER GROUP article headers, construct mail headers from ARTICLE-NUMBER'th
article header.  Gnus manual does say the term `header` is oft conflated."
  (when-let ((header (nndiscourse--get-header server group article-number)))
    (let ((score (plist-get header :score))
          (reads (plist-get header :reads)))
      (make-full-mail-header
       article-number
       (plist-get header :topic_title)
       (plist-get header :username)
       (format-time-string "%a, %d %h %Y %T %z (%Z)" (date-to-time (plist-get header :created_at)))
       (nndiscourse--make-message-id (plist-get header :id))
       (nndiscourse--make-references server (plist-get header :id))
       0 0 nil
       (append `((X-Discourse-Name . ,(plist-get header :name)))
               `((X-Discourse-ID . ,(plist-get header :id)))
               `((X-Discourse-Permalink . ,(format "%s/t/%s/%s/%s"
                                                   server
                                                   (plist-get header :topic_slug)
                                                   (plist-get header :topic_id)
                                                   (plist-get header :id))))
               (and (numberp score)
                    `((X-Discourse-Score . ,(number-to-string (truncate score)))))
               (and (numberp reads)
                    `((X-Discourse-Reads . ,(number-to-string (truncate reads))))))))))

;; CORS denial
(defalias 'nndiscourse--request #'ignore)

(deffoo nndiscourse-request-article (article-number &optional group server buffer)
  (unless buffer (setq buffer nntp-server-buffer))
  (nndiscourse--with-group server group
    (with-current-buffer buffer
      (erase-buffer)
      (let* ((header (nndiscourse--get-header server group article-number))
             (mail-header (nndiscourse--make-header server group article-number))
             (score (cdr (assq 'X-Discourse-Score (mail-header-extra mail-header))))
             (permalink (cdr (assq 'X-Discourse-Permalink (mail-header-extra mail-header))))
             (body (nndiscourse--massage (plist-get header :cooked))))
        (when body
          (insert
           "Newsgroups: " group "\n"
           "Subject: " (mail-header-subject mail-header)  "\n"
           "From: " (or (mail-header-from mail-header) "nobody") "\n"
           "Date: " (mail-header-date mail-header) "\n"
           "Message-ID: " (mail-header-id mail-header) "\n"
           "References: " (mail-header-references mail-header) "\n"
           (format "Content-Type: text/html; charset=%s" nndiscourse-charset) "\n"
           "Archived-at: " permalink "\n"
           "Score: " score "\n"
           "\n")
          (-when-let*
              ((parent (plist-get header :parent))
               (parent-author
                (or (plist-get (nndiscourse--get-header server group parent)
                               :username)
                    "Someone"))
               (parent-body (nndiscourse--massage
                             (plist-get
                              (nndiscourse--get-header server group parent)
                              :cooked))))
            (insert (nndiscourse--citation-wrap parent-author parent-body)))
          (insert body)
          (cons group article-number))))))

(deffoo nndiscourse-retrieve-headers (article-numbers &optional group server _fetch-old)
  (with-current-buffer nntp-server-buffer
    (erase-buffer)
    (nndiscourse--with-group server group
      (dolist (i article-numbers)
        (when-let ((header (nndiscourse--make-header server group i)))
          (nnheader-insert-nov header)))
      'nov)))

;; Primarily because `gnus-get-unread-articles' won't update unreads
;; upon install (nndiscourse won't yet be in type-cache).
;; I am counting on logic in `gnus-read-active-file-1' in `gnus-get-unread-articles'
;; to get here upon install.
(deffoo nndiscourse-retrieve-groups (groups &optional server)
  (when (nndiscourse-good-server server)
    ;; utterly insane thing where `gnus-active-to-gnus-format' expects
    ;; `gnus-request-list' output to be in `nntp-server-buffer'
    ;; and populates `gnus-active-hashtb'
    (nndiscourse-request-list server)
    (with-current-buffer nntp-server-buffer
      (gnus-active-to-gnus-format
       (gnus-server-to-method (format "nndiscourse:%s" server))
       gnus-active-hashtb nil t))
    (mapc (lambda (group)
            (let ((full-name (gnus-group-full-name group `(nndiscourse ,server))))
              (gnus-get-unread-articles-in-group (gnus-get-info full-name)
                                                 (gnus-active full-name))))
          groups)
    ;; `gnus-read-active-file-2' will now repeat what I just did.  Brutal.
    'active))

(deffoo nndiscourse-request-list (&optional server)
  (let (groups)
    (when (and (nndiscourse-good-server server) (nndiscourse-server-opened server))
      (with-current-buffer nntp-server-buffer
        (mapc
         (lambda (plst)
           (let* ((group (plist-get plst :slug))
                  (category-id (plist-get plst :id))
                  (full-name (gnus-group-full-name group `(nndiscourse ,server))))
             (erase-buffer)
             ;; only `gnus-activate-group' seems to call `gnus-parse-active'
             (unless (gnus-get-info full-name)
               (gnus-activate-group full-name nil nil `(nndiscourse ,server))
               (gnus-group-unsubscribe-group full-name
                                             gnus-level-default-subscribed t))
             (nndiscourse-set-category server category-id group)
             (push group groups)))
         (nndiscourse-get-categories server))
        (erase-buffer)
        (mapc (lambda (group)
                (insert
                 (format "%s %d %d y\n" group
                         (aif (nndiscourse--last-article-number server group)
                             it 0)
                         (aif (nndiscourse--first-article-number server group)
                             it 1))))
              groups)))
    t))

(defun nndiscourse-sentinel (process event)
  "Wipe headers state when PROCESS dies from EVENT."
  (unless (string= "open" (substring event 0 4))
    (gnus-message 2 "nndiscourse-sentinel: process %s %s"
                  (car (process-command process))
                  (replace-regexp-in-string "\n$" "" event))
    (nndiscourse-deregister-process (process-name process))
    (gnus-backlog-shutdown)))

(defun nndiscourse--message-user (server beg end _prev-len)
  "Message SERVER related alert with `buffer-substring' from BEG to END."
  (let ((string (buffer-substring beg end))
        (magic "::user::"))
    (when (string-prefix-p magic string)
      (message "%s: %s" server (substring string (length magic))))))

;; C-c C-c from followup buffer
;; message-send-and-exit
;; message-send
;; message-send-method-alist=message-send-news-function=message-send-news
;; gnus-request-post
;; nndiscourse-request-post
(deffoo nndiscourse-request-post (&optional _server)
  nil)

(defun nndiscourse--browse-post (&rest _args)
  "What happens when I click on discourse Subject."
  (-when-let* ((group-article gnus-article-current)
               (header (nndiscourse--get-header
                         (nnoo-current-server 'nndiscourse)
                         (gnus-group-real-name (car group-article))
                         (cdr group-article)))
               (url (format "%s://%s/t/%s/%s/%s"
                            nndiscourse-scheme
                            (nnoo-current-server 'nndiscourse)
                            (plist-get header :topic_slug)
                            (plist-get header :topic_id)
                            (plist-get header :post_number))))
    (browse-url url)))

(defun nndiscourse--header-button-alist ()
  "Construct a buffer-local `gnus-header-button-alist' for nndiscourse."
  (let* ((result (copy-alist gnus-header-button-alist))
         (references-value (assoc-default "References" result
                                          (lambda (x y) (string-match-p y x))))
         (references-key (car (rassq references-value result))))
    (setq result (cl-delete "^Subject:" result :test (lambda (x y) (cl-search x (car y)))))
    (setq result (cl-delete references-key result :test (lambda (x y) (cl-search x (car y)))))
    (push (append '("^\\(Message-I[Dd]\\|^In-Reply-To\\):") references-value) result)
    (push '("^Subject:" ": *\\(.+\\)$" 1 (>= gnus-button-browse-level 0)
            nndiscourse--browse-post 1)
          result)
    result))

(defsubst nndiscourse--fallback-link ()
  "Cannot render post."
  (let* ((header (nndiscourse--get-header (nnoo-current-server 'nndiscourse)
                   (gnus-group-real-name (car gnus-article-current))
                   (cdr gnus-article-current)))
         (body (nndiscourse--massage (plist-get header :cooked))))
    (with-current-buffer gnus-original-article-buffer
      (article-goto-body)
      (delete-region (point) (point-max))
      (insert body))))

(defalias 'nndiscourse--display-article
  (lambda (article &optional all-headers header)
    (condition-case-unless-debug err
        (gnus-article-prepare article all-headers header)
      (error
       (if nndiscourse-render-post
           (progn
             (gnus-message 7 "nndiscourse--display-article: '%s' (falling back...)"
                           (error-message-string err))
             (nndiscourse--fallback-link)
             (gnus-article-prepare article all-headers))
         (error (error-message-string err))))))
  "In case of shr failures, dump original link.")

(defsubst nndiscourse--dense-time (time)
  "Convert TIME to a floating point number.

Written by John Wiegley (https://github.com/jwiegley/dot-emacs)."
  (+ (* (car time) 65536.0)
     (cadr time)
     (/ (or (car (cdr (cdr time))) 0) 1000000.0)))

(defalias 'nndiscourse--format-time-elapsed
  (lambda (header)
    (condition-case nil
        (let ((date (mail-header-date header)))
          (if (> (length date) 0)
              (let*
                  ((then (nndiscourse--dense-time
                          (apply 'encode-time (parse-time-string date))))
                   (now (nndiscourse--dense-time (current-time)))
                   (diff (- now then))
                   (str
                    (cond
                     ((>= diff (* 86400.0 7.0 52.0))
                      (if (>= diff (* 86400.0 7.0 52.0 10.0))
                          (format "%3dY" (floor (/ diff (* 86400.0 7.0 52.0))))
                        (format "%3.1fY" (/ diff (* 86400.0 7.0 52.0)))))
                     ((>= diff (* 86400.0 30.0))
                      (if (>= diff (* 86400.0 30.0 10.0))
                          (format "%3dM" (floor (/ diff (* 86400.0 30.0))))
                        (format "%3.1fM" (/ diff (* 86400.0 30.0)))))
                     ((>= diff (* 86400.0 7.0))
                      (if (>= diff (* 86400.0 7.0 10.0))
                          (format "%3dw" (floor (/ diff (* 86400.0 7.0))))
                        (format "%3.1fw" (/ diff (* 86400.0 7.0)))))
                     ((>= diff 86400.0)
                      (if (>= diff (* 86400.0 10.0))
                          (format "%3dd" (floor (/ diff 86400.0)))
                        (format "%3.1fd" (/ diff 86400.0))))
                     ((>= diff 3600.0)
                      (if (>= diff (* 3600.0 10.0))
                          (format "%3dh" (floor (/ diff 3600.0)))
                        (format "%3.1fh" (/ diff 3600.0))))
                     ((>= diff 60.0)
                      (if (>= diff (* 60.0 10.0))
                          (format "%3dm" (floor (/ diff 60.0)))
                        (format "%3.1fm" (/ diff 60.0))))
                     (t
                      (format "%3ds" (floor diff)))))
                   (stripped
                    (replace-regexp-in-string "\\.0" "" str)))
                (concat (cond
                         ((= 2 (length stripped)) "  ")
                         ((= 3 (length stripped)) " ")
                         (t ""))
                        stripped))))
      ;; print some spaces and pretend nothing happened.
      (error "    ")))
  "Return time elapsed since HEADER was sent.

Written by John Wiegley (https://github.com/jwiegley/dot-emacs).")

;; Evade package-lint!
(fset 'gnus-user-format-function-S
      (symbol-function 'nndiscourse--format-time-elapsed))

(let ((custom-defaults
       ;; For now, revert any user overrides that I can't predict.
       (mapcar (lambda (x)
                 (let* ((var (cl-first x))
                        (sv (get var 'standard-value)))
                   (when (eq var 'gnus-default-adaptive-score-alist)
                     (setq sv (list `(quote
                                      ,(mapcar (lambda (entry)
                                                 (cons (car entry)
                                                       (assq-delete-all 'from (cdr entry))))
                                               (eval (car sv)))))))
                   (cons var sv)))
               (seq-filter (lambda (x) (eq 'custom-variable (cl-second x)))
                           (append (get 'gnus-score-adapt 'custom-group)
                                   (get 'gnus-score-default 'custom-group))))))
  (add-to-list 'gnus-parameters `("^nndiscourse"
                                  ,@custom-defaults
                                  (gnus-summary-make-false-root 'adopt)
                                  (gnus-cite-hide-absolute 5)
                                  (gnus-cite-hide-percentage 0)
                                  (gnus-cited-lines-visible '(2 . 2))
                                  (gnus-simplify-subject-functions (quote (gnus-simplify-subject-fuzzy)))
                                  (gnus-summary-line-format "%3t%U%R%uS %I%(%*%-10,10f  %s%)\n")
                                  (gnus-auto-extend-newsgroup nil)
                                  (gnus-add-timestamp-to-message t)
                                  (gnus-summary-display-article-function
                                   (quote ,(symbol-function 'nndiscourse--display-article)))
                                  (gnus-header-button-alist
                                   (quote ,(nndiscourse--header-button-alist)))
                                  (gnus-visible-headers ,(concat gnus-visible-headers "\\|^Score:")))))

(defun nndiscourse-article-mode-activate ()
  "Augment the `gnus-article-mode-map' conditionally."
  (when (nndiscourse--gate)
    (nndiscourse-article-mode)))

(defun nndiscourse-summary-mode-activate ()
  "Shadow some bindings in `gnus-summary-mode-map' conditionally."
  (when (nndiscourse--gate)
    (nndiscourse-summary-mode)))

(nnoo-define-skeleton nndiscourse)

(defsubst nndiscourse--who-am-i ()
  "Get my Discourse username."
  "dickmao")

;; I believe I did try buffer-localizing hooks, and it wasn't sufficient
(add-hook 'gnus-article-mode-hook 'nndiscourse-article-mode-activate)
(add-hook 'gnus-summary-mode-hook 'nndiscourse-summary-mode-activate)

;; `gnus-newsgroup-p' requires valid method post-mail to return t
(add-to-list 'gnus-valid-select-methods '("nndiscourse" post-mail) t)

(add-function
 :filter-return (symbol-function 'message-make-fqdn)
 (lambda (val)
   (if (and (nndiscourse--gate)
            (cl-search "--so-tickle-me" val))
       "discourse.org" val)))

(add-function
 :before-until (symbol-function 'message-make-from)
 (lambda (&rest _args)
   (when (nndiscourse--gate)
     (concat (nndiscourse--who-am-i) "@discourse.org"))))

;; the let'ing to nil of `gnus-summary-display-article-function'
;; in `gnus-summary-select-article' dates back to antiquity.
(add-function
 :around (symbol-function 'gnus-summary-display-article)
 (lambda (f &rest args)
   (cond ((nndiscourse--gate)
          (let ((gnus-summary-display-article-function
                 (symbol-function 'nndiscourse--display-article)))
            (apply f args)))
         (t (apply f args)))))

;; possible impostors
(setq gnus-valid-select-methods (cl-remove-if (lambda (method)
                                                (equal (car method) "nndiscourse"))
                                              gnus-valid-select-methods))
(gnus-declare-backend "nndiscourse" 'post-mail 'address)

;; correct what I perceive to be a bug
;; (and prevents multibyte Chinese characters from displaying)
(require 'mm-decode)

(defvar shr-blocked-images)
(defvar shr-use-fonts)
(defvar shr-width)
(defvar shr-content-function)
(defvar shr-inhibit-images)

(defmacro nndiscourse-with-part (handle &rest forms)
  "Override `mm-with-part' massaging message from HANDLE then executing FORMS."
  `(let* ((handle ,handle))
     (when (and (mm-handle-buffer handle)
		(buffer-name (mm-handle-buffer handle)))
       (with-temp-buffer
         (set-buffer-multibyte (buffer-local-value 'enable-multibyte-characters
                                                   (mm-handle-buffer handle)))
	 (insert-buffer-substring (mm-handle-buffer handle))
	 (mm-decode-content-transfer-encoding
	  (mm-handle-encoding handle)
	  (mm-handle-media-type handle))
	 ,@forms))))

(defun nndiscourse-shr (handle)
  "Override `mm-shr' for HANDLE."
  (require 'shr)
  (let ((shr-width (if shr-use-fonts
		       nil
		     fill-column))
	(shr-content-function (lambda (id)
				(let ((handle (mm-get-content-id id)))
				  (when handle
				    (nndiscourse-with-part handle
				      (buffer-string))))))
	(shr-inhibit-images mm-html-inhibit-images)
	(shr-blocked-images mm-html-blocked-images)
	charset coding char document)
    (nndiscourse-with-part (or handle (setq handle (mm-dissect-buffer t)))
      (setq case-fold-search t)
      (or (setq charset
		(mail-content-type-get (mm-handle-type handle) 'charset))
	  (progn
	    (goto-char (point-min))
	    (and (re-search-forward "\
<meta\\s-+http-equiv=[\"']?content-type[\"']?\\s-+content=[\"']?\
text/html;\\s-*charset=\\([^\t\n\r \"'>]+\\)[^>]*>" nil t)
		 (setq coding (mm-charset-to-coding-system (match-string 1)
							   nil t))))
	  (setq charset mail-parse-charset))
      (when (and (or coding
		     (setq coding (mm-charset-to-coding-system charset nil t)))
		 (not (eq coding 'ascii)))
	(insert (prog1
		    (decode-coding-string (buffer-string) coding)
		  (erase-buffer)
		  (set-buffer-multibyte t))))
      (goto-char (point-min))
      (while (re-search-forward
	      "&#\\(?:x\\([89][0-9a-f]\\)\\|\\(1[2-5][0-9]\\)\\);" nil t)
	(when (setq char
		    (cdr (assq (if (match-beginning 1)
				   (string-to-number (match-string 1) 16)
				 (string-to-number (match-string 2)))
			       mm-extra-numeric-entities)))
	  (replace-match (char-to-string char))))
      ;; Remove "soft hyphens".
      (goto-char (point-min))
      (while (search-forward "­" nil t)
	(replace-match "" t t))
      (setq document (libxml-parse-html-region (point-min) (point-max))))
    (save-restriction
      (narrow-to-region (point) (point))
      (shr-insert-document document)
      (unless (bobp)
	(insert "\n"))
      (mm-handle-set-undisplayer
       handle
       (let ((min (point-min-marker))
             (max (point-max-marker)))
         (lambda ()
	   (let ((inhibit-read-only t))
	     (delete-region min max))))))))

(setf (alist-get 'shr mm-text-html-renderer-alist) 'nndiscourse-shr)

(provide 'nndiscourse)

;;; nndiscourse.el ends here
