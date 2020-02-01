;;; nndiscourse.el --- Gnus backend for Discourse  -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2019 The Authors of nndiscourse.el

;; Authors: dickmao <github id: dickmao>
;; Version: 0.1.0
;; Keywords: news
;; URL: https://github.com/dickmao/nndiscourse
;; Package-Requires: ((emacs "25.1") (dash "2.16") (dash-functional "1.2.0") (anaphora "1.0.4"))

;; ((dash-functional "20180107") (anaphora "20180618"))

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

(nnoo-declare nndiscourse)

(nnoo-define-basics nndiscourse)

(defvoo nndiscourse-scheme "https"
  "URI scheme for address.")

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

(defsubst nndiscourse--replace-hash (string func hashtable)
  "Set value of STRING to FUNC applied to existing STRING value in HASHTABLE.

Starting in emacs-src commit c1b63af, Gnus moved from obarrays to normal hashtables."
  (declare (indent defun))
  (unless (stringp string)
    (setq string (format "%s" string)))
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

(defsubst nndiscourse-novote ()
  "Retract vote."
  (interactive)
  (nndiscourse-vote-current-article 0))

(defsubst nndiscourse-downvote ()
  "Downvote the article in current buffer."
  (interactive)
  (nndiscourse-vote-current-article -1))

(defsubst nndiscourse-upvote ()
  "Upvote the article in current buffer."
  (interactive)
  (nndiscourse-vote-current-article 1))

(defvar nndiscourse-connections-hashtb (gnus-make-hashtable)
  "Group -> jsonrpc connection struct.")

(defvar nndiscourse-location-hashtb (gnus-make-hashtable)
  "Id -> ( group . index ).")

(defvar nndiscourse-categories-hashtb (gnus-make-hashtable)
  "Category id -> group.")

(defvar nndiscourse-headers-hashtb (gnus-make-hashtable)
  "Group -> headers.")

(defvar nndiscourse-refs-hashtb (gnus-make-hashtable)
  "Who replied to whom (global over all entries).")

(defvar nndiscourse-authors-hashtb (gnus-make-hashtable)
  "For fast lookup of parent-author (global over all entries).")

(defsubst nndiscourse-get-headers (group)
  "List headers from GROUP."
  (nndiscourse--gethash group nndiscourse-headers-hashtb))

(defun nndiscourse-find-header (id server)
  "Retrieve header (plst) of ID.  Use SERVER to recover group if necessary."
  (when-let ((location (nndiscourse--gethash id nndiscourse-location-hashtb)))
    (cl-destructuring-bind (group . index) location
      (nndiscourse--get-header server group (1+ index)))))

(defsubst nndiscourse-refs-for (id)
  "Return descending ancestors as list for ID."
  (cl-loop for prev-id = id then cur-id
           for cur-id = (nndiscourse--gethash prev-id nndiscourse-refs-hashtb)
           until (not cur-id)
           collect cur-id into rresult
           finally return (nreverse rresult)))

(defmacro nndiscourse--with-mutex (mtx &rest body)
  "If capable of threading, lock with MTX and execute BODY."
  (declare (indent defun))
  (if (fboundp 'with-mutex)
      `(with-mutex ,mtx ,@body)
    `(progn ,@body)))

(defvar nndiscourse--mutex-rpc-request (when (fboundp 'make-mutex)
                                         (make-mutex "nndiscourse--mutex-rpc-request"))
  "Only one jsonrpc output buffer, so avoid two requests using at the same time.")

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
  (if (nndiscourse-open-server server)
      (condition-case-unless-debug err
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
                 (gnus-message 3 "nndiscourse-rpc-request: %s" (error-message-string err)))))
    (prog1 nil
      (gnus-message 3 "nndiscourse-rpc-request: could not retrieve jimson process"))))

(defun nndiscourse-vote-current-article (vote)
  "VOTE is +1, -1, 0."
  (unless gnus-newsgroup-name (error "No current newgroup"))
  (if-let ((article-number (or (cdr gnus-article-current)
                               (gnus-summary-article-number))))
      (let* ((header (nndiscourse--get-header nil
                                              (gnus-group-real-name gnus-newsgroup-name)
                                              article-number))
             (orig-score (format "%s" (plist-get header :score)))
             (new-score (if (zerop vote) orig-score
                          (concat orig-score " "
                                  (if (> vote 0) "+" "")
                                  (format "%s" vote))))
             (server (cl-second (gnus-find-method-for-group gnus-newsgroup-name))))
        (save-excursion
          (save-window-excursion
            (with-current-buffer gnus-summary-buffer
              (if (eq (gnus-summary-article-number) (cdr gnus-article-current))
                  (if (nndiscourse--request-vote server (plist-get header :id) vote)
                      (with-current-buffer gnus-article-buffer
                        (let ((inhibit-read-only t))
                          (nnheader-replace-header "Score" new-score)))
                    (gnus-message 5 "nndiscourse-vote-current-article: failed for %s"
                                  (plist-get header :id)))
                (message "Open the article before voting."))))))
    (error "No current article")))

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

(defsubst nndiscourse--server-buffer-name (server)
  "Arbitrary proc buffer name for SERVER."
  (when server
    (format " *%s*" server)))

(defsubst nndiscourse--server-buffer (server &optional create)
  "Get proc buffer for SERVER.  Create if necessary if CREATE."
  (when server
    (let ((name (nndiscourse--server-buffer-name server)))
      (if create
          (get-buffer-create name)
        (get-buffer name)))))

(deffoo nndiscourse-server-opened (&optional server)
  (nndiscourse--server-buffer server))

(deffoo nndiscourse-status-message (&optional _server)
  "")

(defun nndiscourse--initialize ()
  "Run `bundle install` if necessary."
  (let ((bundle-exec (executable-find "bundle"))
        (default-directory
          (expand-file-name "nndiscourse"
                            (file-name-directory
                             (or (locate-library "nndiscourse")
                                 default-directory)))))
    (unless bundle-exec
      (error "`nndiscourse--initialize': nndiscourse requires bundler"))
    (unless (zerop (call-process bundle-exec nil nil nil "check"))
      (let ((bundle-buffer (get-buffer-create "*nndiscourse: bundle install*")))
        (if (zerop (apply #'call-process bundle-exec nil
                          (cons bundle-buffer (list t))
                          nil (split-string "install --deployment --without development")))
            (kill-buffer bundle-buffer)
          (switch-to-buffer bundle-buffer)
          (error "`nndiscourse--initialize': bundle install failed"))))))

(deffoo nndiscourse-open-server (server &optional defs)
  "Retrieve the Jimson process for SERVER."
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

(defun nndiscourse-register-process (port proc)
  "Register PORT and PROC with a server-name-qua-url.
Return PROC if success, nil otherwise."
  (declare (indent defun))
  (nndiscourse-deregister-process (process-name proc))
  (if (process-live-p proc)
      (prog1 proc
        (gnus-message 5 "`nndiscourse-register-process': registering %s"
                      (process-name proc))
        (setf (alist-get (process-name proc) nndiscourse-processes nil nil #'equal)
              (make-nndiscourse-proc-info :port port :process proc)))
    (prog1 nil
      (gnus-message 3 "`nndiscourse-register-process': dead process %s"
                    (process-name proc))
      (nndiscourse-deregister-process (process-name proc)))))

(defun nndiscourse-deregister-process (server)
  "Disavow any knowledge of SERVER's process."
  (aif (alist-get server nndiscourse-processes nil nil #'equal)
      (let ((proc (nndiscourse-proc-info-process it)))
        (gnus-message 5 "`nndiscourse-deregister-process': deregistering %s %s pid=%s"
                      server (process-name proc) (process-id proc))
        (delete-process proc)))
  (setf (alist-get server nndiscourse-processes nil nil #'equal) nil))

(deffoo nndiscourse-close-server (&optional server _defs)
  "Patterning after nnimap.el."
  (aif (nndiscourse--server-buffer server)
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
           (indent 1))
  `(let* ((server (or ,server (nnoo-current-server 'nndiscourse)))
          (group (or ,group (gnus-group-real-name gnus-newsgroup-name)))
          (gnus-newsgroup-name (or gnus-newsgroup-name
                                   (gnus-group-full-name
                                    group (cons 'nndiscourse (list server))))))
     ,@body))

(defun nndiscourse--get-header (server group article-number)
  "In context of SERVER GROUP, get header indexed ARTICLE-NUMBER."
  (nndiscourse--with-group server group
    (let ((headers (nndiscourse-get-headers group)))
      (elt headers (1- article-number)))))

(defun nndiscourse--get-body (header)
  "Get full text of submission or post HEADER."
  (if-let ((url (plist-get header :url)))
      (format "<div><p><a href=\"%s\">%s</a></div>" url url)
    (or (plist-get header :text) "")))

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
    (gnus-activate-group gnus-newsgroup-name)
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
(deffoo nndiscourse-request-group (group &optional server _fast info)
  (nndiscourse--with-group server group
    (let* ((info (or info (gnus-get-info gnus-newsgroup-name)))
           (num-headers (length (nndiscourse-get-headers group)))
           (status (format "211 %d %d %d %s" num-headers 1 num-headers group)))
      (gnus-message 7 "nndiscourse-request-group: %s" status)
      (nnheader-insert "%s\n" status)
      (when info
        (gnus-info-set-marks
         info
         (append (assq-delete-all 'seen (gnus-info-marks info))
                 (list `(seen (1 . ,num-headers)))))
        (gnus-set-info gnus-newsgroup-name info)))
    t))

(defun nndiscourse--request-vote (server item vote)
  "Tally on SERVER for ITEM the VOTE."
  (if (> vote 0)
      (nndiscourse-rpc-request server "create_post_action"
                               :id (plist-get item :id)
                               :post_action_type_id 2)
    (nndiscourse-rpc-request server "destroy_post_action"
                             :id (plist-get item :id)
                             :post_action_type_id 2)))

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
              (funcall #'nndiscourse-rpc-request server "categories")))

(cl-defun nndiscourse-get-topics (server slug &key (page 0))
  "Query SERVER /c/SLUG/l/latest.json, optionally for PAGE."
  (funcall #'nndiscourse-rpc-request server
           "category_latest_topics"
           :category_slug slug :page page))

(cl-defun nndiscourse-get-posts (server &key (before 0))
  "Query SERVER /posts.json for posts before BEFORE."
  (plist-get (funcall #'nndiscourse-rpc-request server "posts" :before before) :latest_posts))

(defun nndiscourse--number-to-header (server category-id topic-id post-number)
  "O(n) search for SERVER CATEGORY-ID TOPIC-ID POST-NUMBER in headers."
  (-when-let* ((group (nndiscourse--gethash category-id nndiscourse-categories-hashtb))
               (headers (nndiscourse-get-headers group))
               (found (seq-position
                       headers (cons topic-id post-number)
                       (lambda (plst loc)
                         (cl-destructuring-bind (topic-id* post-number*) loc
                           (and (= topic-id* (plist-get plst :topic_id))
                                (= post-number* (plist-get plst :post_number))))))))
    (nndiscourse--get-header server group (1+ found))))

(defsubst nndiscourse--append-header (plst group)
  "Update hash tables for PLST \"header\" in GROUP."
  (let ((id (plist-get plst :id))
        (headers (nndiscourse-get-headers group)))
    (nndiscourse--sethash id (cons group (length headers)) nndiscourse-location-hashtb)
    (nndiscourse--sethash group (nconc headers (list plst)) nndiscourse-headers-hashtb)
    plst))

(defun nndiscourse--incoming (server)
  "Drink from the SERVER firehose."
  (interactive)
  (setq nndiscourse--debug-request-posts nil)
  (cl-loop
   with counts = (gnus-make-hashtable)
   for page-bottom = 0 then (plist-get (elt posts (1- (length posts))) :id)
   for posts = (nndiscourse-get-posts server :before page-bottom)
   do (seq-map
       (lambda (plst)
         (when-let ((not-deleted (not (plist-get plst :deleted_at)))
                    (type (plist-get plst :post_type)))
           (when-let ((parent-number (plist-get plst :reply_to_post_number)))
             (nndiscourse--sethash
               (plist-get plst :id)
               (plist-get (nndiscourse--number-to-header
                           server
                           (plist-get plst :category_id)
                           (plist-get plst :topic_id)
                           parent-number)
                          :id)
               nndiscourse-refs-hashtb))
           (nndiscourse-add-entry nndiscourse-authors-hashtb plst :username)
           (nndiscourse--replace-hash type (lambda (x) (1+ (or x 0))) counts)
           (nndiscourse--append-header plst (nndiscourse--gethash
                                              (plist-get plst :category_id)
                                              nndiscourse-categories-hashtb))))
       posts)
   until (or (not nndiscourse--last-id) (member nndiscourse--last-id posts))
   finally do
   (progn
     (setq nndiscourse--last-id (plist-get (elt posts 0) :id))
     (gnus-message
      5 (concat "nndiscourse--incoming: "
                (format "%d requests, " (length nndiscourse--debug-request-posts))
                (let ((result ""))
                  (nndiscourse--maphash
                   (lambda (key value)
                     (setq result (concat result (format "%s +%s " value key))))
                   counts)
                  result))))))

(deffoo nndiscourse-request-scan (&optional _group server)
  (when server
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

(defsubst nndiscourse--make-references (id)
  "Construct a space delimited string of message ancestors of ID."
  (mapconcat (lambda (ref) (nndiscourse--make-message-id ref))
             (nndiscourse-refs-for id) " "))

(defsubst nndiscourse--make-header (server group article-number)
  "In context of SERVER GROUP, construct full headers of article indexed ARTICLE-NUMBER."
  (let* ((header (nndiscourse--get-header server group article-number))
         (score (plist-get header :score))
         (reads (plist-get header :reads)))
    (make-full-mail-header
     article-number
     (plist-get header :topic_title)
     (plist-get header :username)
     (format-time-string "%a, %d %h %Y %T %z (%Z)" (plist-get header :created_at))
     (nndiscourse--make-message-id (plist-get header :id))
     (nndiscourse--make-references (plist-get header :id))
     0 0 nil
     (append `((X-Discourse-Name . ,(plist-get header :name)))
             `((X-Discourse-ID . ,(plist-get header :id)))
             `((X-Discourse-Permalink . ,(format "%s/t/%s/%s/%s"
                                                 server
                                                 (plist-get header :topic_slug)
                                                 (plist-get header :topic_id)
                                                 (plist-get header :id))))
             (and (integerp score)
                  `((X-Discourse-Score . ,(number-to-string score))))
             (and (integerp reads)
                  `((X-Discourse-Reads . ,(number-to-string reads))))))))

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
             (body (nndiscourse--massage (nndiscourse--get-body header))))
        (when body
          (insert
           "Newsgroups: " group "\n"
           "Subject: " (mail-header-subject mail-header)  "\n"
           "From: " (or (mail-header-from mail-header) "nobody") "\n"
           "Date: " (mail-header-date mail-header) "\n"
           "Message-ID: " (mail-header-id mail-header) "\n"
           "References: " (mail-header-references mail-header) "\n"
           "Content-Type: text/html; charset=utf-8" "\n"
           "Archived-at: " permalink "\n"
           "Score: " score "\n"
           "\n")
          (-when-let*
              ((parent (plist-get header :parent))
               (parent-author
                (or (nndiscourse--gethash parent nndiscourse-authors-hashtb)
                    "Someone"))
               (parent-body (nndiscourse--get-body (nndiscourse-find-header parent server))))
            (insert (nndiscourse--citation-wrap parent-author parent-body)))
          (aif (and nndiscourse-render-post (plist-get header :url))
              (condition-case err
                  (nndiscourse--request "nndiscourse-request-article" it
                                         :success (cl-function
                                                   (lambda (&key data &allow-other-keys)
                                                     (insert data))))
                (error (gnus-message 5 "nndiscourse-request-article: %s"
                                     (error-message-string err))
                       (insert body)))
            (insert body))
          (cons group article-number))))))

(deffoo nndiscourse-retrieve-headers (article-numbers &optional group server _fetch-old)
  (nndiscourse--with-group server group
    (with-current-buffer nntp-server-buffer
      (erase-buffer)
      (dolist (i article-numbers)
        (nnheader-insert-nov (nndiscourse--make-header server group i)))
      'nov)))

(deffoo nndiscourse-request-list (&optional server)
  (let (groups)
    (when server
      (with-current-buffer nntp-server-buffer
        (erase-buffer)
        (nndiscourse-request-scan nil server)
        (mapc (lambda (plst)
                (let* ((id (plist-get plst :id))
                       (group (plist-get plst :slug))
                       (full-name (gnus-group-full-name group `(nndiscourse ,server))))
                  (gnus-activate-group full-name)
                  (gnus-group-unsubscribe-group full-name gnus-level-default-subscribed t)
                  (nndiscourse--sethash id group nndiscourse-categories-hashtb)
                  (insert (format "%s %d 1 y\n" group
                                  (length (nndiscourse-get-headers group))))
                  (push group groups)))
              (nndiscourse-get-categories server))))
    (nreverse groups)))

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
               (url (plist-get (nndiscourse--get-header
                                (nnoo-current-server 'nndiscourse)
                                (gnus-group-real-name (car group-article))
                                (cdr group-article))
                           :url)))
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
  (let* ((header (nndiscourse--get-header
                  (nnoo-current-server 'nndiscourse)
                  (gnus-group-real-name (car gnus-article-current))
                  (cdr gnus-article-current)))
         (body (nndiscourse--massage (nndiscourse--get-body header))))
    (with-current-buffer gnus-original-article-buffer
      (article-goto-body)
      (delete-region (point) (point-max))
      (insert body))))

(defalias 'nndiscourse--display-article
  (lambda (article &optional all-headers _header)
    (condition-case err
        (gnus-article-prepare article all-headers)
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

;; I believe I did try buffer-localizing hooks, and it wasn't sufficient
(add-hook 'gnus-article-mode-hook 'nndiscourse-article-mode-activate)
(add-hook 'gnus-summary-mode-hook 'nndiscourse-summary-mode-activate)

;; "Can't figure out hook that can remove itself (quine conundrum)"
(add-function :around (symbol-function 'gnus-summary-exit)
              (lambda (f &rest args)
                (let ((gnus-summary-next-group-on-exit
                       (if (nndiscourse--gate) nil
                         gnus-summary-next-group-on-exit)))
                  (apply f args))))

;; `gnus-newsgroup-p' requires valid method post-mail to return t
(add-to-list 'gnus-valid-select-methods '("nndiscourse" post-mail) t)

(add-function
 :around (symbol-function 'message-supersede)
 (lambda (f &rest args)
   (cond ((nndiscourse--gate)
          (add-function :override
                        (symbol-function 'mml-insert-mml-markup)
                        'ignore)
          (condition-case err
              (prog1 (apply f args)
                (remove-function (symbol-function 'mml-insert-mml-markup) 'ignore)
                (save-excursion
                  (save-restriction
                    (message-replace-header "From" (message-make-from))
                    (message-goto-body)
                    (narrow-to-region (point) (point-max))
                    (goto-char (point-max))
                    (mm-inline-text-html nil)
                    (delete-region (point-min) (point)))))
            (error (remove-function (symbol-function 'mml-insert-mml-markup) 'ignore)
                   (error (error-message-string err)))))
         (t (apply f args)))))

(add-function
 :around (symbol-function 'message-send-news)
 (lambda (f &rest args)
   (cond ((nndiscourse--gate)
          (let* ((dont-ask (lambda (prompt)
                             (when (cl-search "mpty article" prompt) t)))
                 (link-p (not (null (message-fetch-field "Link"))))
                 (message-shoot-gnksa-feet (if link-p t message-shoot-gnksa-feet)))
            (condition-case err
                (progn
                  (when link-p
                    (add-function :before-until (symbol-function 'y-or-n-p) dont-ask))
                  (prog1 (apply f args)
                    (remove-function (symbol-function 'y-or-n-p) dont-ask)))
              (error (remove-function (symbol-function 'y-or-n-p) dont-ask)
                     (error (error-message-string err))))))
         (t (apply f args)))))

(add-function
 :around (symbol-function 'gnus-summary-post-news)
 (lambda (f &rest args)
   (cond ((nndiscourse--gate)
          (let* ((nndiscourse-post-type (read-char-choice "[l]ink / [t]ext: " '(?l ?t)))
                 (link-header (apply-partially #'message-add-header "Link: https://"))
                 (add-link-header (apply-partially #'add-hook
                                                   'message-header-setup-hook
                                                   link-header))
                 (remove-link-header (apply-partially #'remove-hook
                                                      'message-header-setup-hook
                                                      link-header)))
            (cl-case nndiscourse-post-type
              (?l (funcall add-link-header)))
            (condition-case err
                (progn
                  (apply f args)
                  (funcall remove-link-header))
              (error (funcall remove-link-header)
                     (error (error-message-string err))))))
         (t (apply f args)))))

(defsubst nndiscourse--who-am-i ()
  "Get my Discourse username."
  "dickmao")

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

(add-function
 :around (symbol-function 'message-is-yours-p)
 (lambda (f &rest args)
   (let ((concat-func (lambda (f &rest args)
                       (let ((fetched (apply f args)))
                         (if (string= (car args) "from")
                             (concat fetched "@discourse.org")
                           fetched)))))
     (when (nndiscourse--gate)
       (add-function :around
                     (symbol-function 'message-fetch-field)
                     concat-func))
     (condition-case err
         (prog1 (apply f args)
           (remove-function (symbol-function 'message-fetch-field) concat-func))
       (error (remove-function (symbol-function 'message-fetch-field) concat-func)
              (error (error-message-string err)))))))

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

(unless (assoc "nndiscourse" gnus-valid-select-methods)
  (gnus-declare-backend "nndiscourse" 'post-mail 'address))

(provide 'nndiscourse)

;;; nndiscourse.el ends here
