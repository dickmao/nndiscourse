;;; nndiscourse.el --- Gnus backend for Discourse  -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2019 The Authors of nndiscourse.el

;; Authors: dickmao <github id: dickmao>
;; Version: 0.1.0
;; Keywords: news
;; URL: https://github.com/dickmao/nndiscourse
;; Package-Requires: ((emacs "25") (request "20190819") (dash "20190401") (anaphora "20180618"))

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

(eval-when-compile (require 'cl-lib)
                   (cl-assert (fboundp 'libxml-parse-html-region) nil
                              "nndiscourse requires emacs built with libxml support"))

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
(require 'request)
(require 'dash)
(require 'anaphora)
(require 'url-http)

(nnoo-declare nndiscourse)

(defcustom nndiscourse-public-keyfile (expand-file-name "~/.ssh/id_rsa.pub")
  "Location of rsa private key."
  :type '(file :must-match t)
  :group 'nnhackernews)

(defcustom nndiscourse-localhost "127.0.0.1"
  "Some users keep their browser in a separate domain."
  :type 'string
  :group 'nndiscourse)

(defcustom nndiscourse-localhost 8999
  "Some users use 8999 for something else."
  :type 'integer
  :group 'nndiscourse)

(defvoo nndiscourse-status-string "")

(defvar nndiscourse--last-id nil "Keep track of where we are.")

(defsubst nndiscourse--gethash (string hashtable &optional dflt)
  "Get corresponding value of STRING from HASHTABLE, or DFLT if undefined.

Starting in emacs-src commit c1b63af, Gnus moved from obarrays to normal hashtables."
  (if (fboundp 'gnus-gethash)
      (let ((sym (intern-soft string hashtable)))
        (if (or (null sym) (not (boundp sym))) dflt (symbol-value sym)))
    (gethash string hashtable dflt)))

(defsubst nndiscourse--replace-hash (string func hashtable)
  "Set value of STRING to FUNC applied to existing STRING value in HASHTABLE.

Starting in emacs-src commit c1b63af, Gnus moved from obarrays to normal hashtables."
  (let* ((capture (nndiscourse--gethash string hashtable))
         (replace-with (funcall func capture)))
    (if (fboundp 'gnus-sethash)
       (set (intern string hashtable) replace-with)
      (puthash string replace-with hashtable))))

(defmacro nndiscourse--remhash (string hashtable)
  "Remove STRING from HASHTABLE.

Starting in emacs-src commit c1b63af, Gnus moved from obarrays to normal hashtables."
  `(,(if (fboundp 'gnus-sethash)
         'unintern
       'remhash)
    ,(if (fboundp 'gnus-sethash)
         (cons 'intern (list string hashtable))
       string)
     ,hashtable))

(defmacro nndiscourse--sethash (string value hashtable)
  "Set corresponding value of STRING to VALUE in HASHTABLE.

Starting in emacs-src commit c1b63af, Gnus moved from obarrays to normal hashtables."
  `(,(if (fboundp 'gnus-sethash)
         'gnus-sethash
       'puthash)
    ,string ,value ,hashtable))

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
  :lighter " HN"
  :keymap nndiscourse-article-mode-map)

(define-minor-mode nndiscourse-summary-mode
  "Disallow \"reply\" commands in `gnus-summary-mode-map'.

\\{nndiscourse-summary-mode-map}
"
  :lighter " HN"
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

(defmacro nndiscourse--normalize-server ()
  "Disallow \"server\" from being empty string, which is unsettling.
Normalize it to \"nndiscourse-default\"."
  `(let ((canonical "nndiscourse-default"))
    (when (equal server "")
      (setq server nil))
    (unless server
      (setq server canonical))
    (unless (string= server canonical)
      (error "nndiscourse--normalize-server: multiple servers unsupported!"))))

(defvar nndiscourse-score-files nil "For `nndiscourse--ensure-score-file'.")

(defvar nndiscourse-location-hashtb (gnus-make-hashtable)
  "Id -> ( group . index ).")

(defvar nndiscourse-root-hashtb (gnus-make-hashtable)
  "Id -> possibly ancient header.")

(defvar nndiscourse-headers-hashtb (gnus-make-hashtable)
  "Group -> headers.")

(defvar nndiscourse-refs-hashtb (gnus-make-hashtable)
  "Who replied to whom (global over all entries).")

(defvar nndiscourse-authors-hashtb (gnus-make-hashtable)
  "For fast lookup of parent-author (global over all entries).")

(defun nndiscourse-first-to-succeed (&rest commands)
  "Return output of first command among COMMANDS to succeed, NIL if none."
  (let (conds)
    (dolist (c
             (nreverse commands)
             (eval `(with-temp-buffer
                      ,(cons 'cond conds))))
      (push `((let ((_ (erase-buffer))
                    (rv (apply #'call-process
                               ,(substring c 0 (search " " c))
                               nil (quote (t nil)) nil
                               (split-string ,(aif (search " " c) (substring c (1+ it)) "")))))
                (and (numberp rv) (zerop rv)))
              (buffer-string))
            conds))))

(defun nndiscourse-rpc-get (&optional server)
  "Retrieve the Jimson process for SERVER."
  (nndiscourse--normalize-server)
  (let ((proc (get-buffer-process (get-buffer-create (format " *%s*" server)))))
    (unless proc
      (let* ((ruby-command (append (list (executable-find "bundle") "exec" "rpc"))))
        (when nndiscourse-log-rpc
          (setq nndiscourse-rpc-log-filename
                (concat (file-name-as-directory temporary-file-directory)
                        "nndiscourse-rpc-log."))
          (setq ruby-command (append ruby-command
                                     (list "--log" nndiscourse-rpc-log-filename))))
        (setq proc (make-process :name server
                                 :buffer (get-buffer-create (format " *%s*" server))
                                 :command ruby-command
                                 :connection-type 'pipe
                                 :noquery t
                                 :sentinel #'nndiscourse-sentinel
                                 :stderr (get-buffer-create (format " *%s-stderr*" server))))
        (with-current-buffer (get-buffer-create (format " *%s-stderr*" server))
          (add-hook 'after-change-functions
                    (apply-partially 'nndiscourse--message-user server)
                    nil t)))
      (push proc nndiscourse-processes))
    proc))

(defsubst nndiscourse-get-headers (group)
  "List headers from GROUP."
  (nndiscourse--gethash group nndiscourse-headers-hashtb))

(defun nndiscourse-find-header (id &optional noquery)
  "Retrieve property list of ID.

If NOQUERY, return nil and avoid querying if not extant."
  (if-let ((location (nndiscourse--gethash id nndiscourse-location-hashtb)))
      (cl-destructuring-bind (group . index) location
        (nndiscourse--get-header (1+ index) group))
    (unless noquery
      (nndiscourse--request-item id))))

(defsubst nndiscourse-refs-for (id)
  "Return descending ancestors as list for ID."
  (cl-loop with root-plst
           for prev-id = id then cur-id
           for cur-id =
           (let ((cached (nndiscourse--gethash prev-id
                                                nndiscourse-refs-hashtb
                                                'NULL)))
             (if (eq cached 'NULL)
                 (progn (setq root-plst (nndiscourse--request-item prev-id))
                        (nndiscourse-add-entry nndiscourse-refs-hashtb
                                                root-plst :parent)
                        (plist-get root-plst :parent))
               (setq root-plst (nndiscourse-find-header prev-id t))
               cached))
           until (null cur-id)
           collect cur-id into rresult
           finally do
           (let ((result (nreverse rresult)))
             (when (and result
                        (string= (plist-get root-plst :id) (car result)))
               (nndiscourse--sethash (car result) root-plst
                                      nndiscourse-root-hashtb))
             (cl-return result))))

(defun nndiscourse--retrieve-root (header)
  "Retrieve and cache property list HEADER root."
  (if-let ((root-id (car (nndiscourse-refs-for (plist-get header :id)))))
      (nndiscourse--gethash root-id nndiscourse-root-hashtb)
    header))

(defun nndiscourse--group-for (header)
  "Classify HEADER as one of ask, show, or news based on title."
  (let* ((root-plst (nndiscourse--retrieve-root header))
         (title (or (plist-get header :link_title) ""))
         (type (or (plist-get root-plst :type) "")))
    ;; string-match-p like all elisp searching is case-insensitive
    (cond ((string= type "job") nndiscourse--group-job)
          ((string-match-p "^\\(Launch\\|Show\\) HN" title) nndiscourse--group-show)
          ((string-match-p "^\\(Ask\\|Tell\\) HN" title) nndiscourse--group-ask)
          (t nndiscourse--group-stories))))

(defsubst nndiscourse--domify (html)
  "Parse HTML into dom."
  (with-temp-buffer
    (insert html)
    (if (fboundp 'libxml-parse-html-region)
        (libxml-parse-html-region (point-min) (point-max))
      (error "nndiscourse--domify: need libxml-parse-html-region"))))

(cl-defun nndiscourse--request-login-success (&key data &allow-other-keys)
  "After some time, logging in via browser recaptcha might be necessary.

Remember `string-match-p' is always case-insensitive as is all elisp pattern matching."
  (when (and (stringp data)
             (string-match-p (regexp-quote "validation required") data))
    (display-warning
     'nndiscourse
     (concat "Recaptcha required.  Please login via browser and try again."))
    (error "Recaptcha required"))
  data)

(defun nndiscourse--request-login (url &optional hidden)
  "Store a cookie from URL with HIDDEN plist."
  (let* (result
         (auth-source-do-cache nil)
         (auth-source-creation-prompts '((user . "news.ycombinator.com user: ")
                                         (secret . "Password for %u: ")))
         (found (car (auth-source-search :max 1 :host "news.ycombinator.com" :require
                                         '(:user :secret) :create t))))
    (nndiscourse--request
     "nndiscourse--request-login"
     url
     :backend 'curl
     :data (append
            (cl-loop for (k v) on hidden by (function cddr)
                     collect (cons (cl-subseq (symbol-name k) 1) v))
            `(("acct" . ,(plist-get found :user))
              ("pw" . ,(let ((secret (plist-get found :secret)))
                         (if (functionp secret)
                             (funcall secret)
                           secret)))))
     :success (nndiscourse--callback result #'nndiscourse--request-login-success))
    result))

(defmacro nndiscourse--extract-hidden (dom hidden)
  "Extract hidden tag-value pairs from DOM into plist HIDDEN."
  `(-tree-map-nodes
    (lambda (x)
      (and (listp x)
           (eq (car x) 'input)
           (string= "hidden" (alist-get 'type (cl-second x)))
           (not (string= "creating" (alist-get 'name (cl-second x))))))
    (lambda (x)
      (let ((keyname (intern (concat ":" (alist-get 'name (cl-second x))))))
        (unless (plist-get ,hidden keyname)
          (!cons (alist-get 'value (cl-second x)) ,hidden)
          (!cons keyname ,hidden))))
    ,dom))

(cl-defun nndiscourse--request-hidden-success (&key data response &allow-other-keys)
  "If necessary, login first, then return plist of :fnid and :fnop."
  (let* ((dom (nndiscourse--domify data))
         (form (car (alist-get 'form (alist-get 'body dom))))
         (url (request-response-url response))
         (path (car (url-path-and-query (url-generic-parse-url url))))
         (login-p (aif (alist-get 'action form) (cl-search it path)))
         hidden)
    (nndiscourse--extract-hidden dom hidden)
    (when login-p
      (setq dom (nndiscourse--domify (nndiscourse--request-login url hidden)))
      (nndiscourse--extract-hidden dom hidden))
    hidden))

(defun nndiscourse--request-hidden (url)
  "Get the hidden fields (e.g., FNID, FNOP, HMAC) from URL."
  (let (result)
    (nndiscourse--request
     "nndiscourse--request-hidden"
     url
     :backend 'curl
     :success (nndiscourse--callback result #'nndiscourse--request-hidden-success))
    result))

(defsubst nndiscourse--append-header (plst &optional group)
  "Update data structures for PLST \"header\".

If GROUP classification omitted, figure it out."
  (let* ((id (plist-get plst :id))
         (group (or group (nndiscourse--group-for plst))))
    (nndiscourse--sethash id
                           (cons group (length (nndiscourse-get-headers group)))
                           nndiscourse-location-hashtb)
    (nndiscourse--sethash group (nconc (nndiscourse-get-headers group) (list plst))
                           nndiscourse-headers-hashtb)
    plst))

(nnoo-define-basics nndiscourse)

(defun nndiscourse-vote-current-article (vote)
  "VOTE is +1, -1, 0."
  (unless gnus-newsgroup-name (error "No current newgroup"))
  (if-let ((article-number (or (cdr gnus-article-current)
                               (gnus-summary-article-number))))
      (let* ((header (nndiscourse--get-header article-number
                                           (gnus-group-real-name gnus-newsgroup-name)))
             (orig-score (format "%s" (plist-get header :score)))
             (new-score (if (zerop vote) orig-score
                          (concat orig-score " "
                                  (if (> vote 0) "+" "")
                                  (format "%s" vote)))))
        (save-excursion
          (save-window-excursion
            (with-current-buffer gnus-summary-buffer
              (if (eq (gnus-summary-article-number) (cdr gnus-article-current))
                  (if (nndiscourse--request-vote (plist-get header :id) vote)
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

(deffoo nndiscourse-server-opened (&optional server)
  (nndiscourse--normalize-server)
  t)

(deffoo nndiscourse-status-message (&optional server)
  (nndiscourse--normalize-server)
  "")

(deffoo nndiscourse-open-server (_server &optional _defs)
  t)

(deffoo nndiscourse-close-group (_group &optional server)
  (nndiscourse--normalize-server)
  t)

(defmacro nndiscourse--with-group (group &rest body)
  "Disambiguate GROUP if it's empty and execute BODY."
  (declare (debug (form &rest form))
           (indent 1))
  `(let* ((group (or ,group (gnus-group-real-name gnus-newsgroup-name)))
          (gnus-newsgroup-name (gnus-group-full-name group "nndiscourse:")))
     ,@body))

(defun nndiscourse--get-header (article-number &optional group)
  "Get header indexed ARTICLE-NUMBER for GROUP."
  (nndiscourse--with-group group
    (let ((headers (nndiscourse-get-headers group)))
      (elt headers (1- article-number)))))

(defun nndiscourse--get-body (header &optional server)
  "Get full text of submission or comment HEADER at SERVER."
  (nndiscourse--normalize-server)
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
  "Add to HASHTB the pair consisting of entry E's name to its FIELD."
  (nndiscourse--sethash (plist-get e :id) (plist-get e field) hashtb))

(defsubst nndiscourse--summary-exit ()
  "Call `gnus-summary-exit' without the hackery."
  (remove-function (symbol-function 'gnus-summary-exit)
                   (symbol-function 'nndiscourse--score-pending))
  (gnus-summary-exit)
  (add-function :after (symbol-function 'gnus-summary-exit)
                (symbol-function 'nndiscourse--score-pending)))

(defsubst nndiscourse--ensure-score-files (group)
  "File I/O remains a perennial problem for score files for GROUP."
  (if-let ((files (alist-get (intern group) nndiscourse-score-files)))
      (condition-case nil
        (progn
          (dolist (file files t)
            (gnus-score-load-score-alist file)))
        (error nil))
    t))

(defsubst nndiscourse--rescore (group &optional force)
  "Can't figure out GROUP hook that can remove itself (quine conundrum).

FORCE is generally t unless coming from `nndiscourse--score-pending'."
  (when (nndiscourse--gate group)
    (cl-loop repeat 5
             for ensured = (nndiscourse--ensure-score-files group)
             until ensured
             do (sleep-for 0 300)
             finally (unless ensured
                       (gnus-message 2 "nndiscourse--rescore: Bad score files %s!"
                                     (alist-get (intern group)
                                                nndiscourse-score-files))))
    (let* ((num-headers (length (nndiscourse-get-headers
                                 (gnus-group-real-name group))))
           (marks (gnus-info-marks (nth 2 (gnus-group-entry group))))
           (seen (or (cdr (--max-by (> (or (cdr it) 0) (or (cdr other) 0))
                                    (alist-get 'seen marks)))
                     0)))
      (unless (zerop seen)
        (when (or force (> num-headers seen))
          (save-excursion
            (let ((gnus-auto-select-subject nil)
                  (gnus-summary-next-group-on-exit nil))
              (gnus-summary-read-group group nil t)
              (nndiscourse--summary-exit))))))))

(defalias 'nndiscourse--score-pending
  (lambda (&rest _args) (nndiscourse--rescore (gnus-group-name-at-point))))

(defun nndiscourse--score-unread (group)
  "Filter unread messages for GROUP now.

Otherwise *Group* buffer annoyingly overrepresents unread."
  (nndiscourse--with-group group
    (let ((extant (get-buffer (gnus-summary-buffer-name gnus-newsgroup-name))))
      (unless extant
        (nndiscourse--rescore gnus-newsgroup-name t)))))

(defun nndiscourse--mark-scored-as-read (group)
  "If a root article (story) is scored in GROUP, that means we've already read it."
  (nndiscourse--with-group group
    (let ((preface (format "nndiscourse--mark-scored-as-read: %s not rescoring " group))
          (extant (get-buffer (gnus-summary-buffer-name gnus-newsgroup-name)))
          (unread (gnus-group-unread gnus-newsgroup-name)))
      (cond ((or (not (numberp unread)) (<= unread 0))
             (gnus-message 7 (concat preface "(unread %s)") unread))
            ((and extant (buffer-local-value 'gnus-newsgroup-prepared extant))
             ;; reflect the extant logic in `gnus-summary-setup-buffer'
             (gnus-message 7 (concat preface "(extant %s)") (buffer-name extant)))
            (t
             (save-excursion
               (let ((gnus-auto-select-subject nil))
                 (gnus-summary-read-group gnus-newsgroup-name nil t)
                 (dolist (datum gnus-newsgroup-data)
                   (-when-let* ((article (gnus-data-number datum))
                                (plst (nndiscourse--get-header article))
                                (scored-story-p (and (plist-get plst :title)
                                                     (> (gnus-summary-article-score article) 0))))
                     (gnus-message 7 "nndiscourse--mark-scored-as-read: %s (%s %s)"
                                   (plist-get plst :title) group article)
                     (gnus-summary-mark-as-read article)))
                 (nndiscourse--summary-exit))))))))

(deffoo nndiscourse-request-group-scan (group &optional server info)
  "M-g from *Group* calls this."
  (nndiscourse--normalize-server)
  (nndiscourse--with-group group
    (gnus-message 5 "nndiscourse-request-group-scan: scanning %s..." group)
    (gnus-activate-group gnus-newsgroup-name t)
    (gnus-get-unread-articles-in-group
     (or info (gnus-get-info gnus-newsgroup-name))
     (gnus-active (gnus-info-group info)))
    (gnus-message 5 "nndiscourse-request-group-scan: scanning %s...done" group)
    (nndiscourse--score-unread group))
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
  (nndiscourse--normalize-server)
  (nndiscourse--with-group group
    (let* ((info (or info (gnus-get-info gnus-newsgroup-name)))
           (headers (nndiscourse-get-headers group))
           (num-headers (length headers))
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

(cl-defun nndiscourse--request (caller url
                                 &rest attributes &key parser (backend 'url-retrieve)
                                 &allow-other-keys)
  "Prefix errors with CALLER when executing synchronous request to URL."
  (unless parser
    (setq attributes (append attributes (list :parser #'buffer-string))))
  (setq attributes
        (cl-loop for (k v) on attributes by (function cddr)
                 unless (eq k :backend)
                 collect k and collect v))
  (let ((request-backend backend))
    (apply #'request url
           :sync t
           :error (apply-partially #'nndiscourse--request-error caller)
           attributes)))

(cl-defun nndiscourse--request-vote-success (item vote &key data &allow-other-keys)
  "If necessary, login first, then locate vote link depending on VOTE sign."
  (let* ((dom (nndiscourse--domify data))
         (before (format "%s_%s" (if (> vote 0) "up" "un") item))
         (after (format "%s_%s" (if (<= vote 0) "up" "un") item))
         before-found result)
    (-tree-map-nodes
     (lambda (x)
       (and (listp x)
            (eq (car x) 'a)
            (let ((id (alist-get 'id (cl-second x))))
              (or (string= before id) (string= after id)))))
     (lambda (x)
       (let ((id (alist-get 'id (cl-second x)))
             (href (alist-get 'href (cl-second x))))
         (cond ((string= before id)
                (setq before-found (concat (file-name-as-directory nndiscourse-hacker-news-url) href)))
               ((string= after id) (setq result t)))))
     dom)
    (when before-found
      (nndiscourse--request
       "nndiscourse--request-vote-success"
       before-found
       :backend 'curl
       :success (nndiscourse--callback
                 result
                 (cl-function
                  (lambda (&key data response &allow-other-keys)
                    (let* ((dom (nndiscourse--domify data))
                           (form (alist-get 'form (alist-get 'body dom)))
                           (url (request-response-url response))
                           hidden)
                      (nndiscourse--extract-hidden dom hidden)
                      (when (string= "vote" (alist-get 'action (car form)))
                        (setq dom (nndiscourse--domify
                                   (nndiscourse--request-login url hidden))))
                      (let (result0)
                        (-tree-map-nodes
                         (lambda (x)
                           (and (listp x)
                                (eq (car x) 'a)
                                (string= after (alist-get 'id (cl-second x)))))
                         (lambda (_x) (setq result0 t))
                         dom)
                        result0)))))))
    result))

(defun nndiscourse--request-vote (item vote)
  "Tally for ITEM the VOTE."
  (let (result)
    (nndiscourse--request
     "nndiscourse--request-vote"
     (format "%s/item?id=%s" nndiscourse-hacker-news-url item)
     :backend 'curl
     :success (nndiscourse--callback
               result
               (apply-partially #'nndiscourse--request-vote-success item vote)))
    result))

(defsubst nndiscourse--enforce-curl ()
  "Curl must exist."
  (unless (executable-find "curl")
    (error "nndiscourse--enforce-curl: the 'curl' program was not found")))

(defun nndiscourse--request-reply (url text hidden)
  "Reply URL with TEXT using HIDDEN credentials."
  (nndiscourse--enforce-curl)
  (let (result)
    (nndiscourse--request
     "nndiscourse--request-reply"
     url
     :backend 'curl
     :data (append (cl-loop for (k v) on hidden by (function cddr)
                            collect (cons (cl-subseq (symbol-name k) 1) v))
                   `(("text" . ,text)))
     :success (nndiscourse--callback result))
    result))

(defun nndiscourse--request-edit (_item _body)
  "Replace body of ITEM with BODY."
  (let (result)
    ;; (nndiscourse--request
    ;;  "nndiscourse--request-edit"
    ;;  (format "%s/delete-confirm?%s"
    ;;          nndiscourse-hacker-news-url
    ;;          (url-build-query-string
    ;;           (cons `(id ,item)
    ;;                 (when root `((goto ,(format "item?id=%s" root)))))))
    ;;  :backend 'curl
    ;;  :success (nndiscourse--callback result #'nndiscourse--request-delete-success))
    result))

(defun nndiscourse--request-delete (item &optional root)
  "Cancel ITEM at root ROOT."
  (let (result)
    (nndiscourse--request
     "nndiscourse--request-delete"
     (format "%s/delete-confirm?%s"
             nndiscourse-hacker-news-url
             (url-build-query-string
              (cons `(id ,item)
                    (when root `((goto ,(format "item?id=%s" root)))))))
     :backend 'curl
     :success (nndiscourse--callback result #'nndiscourse--request-delete-success))
    result))

(cl-defun nndiscourse--request-error (caller
                                       &key response symbol-status error-thrown
                                       &allow-other-keys
                                       &aux (response-status
                                             (request-response-status-code response)))
  "Refer to CALLER when reporting a submit error."
  (gnus-message 3 "%s %s: http status %s, %s" caller symbol-status response-status
                (error-message-string error-thrown)))

(cl-defun nndiscourse--request-submit-success
    (caller posturl postdata retry &key data response &allow-other-keys)
  "If necessary, login, then \"goto\" fields take us to target.

And if accused of being a bot, retry with CALLER, POSTURL, POSTDATA (and toggle RETRY)."
  (let* ((dom (nndiscourse--domify data))
         (form (car (alist-get 'form (alist-get 'body dom))))
         (url (request-response-url response))
         (path (car (url-path-and-query (url-generic-parse-url url))))
         (login-p (aif (alist-get 'action form) (cl-search it path)))
         (result data))
    (when login-p
      (let (hidden)
        (nndiscourse--extract-hidden dom hidden)
        (setq result (nndiscourse--request-login url hidden))))
    (-tree-map-nodes
     (lambda (x)
       (and (listp x)
            (eq (car x) 'td)
            (stringp (cl-third x))
            (string-match-p (regexp-quote "try again") (cl-third x))))
     (lambda (_x)
       (if retry
           (let (hidden)
             (nndiscourse--extract-hidden dom hidden)
             (setq result (nndiscourse--request-submit caller posturl postdata hidden nil)))
         (setq result nil)
         (setq nndiscourse-status-string "Retried and failed")))
     dom)
    result))

(defun nndiscourse--request-submit (caller posturl postdata hidden retry)
  "Submit from CALLER to POSTURL the POSTDATA with HIDDEN credentials.

Bool RETRY is non-nil on first attempt.
Factor out commonality between text and link submit."
  (nndiscourse--enforce-curl)
  (let (result)
    (nndiscourse--request
     caller
     posturl
     :backend 'curl
     :data (append (cl-loop for (k v) on hidden by (function cddr)
                            collect (cons (cl-subseq (symbol-name k) 1) v))
                   postdata)
     :success (nndiscourse--callback
               result
               (apply-partially #'nndiscourse--request-submit-success
                                caller posturl postdata retry)))
    result))

(defsubst nndiscourse--request-submit-link (url title link hidden)
  "Submit to URL the TITLE with LINK and HIDDEN credentials."
  (nndiscourse--request-submit "nndiscourse--request-submit-link"
                                url
                                `(("title" . ,title) ("url" . ,link) ("text" . ""))
                                hidden t))

(defsubst nndiscourse--request-submit-text (url title text hidden)
  "Submit to URL the TITLE with TEXT and HIDDEN credentials."
  (nndiscourse--request-submit "nndiscourse--request-submit-text"
                                url
                                `(("title" . ,title) ("url" . "") ("text" . ,text))
                                hidden t))

(defun nndiscourse--request-item (id)
  "Retrieve ID as a property list."
  (let ((rpc (json-rpc-connect nndiscourse-localhost nndiscourse-port))
        (utf-decoder (lambda (x)
                       (decode-coding-string (with-temp-buffer
                                               (set-buffer-multibyte nil)
                                               (insert x)
                                               (buffer-string))
                                             'utf-8))))
    (add-function :filter-return (symbol-function 'json-read-string) utf-decoder)
    (condition-case err
        (prog1 (json-rpc rpc "get_post" id)
          (remove-function (symbol-function 'json-read-string) utf-decoder))
      (error (gnus-message 3 "nndiscourse--request-item: %s" (error-message-string err))
             (remove-function (symbol-function 'json-read-string) utf-decoder)
             nil))))

(defun nndiscourse--select-items (start-item max-id all-stories)
  "Return a list of items to retrieve between START-ITEM and MAX-ID.

Since we are constrained by `nndiscourse-max-ids-per-scan', we prioritize
ALL-STORIES and may throw away comments, etc."
  (mapcar
   #'number-to-string
   (if (> (1+ (- max-id start-item)) nndiscourse-max-ids-per-scan)
       (let* ((stories (seq-take-while (lambda (x) (>= x start-item))
                                       all-stories))
              (excess (- nndiscourse-max-ids-per-scan (length stories))))
         (if (<= excess 0)
             (nreverse (cl-subseq stories 0 nndiscourse-max-ids-per-scan))
           (cl-loop with excess-count = 0
                    with j = 0
                    for i from max-id downto start-item by 1
                    until (or (>= excess-count excess) (>= j (length stories)))
                    if (= i (elt stories j))
                    do (cl-incf j)
                    else
                    do (cl-incf excess-count)
                    end
                    collect i into result
                    finally return (nreverse (append result (nthcdr j stories)
                                                     (cl-loop for k from 0 below
                                                              (- excess excess-count) by 1
                                                              collect (- i k)))))))
     (cl-loop for i from start-item to max-id by 1
              collect i))))

(defun nndiscourse--incoming (&optional static-max-id static-newstories)
  "Drink from the firehose.

Optionally provide STATIC-MAX-ID and STATIC-NEWSTORIES to prevent querying out."
  (interactive)
  (setq nndiscourse--debug-request-items nil)
  (unless nndiscourse--last-id
    (mapc (lambda (group)
            (-when-let* ((full-name (gnus-group-full-name group "nndiscourse:"))
                         (info (gnus-get-info full-name)))
              (gnus-info-set-read info nil)
              (gnus-set-info full-name info)))
          `(,nndiscourse--group-ask
            ,nndiscourse--group-show
            ,nndiscourse--group-job
            ,nndiscourse--group-stories)))
  (when-let ((max-id (or static-max-id (nndiscourse--request-max-id))))
    (let* ((stories (or static-newstories (nndiscourse--request-newstories)))
           (earliest-story (nth (1- (min nndiscourse-max-ids-per-scan
                                         (length stories)))
                                stories))
           (start-item (if nndiscourse--last-id
                           (1+ nndiscourse--last-id)
                         (min earliest-story
                              (- max-id nndiscourse-max-ids-per-scan))))
           (counts (gnus-make-hashtable))
           (items (nndiscourse--select-items start-item max-id stories)))
      (dolist (item items)
        (-when-let* ((plst (nndiscourse--request-item item))
                     (not-deleted (not (plist-get plst :deleted)))
                     (type (plist-get plst :type)))
          (nndiscourse-add-entry nndiscourse-refs-hashtb plst :parent)
          (nndiscourse-add-entry nndiscourse-authors-hashtb plst :by)
          (nndiscourse--replace-hash type (lambda (x) (1+ (or x 0))) counts)
          (setq plst (plist-put plst :link_title
                                (or (plist-get
                                     (nndiscourse--retrieve-root plst)
                                     :title) "")))
          (cl-case (intern type)
            (job (nndiscourse--append-header plst nndiscourse--group-job))
            ((story comment) (nndiscourse--append-header plst))
            (otherwise (gnus-message 5 "nndiscourse-incoming: ignoring type %s" type)))))
      (setq nndiscourse--last-id max-id)
      (gnus-message
       5 (concat "nndiscourse--incoming: "
                 (format "%d requests, " (length nndiscourse--debug-request-items))
                 (let ((result ""))
                   (nndiscourse--maphash
                    (lambda (key value)
                      (setq result (concat result (format "%s +%s " value key))))
                    counts)
                   result))))))

(deffoo nndiscourse-request-scan (&optional group server)
  (nndiscourse--normalize-server)
  (when group
    (if (> 2 (- (truncate (float-time)) nndiscourse--last-scan-time))
        (gnus-message 7 "nndiscourse-request-scan: last scanned at %s"
                      (current-time-string nndiscourse--last-scan-time))
      (nndiscourse--with-group group
        (cl-destructuring-bind (seconds num-gc seconds-gc)
            (benchmark-run (nndiscourse--incoming))
          (setq nndiscourse--last-scan-time (truncate (float-time)))
          (gnus-message 5 (concat "nndiscourse-request-scan: Took %s seconds,"
                                  " with %s gc runs taking %s seconds")
                        seconds num-gc seconds-gc))))))

(defsubst nndiscourse--make-message-id (id)
  "Construct a valid Gnus message id from ID."
  (format "<%s@ycombinator.com>" id))

(defsubst nndiscourse--make-references (id)
  "Construct a space delimited string of message ancestors of ID."
  (mapconcat (lambda (ref) (nndiscourse--make-message-id ref))
             (nndiscourse-refs-for id) " "))

(defsubst nndiscourse--make-header (article-number &optional group)
  "Construct full headers of articled indexed ARTICLE-NUMBER in GROUP."
  (let* ((header (nndiscourse--get-header article-number group))
         (score (plist-get header :score))
         (num-comments (plist-get header :num_comments)))
    (make-full-mail-header
     article-number
     (replace-regexp-in-string "\\S-+ HN: " ""
                               (or (plist-get header :title)
                                   (plist-get header :link_title)))
     (plist-get header :by)
     (format-time-string "%a, %d %h %Y %T %z (%Z)" (plist-get header :time))
     (nndiscourse--make-message-id (plist-get header :id))
     (nndiscourse--make-references (plist-get header :id))
     0 0 nil
     (append `((X-Discourse-Name . ,(plist-get header :id)))
             `((X-Discourse-ID . ,(plist-get header :id)))
             `((X-Discourse-Permalink . ,(format "%s/item?id=%s" nndiscourse-hacker-news-url (plist-get header :id))))
             (and (integerp score)
                  `((X-Discourse-Score . ,(number-to-string score))))
             (and (integerp num-comments)
                  `((X-Discourse-Comments . ,(number-to-string num-comments))))))))

(deffoo nndiscourse-request-article (article-number &optional group server buffer)
  (nndiscourse--normalize-server)
  (unless buffer (setq buffer nntp-server-buffer))
  (nndiscourse--with-group group
    (with-current-buffer buffer
      (erase-buffer)
      (let* ((header (nndiscourse--get-header article-number group))
             (mail-header (nndiscourse--make-header article-number))
             (score (cdr (assq 'X-Discourse-Score (mail-header-extra mail-header))))
             (permalink (cdr (assq 'X-Discourse-Permalink (mail-header-extra mail-header))))
             (body (nndiscourse--massage (nndiscourse--get-body header server))))
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
               (parent-body (nndiscourse--get-body
                             (nndiscourse-find-header parent) server)))
            (insert (nndiscourse--citation-wrap parent-author parent-body)))
          (aif (and nndiscourse-render-story (plist-get header :url))
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
  (nndiscourse--normalize-server)
  (nndiscourse--with-group group
    (with-current-buffer nntp-server-buffer
      (erase-buffer)
      (dolist (i article-numbers)
        (nnheader-insert-nov (nndiscourse--make-header i group)))
      'nov)))

(deffoo nndiscourse-close-server (&optional server)
  (nndiscourse--normalize-server)
  t)

(deffoo nndiscourse-request-list (&optional server)
  (nndiscourse--normalize-server)
  (with-current-buffer nntp-server-buffer
    (erase-buffer)
    (mapc (lambda (group)
            (let ((full-name (gnus-group-full-name group `("nndiscourse" ,(or server "")))))
              (gnus-activate-group full-name t)
              (gnus-group-unsubscribe-group full-name gnus-level-default-subscribed t))
            (insert (format "%s %d 1 y\n" group
                            (length (nndiscourse-get-headers group)))))
          `(,nndiscourse--group-ask
            ,nndiscourse--group-show
            ,nndiscourse--group-job
            ,nndiscourse--group-stories)))
  t)

(defun nndiscourse-sentinel (process event)
  "Wipe headers state when PROCESS dies from EVENT."
  (unless (string= "open" (substring event 0 4))
    (gnus-message 2 "nndiscourse-sentinel: process %s %s"
                  (car (process-command process))
                  (replace-regexp-in-string "\n$" "" event))
    (setq nndiscourse-location-hashtb (gnus-make-hashtable))
    (gnus-backlog-shutdown)))

(defun nndiscourse--message-user (server beg end _prev-len)
  "Message SERVER related alert with `buffer-substring' from BEG to END."
  (let ((string (buffer-substring beg end))
        (magic "::user::"))
    (when (string-prefix-p magic string)
      (message "%s: %s" server (substring string (length magic))))))

(cl-defun nndiscourse--request-delete-success (&key data &allow-other-keys)
  "Delete with extreme prejudice."
  (let* ((dom (nndiscourse--domify data))
         hidden)
    (nndiscourse--extract-hidden dom hidden)
    (let (result)
      (nndiscourse--request
       "nndiscourse--request-delete-success"
       (format "%s/xdelete" nndiscourse-hacker-news-url)
       :backend 'curl
       :data (append (cl-loop for (k v) on hidden by (function cddr)
                              collect (cons (cl-subseq (symbol-name k) 1) v))
                     '(("d" . "Yes")))
       :success (nndiscourse--callback result))
      result)))

(defsubst nndiscourse--extract-unique (message-id)
  "Get unique from <unique@fqdn> in MESSAGE-ID."
  (when (string-match "\\(<[^>]+>\\)" message-id)
    (car (split-string
          (replace-regexp-in-string
           "[<>]" "" (match-string 1 message-id)) "@"))))

(defsubst nndiscourse--request-post-reply-url (headers)
  "Return hexified reply url from HEADERS."
  (let ((references (mail-header-references headers))
        (immediate (nndiscourse--extract-unique
                    (mail-header-message-id headers))))
    (if references
        (let ((root (nndiscourse--extract-unique
                     (car (split-string references " ")))))
          (format "%s/reply?%s"
                  nndiscourse-hacker-news-url
                  (url-build-query-string
                   `((id ,immediate) (goto ,(format "item?id=%s#%s"
                                                    root
                                                    immediate))))))
      (format "%s/comment?%s"
              nndiscourse-hacker-news-url
              (url-build-query-string
                   `((id ,immediate) (goto ,(format "item?id=%s"
                                                    immediate))))))))

(defmacro nndiscourse--set-status-string (dom)
  "Set `nndiscourse-status-string' to DOM remarks for benefit of `nnheader-report'."
  `(let ((body (alist-get 'body ,dom))
         remarks)
     (-tree-map-nodes
      (lambda (x)
        (and (listp x)
             (eq (car x) 'td)
             (stringp (cl-third x))))
      (lambda (x) (!cons (cl-third x) remarks))
      body)
     (setq nndiscourse-status-string
           (mapconcat #'string-trim remarks " "))))

;; C-c C-c from followup buffer
;; message-send-and-exit
;; message-send
;; message-send-method-alist=message-send-news-function=message-send-news
;; gnus-request-post
;; nndiscourse-request-post
(deffoo nndiscourse-request-post (&optional server)
  (nndiscourse--normalize-server)
  (-when-let* ((url (aif message-reply-headers
                        (nndiscourse--request-post-reply-url it)
                      (format "%s/submit" nndiscourse-hacker-news-url)))
               (hidden (nndiscourse--request-hidden url)))
    (let ((ret t)
          (title (or (message-fetch-field "Subject") (error "No Subject field")))
          (link (message-fetch-field "Link"))
          (reply-p (not (null message-reply-headers)))
          (edit-item (aif (message-fetch-field "Supersedes")
                         (nndiscourse--extract-unique it)))
          (cancel-item (aif (message-fetch-field "Control")
                           (nndiscourse--extract-unique it)))
          (body
           (save-excursion
             (save-restriction
               (message-goto-body)
               (narrow-to-region (point) (point-max))
               (buffer-string)))))
      (cond (cancel-item
             (let* ((header (nndiscourse-find-header cancel-item))
                    (result (nndiscourse--request-delete
                             cancel-item
                             (and (null (plist-get header :title))
                                  (plist-get (nndiscourse--retrieve-root header) :id))))
                    (dom (nndiscourse--domify result)))
               (cl-destructuring-bind (tag params &rest args) dom
                 (setq ret (and (eq tag 'html)
                                (string= (alist-get 'op params) "item")))
                 (unless ret (nndiscourse--set-status-string dom)))))
            (edit-item
             (let* ((result (nndiscourse--request-edit edit-item body))
                    (dom (nndiscourse--domify result)))
               (cl-destructuring-bind (tag params &rest args) dom
                 (setq ret (and (eq tag 'html)
                                (string= (alist-get 'op params) "item")))
                 (unless ret (nndiscourse--set-status-string dom)))))
            (reply-p
             (let* ((path (car (url-path-and-query (url-generic-parse-url url))))
                    (url (replace-regexp-in-string path "/comment" url))
                    (result (nndiscourse--request-reply url body hidden))
                    (dom (nndiscourse--domify result)))
               (cl-destructuring-bind (tag params &rest args) dom
                 (setq ret (and (eq tag 'html)
                                (string= (alist-get 'op params) "item")))
                 (unless ret (nndiscourse--set-status-string dom)))))
            (link
             (let* ((parsed-url (url-generic-parse-url link))
                    (host (url-host parsed-url))
                    (path (car (url-path-and-query (url-generic-parse-url url))))
                    (url (replace-regexp-in-string path "/r" url)))
               (if (and (stringp host) (not (zerop (length host))))
                   (setq ret (nndiscourse--request-submit-link url title link hidden))
                 (gnus-message 3 "nndiscourse-request-post: invalid url \"%s\"" link)
                 (setq ret nil))))
            (t
             (let* ((path (car (url-path-and-query (url-generic-parse-url url))))
                    (url (replace-regexp-in-string path "/r" url)))
               (setq ret (nndiscourse--request-submit-text url title body hidden)))))
      ret)))

(defun nndiscourse--browse-story (&rest _args)
  "What happens when I click on discourse Subject."
  (-when-let* ((group-article gnus-article-current)
               (url (plist-get (nndiscourse--retrieve-root
                                (nndiscourse--get-header
                                 (cdr group-article)
                                 (gnus-group-real-name (car group-article))))
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
            nndiscourse--browse-story 1)
          result)
    result))

(defsubst nndiscourse--fallback-link ()
  "Cannot render story."
  (let* ((header (nndiscourse--get-header
                  (cdr gnus-article-current)
                  (gnus-group-real-name (car gnus-article-current))))
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
       (if nndiscourse-render-story
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
                                  (gnus-use-cache nil)
                                  (gnus-use-adaptive-scoring (quote (line)))
                                  (gnus-newsgroup-adaptive t)
                                  (gnus-simplify-subject-functions (quote (gnus-simplify-subject-fuzzy)))

                                  (gnus-summary-line-format "%3t%U%R%uS %I%(%*%-10,10f  %s%)\n")
                                  (gnus-auto-extend-newsgroup nil)
                                  (gnus-add-timestamp-to-message t)
                                  (gnus-summary-display-article-function
                                   (quote ,(symbol-function 'nndiscourse--display-article)))
                                  (gnus-header-button-alist
                                   (quote ,(nndiscourse--header-button-alist)))
                                  (gnus-visible-headers ,(concat gnus-visible-headers "\\|^Score:")))))

(nnoo-define-skeleton nndiscourse)

(defun nndiscourse-article-mode-activate ()
  "Augment the `gnus-article-mode-map' conditionally."
  (when (nndiscourse--gate)
    (nndiscourse-article-mode)))

(defun nndiscourse-summary-mode-activate ()
  "Shadow some bindings in `gnus-summary-mode-map' conditionally."
  (when (nndiscourse--gate)
    (nndiscourse-summary-mode)))

;; I believe I did try buffer-localizing hooks, and it wasn't sufficient
(add-hook 'gnus-article-mode-hook 'nndiscourse-article-mode-activate)
(add-hook 'gnus-summary-mode-hook 'nndiscourse-summary-mode-activate)

;; Avoid having to select the GROUP to make the unread number go down.
(mapc (lambda (hook)
        (add-hook hook
                  (lambda () (mapc (lambda (group)
                                     (nndiscourse--score-unread group))
                                   `(,nndiscourse--group-ask
                                     ,nndiscourse--group-show
                                     ,nndiscourse--group-job
                                     ,nndiscourse--group-stories)))))
      '(gnus-started-hook gnus-after-getting-new-news-hook))

(add-hook 'gnus-started-hook
          (lambda () (mapc (lambda (group)
                             (nndiscourse--mark-scored-as-read group))
                           `(,nndiscourse--group-ask
                             ,nndiscourse--group-show
                             ,nndiscourse--group-job
                             ,nndiscourse--group-stories)))
          t)

;; "Can't figure out hook that can remove itself (quine conundrum)"
(add-function :around (symbol-function 'gnus-summary-exit)
              (lambda (f &rest args)
                (let ((gnus-summary-next-group-on-exit
                       (if (nndiscourse--gate) nil
                         gnus-summary-next-group-on-exit)))
                  (apply f args))))
(add-function :after (symbol-function 'gnus-summary-exit)
              (symbol-function 'nndiscourse--score-pending))

(add-function :before (symbol-function 'gnus-score-save)
              (lambda (&rest _)
                (when (nndiscourse--gate)
                  (setq nndiscourse-score-files
                        (assq-delete-all (intern gnus-newsgroup-name)
                                         nndiscourse-score-files)))))

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

(add-function
 :filter-return (symbol-function 'message-make-fqdn)
 (lambda (val)
   (if (and (nndiscourse--gate)
            (cl-search "--so-tickle-me" val))
       "ycombinator.com" val)))

(add-function
 :before-until (symbol-function 'message-make-from)
 (lambda (&rest _args)
   (when (nndiscourse--gate)
     (concat (nndiscourse--who-am-i) "@ycombinator.com"))))

(add-function
 :around (symbol-function 'message-is-yours-p)
 (lambda (f &rest args)
   (let ((concat-func (lambda (f &rest args)
                       (let ((fetched (apply f args)))
                         (if (string= (car args) "from")
                             (concat fetched "@ycombinator.com")
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

(let ((protect (lambda (caller)
                 (add-function
                  :around (symbol-function caller)
                  (lambda (f &rest args)
                    (cond ((nndiscourse--gate)
                           (condition-case err
                               (apply f args)
                             (error (gnus-message 7 "%s: %s"
                                                  caller
                                                  (error-message-string err)))))
                          (t (apply f args))))))))
  (funcall protect 'url-http-generic-filter)
  (funcall protect 'url-http-end-of-document-sentinel))

;; Make the scoring entries Markovian
(add-function
 :around (symbol-function 'gnus-summary-score-entry)
 (lambda (f header match &rest args)
   (cond ((nndiscourse--gate)
          (let* ((new-touched
                  (let ((gnus-score-alist (copy-alist '((touched nil)))))
                    (cons (apply f header match args)
                          (cl-some #'identity (gnus-score-get 'touched)))))
                 (new (car new-touched))
                 (touched (cdr new-touched)))
            (when (and touched new)
              (-if-let* ((old (gnus-score-get header))
                         (elem (assoc match old))
                         (match-type (eq (nth 3 elem) (nth 3 new)))
                         (match-date (or (and (numberp (nth 2 elem)) (numberp (nth 2 new)))
                                         (and (not (nth 2 elem)) (not (nth 2 new))))))
                  (setcar (cdr elem) (nth 1 new))
                (gnus-score-set header (cons new old) nil t))
              (gnus-score-set 'touched '(t)))
            new))
         (t (apply f header match args)))))

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

;; disallow caching as firebase might change the article numbering?
(setq gnus-uncacheable-groups
      (aif gnus-uncacheable-groups
          (format "\\(%s\\)\\|\\(^nndiscourse\\)" it)
        "^nndiscourse"))

(custom-set-variables
 '(gnus-score-after-write-file-function
   (lambda (file)
     (when (nndiscourse--gate)
       (unless (member file (alist-get (intern gnus-newsgroup-name)
                                       nndiscourse-score-files))
         (push file (alist-get (intern gnus-newsgroup-name)
                               nndiscourse-score-files)))))))

;; (push '((and (eq (car gnus-current-select-method) 'nndiscourse)
;;              (eq mark gnus-unread-mark)
;;              (not (string-match-p
;;                    "^Re: " (gnus-summary-article-subject))))
;;         . gnus-summary-high-unread)
;;       gnus-summary-highlight)

(provide 'nndiscourse)

;;; nndiscourse.el ends here
