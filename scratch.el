(require 'request)
(require 'shr)

(require 'simple-httpd)

(require 'json)
(require 'json-rpc)
(require 'shr)

;; poor man's
(let ((result))
  (request "localhost:8999"
    :type "POST"
    :data (json-encode '(("method" . "category_latest_topics") ("jsonrpc" . "2.0") ("params" :category_slug "emacs") ("id" . 1)))
    :sync t
    :parser 'json-read
    :success (cl-function
              (lambda (&key data &allow-other-keys)
                (setq result (cdr (assq 'result data))))))
  result)

;; rich man's
(add-to-list 'gnus-secondary-select-methods '(nndiscourse "emacs-china.org" (nndiscourse-scheme "https")))

(let ((server "emacs-china.org"))
  (nndiscourse-open-server server)
  (seq-filter (lambda (raw) (cl-search "emacs" (cdr raw)))
              (seq-map (lambda (x) (cons (plist-get x :raw) (plist-get x :topic_title)))
                       (nndiscourse-get-posts server))))

(let* ((server "emacs-china.org")
       (group "emacs")
       (headers (nndiscourse-get-headers server group)))
  (nndiscourse-open-server server)
  (cons (nndiscourse--first-article-number server group)
        (nndiscourse--last-article-number server group))
  (nndiscourse--get-header server group 72124))

(let ((nntp-server-buffer (get-buffer-create "foo")))
  (nndiscourse-request-list "emacs-china.org"))

(let ((server "emacs-china.org")
      result)
  (nndiscourse-open-server server)
  (with-current-buffer (nndiscourse--server-buffer server)
    (mapatoms (lambda (k)
                (push (nndiscourse-get-category server k) result))
              nndiscourse--categories-hashtb)
    result))

(let ((server "emacs-china.org")
      (nndiscourse--last-id nil)
      (group "emacs"))
  (nndiscourse-open-server server)
  (with-current-buffer (nndiscourse--server-buffer server)
    (mapatoms (lambda (k)
                (nndiscourse-set-headers server k nil))
              nndiscourse--headers-hashtb))
  (nndiscourse--incoming server)
  (length (nndiscourse-get-headers server group)))

(nndiscourse-get-headers "emacs-china.org" "emacs")

(let ((server "emacs-china.org"))
  (nndiscourse-open-server server)
  (nndiscourse--incoming server)
  (length (nndiscourse-get-headers server "emacs")))

(let ((server "emacs-china.org"))
  (nndiscourse-get-ref
    server
    (plist-get (car (last (nndiscourse-get-headers server))) :id)))

(let ((server "emacs-china.org")
      result)
  (length (nndiscourse-get-headers server))
  (with-current-buffer (nndiscourse--server-buffer server)
    (nndiscourse--maphash
     (lambda (key value)
       (!cons `(,key ,(nndiscourse-get-ref server key)) result))
     nndiscourse-refs-hashtb))
  result)

(let ((foo (lambda (f &rest args)
             (cl-macrolet ((gnus-active (_group) `(cons 1 10)))
               (apply f args)))))
  (add-function :around (symbol-function 'gnus-group-insert-group-line-info)
                foo)
  (unwind-protect (gnus-group-insert-group-line-info "nndiscourse:emacs")
    (remove-function (symbol-function 'gnus-group-insert-group-line-info) foo)))

(gnus-group-entry "nndiscourse:emacs")

(gnus-info-read (gnus-get-info "nndiscourse:emacs"))

(let ((foo (lambda (args)
             (let ((group (car args)))
               (if (gnus-group-entry group)
                   args
                 (setf (nthcdr 3 args) 10)
                 args))))
      (group "nndiscourse:emacs"))
  (add-function :filter-args (symbol-function 'gnus-group-insert-group-line) foo)
  (unwind-protect
      (gnus-group-insert-group-line group
                                    gnus-level-killed
                                    nil
                                    40
                                    (gnus-method-simplify
                                     (gnus-find-method-for-group group)))
    (remove-function (symbol-function 'gnus-group-insert-group-line) foo)))



(nndiscourse--gethash "emacs-china.org" nndiscourse-headers-hashtb)

(defun nndiscourse--get-header (server group article-number)
  "Amongst SERVER GROUP headers, binary search ARTICLE-NUMBER."
  (declare (indent defun))
  (let ((headers (nndiscourse-get-headers server group)))
   (cl-flet ((id-of (k) (plist-get (elt headers k) :id)))
     (cl-do* ((x article-number)
              (l 0 (if (> x m) (1+ m) l))
              (r (length headers) (if (< x m) m r))
              (m (/ (- r l) 2)))
         ((or (<= (- r l) 1) (= x (id-of m)))
          (and (< m (length headers)) (>= m 0) (= x (id-of m)) (elt headers m)))))))

(defun bsearch (article-number)
  (let ((headers '((:id 3) (:id 5) (:id 13) (:id 23) (:id 30))))
    (cl-flet ((id-of (k) (plist-get (elt headers k) :id)))
      (cl-do* ((x article-number)
               (l 0 (if dir (1+ m) l))
               (r (length headers) (if dir r m))
               (m (/ (- r l) 2) (+ m (* (if dir 1 -1) (max 1 (/ (- r l) 2)))))
               (dir (> x (id-of m)) (> x (id-of m))))
          ((or (<= (- r l) 1) (= x (id-of m)))
           (and (< m (length headers)) (>= m 0) (= x (id-of m)) (elt headers m)))
        ))))

(bsearch 29)


(setq nndiscourse-headers-hashtb (gnus-make-hashtable))
(let ((server "emacs-china.org"))
  (seq-map (-rpartial #'plist-get :post_number)
           (plist-get (nndiscourse-rpc-request "" "posts" :before 0) :latest_posts)))

(defun nnreddit-rpc-get (&optional server)
  "Retrieve the PRAW process for SERVER."
  (setq proc (make-process :name server
                           :buffer (get-buffer-create (format " *%s*" server))
                           :command praw-command
                           :connection-type 'pipe
                           :noquery t
                           :sentinel #'nnreddit-sentinel
                           :stderr (get-buffer-create (format " *%s-stderr*" server))))
  proc)



;; run thor on command line, and don't instantiate it in emacs
(let ((server "emacs-china.org"))
  (cl-letf (((symbol-function 'nndiscourse-open-server) (lambda (&rest args) t)))
    ;; (nndiscourse-rpc-request "" "category_latest_topics" '(:category_slug . "emacs"))
    (nndiscourse-rpc-request server "category_latest_topics" :category_slug "emacs")))

(let ((server "emacs-china.org"))
  (seq-map (-rpartial #'plist-get :title) (nndiscourse-get-topics server "emacs")))

(let ((server "emacs-china.org"))
  (seq-map (-rpartial #'plist-get :topic_title) (nndiscourse-get-posts server :before 71000)))

(let ((server "emacs-china.org"))
  (apply #'min (seq-map (-rpartial #'plist-get :id) (nndiscourse-get-posts server))))

(let ((server "emacs-china.org"))
  (seq-filter (lambda (raw) (cl-search "org-mode" raw))
              (seq-map (lambda (x) (plist-get x :raw)) (nndiscourse-get-posts server))))

(let ((server "emacs-china.org"))
  (seq-filter (lambda (raw) (cl-search "word wrap" (cdr raw)))
              (seq-map (lambda (x) (cons (plist-get x :raw) (plist-get x :topic_title)))
                       (nndiscourse-get-posts server))))

(let ((server "emacs-china.org"))
  (seq-map (-rpartial #'plist-get :slug) (nndiscourse-get-categories server)))

(gnus-group-full-name "programming" "nndiscourse:")

(let ((server "emacs-china.org"))
  (gnus-get-info (gnus-group-full-name "programming" `(nndiscourse ,server))))

(let* ((rpc (json-rpc-connect "localhost" 8999))
       (cooked (plist-get (json-rpc rpc "get_post" 12) :cooked)))
  (with-temp-buffer
    (insert cooked)
    (shr-render-buffer (current-buffer))))

(json-read-from-string (buffer-string))

(makunbound 'httpd-root)
(custom-set-default 'httpd-root "/home/dick/nndiscourse")
(custom-set-default 'httpd-port 9009)
(httpd-start)

(let ((site "http://localhost:3000/login")
      result)
  (request site
           :parser (lambda ()
                     (let ((foo (make-temp-file "foo")))
                       (write-region (point-min) (point-max) foo)
                       (eww-open-file foo)))
           :data '(("username" . "priapushk@gmail.com")
                   ("password" . "StT9nyTvyD")
                   ("redirect" . site))
           :sync t
           :success (cl-function
                     (lambda (&key data symbol-status response error-thrown
                                   &allow-other-keys
                                   &aux (response-status (request-response-status-code response)))
                       (setq result (format "SUCCESS: ss=%s r=%s et=%s data=%s"
                                            symbol-status response-status error-thrown data))))
           :error (cl-function
                   (lambda (&key data symbol-status response error-thrown
                                 &allow-other-keys
                                 &aux (response-status (request-response-status-code response)))
                     (setq result (format "ERROR: ss=%s r=%s et=%s data=%s"
                                          symbol-status response-status error-thrown data)))))
  result)

(let (result)
  (request "http://localhost:3000/user-api-key/new"
           :type "GET"
           ;; :parser (lambda () (shr-render-buffer (current-buffer)))
           :parser (lambda ()
                     (let ((foo (make-temp-file "foo")))
                       (write-region (point-min) (point-max) foo)
                       (eww-open-file foo)))
           :data `(("auth_redirect" . "https://localhost:9009")
                   ("application_name" . "nndiscourse")
                   ("client_id" . "nndiscourse-0")
                   ("scopes" . "read,write,message_bus,session_info")
                   ("public_key" . ,(shell-command-to-string "gpg --export-secret-keys 87681210 | openpgp2ssh 87681210 | openssl rsa -pubout"))
                   ("nonce" . ,(shell-command-to-string "head /dev/urandom | tr -dc A-Za-z0-9 | head -c10 -")))
           :sync t
           :success (cl-function
                     (lambda (&key data symbol-status response error-thrown
                                   &allow-other-keys
                                   &aux (response-status (request-response-status-code response)))
                       (setq result (format "SUCCESS: ss=%s r=%s et=%s data=%s"
                                            symbol-status response-status error-thrown data))))
           :error (cl-function
                   (lambda (&key data symbol-status response error-thrown
                                 &allow-other-keys
                                 &aux (response-status (request-response-status-code response)))
                     (setq result (format "ERROR: ss=%s r=%s et=%s data=%s"
                                          symbol-status response-status error-thrown data)))))
  result)

;; can't use curl because authorization requires javascript
;; need to go through the browser, but can't auth_redirect, must
;; copy-paste to emacs sadly.

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


(print-out (cl-macroexpand '(nndiscourse-first-to-succeed "true" "false")))
(nndiscourse-first-to-succeed "false" "false" "true")

(let* ((nndiscourse-public-keyfile (expand-file-name "~/.ssh/id_rsa.pub"))
       (nndiscourse-private-keyfile (file-name-sans-extension nndiscourse-public-keyfile)))
  (nndiscourse-first-to-succeed
   (format "ssh-keygen -f %s -e -m pkcs8" nndiscourse-public-keyfile)
   (format "openssl rsa -in %s -pubout" nndiscourse-private-keyfile)))

(defun build-query (&optional site)
  (unless site
    (setq site "http://localhost:3000"))
  (let* (result
         (shell-command-default-error-buffer "*scratch*")
         (nndiscourse-public-keyfile (expand-file-name "~/.ssh/id_rsa.pub"))
         (nndiscourse-private-keyfile (file-name-sans-extension nndiscourse-public-keyfile)))
    (format "%s/user-api-key/new?%s"
            site
            (url-build-query-string
             `((auth_redirect "https://api.discourse.org/api/auth_redirect")
               (application_name "nndiscourse")
               (client_id "nndiscourse-0")
               (scopes "read,write,message_bus,session_info")
               ;; (public_key ,(shell-command-to-string (format "gpg --export-secret-keys %s | openpgp2ssh %s | openssl rsa -pubout 2>/dev/null" "87681210" "87681210")))
               (public_key ,(or (with-temp-buffer
                                  (let ((retval
                                         (apply #'call-process
                                                "ssh-keygen" nil t nil
                                                (split-string
                                                 (format "-f %s -e -m pkcs8"
                                                         nndiscourse-public-keyfile)))))
                                    (when (and (numberp retval) (zerop retval))
                                      (buffer-string))))
                                (with-temp-buffer
                                  (let ((retval
                                         (apply #'call-process
                                                "openssl" nil t nil
                                                (split-string
                                                 (format "rsa -in %s -pubout"
                                                         nndiscourse-private-keyfile)))))
                                    (when (and (numberp retval) (zerop retval))
                                      (buffer-string))))))
               (nonce ,(shell-command-to-string "head /dev/urandom | tr -dc A-Za-z0-9 | head -c10 -")))))))

;; eww doesn't fly for lack of javascript
(build-query "http://localhost:3000")

;; client = DiscourseApi::Client.new("http://localhost:3000")
;; client = DiscourseApi::Client.new('http://localhost:3000', 'b28f0cea1b4fb749b9a3b8683760388c', 'priapushk', 'User-Api-Key', 'User-Api-Client-Id')
;; proc = Nndiscourse::Process.new('http://localhost:3000', 'b28f0cea1b4fb749b9a3b8683760388c', 'priapushk')
;; (let ((user-api-key
;;        (alist-get
;;         'key
;;         (with-temp-buffer
;;           (shell-command (concat "openssl pkeyutl -decrypt"
;;                                  " -inkey <(gpg --export-secret-keys 87681210 | "
;;                                  "openpgp2ssh 87681210 | openssl rsa 2>/dev/null) "
;;                                  "-in <(cat /tmp/decryptme | base64 --decode)")
;;                          t)
;;           (json-read)))))
;;   user-api-key)

(user-api-key "https://api.discourse.org/api/auth_redirect?payload=DiHDYIoM2pzmLfdh2FnZhwTyQfK8bdbebiol2jBouObQGojI5yF%2ByoO00ael%0AO7LstQj1uCBjQnO%2BjrbI03Bvbz1LDvQyVAMYMIPBmwam48JqfCQHm73Z0Qkc%0A%2Bid4LNo8xiP2EiycQKgYRh2KY1y19v%2FXD3Osm6o%2Fn%2BrawpVdJ0fSZTgBkHV%2F%0AcjaCAIRpOOoFzlH1CeSZBUTEl6GhT1ALKR7yurqS2GZ5MW8bIts3MYV5FQss%0A0jSDH6AG2xJENBpRJ9x%2FvM5t5DRbp2jy3105H4d4se2Qfexgf1dfrvDOpaIZ%0Aq5UTqnOatPZ94vIqjBYTnlroh8hlGGU8QKK01k6QhQ%3D%3D%0A")

(defun user-api-key (return-url)
  (let* ((emacs-china-url "https://api.discourse.org/api/auth_redirect?payload=ZYOTYZz0uM%2B3xBIRTN%2FsOoz3iZvLW%2BdWtPT%2FAOD8Ge1PWJjpGWPijLQlukl7%0AjcD%2Fd2IOTM8fBUUxO9R2P314frGKTHQ1bx%2FLCVxXhcD7CN%2FQxxPUYkr3BEui%0ANAUVW0uIH8el6VbPfPoeUfTp%2BGGYNBNkpqdZJj1sTqi%2FcbrXMlMUSfsYlqKW%0AQLQYr1XuY42vT1B%2FmVUH1i7xad6c3bb6ayQrBoTFJicEG14tEa%2BAtICUu9KI%0Aod%2FlZ2Sq%2Ffid7qnS9q0Z7l4vl6nOkT3T8ngqU2Bajx0Jo6pVcDLw6lcLv8bk%0A%2Bc2HI%2BY4zT98cEzdJjJ2XxvUnEqCPXvs6VvYe9vRcw%3D%3D%0A")
         (parsed-url (url-generic-parse-url return-url))
         (encrypted (cl-second
                     (assoc-string "payload" (url-parse-query-string
                                              (cdr (url-path-and-query parsed-url))))))
         (user-api-key
          (alist-get
           'key
           (with-temp-buffer
             (shell-command (concat "openssl pkeyutl -decrypt"
                                    " -inkey " (expand-file-name "~/.ssh/id_rsa")
                                    " -in "
                                    (format "<(echo %s | base64 --decode)" (replace-regexp-in-string "\\s-" "" encrypted)))
                            t)
             (json-read)))))
    user-api-key))

(let ((foo (gnus-make-hashtable)))
  (nndiscourse--sethash 1 '(a b c (c . e)) foo))

(let ((foo (gnus-make-hashtable)))
  (nndiscourse--sethash "froo" '(a b c (c . e)) foo)
  (let ((posts (nndiscourse--gethash "froo" foo))
        (index 2))
    (when (< (length posts) (1+ index))
      (nndiscourse--sethash "froo"
        (nconc posts (make-list (- (1+ index)
                                   (length posts))
                                nil))
        foo))

    (setf (elt (nndiscourse--gethash "froo" foo) index) "hi!")
    (nndiscourse--gethash "froo" foo)))

(let ((id (plist-get plst :id))
      (group (plist-get plst :creation_id))
      (topic-title (plist-get plst :topic_title))
      (post_number (plist-get plst :post_number)))
  (nndiscourse--sethash
    id
    (list group topic-title post_number)
    nndiscourse-location-hashtb)
  (nndiscourse--sethash
    group
    (let* ((posts-hashtb
            (or (nndiscourse--gethash group nndiscourse-headers-hashtb)
                (gnus-make-hashtable)))
           (posts (nndiscourse--gethash topic-title posts-hashtb)))
      (when (< (length posts) post-number)
        (nndiscourse--sethash
          topic-title
          (nconc posts (make-list (- post-number (length posts)) nil))
          posts-hashtb))
      (setf (elt (nndiscourse--gethash topic-title posts-hashtb)
                 post-number)
            plst)
      posts-hashtb)
    nndiscourse-headers-hashtb))

(setq gnus-server-method-cache nil)
(gnus-server-to-method "nndiscourse:emacs-china.org")

(setq gnus-secondary-select-methods (cdr gnus-secondary-select-methods))

(gnus-method-to-server-name '(nndiscourse "emacs-china.org" :scheme "https"))
(gnus-find-method-for-group "nndiscourse+emacs-china.org:emacs-general")

(add-to-list 'gnus-secondary-select-methods '(nndiscourse "emacs-china.org" (nndiscourse-scheme "https")))

(nndiscourse-open-server "emacs-china.org")
(nndiscourse-proc-info-process (cdr (assoc "emacs-china.org" nndiscourse-processes)))
(let ((nntp-server-buffer (get-buffer-create "foo")))
  (nndiscourse-request-list "emacs-china.org"))

(process-contact (alist-get "emacs-china.org" nndiscourse-processes nil nil #'equal))

(mapcan (lambda (b) (let ((foo (buffer-name b)))
                      (and (cl-search "stderr" foo) (list foo))))
        (buffer-list))

(gnus-find-method-for-group "nndiscourse+emacs-china.org:emacs")


(condition-case nil
    (prog1 t
      (delete-process (make-network-process :name "test-port"
                                            :noquery t
                                            :host nndiscourse-localhost
                                            :service 37529
                                            :buffer nil
                                            :stop t)))
  (file-error nil))

(gnus-compress-sequence (gnus-range-normalize '(72330 . 72367)))

(set (gv-ref (with-current-buffer "scratch.el<nndiscourse>"
               this-is-mine)) t)


(format-time-string "%a, %d %h %Y %T %z (%Z)" (date-to-time "2020-02-04T12:52:06.942Z"))
(date-to-time "2020-02-04T12:52:06.942Z")

(let ((marks '((unexist (1 . 1) 4) (halle t)))
      (marks nil))
  (setf (alist-get 'unexist marks) `((2 . 2) (1 . 1)))
  (alist-get 'unexist marks)
  )

(require 'shr)
(rfc2231-parse-qp-string "Content-Type: text/html; charset=UTF-8")

(defmacro mm-with-part (handle &rest forms)
  "Run FORMS in the temp buffer containing the contents of HANDLE."
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

(defun mm-shr (handle)
  (let ((shr-width (if shr-use-fonts
		       nil
		     fill-column))
	(shr-content-function (lambda (id)
				(let ((handle (mm-get-content-id id)))
				  (when handle
				    (mm-with-part handle
				      (buffer-string))))))
	(shr-inhibit-images mm-html-inhibit-images)
	(shr-blocked-images mm-html-blocked-images)
	charset coding char document)
    (mm-with-part (or handle (setq handle (mm-dissect-buffer t)))
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
        (let ((convert (buffer-string)))
          (insert (prog1
                      (decode-coding-string convert coding)
                    (erase-buffer)
                    (set-buffer-multibyte t)))))
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

(with-temp-buffer
  (set-buffer-multibyte t)
  (save-excursion
    (insert "<p>最近也想尝试,但是感觉蛮难的,比如不知道如何在"))
  (let ((handle (mm-make-handle
                 (current-buffer)
                 (rfc2231-parse-qp-string "Content-Type: text/html; charset=UTF-8"))))
    (cl-assert (not (zerop (length (with-temp-buffer (mm-shr handle)
                                                     (buffer-string))))))))

(require 'polymode-core)
(defun ein:markdown-syntax-propertize (start end)
  "Function used as `syntax-propertize-function'.
START and END delimit region to propertize."
  (message "got here %s %s %s" start end (pm-innermost-range start))
  (with-silent-modifications
    (save-excursion
      (remove-text-properties start end ein:markdown--syntax-properties)
      (ein:markdown-syntax-propertize-fenced-block-constructs start end)
      (ein:markdown-syntax-propertize-list-items start end)
      (ein:markdown-syntax-propertize-pre-blocks start end)
      (ein:markdown-syntax-propertize-blockquotes start end)
      (ein:markdown-syntax-propertize-headings start end)
      (ein:markdown-syntax-propertize-hrs start end)
      (ein:markdown-syntax-propertize-comments start end))))

;; Same boat here, although I am only trying to use User-Api-Key (not Api-Key) to create a topic post and am getting CSRF denial from the actionpack library.

;; Unless the discourse server has turned off CSRF checking, posting from a third-party desktop app seems hard. I’m not about to emulate a browser.

;; which see ~/discourse_api/gem-transcript{,2}

(with-temp-buffer
  (let ((html "<p>最近也想尝试,但是感觉蛮难的,比如不知道如何在"))
    (insert
     "Subject: " "foo" "\n"
     "From: " "nobody" "\n"
     "\n")
    (mml-insert-multipart "alternative")
    ;;    (mml-insert-empty-tag 'part 'type "text/html")
    (mml-insert-part "text/html")
    (insert html)
    (insert "\n")
    (when (mml-validate)
      (message-encode-message-body))
    (buffer-string)))
