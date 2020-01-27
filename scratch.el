(require 'request)
(require 'shr)

(require 'simple-httpd)

(require 'json)
(require 'json-rpc)
(require 'shr)

(let ((result))
  (request "localhost:8999"
    :type "POST"
    :data (json-encode '(("method" . "sum") ("jsonrpc" . "2.0") ("params" . (1 2)) ("id" . 1)))
    :sync t
    :parser 'json-read
    :success (cl-function
              (lambda (&key data &allow-other-keys)
                (setq result (cdr (assq 'result data))))))
  result)

(let ((rpc (json-rpc-connect "localhost" 8999)))
  (seq-map (-rpartial #'plist-get :created_at)
           (plist-get (json-rpc rpc "posts" 2) :latest_posts)))

(let ((rpc (json-rpc-connect "localhost" 8999)))
  (seq-map (-rpartial #'plist-get :created_at)
           (plist-get (json-rpc rpc "posts" 2) :latest_posts)))

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

;; client = DiscourseApi::Client.new("localhost:3000")
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
