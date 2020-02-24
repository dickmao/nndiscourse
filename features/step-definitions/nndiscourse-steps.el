(When "^of-record unreads for \"\\(.+\\)\" is \\([.0-9]+\\)$"
      (lambda (group count)
        (should (= (string-to-number count) (gnus-group-unread group)))))

(When "^prospective unreads for \"\\(.+\\)\" is \\([.0-9]+\\)$"
      (lambda (group count)
        (when (< emacs-major-version 27)
          (should (= (string-to-number count) (length gnus-newsgroup-unreads))))))

(When "^I should be in buffer like \"\\(.+\\)\"$"
      (lambda (prefix)
        (should (string-prefix-p prefix (buffer-name)))))

(When "^I go to string \"\\(.+\\)\"$"
      (lambda (string)
        (goto-char (point-min))
        (let ((search (re-search-forward string nil t))
              (message "Can not go to string '%s' since it does not exist in the current buffer: %s"))
          (cl-assert search nil message string (buffer-string)))
        (backward-char (length string))))

(When "^I clear buffer \"\\(.*\\)\"$"
      (lambda (buffer)
        (with-current-buffer buffer
          (let ((inhibit-read-only t))
            (erase-buffer)))))

(When "^I scan news$"
      (lambda ()
        (setq nndiscourse--last-scan-time 0)
        (And "I switch to buffer \"*Group*\"")
        (And "I press \"g\"")
        (And "I dump buffer")))

(When "^I dump buffer"
      (lambda () (message "%s" (buffer-string))))

(When "^gnus \\(try \\)?start\\(\\)$"
      (lambda (demote _workaround)
        (aif (get-buffer gnus-group-buffer)
            (switch-to-buffer it)
          (if-demote demote
            (When "I call \"gnus\"")
            (Then "I should be in buffer \"%s\"" gnus-group-buffer)))))

(When "^gnus stop$"
      (lambda ()
        (aif (get-buffer gnus-group-buffer)
            (progn (switch-to-buffer it)
                   (And "I press \"q\"")
                   (switch-to-buffer "*scratch*")))))

(When "^I open latest \"\\(.+\\)\"$"
      (lambda (relative-prefix)
        (let* ((prefix (concat (file-name-as-directory gnus-home-directory)
                               relative-prefix))
               (dir (file-name-directory prefix))
               (base (file-name-base prefix))
               (alist
                (directory-files-and-attributes dir t (regexp-quote base) t))
               (sofar (cl-first alist))
               (most-recent (dolist (cand alist (car sofar))
                              (if (> (float-time (nth 5 (cdr cand)))
                                     (float-time (nth 5 (cdr sofar))))
                                  (setq sofar cand)))))
          (find-file most-recent))))

(When "^I wait \\([.0-9]+\\) seconds?$"
      (lambda (seconds)
        (sleep-for (string-to-number seconds))))

(When "^I wait for buffer to\\( not\\)? say \"\\(.+\\)\"$"
      (lambda (negate bogey)
        (nndiscourse-test-wait-for
         (lambda ()
           (let* ((says (s-contains? (s-replace "\\n" "\n" bogey) (buffer-string))))
             (revert-buffer :ignore-auto :noconfirm)
             (if negate (not says) says)))
         nil 5000 1000)))
