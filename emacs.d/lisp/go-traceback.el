
(defun go-traceback-refresh ()
  "Refresh traceback buffer"
  (interactive)
  (go-traceback go-traceback-url))


(defconst my-js-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; ' is a string delimiter
    (modify-syntax-entry ?' "\"" table)
    ;; " is a string delimiter too
    (modify-syntax-entry ?\" "\"" table)

    ;; / is punctuation, but // is a comment starter
    (modify-syntax-entry ?/ ". 12" table)
    ;; \n is a comment ender
    (modify-syntax-entry ?\n ">" table)
    table))

(define-derived-mode go-traceback-mode compilation-mode "go traceback mode"
  ;; :syntax-table my-js-mode-syntax-table
  (font-lock-fontify-buffer)

  (set (make-local-variable 'go-traceback-url) nil)
  (my-set-text-scale-smaller)
  (define-key go-traceback-mode-map (kbd "g" )
    '(lambda ()
       (interactive)
       (go-traceback go-traceback-url t (window-start)))))


(defun go-traceback (&optional url no-move w-start)
  "Fetch trackback from running go program"
  (interactive)
  (lexical-let* ((no-move no-move)
                 (w-start w-start))
    (request
     (or url "http://localhost:6067/debug/pprof/goroutine?debug=2")
     :type "GET"
     :parser (lambda ()(buffer-string))
     :success (function*
               (lambda (&key data response &allow-other-keys)
                 (let ((inhibit-read-only t)
                       (b (get-buffer-create "*go-traceback*")))
                   (with-current-buffer b
                     (let ( (p-point (point)))
                       (unless (eq major-mode 'go-traceback-mode)
                         (go-traceback-mode))

                       (setq go-traceback-url (request-response-url response))
                       (delete-region (point-min) (point-max))
                       (insert data)
                       (goto-char (if no-move p-point (point-min)))))
                   (let ((w (display-buffer b)))
                     (when w-start
                   (set-window-start w w-start)))))))))

(provide 'go-traceback)
