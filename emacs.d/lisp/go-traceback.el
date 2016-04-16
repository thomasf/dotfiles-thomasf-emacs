
(require 'request)

(defun go-traceback-refresh ()
  "Refresh traceback buffer"
  (interactive)
  (go-traceback go-traceback-url))


;; (defconst my-js-mode-syntax-table
;;   (let ((table (make-syntax-table)))
;;     ;; ' is a string delimiter
;;     (modify-syntax-entry ?' "\"" table)
;;     ;; " is a string delimiter too
;;     (modify-syntax-entry ?\" "\"" table)

;;     ;; / is punctuation, but // is a comment starter
;;     (modify-syntax-entry ?/ ". 12" table)
;;     ;; \n is a comment ender
;;     (modify-syntax-entry ?\n ">" table)
;;     table))

(define-compilation-mode go-traceback-mode "go traceback mode"
  ;; :syntax-table my-js-mode-syntax-table
  (font-lock-fontify-buffer)
  (setq-local compilation-error-regexp-alist
              '(("^[\t ]*\\[\\([^(].*\\):\\([1-9][0-9]*\\)\\(\\]\\)?:in " 2 3)))
  (setq-local go-traceback-url nil)
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
                     (let ((p-point (point)))
                       (delete-region (point-min) (point-max))
                       (insert data)
                       (goto-char (if no-move p-point (point-min)))
                       (go-traceback-mode)
                       (setq go-traceback-url (request-response-url response))))

                   (let ((w (display-buffer b)))
                     (when w-start
                       (set-window-start w w-start)))))))))

(provide 'go-traceback)
