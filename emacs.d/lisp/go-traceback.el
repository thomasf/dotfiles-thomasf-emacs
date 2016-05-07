
(require 'request)

(defun go-traceback-refresh ()
  "Refresh traceback buffer"
  (interactive)
  (go-traceback go-traceback-buffer-url))

(defcustom go-traceback-url "http://localhost:8080/debug/pprof/goroutine?debug=2"
  "The default url where pprof is expeted to be running at. ")

(define-compilation-mode go-traceback-mode "go traceback mode"
  (font-lock-fontify-buffer)
  (setq-local compilation-error-regexp-alist
              '(("^[\t ]*\\[\\([^(].*\\):\\([1-9][0-9]*\\)\\(\\]\\)?:in " 2 3)))
  (setq-local go-traceback-buffer-url nil)
  (define-key go-traceback-mode-map (kbd "g" )
    '(lambda ()
       (interactive)
       (go-traceback go-traceback-buffer-url t (window-start)))))


(defun go-traceback (&optional url no-move w-start)
  "Fetch trackback from running go program"
  (interactive)
  (lexical-let* ((no-move no-move)
                 (w-start w-start))
    (request
     (or url go-traceback-url)
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
                       (setq go-traceback-buffer-url (request-response-url response))))

                   (let ((w (display-buffer b)))
                     (when w-start
                       (set-window-start w w-start)))))))))

(provide 'go-traceback)
