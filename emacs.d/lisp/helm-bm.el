
;;; https://gist.github.com/1616018

(require 'cl)
(require 'helm)
(require 'helm-config)
(require 'helm-utils)

(defvar helm-c-source-bm-global
  '((name . "Global Bookmarks")
    (init . helm-c-bm-global-init)
    (candidates-in-buffer)
    (type . global-bm))
  "Bookmarks exist in `bm-repository'.")

(define-helm-type-attribute 'global-bm
  `((filtered-candidate-transformer helm-c-filtered-candidate-transformer-file-line)
    (multiline)
    (action ("Go to" . helm-c-action-file-line-goto)
            ("Remove Bookmark" . helm-c-action-bm-remove-annotation)
            ("Change Annotation" . helm-c-action-bm-change-annotation)))
  "")

(defun helm-c-action-bm-helper-with-bookmark (file-line-content callback)
  (when (stringp file-line-content)
    (setq file-line-content
          (cdr (helm-c-filtered-candidate-transformer-file-line-1
                file-line-content))))
  (destructuring-bind (file lineno content) file-line-content
    (when file
      (let ((newly-opened (null (get-file-buffer file))))
        (with-current-buffer (find-file-noselect file)
          (goto-line lineno)
          (let ((bookmark (bm-bookmark-at (point))))
            (when bookmark
              (funcall callback bookmark file lineno content)
              (bm-buffer-save)))
            (when newly-opened
              (kill-buffer (current-buffer))))))))

(defun* helm-c-action-bm-remove-annotation
    (file-line-content &optional (find-file-function #'find-file))
  (helm-c-action-bm-helper-with-bookmark file-line-content
    (lambda (bookmark file lineno content)
      (bm-bookmark-remove bookmark))))

(defun* helm-c-action-bm-change-annotation
    (file-line-content &optional (find-file-function #'find-file))
  (helm-c-action-bm-helper-with-bookmark file-line-content
    (lambda (bookmark file lineno content)
      (bm-bookmark-annotate
       bookmark
       (read-from-minibuffer (format "Annotation for %s: "
                                     content
                                     (overlay-get bookmark 'annotation)))))))

;; http://d.hatena.ne.jp/peccu/20100402/bmglobal
(defun helm-c-bm-global-init ()
  "Init function for `helm-c-source-bm-global'."
  (when (require 'bm nil t)
    (with-no-warnings
      (let ((helm-output-buffer (helm-candidate-buffer 'global)))
        (loop for (filename . bm-info) in bm-repository
              when (file-exists-p filename)
              do
              (with-temp-buffer
                (insert-file-contents filename) ; insert-file-contents-literally is faster
                (loop for bookmark in (cdr (assoc 'bookmarks bm-info))
                      with output-line
                      do
                      (goto-char (cdr (assoc 'position bookmark)))
                      (beginning-of-line)
                      (setq output-line
                            (format "%s:%d: [%s]: %s\n"
                                    filename
                                    (line-number-at-pos)
                                    (or (cdr (assoc 'annotation bookmark)) "")
                                    (car (split-string (thing-at-point 'line) "[\n\r]"))))
                      (with-current-buffer helm-output-buffer
                        (insert output-line)))))))))


(defvar helm-c-source-bm
  '((name . "Visible Bookmarks")
    (init . helm-c-source-bm-init)
    (candidates-in-buffer)
    (action . (("Goto line" . (lambda (candidate)
                                (goto-line (string-to-number candidate))))))))

(defun helm-c-source-bm-init ()
  (let ((bookmarks (bm-lists))
        (buf (helm-candidate-buffer 'global)))
    (dolist (bm (sort* (append (car bookmarks) (cdr bookmarks))
                       '< :key 'overlay-start))
      (let ((start (overlay-start bm))
            (end (overlay-end bm))
            (annotation (or (overlay-get bm 'annotation) "")))
        (unless (< (- end start) 1)   ; org => (if (< (- end start) 2)
          (let ((str (format "%7d: [%s]: %s\n"
                             (line-number-at-pos start)
                             annotation
                             (buffer-substring start (1- end)))))
            (with-current-buffer buf (insert str))))))))

(defun helm-bm ()
  "Preconfigured `helm' for bm."
  (interactive)
  (helm :sources '(helm-c-source-bm-global)
        :buffer "*helm bm*"
        :default (buffer-name helm-current-buffer)))

(provide 'helm-bm)
