;;;; experimental implementation to assess feature requirements

;;; example usage: create an interactive function with your repos
;;
;; (defun magit-multi-status-dotfiles ()
;;   (interactive)
;;   (magit-multi-status
;;    '("~/.config/dotfiles/emacs" "~/.config/dotfiles/shell"
;;      "~/.config/dotfiles/desktop" "~/.config/dotfiles/notes")))


(require 'magit)
;; (makunbound 'magit-multi-status-mode-map)

(defvar-local magit-multi-status-repos nil "list of buffer repos")


(defvar magit-multi-status-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "n") 'magit-multi-status-next-section)
    (define-key map (kbd "p") 'magit-multi-status-previous-section)
    (define-key map (kbd "h") 'magit-multi-status-toggle-show-unchanged)
    (define-key map (kbd "q") 'bury-buffer)
    (define-key map (kbd "<RET>") 'magit-multi-status-goto)
    ;; (define-key map (kbd "<tab>") 'magit-multi-status-next-section)
    ;; (define-key map (kbd "<backtab>") 'magit-multi-status-previous-section)
    (define-key map (kbd "g") 'magit-multi-status-refresh)
    map)
  "Keymap for `magit-multi-status-mode-map'.")

(define-derived-mode magit-multi-status-mode nil "magit-multi-status"
  "Major mode for start up buffer.
\\{magit-multi-status-mode-map}"
  (font-lock-mode 1)
  (read-only-mode 1))


(defun magit-multi-status-goto ()
  "asd"
  (interactive)
  (save-excursion
    (end-of-line)
    (re-search-backward "^[^ \n]+" nil t)
    (let ((default-directory (thing-at-point 'line t)) )
      (call-interactively 'magit-status ))))

(defun magit-multi-status-next-section ()
  "TODO"
  (interactive)
  (end-of-line)

  (unless (re-search-forward "^[^ \n]+" nil t)
    (goto-char (point-max))
    )
  (beginning-of-line))

(defun magit-multi-status-previous-section ()
  "TODO"
  (interactive)
  (beginning-of-line)
  (re-search-backward "^[^ \n]+" nil t))

(defvar-local magit-multi-status-show-unchanged nil "show repos which doenst have changes")
(defun magit-multi-status-toggle-show-unchanged ()
  "TODO"
  (interactive)
  (setq-local  magit-multi-status-show-unchanged (not magit-multi-status-show-unchanged))
  (magit-multi-status-refresh))

(defun magit-multi-status-refresh ()
  "TODO"
  (interactive)
  (save-excursion
    (let ((directory)
          (item)
          (inhibit-read-only t))
      (kill-region (point-min) (point-max))
      (dolist (directory magit-multi-status-repos)
        (let ((default-directory (concat directory "/"))
              (bol))
          (when (or
                 magit-multi-status-show-unchanged
                 (magit-anything-modified-p)
                 (magit-git-string
                  "ls-files" "--others" "--directory"
                  "--exclude-standard" "--no-empty-directory"))
            (insert
             (propertize (abbreviate-file-name default-directory)
                         'face '(font-lock-comment-face underline))
             "\n")
            (dolist (item (magit-git-items "status" "--short" "--porcelain" "--null"))
              (setq bol (point))
              (insert " " item)
              ;; (add-face-text-property bol (+ bol 3))
              (add-face-text-property bol (point) '(:height 0.8))
              (add-face-text-property bol (+ bol 4) 'font-lock-comment-face)
              (insert "\n"))
            (insert "\n")))))))

(defun magit-multi-status (repos)
  "yea yea"
  (with-current-buffer (get-buffer-create "*magit-multi-status*")
    (magit-multi-status-mode)
    (setq-local  magit-multi-status-repos repos)
    (magit-multi-status-refresh)
    (switch-to-buffer (current-buffer))))

(provide 'magit-multi-status)
