;;; init-commands.el --- general purpose commands and helpers -*- lexical-binding: nil; byte-compile-warnings: (not docstrings-wide); -*-

;;; Commentary:

;; Personal commands and helper functions that are not tied
;; to a single package:
;;
;; - replacements for standard commands (other-window, next-error...)
;; - buffers, files and directories, calling external programs
;; - editing and in-buffer navigation, misc
;;
;; Helpers for one package live in that package's use-package block;
;; appearance and window/frame commands live in init-ui.
;;
;; Loaded by init.el in this order: init-core, init-ui, init-commands,
;; init-security, init-packages, init-org.


;;; Code:

(require 'init-lib)
(eval-when-compile
  (require 'cl)
  (require 'subr-x)
  (require 'use-package)
  (require 'dash)
  (require 's)
  (require 'f)
  (require 'hydra)
  (require 'smartrep)
  (require 'fringe-helper))


;;; functions

;;;; replacements for some standard emacs command

;;;;; other-frame / other-window

(defun my-other-frame ()
  "Save-some-buffers, then other frame."
  (interactive)
  (call-interactively 'other-frame))

(defun my-other-window ()
  "Save-some-buffers, then other window."
  (interactive)
  (my-pulse-cancel-timer)
  (call-interactively 'other-window)
  ;; (my-pulse-later)
  (my-pulse-soon)
  (silent-save-some-buffers))


;;;;; next-error / previous-error

(defun my-next-error (&optional arg reset)
  (interactive "P")
  (let ((display-buffer-overriding-action
         '((display-buffer-same-window))))
    (next-error arg reset)))


(defun my-previous-error (&optional n)
  (interactive "P")
  (my-next-error (- (or n 1))))

(global-set-key [remap next-error] 'my-next-error)
(global-set-key [remap previous-error] 'my-previous-error)


;;;;; recenter-top-bottom / my-move-to-window-line-top-bottom


(defun my-recenter-top-bottom (&optional arg)
  (interactive "P")
  (call-interactively #'recenter-top-bottom)
  (my-pulse-line))

(defun my-move-to-window-line-top-bottom (&optional arg)
  (interactive "P")
  (call-interactively #'move-to-window-line-top-bottom)
  (my-pulse-line))


(global-set-key [remap recenter-top-bottom] 'my-recenter-top-bottom)
(global-set-key [remap move-to-window-line-top-bottom] 'my-move-to-window-line-top-bottom)


;;;;; scroll-up-command / scroll-down-command

(defun my-scroll-up-command (&optional arg)
  (interactive "P")
  (call-interactively #'scroll-up-command)
  (my-pulse-soon))


(defun my-scroll-down-command (&optional arg)
  (interactive "P")
  (call-interactively #'scroll-down-command)
  (my-pulse-soon))


(global-set-key [remap scroll-up-command] 'my-scroll-up-command)
(global-set-key [remap scroll-down-command] 'my-scroll-down-command)


;;;;; scroll-other-window-up / scroll-other-window-down

(defun my-scroll-other-window-down ()
  "Scrolls other window down one line"
  (interactive)
  (scroll-other-window-down 1))

(defun my-scroll-other-window-up ()
  "Scrolls other window up one line"
  (interactive)
  (scroll-other-window-down -1))


;;;; misc...

;;;; functions: buffers

;;;;; get-buffers-matching-mode

(defun get-buffers-matching-mode (mode)
  "Returns a list of buffers where their major-mode is equal to MODE."
  (let ((buffer-mode-matches '()))
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (if (eq mode major-mode)
            (add-to-list 'buffer-mode-matches buf))))
    buffer-mode-matches))


;;;;; kill buffers based on mode

(defun kill-buffers (mode)
  "Kill buffers."
  (save-excursion
    (let((count 0))
      (dolist(buffer (buffer-list))
        (set-buffer buffer)
        (when (equal major-mode mode)
          (setq count (1+ count))
          (kill-buffer buffer)))
      (message "Killed %i buffer(s)." count ))))

(defun kill-all-elisp-buffers ()
  "Kill all elisp buffers."
  (interactive)
  (kill-buffers 'emacs-lisp-mode))

(defun kill-all-org-buffers ()
  "Kill all org buffers."
  (interactive)
  (kill-buffers 'org-mode))


;;;;; kill this buffer if not modified

(defun kill-this-buffer-if-not-modified ()
  (interactive)
  ;; taken from menu-bar.el
  (if (menu-bar-non-minibuffer-window-p)
      (kill-buffer-if-not-modified (current-buffer))
    (abort-recursive-edit)))


;;;;; switch to minibuffer

(defun switch-to-minibuffer ()
  "Switch to minibuffer window."
  (interactive)
  (if (active-minibuffer-window)
      (select-window (active-minibuffer-window))
    (error "Minibuffer is not active")))
;; (bind-key "C-c o" 'switch-to-minibuffer)


;;;;; stop-using-minibuffer

(defun stop-using-minibuffer ()
  "kill the minibuffer"
  (when (and (>= (recursion-depth) 1) (active-minibuffer-window))
    (abort-recursive-edit)))
;; NOTE this is slightly annoying
;; (add-hook 'mouse-leave-buffer-hook 'stop-using-minibuffer)

;; (add-hook 'focus-out-hook 'stop-using-minibuffer)


;;;;; recursive-minibuffer-minor-mode

(define-minor-mode recursive-minibuffer-mode
  "Minor mode to enable recursive minibuffer"
  :init-value nil
  :global t
  :group 'minibuffer
  (if recursive-minibuffer-mode
      (progn
        (setq enable-recursive-minibuffers t)
        (minibuffer-depth-indicate-mode))
    (setq enable-recursive-minibuffers nil)))
;; (recursive-minibuffer-mode)


;;;; files / directories

;;;;; predicates

;;;;;; current-buffer-remote-p

(defun current-buffer-remote-p ()
  (--any? (and it (file-remote-p it))
          (list
           (buffer-file-name)
           list-buffers-directory
           default-directory))
  ;; (and (fboundp 'tramp-tramp-file-p) (-any? 'tramp-tramp-file-p
  ;;             (list
  ;;              (buffer-file-name)
  ;;              list-buffers-directory
  ;;              default-directory)))
  )


;;;;; dired jump commands

(defun dired-project-root ()
  "Open dired buffer at the project root."
  (interactive)
  (if (project-root-function)
      (dired (project-root-function))
    (message "No active project for this buffer.")))
(bind-key "C-x d e" 'dired-project-root)

(defun dired-23c ()
  "dired-23c"
  (interactive)
  (dired "~/src/gitlab.23c.se/"))
(bind-key "C-x d 2" 'dired-23c)

(defun dired-src ()
  "dired-src"
  (interactive)
  (dired "~/src/"))
(bind-key "C-x d r" 'dired-src)

(defun dired-repos ()
  "dired-repos"
  (interactive)
  (dired-src))

(defun dired-downloads ()
  "dired-downloads"
  (interactive)
  (dired "~/Downloads/"))

(defun dired-notes ()
  "dired-notes"
  (interactive)
  (dired "~/notes/"))
(bind-key "C-x d n" 'dired-notes)

(defun dired-present ()
  "dired-present"
  (interactive)
  (dired "~/notes/present/"))

(defun dired-scratch ()
  "dired-scratch"
  (interactive)
  (dired "~/scratch/"))

(defun dired-sitepackages ()
  "Jump to sitepackages directory."
  (interactive)
  (let* ((program (concat
                   "python -c 'import distutils.sysconfig;"
                   "print(distutils.sysconfig.get_python_lib())';"))
         (output (shell-command-to-string program))
         (fun (first (split-string output)))
         (directory (car (last (split-string output)))))
    (when directory
      (dired directory))))
(bind-key "C-x d s" 'dired-sitepackages)

(defun dired-virtualenv ()
  "Open a dired buffer at to current virtualenv"
  (interactive)
  (let
      ((virtual-env (getenv "VIRTUAL_ENV")))
    (when (not (equal virtual-env 'nil))
      (dired virtual-env))))


;;;;; file jump commands

(defun find-custom-set-variables ()
  "Opens emacs init"
  (interactive)
  (find-file
   (expand-file-name
    "custom-set-variables.el" user-data-directory)))

(defun find-init ()
  "Opens emacs init"
  (interactive)
  (find-file
   (expand-file-name
    "init.el" user-emacs-directory)))

(defun reload-init ()
  "Opens emacs init"
  (interactive)
  (load (expand-file-name "init.el" user-emacs-directory) nil nil t)
  (load (expand-file-name "lisp/my-solarized.el" user-emacs-directory) nil nil t)
  (mf-mode-theme t))


(defun find-bash-history ()
  "Open bash history file"
  (interactive)
  (find-file "~/.bash_history")
  (read-only-mode 1)
  (goto-char (point-max))
  (auto-revert-tail-mode 1))

(defun find-syslog ()
  "Open syslog"
  (interactive)
  (find-file "/var/log/syslog")
  (read-only-mode 1)
  (goto-char (point-max))
  (auto-revert-tail-mode 1))

(defun find-notes ()
  "find file in notes, FAST."
  (interactive)
  (let* ((default-directory user-notes-directory)
         (files (->> (-concat (f-entries "agenda/" nil t)
                              (f-entries "org/" nil t)
                              (f-entries "md/" nil t)
                              (f-entries "library/" nil t)
                              (f-entries "txt/" nil t)
                              (f-entries "elisp/" nil t))
                     (--map (s-chop-prefix (s-concat default-directory "/") it))
                     (mapcar #'(lambda (x)
                                 (if (s-matches-p
                                      (rx
                                       (or (and "."
                                                (or "md" "markdown" "org" "txt" "plu" "org.gpg")
                                                eol)
                                          (and bol "elisp/" (* not-newline) (group "el" eol))
                                          (and bol "txt/" (* not-newline) (group "txt" eol))))
                                      x)
                                     x)))
                     (-flatten)
                     (--filter (not (s-matches? (rx "/reveal.js/") it )))
                     (projectile-sort-by-recentf-first)
                     (projectile-sort-by-recently-active-first)
                     )))
    (if files
        (find-file (completing-read "" files))
      (message "Err0#wr"))))
(bind-key "C-x f n" 'find-notes)
(bind-key "C-h n" 'find-notes)

(defun my/find-file-at-point ()
  "Like `find-file-at-point' but understands line number suffixes.
Handles FILE:LINE, FILE:LINE:COL and FILE:START-END.  For a range the
lines are marked as the active region.  Without a line suffix, or if
the file does not exist, falls back to `find-file-at-point'."
  (interactive)
  (let ((thing (thing-at-point 'filename t)))
    (if (and thing
             (string-match
              (rx bos (group (+? anychar))
                  ":" (group (+ digit))
                  (? (or (seq "-" (group (+ digit)))
                         (seq ":" (group (+ digit)))))
                  (* (any ".,;:"))
                  eos)
              thing)
             (file-exists-p (expand-file-name (match-string 1 thing))))
        (let ((file (expand-file-name (match-string 1 thing)))
              (line (string-to-number (match-string 2 thing)))
              (end (and (match-string 3 thing)
                        (string-to-number (match-string 3 thing))))
              (col (and (match-string 4 thing)
                        (string-to-number (match-string 4 thing)))))
          (find-file file)
          (widen)
          (goto-char (point-min))
          (forward-line (1- line))
          (when col
            (move-to-column (1- col)))
          (when (and end (> end line))
            (push-mark (save-excursion
                         (forward-line (- end line))
                         (line-end-position))
                       t t)))
      (call-interactively #'find-file-at-point))))
(defalias 'my/ffap #'my/find-file-at-point)
(defalias 'visit-file #'my/find-file-at-point)


;;;;; sudo-edit

(defalias 'sudo-edit-current-file #'tramp-revert-buffer-with-sudo)


;;;;; buffer saving

(defun silent-save-some-buffers ()
  "Save buffers..."
  (save-window-excursion
    (--each (buffer-list)
      (with-current-buffer it
        (and
         (buffer-live-p it)
         (buffer-modified-p it)
         (not (eq major-mode 'messages-buffer-mode))
         (not (buffer-base-buffer it))
         (buffer-file-name it)
         (verify-visited-file-modtime it)
         (save-buffer))))))

(add-hook 'focus-out-hook 'silent-save-some-buffers)


;;;;; touch-file

(defun touch-file ()
  "updates mtime on the file for the current buffer"
  (interactive)
  (shell-command (concat "touch " (shell-quote-argument (buffer-file-name))))
  (clear-visited-file-modtime))


;;;;; shell command after save

(defvar shell-command-after-save-cmd nil
  "This string will be executed as a shell command after saving
  the buffer.")

(defun shell-command-after-save-run ()
  (unless (s-blank? shell-command-after-save-cmd)
    (save-window-excursion
      (async-shell-command shell-command-after-save-cmd))))

(add-hook 'after-save-hook 'shell-command-after-save-run)

(defun shell-command-after-save (cmd)
  (interactive
   (list
    (read-string "After save shell command:" shell-command-after-save-cmd)))
  (setq-local shell-command-after-save-cmd cmd))


;;;;; make-script-executable

(when (not noninteractive)
  (add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p))


;;;; calling external commands

(defun pip-freeze ()
  "Run pip freeze"
  (interactive)
  (shell-command "pip freeze" "*pip-freeze*")
  (with-current-buffer "*pip-freeze*"
    (read-only-mode 1)))

(defun image-identify ()
  "Run identify on current picture mode bufffer."
  (interactive)
  (shell-command
   (concat  "identify -verbose "  (buffer-file-name)) "*identify*")
  (with-current-buffer "*identify*"
    (read-only-mode 1)))


;;;; editing/inserting/in buffer navigation

;;;;; yank-selection

(defun yank-selection ()
  "Insert the primary selection"
  (interactive)
  (when select-active-regions
    (let (select-active-regions)
      (deactivate-mark)))
  (let ((primary (gui-get-selection)))
    (push-mark)
    (insert-for-yank primary)))
(bind-key "C-s-v" 'yank-selection)


;;;;; yank-all-visible-text

(defun yank-all-visible-text ()
  "Yank all visible text from all frames and windows into the kill ring."
  (interactive)
  (let ((text ""))
    (dolist (frame (visible-frame-list))
      (dolist (window (window-list frame 'no-minibuffer))
        (with-current-buffer (window-buffer window)
          (setq text (concat text
                             (format "\n--- Buffer: %s ---\n" (buffer-name))
                             (buffer-substring-no-properties
                              (window-start window)
                              (window-end window)))))))
    (when (string< "" text)
      (kill-new text)
      (message "Yanked %d characters from all visible windows." (length text)))))


;;;;; enable "regular" backspace behaviour in isearch

(defun isearch-delete-something ()
  "Delete non-matching text or the last character."
  (interactive)
  (if (= 0 (length isearch-string))
      (ding)
    (setq isearch-string
          (substring isearch-string
                     0
                     (or (isearch-fail-pos) (1- (length isearch-string)))))
    (setq isearch-message
          (mapconcat #'isearch-text-char-description isearch-string "")))
  (if isearch-other-end (goto-char isearch-other-end))
  (isearch-search)
  (isearch-push-state)
  (isearch-update))

(define-key isearch-mode-map (kbd "<backspace>")
  #'isearch-delete-something)


;;;;; duplicate line /  region

(defun duplicate-line-or-region (arg)
  "Duplicates the current line or region ARG times.
If there's no region, the current line will be duplicated. However, if
there's a region, all lines that region covers will be duplicated."
  (interactive "p")
  (let (beg end (n (abs arg)) (origin (point)))
    (if (and mark-active (> (point) (mark)))
        (exchange-point-and-mark))
    (setq beg (line-beginning-position))
    (if mark-active
        (exchange-point-and-mark))
    (setq end (line-end-position))
    (let ((beg beg) (end end) (region (buffer-substring-no-properties beg end)))
      (dotimes (i n)
        (goto-char end)
        (newline)
        (insert region)
        (setq end (point)))
      (goto-char (+ origin (* (length region) n) n)))
    (if (< arg 0)
        (comment-region beg end))))

(defun duplicate-line-or-region-comment-original ()
  "See `duplicate-line-or-region'"
  (interactive)
  (duplicate-line-or-region -1))

(bind-key "C-x y" 'duplicate-line-or-region)
(bind-key "C-x C-y" 'duplicate-line-or-region-comment-original)

(defun one-shot-keybinding (key command)
  (set-transient-map
   (let ((map (make-sparse-keymap)))
     (define-key map (kbd key) command)
     map) t))

(defun duplicate-region (&optional num start end)
  "Duplicates the region bounded by START and END NUM times.
If no START and END is provided, the current region-beginning and
region-end is used."
  (interactive "p")
  (save-excursion
    (let* ((start (or start (region-beginning)))
           (end (or end (region-end)))
           (region (buffer-substring start end)))
      (goto-char end)
      (dotimes (i num)
        (insert region)))))

(bind-key "Y" 'duplicate-region region-bindings-mode-map)


;;;;; open line above/below

(defun open-line-below ()
  "Open a line below the line the point is at.
Then move to that line and indent accordning to mode"
  (interactive)
  (cond ((or (eq major-mode 'feature-mode))
         (let ((column
                (save-excursion
                  (back-to-indentation)
                  (current-column))))
           (move-end-of-line 1)
           (newline)
           (move-to-column column t)))
        (t
         (move-end-of-line 1)
         (newline)
         (indent-according-to-mode))))

(defun open-line-above ()
  "Open a line above the line the point is at.
Then move to that line and indent accordning to mode"
  (interactive)
  (cond ((or (eq major-mode 'feature-mode))
         (let ((column
                (save-excursion
                  (back-to-indentation)
                  (current-column))))
           (move-beginning-of-line 1)
           (newline)
           (forward-line -1)
           (move-to-column column t)))
        (t
         (move-beginning-of-line 1)
         (newline)
         (forward-line -1)
         (indent-according-to-mode))))

(defun new-line-in-between ()
  (interactive)
  (newline)
  (save-excursion
    (newline)
    (indent-for-tab-command))
  (indent-for-tab-command))

;; (bind-key "C-c C-p" 'open-line-above)
;; (bind-key "C-c C-n" 'open-line-below)
;; (bind-key "C-c C-j" 'new-line-in-between)

(defun new-line-dwim ()
  (interactive)
  (let ((break-open-pair (or (and (looking-back "{" 1) (looking-at "}"))
                             (and (looking-back ">" 1) (looking-at "<"))
                             (and (looking-back "(" 1) (looking-at ")"))
                             (and (looking-back "\\[" 1) (looking-at "\\]")))))
    (newline)
    (when break-open-pair
      (save-excursion
        (newline)
        (indent-for-tab-command)))
    (indent-for-tab-command)))
(bind-key "<M-return>" 'new-line-dwim)


;;;;; join line

(defun my-join-line ()
  "Join lines"
  (interactive)
  (save-excursion
    (join-line -1)))


;;;;; join-region

(defun join-region (beg end)
  "Apply join-line over region."
  (interactive "r")
  (if mark-active
      (let ((beg (region-beginning))
            (end (copy-marker (region-end))))
        (goto-char beg)
        (while (< (point) end)
          (join-line 1)))))


;;;;; character coding conversion

(defun has-revisit-file-with-coding-windows-1252 ()
  "Re-opens currently visited file with the windows-1252 coding. (By: hassansrc at gmail dot com)
    Example:
    the currently opened file has french accents showing as codes such as:
        french: t\342ches et activit\340s   (\340 is shown as a unique char)
    then execute this function: has-revisit-file-with-coding-windows-1252
      consequence: the file is reopened with the windows-1252 coding with no other action on the part of the user.
                   Hopefully, the accents are now shown properly.
                   Otherwise, find another coding... "
  (interactive)
  (let ((coding-system-for-read 'windows-1252)
        (coding-system-for-write 'windows-1252)
        (coding-system-require-warning t)
        (current-prefix-arg nil))
    (find-alternate-file buffer-file-name)))


;;;;; align

(defun align= (begin end)
  "Align region to equal signs"
  (interactive "r")
  (align-regexp begin end "\\(\\s-*\\)[=|:]" 1 1))


;;;;; insert random number

(defun insert-random-number (NUM)
  "Insert NUM random digits.
NUM default to 8."
  (interactive "P")
  (let ((charset "1234567890" )
        (base-count 10))
    (dotimes (_ (if (numberp NUM) (abs NUM) 8))
      (insert (elt charset (random base-count))))))


;;;;; insert date/time formatted strings

(defvar current-date-time-format "%Y-%m-%d %H:%M"
  "Format of date to insert with `insert-current-date-time' func
See help of `format-time-string' for possible replacements")

(defvar current-date-time-format-long "%a %b %d %H:%M:%S %Z %Y"
  "Format of date to insert with `insert-current-date-time' func
See help of `format-time-string' for possible replacements")

(defvar current-date-time-format-compact "%Y%m%d-%H%M%S-%Z"
  "Format of date to insert with `insert-current-date-time' func
See help of `format-time-string' for possible replacements")

(defun insert-current-date-time ()
  "insert the current date and time into current buffer.
Uses `current-date-time-format' for the formatting the date/time."
  (interactive)
  (insert (format-time-string current-date-time-format (current-time))))

(defun insert-current-date-time-long ()
  "insert the current date and time into current buffer.
Uses `current-date-time-format' for the formatting the date/time."
  (interactive)
  (insert (format-time-string current-date-time-format-long (current-time))))

(defun insert-current-date-time-compact ()
  "insert the current date and time into current buffer.
Uses `current-date-time-format' for the formatting the date/time."
  (interactive)
  (insert (format-time-string current-date-time-format-compact (current-time))))

(defun week-number (date)
  (org-days-to-iso-week
   (calendar-absolute-from-gregorian date)))

(defun insert-current-week-number ()
  (interactive)
  (insert (number-to-string (week-number (calendar-current-date)))))


;;;;; CamelCase transform

(defun backward-hyphenated-or-underscore-word ()
  (interactive)
  (backward-word)
  (while (looking-back "[-_]" 1) (backward-word)))

(defun camelCase-previous-word ()
  "Convert the previous word (including hyphens and underscores) to camelCase."
  (interactive)
  (let ((case-fold-search nil)
        (bound (point)))
    (save-excursion
      (backward-hyphenated-or-underscore-word)
      (while (re-search-forward "[\-_]\\([a-zA-Z]\\)" bound t)
        (replace-match (upcase (match-string 1)) nil 'literal)))))

(defun unCamelCase-previous-word (&optional sep)
  "If previous word is camelCased, convert it to a word separated by SEP.

Default separator is underscore."
  (interactive)
  (let ((case-fold-search nil)
        (bound (point))
        (sep (or sep "_")))
    (save-excursion
      (backward-hyphenated-or-underscore-word)
      (while (re-search-forward "\\([a-z]\\)\\([A-Z]\\)" bound t)
        (replace-match (concat (match-string 1) sep
                               (downcase (match-string 2))) nil 'literal)))))


;;;;; eval-and-replace

(defun eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))


;;;;; beginning of line or indentation

(defun beginning-of-line-or-indentation ()
  "move to beginning of line, or indentation"
  (interactive)
  (if (bolp)
      (back-to-indentation)
    (beginning-of-line)))


;;;;; delete trailing blank lines

(defun my-delete-trailing-blank-lines ()
  "Deletes all blank lines at the end of the file, even the last one"
  (interactive)
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-max))
      (delete-blank-lines)
      (let ((trailnewlines (abs (skip-chars-backward "\n\t"))))
        (if (> trailnewlines 0)
            (progn
              (delete-char trailnewlines)))))))


;;;;; collapse blank lines

(defun collapse-blank-lines (start end)
  (interactive "r")
  (replace-regexp "^\n\\{2,\\}" "\n" nil start end))


;;;;; flush blank lines

(defun flush-blank-lines (start end)
  (interactive "r")
  (flush-lines "^\\s-*$" start end nil))


;;;;; swap-string

(defun swap-string (rit lft)
  "Swaps rit to lft."
  (interactive "sChange this:
sTo this: ")
  (save-excursion
    (goto-char (region-beginning))
    (while  (search-forward-regexp (format "%s\\|%s"
                                           (regexp-quote rit)
                                           (regexp-quote lft)) (region-end) t)
      (replace-match (if (equal rit (match-string 0)) lft rit) t nil))))


;;;;; clean buffer

(defun cleanup-buffer ()
  "Perform a bunch of operations on the whitespace content of a buffer."
  (interactive)
  (save-excursion
    (mark-whole-buffer)
    (clean-up-buffer-or-region)))

(defun clean-up-buffer-or-region ()
  "Untabifies, indents and deletes trailing whitespace from buffer or region."
  (interactive)
  (save-excursion
    (unless (region-active-p)
      (mark-whole-buffer))
    (untabify (region-beginning) (region-end))
    (indent-region (region-beginning) (region-end))
    (save-restriction
      (narrow-to-region (region-beginning) (region-end))
      (delete-trailing-whitespace))))


;;;;; sort-words

(defun sort-words (reverse beg end)
  "Sort words in region alphabetically, in REVERSE if negative.
    Prefixed with negative \\[universal-argument], sorts in reverse.

    The variable `sort-fold-case' determines whether alphabetic case
    affects the sort order.

    See `sort-regexp-fields'."
  (interactive "*P\nr")
  (sort-regexp-fields reverse "\\w+" "\\&" beg end))


;;;;; sort-symbols

(defun sort-symbols (reverse beg end)
  "Sort symbols in region alphabetically, in REVERSE if negative.
    See `sort-words'."
  (interactive "*P\nr")
  (sort-regexp-fields reverse "\\(\\sw\\|\\s_\\)+" "\\&" beg end))


;;;; MISC

;;;;; dring - cycle values from a list

;; it's a dumb ring

(defun dring-next-element (value lst)
  "Returns the element next after value from or the first element if value isn't in the lst."
  (nth (mod (+ 1 (or (cl-position value lst :test 'equal) -1))
            (length lst))
       lst))

(defun dring-set-next-element (symbol lst)
  (set symbol (dring-next-element (symbol-value symbol) lst)))


;;;;; cycle ispell languages

;; Languages for spellinc cycling
(let ((langs '("svenska" "english")))
  (setq lang-ring (make-ring (length langs)))
  (dolist (elem langs) (ring-insert lang-ring elem)))

(defun cycle-ispell-languages ()
  "Cycle spelling dictionaries from a list"
  (interactive)
  (let ((lang (ring-ref lang-ring -1)))
    (ring-insert lang-ring lang)
    (ispell-change-dictionary lang)
    (when (and (boundp 'flyspell-mode)
               (eq flyspell-mode t))
      (flyspell-buffer))))


;;;;; goto line with feedback

(defun goto-line-with-feedback ()
  "Show line numbers temporarily, while prompting for the line number input"
  (interactive)
  (unwind-protect
      (progn
        (display-line-numbers-mode 1)
        (call-interactively 'goto-line))
    (display-line-numbers-mode -1))
  (my-pulse-line))


(global-set-key [remap goto-line] 'goto-line-with-feedback)


;;;;; menu-bar-go

(defvar menu-bar-go-active nil)
(defun menu-bar-go-post ()
  (remove-hook 'pre-command-hook 'menu-bar-go-post)
  (when (and menu-bar-go-active (not menu-bar-mode))
    (set-frame-parameter (selected-frame) 'menu-bar-lines 0)))
(defun menu-bar-go ()
  "Open menu bar"
  (interactive)
  (if (or menu-bar-mode (not window-system))
      (menu-bar-open)
    (set-frame-parameter (selected-frame) 'menu-bar-lines 1)
    ;; (menu-bar-mode 1)
    (run-with-idle-timer
     0.2 nil (lambda ()
               (menu-bar-open)
               (setq menu-bar-go-active t)
               (add-hook 'pre-command-hook 'menu-bar-go-post)))))
(bind-key "M-o M-m" 'menu-bar-go)


;;;;; centering margins + easy-read-mode

(defun auto-window-margins ()
  "Set window margins according to fill column."
  (when (not (or

              (window-minibuffer-p)))
    (and (boundp 'org-indent-mode)
         org-indent-mode
         (org-indent-mode -1))
    (and (boundp 'page-break-lines-mode)
         page-break-lines-mode
         (page-break-lines-mode -1))
    (set-window-margins nil 0)
    (let
        ((margin (max 0 (/ (- (window-body-width) fill-column) 2))))
      (set-window-margins nil margin margin))))


(define-minor-mode auto-window-margins-mode
  "..."
  :group 'easy-read
  :global t
  (if auto-window-margins-mode
      (progn
        (dolist (win (window-list))
          (with-selected-window win
            (auto-window-margins)))
        (add-hook 'window-configuration-change-hook 'auto-window-margins))
    (remove-hook 'window-configuration-change-hook 'auto-window-margins)
    (dolist (win (window-list))
      (set-window-margins win 0))))

(define-minor-mode easy-read-mode
  "..."
  :group 'easy-read
  :global t
  (if easy-read-mode
      (progn
        (delete-other-windows)
        (defvar font-normal-height (face-attribute 'default :height))
        (defvar normal-line-spacing line-spacing)
        (set-face-attribute 'default (selected-frame) :height 135)
        (set-face-attribute 'fringe (selected-frame)
                            :background (frame-parameter nil 'background-color))
        (setq-default line-spacing 0.2)
        (and (boundp 'rainbow-delimiters-mode)
             rainbow-delimiters-mode
             (rainbow-delimiters-mode -1))
        (and (boundp 'yascroll-bar-mode)
             yascroll-bar-mode
             (yascroll-bar-mode -1))
        (auto-window-margins-mode 1))

    (auto-window-margins-mode -1) (set-face-attribute 'default (selected-frame)  :height font-normal-height)
    (setq-default line-spacing normal-line-spacing)))



(define-minor-mode present-read-mode
  "..."
  :group 'present-read
  :global t
  (if present-read-mode
      (progn
        (delete-other-windows)
        (defvar font-normal-height (face-attribute 'default :height))
        (defvar normal-line-spacing line-spacing)
        (set-face-attribute 'default (selected-frame) :height 170)
        (set-face-attribute 'fringe (selected-frame)
                            :background (frame-parameter nil 'background-color))
        (setq-default line-spacing 0.25)
        (and (boundp 'rainbow-delimiters-mode)
             rainbow-delimiters-mode
             (rainbow-delimiters-mode -1))
        (and (boundp 'yascroll-bar-mode)
             yascroll-bar-mode
             (yascroll-bar-mode -1)))

    (set-face-attribute 'default (selected-frame)  :height font-normal-height)
    (setq-default line-spacing normal-line-spacing)))


;;;;; init magit status

(defun init-magit-status (path)
  "Magit status, no other windows.
Used to launch magit status from command line."
  (interactive)
  (magit-status (f-full path))
  (delete-other-windows)
  (kill-buffer "*scratch*")
  (kill-buffer "*Messages*"))


;;;;; my notify

(defvar my-notify-method nil)

(defun my-notify-setup nil
  (setq my-notify-method
        (cond
         ((and
           window-system
           (not (eq system-type 'darwin))
           (require 'dbus nil t)
           (dbus-ping :session "org.freedesktop.Notifications" 250))
          'my-notify-dbus)
         (t 'my-notify-message) )))

(defun my-notify-message (summary body)
  (message "%s: %s" summary body))

(defun my-notify-dbus (summary body)
  (dbus-call-method
   :session "org.freedesktop.Notifications"
   "/org/freedesktop/Notifications"
   "org.freedesktop.Notifications" "Notify"
   "shorter"
   0
   ""
   summary
   body
   '(:array)
   '(:array :signature "{sv}")
   :int32 -1))

(defun my-notify (summary body)
  (unless my-notify-method (my-notify-setup))
  (funcall my-notify-method summary body))


;;;;; invert-shift-number-keys-mode

(define-minor-mode invert-shift-number-keys-mode
  "..."
  :group 'my
  :global t
  (if (not invert-shift-number-keys-mode)
      (--each us-number-shift-keys
        (define-key key-translation-map  (kbd (car it) ) nil )
        (define-key key-translation-map  (kbd (cdr it) ) nil ))
    (--each us-number-shift-keys
      (define-key key-translation-map (kbd (car it)) (kbd (cdr it)))
      (define-key key-translation-map (kbd (cdr it)) (kbd (car it))))
    (bind-key "C-x !" 'delete-other-windows)
    (bind-key "C-x @" 'split-window-below)
    (bind-key "C-x #" 'split-window-right)
    (bind-key "C-x $" 'ctl-x-4-prefix)
    (bind-key "C-x %" 'ctl-x-5-prefix)))
(bind-key "M-o n" 'invert-shift-number-keys-mode)

(defvar us-number-shift-keys
  '(("1" . "!")
    ("2" . "@")
    ("3" . "#")
    ("4" . "$")
    ("5" . "%")
    ("6" . "^")
    ("7" . "&")
    ("8" . "*")
    ("9" . "(")
    ("0" . ")")))


;;;;; get-dwim-at-point

(defun get-dwim-at-point ()
  "If there's an active selection, return that.
Otherwise, get the symbol at point, as a string."
  (cond ((use-region-p)
         (buffer-substring-no-properties (region-beginning) (region-end)))
        ((symbol-at-point)
         (substring-no-properties
          (symbol-name (symbol-at-point))))))


;;;;; go-build-tags

(defgroup go-build-tags nil
  "Manage Go build tags for Flycheck, lsp-mode and possibly other tools."
  :group 'languages)

(defcustom go-build-tags-list '()
  "List of currently active Go build tags."
  :type '(repeat string)
  :group 'go-build-tags)

(defun go-build-tags-sync ()
  "Sync `go-build-tags-list` to Flycheck and LSP variables."
  (let* ((tags go-build-tags-list)
         (comma-tags (mapconcat 'identity tags ",")))

    ;; Update Flycheck
    (setq flycheck-go-build-tags tags)

    ;; Update LSP (gopls) as a single comma-separated flag in a vector
    (setq lsp-gopls-build-flags
          (if (string-empty-p comma-tags)
              []
            (vector (concat "-tags=" comma-tags))))

    ;; Refresh LSP workspace
    (when (fboundp 'lsp-workspace-restart)
      (condition-case nil
          (lsp-workspace-restart (lsp-find-workspace 'gopls (buffer-file-name)))
        (error (message "Gopls not active; tags updated."))))

    (go-build-tags-show-active)))

(defun go-build-tags-show-active ()
  "Display the currently active Go build tags in the minibuffer."
  (interactive)
  (if go-build-tags-list
      (message "Active Go build tags: [%s]"
               (mapconcat 'identity go-build-tags-list ", "))
    (message "No Go build tags currently active.")))

(defun go-build-tags-add (tag)
  "Add a Go build TAG and sync."
  (interactive "sTag to add: ")
  (unless (member tag go-build-tags-list)
    (push tag go-build-tags-list)
    (go-build-tags-sync)))

(defun go-build-tags-remove (tag)
  "Remove a Go build TAG and sync."
  (interactive
   (list (completing-read "Tag to remove: " go-build-tags-list)))
  (setq go-build-tags-list (delete tag go-build-tags-list))
  (go-build-tags-sync))

(defun go-build-tags-toggle-at-point ()
  "Add or remove the Go build tag at point."
  (interactive)
  (let ((tag (thing-at-point 'word t)))
    (if (not tag)
        (message "No word found at point.")
      (if (member tag go-build-tags-list)
          (go-build-tags-remove tag)
        (go-build-tags-add tag)))))


;;;;; selective display menu


(defun create-temp-selective-display-keymap ()
  (set-transient-map
   (let ((map (make-sparse-keymap)))
     (define-key map (kbd "=") 'inc-selective-display)
     (define-key map (kbd "+") 'inc-selective-display)
     (define-key map (kbd "-") 'dec-selective-display)
     (define-key map (kbd "0") 'clear-selective-display)
     map))
  (message "Type +/= to reveal more, - for less, 0 to reset. (%s)" selective-display))

(defun inc-selective-display (arg)
  (interactive "P")
  (if (numberp arg)
      (set-selective-display arg)
    (if (numberp selective-display)
        (set-selective-display (+ 2 selective-display))
      (set-selective-display 2)))
  (create-temp-selective-display-keymap))

(defun dec-selective-display ()
  (interactive)
  (when (and (numberp selective-display)
             (> selective-display 2))
    (set-selective-display (- selective-display 2)))
  (create-temp-selective-display-keymap))

(defun clear-selective-display ()
  (interactive)
  (when (numberp selective-display)
    (set-selective-display nil)))

(global-set-key (kbd "C-x $") 'inc-selective-display)


;;;; ////uncategorized////

(defun buffer-line-position ()
  "Current position formatted as file-name:line-number"
  (format "%s:%d" (buffer-file-name) (line-number-at-pos)))

(defun position-to-kill-ring ()
  "Copy to the kill ring a string in the format \"file-name:line-number\"
for the current buffer\\='s file name, and the line number at point."
  (interactive)
  (kill-new (buffer-line-position)))

(defun position-pdb-breakpoint-to-kill-ring ()
  ""
  (interactive)
  (kill-new (format "break %s" (buffer-line-position))))

(defun position-pdb-breakpoint-to-clipbard ()
  ""
  (interactive)
  (kill-new (format "break %s" (buffer-line-position))))

(defun buffer-file-name-to-clipboard  ()
  ""
  (interactive)
  (simpleclip-set-contents buffer-file-name))


(defun clone-buffer-and-narrow-to-function ()
  (interactive)
  (require 'which-func)
  (clone-indirect-buffer-other-window (which-function) 'pop-to-buffer)
  (mark-defun)
  (narrow-to-region (mark) (point))
  (pop-mark))
(bind-key "C-x n f" 'clone-buffer-and-narrow-to-function)

(defun narrow-to-region-indirect (start end)
  "Restrict editing in this buffer to the current region, indirectly."
  (interactive "r")
  (deactivate-mark)
  (let ((buf (clone-indirect-buffer nil nil)))
    (with-current-buffer buf
      (narrow-to-region start end))
    (switch-to-buffer buf)))
(bind-key "C-x n c" 'narrow-to-region-indirect)

(defun toggle-fold ()
  "Toggle fold all lines larger than indentation on current line"
  (interactive)
  (let ((col 1))
    (save-excursion
      (back-to-indentation)
      (setq col (+ 1 (current-column)))
      (set-selective-display
       (if selective-display nil (or col 1))))))
;; (bind-key* "C-M-f" 'toggle-fold)


(hook-into-modes #'(lambda () (setq-local fill-column 120))
                 my-html-like-mode-hooks)



(provide 'init-commands)

;; Local Variables:
;; eval: (outline-minor-mode 1)
;; eval: (require 'use-package)
;; End:


;;; init-commands.el ends here
