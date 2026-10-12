;;; init-org.el --- org, calendar and mail -*- lexical-binding: nil; byte-compile-warnings: (not docstrings-wide); -*-

;;; Commentary:

;; use-package configuration for org, calendar, todo lists and
;; mail (mu4e).
;;
;; Sections are kept in alphabetical order.
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


;;; use-package

;;;; calendar

(use-package calendar
  :defer
  :init
  (progn
    (setq ;; Geolocation (Stockholm)
     calendar-latitude 59.3
     calendar-longitude 18.0
     calendar-location-name "Stockholm, SE"))
  :config
  (progn
    (load "sv-kalender" 'noerror 'nomessage)))


;;;; calfw

(use-package calfw
  :ensure t
  :commands cfw:open-calendar-buffer
  :init
  (progn

    (use-package calfw-org
      :ensure t
      :commands cfw:open-org-calendar
      :config
      (progn
        (defun cfw:open-org-calendar ()
          "Open an org schedule calendar in the new buffer."
          (interactive)
          (let* ((source1 (cfw:org-create-source))
                 (cp (cfw:create-calendar-component-buffer
                      :view 'two-weeks
                      :contents-sources (list source1)
                      :custom-map cfw:org-schedule-map
                      :sorter 'cfw:org-schedule-sorter)))
            (switch-to-buffer (cfw:cp-get-buffer cp))))))))


;;;; mu4e

(use-package mu4e
  :if (file-exists-p "~/.mail/gmail")
  :commands (mu4e
             mu4e~headers-jump-to-maildir)
  :bind (("C-x m" . mu4e))
  :init
  (progn
    (add-hook 'mu4e-compose-mode-hook 'epa-mail-mode)
    (add-hook 'mu4e-view-mode-hook 'epa-mail-mode)
    (when window-system
      (setq
       mu4e-headers-draft-mark     '("D" . "⚒")
       mu4e-headers-flagged-mark   '("!" . "⚑")
       mu4e-headers-new-mark       '("" . "⭑")
       mu4e-headers-passed-mark    '(">" . "❯")
       mu4e-headers-replied-mark   '("<" . "❮")
       mu4e-headers-seen-mark      '("" . "✔")
       mu4e-headers-trashed-mark   '("T" . "♻")
       mu4e-headers-attach-mark    '("a" . "⚓")
       mu4e-headers-encrypted-mark '("x" . "⚴")
       mu4e-headers-signed-mark    '("s" . "☡")
       mu4e-headers-unread-mark    '("" . "☐")
       ;; thread prefix marks
       mu4e-headers-has-child-prefix    '("+"  . "◼")
       mu4e-headers-empty-parent-prefix '("-"  . "◽")
       mu4e-headers-first-child-prefix  '("\\" . "┗▶")
       mu4e-headers-duplicate-prefix    '("="  . "⚌")
       mu4e-headers-default-prefix       '("|"  . "┃")))

    (setq mu4e-maildir "~/.mail/gmail"
          mu4e-attachment-dir "~/Downloads"
          mu4e-drafts-folder "/[Gmail].Drafts"
          mu4e-get-mail-command "mbsync -q -V gmail"
          mu4e-confirm-quit nil
          mu4e-headers-date-format "%x %R"
          mu4e-headers-fields '
          ((:date . 17)
           (:flags . 6)
           (:from . 22)
           ;; (:thread-subject)
           (:subject)
           )
          mu4e-headers-include-related nil
          mu4e-headers-leave-behavior 'apply
          mu4e-headers-results-limit 1000
          mu4e-headers-show-threads nil
          mu4e-headers-skip-duplicates t
          mu4e-refile-folder "/[Gmail].All Mail"
          mu4e-sent-folder   "/[Gmail].Sent Mail"
          mu4e-sent-messages-behavior 'delete
          mu4e-trash-folder  "/[Gmail].Trash"
          mu4e-use-fancy-chars nil
          mu4e-view-show-addresses t
          mu4e-view-show-images t
          ;;mu4e-update-interval (* 4 60)
          ))

  :config
  (progn

    (use-package mu4e-contrib
      :config
      (progn
        (defun mu4e-shr2text ()
          "Html to text using the shr engine; this can be used in
`mu4e-html2text-command' in a new enough emacs. Based on code by
Titus von der Malsburg."
          (interactive)
          (let ((dom (libxml-parse-html-region (point-min) (point-max)))
                (shr-inhibit-images t))
            (erase-buffer)
            (shr-insert-document dom)
            (goto-char (point-min))))

        (setq mu4e-html2text-command 'mu4e-shr2text)))

    (defun mu4e-msgv-action-view-in-browser (msg)
      "View the body of the message in a web browser."
      (interactive)
      (let ((html (mu4e-msg-field (mu4e-message-at-point t) :body-html))
            (tmpfile (format "%s/%d.html" temporary-file-directory (random))))
        (unless html (error "No html part for this message"))
        (with-temp-file tmpfile
          (insert
           "<html>"
           "<head><meta http-equiv=\"content-type\""
           "content=\"text/html;charset=UTF-8\">"
           html))
        (browse-url (concat "file://" tmpfile))))
    (add-to-list 'mu4e-view-actions
                 '("View in browser" . mu4e-msgv-action-view-in-browser) t)))


;;;; org

(eval-and-compile
  (with-eval-after-load "package"
    (setq package--builtins (assq-delete-all 'org package--builtins))
    (setq package--builtin-versions (assq-delete-all 'org package--builtin-versions))))

(use-package org
  ;; :ensure org
  :defer 28
  :commands (org
             org-capture
             org-mode
             org-store-link
             update-org-hours
             my-term-agenda
             dired-notes
             jump-to-org-agenda)
  :mode (("\\.org_archive\\'" . org-mode)
         ("\\.org\\'" . org-mode))
  :bind (("C-c l" . org-store-link)
         ("C-M-r" . org-capture)
         ("C-h t" . org-capture)
         ("C-c a" . org-agenda)

         ;; ("<f10>" . jump-to-org-agenda)
         )
  :init
  (progn
    (setq
     org-directory user-notes-directory
     org-clock-persist-file (expand-file-name
                             "org-clock-save.el" user-data-directory)
     org-id-locations-file (expand-file-name
                            "org-id-locations" user-data-directory))

    (use-package org-annotate-file
      :bind ("C-c C-l" . org-annotate-file)))
  :config
  (progn

    (defun my/update-org-yank-image-dir ()
      "Set the image save directory relative to the current org file."
      (when (derived-mode-p 'org-mode)
        (let* ((filename (buffer-file-name))
               (dir-name (when filename
                           (concat (file-name-sans-extension (file-name-nondirectory filename))
                                   "-images/"))))
          (setq-local org-yank-image-save-method dir-name))))

    (add-hook 'org-mode-hook #'my/update-org-yank-image-dir)

    (defun my/org-timestamp-us-weekday (orig format-string &rest args)
      "Format Org timestamps with English weekday names, keeping the locale elsewhere."
      (let ((system-time-locale
             (if (and (stringp format-string)
                      (string-search (car org-timestamp-formats) format-string))
                 "C"
               system-time-locale)))
        (apply orig format-string args)))

    (advice-add 'format-time-string :around #'my/org-timestamp-us-weekday)

    (unbind-key "M-h" org-mode-map)
    (bind-key "C-c h" 'org-todo org-mode-map )
    (setq org-modules '(org-bbdb org-bibtex org-docview org-habit
                                 org-id org-info org-man org-w3m))
    ;; org-git-link
    ;; (require 'ox-reveal)

    (and window-system
         (require 'org-bullets nil t)
         (fboundp 'org-bullets-mode)
         (add-hook 'org-mode-hook 'org-bullets-mode))


    (use-package server
      :defer
      :config
      (progn

        (use-package org-protocol)))
    (when (require 'org-mu4e nil t)
      (defun my-mu4e-link-descr (msg)
        (let ((subject (or (plist-get msg :subject)
                           "No subject"))
              (date (or (format-time-string mu4e-headers-date-format
                                            (mu4e-msg-field msg :date))
                        "No date")))
          (concat subject " " date)))
      (setq org-mu4e-link-desc-func 'my-mu4e-link-descr))

    (setq *orgtrello-log/level* 2)
    ;; (require 'org-trello)
    (add-hook 'org-mode-hook
              (lambda ()
                (when (outline-invisible-p)
                  (save-excursion
                    (outline-previous-visible-heading 1)
                    (org-fold-show-subtree)))))

    (defun update-org-hours (n)
      "Change all org-mode timestamps in the current buffer by N hours."
      (interactive "nModify hours: ")
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward "[[<]" nil t)
          (when (org-at-timestamp-p t)
            (org-timestamp-change n 'hour)))))

    (defun insert-file-as-org-table (filename)
      "Insert a file at point and convert it to an org table.
Argument FILENAME File to insert."
      (interactive (list (read-file-name "csv file: ")))
      (let* ((start (point))
             (end (+ start (nth 1 (insert-file-contents filename)))))
        (org-table-convert-region start end)))

    (let*
        ((agenda-dir (f-expand "agenda" user-notes-directory))
         (org-dir (f-expand "agenda" user-notes-directory))

         (agenda-with-archives
          (if (f-dir? agenda-dir)
              (f-entries agenda-dir
                         (lambda (s) (--any? (s-suffix? it s)
                                             '(".org" ".org_archive"))))
            '()))
         (org-with-archives
          (if (f-dir? org-dir)
              (f-entries org-dir
                         (lambda (s) (--any? (s-suffix? it s)
                                             '(".org" ".org_archive"))))
            '()))
         (all-with-archives (-concat agenda-with-archives org-with-archives))
         (agenda (--filter (s-suffix? ".org" it) agenda-with-archives)))
      (setq
       org-agenda-files agenda
       org-agenda-text-search-extra-files all-with-archives
       ))

    (setq
     org-src-fontify-natively t
     org-src-preserve-indentation t
     org-adapt-indentation nil
     org-contacts-files (list (f-expand
                               "agenda/contacts.org"
                               user-notes-directory))
     org-annotate-file-storage-file (f-expand
                                     "org/file-annotations.org"
                                     user-notes-directory)
     ;; org-annotate-file-add-search nil

     ;; org-hide-block-startup t
     org-agenda-block-separator nil
     org-agenda-dim-blocked-tasks nil
     org-agenda-show-all-dates nil
     org-agenda-span 'fortnight
     org-agenda-sticky (not noninteractive)
     org-agenda-tags-todo-honor-ignore-options t
     org-deadline-warning-days 14
     org-agenda-todo-ignore-deadlines t
     org-agenda-todo-ignore-with-date t
     org-agenda-todo-ignore-scheduled t
     org-agenda-hide-tags-regexp "work\\|personal\\|23c"
     org-agenda-skip-deadline-if-done t
     org-agenda-skip-scheduled-if-done t
     org-agenda-skip-unavailable-files t
     org-agenda-use-tag-inheritance t
     org-agenda-show-inherited-tags t
     org-agenda-window-setup 'current-window
     org-columns-ellipses "…"
     org-cycle-separator-lines 1
     org-datetree-add-timestamp t
     org-default-notes-file (expand-file-name "agenda/refile.org" user-notes-directory)
     org-ellipsis "…"
     org-icalendar-alarm-time 240
     org-icalendar-include-body t
     org-icalendar-include-sexps nil
     ;; org-icalendar-include-todo t
     org-icalendar-store-UID t
     org-icalendar-use-deadline '(todo-due event-if-todo event-if-not-todo)
     org-icalendar-use-scheduled '(todo-start)
     org-image-max-width 0.95
     org-log-done 'time
     org-log-reschedule 'time
     org-log-into-drawer t
     org-mobile-inbox-for-pull (expand-file-name "from-mobile.org" user-notes-directory)
     org-outline-path-complete-in-steps t
     org-refile-targets '((nil :maxlevel . 9)
                          (org-agenda-files :maxlevel . 9))
     org-refile-use-outline-path 'file
     org-return-follows-link t
     org-startup-folded nil
     org-startup-indented t
     org-startup-with-inline-images (not noninteractive)
     org-treat-S-cursor-todo-selection-as-state-change nil
     org-use-fast-todo-selection t
     org-use-sub-superscripts '{}
     org-drawers '("PROPERTIES" "LOGBOOK")
     org-clock-continuously t
     org-clock-into-drawer t
     org-clock-history-length 50
     org-clock-in-resume t
     org-clock-idle-time nil
     org-clock-out-remove-zero-time-clocks t
     org-clock-persist t
     org-clock-report-include-clocking-task t
     org-clock-sound t
     org-confirm-babel-evaluate nil
     org-export-use-babel nil
     org-tags-exclude-from-inheritance '("crypt" "flagged"))

    (setq
     org-todo-keywords
     '((sequence "TODO(t)" "NEXT(n)"
                 "|"
                 "DONE(d!/!)")
       (sequence "WAITING(w@/!)" "HOLD(h@/!)"
                 "|"
                 "SOMEDAY(s)" "CANCELLED(c@/!)")
       (sequence "NOTE"))

     org-todo-state-tags-triggers
     '(("CANCELLED")
       ("WAITING")
       ("HOLD")
       ("TODO")
       ("NEXT")
       ("DONE" ("flagged") ))
     org-tag-alist '((:startgroup)
                     ("personal" . ?P)
                     ("work" . ?W)
                     (:endgroup)
                     (:startgroup)
                     ("@errand" . ?E)
                     ("@office" . ?O)
                     ("@travel" . ?T)
                     ("@home" . ?H)
                     (:endgroup)
                     (:newline)
                     (:startgroup)
                     ("task" . ?t)
                     ("bug" . ?b)
                     ("enhancement" . ?e)
                     ("note" . ?n)
                     (:endgroup)
                     ("flagged" . ??)
                     ("project" . ?p)
                     ;; ("calendar_public" . ?C)
                     (:newline)
                     ("waiting" . ?w))
     org-capture-templates nil
     )

    ;; Disable default key bindings for include/remove from org agenda
    (add-hook 'org-mode-hook
              (lambda ()
                (org-defkey org-mode-map "\C-c[" 'undefined)
                (org-defkey org-mode-map "\C-c]" 'undefined)))



    (use-package ob-http :ensure t)
    (use-package ob-go :ensure t)

    (org-babel-do-load-languages
     'org-babel-load-languages
     '((perl . t)
       (ruby . t)
       (shell . t)
       (python . t)
       (emacs-lisp . t)
       (dot . t)
       (ditaa . t)
       (plantuml . t)
       (sql . t)
       (go . t)
       (http . t)))

    (setq-default org-src-lang-modes
                  '(("ocaml" . tuareg) ("elisp" . emacs-lisp) ("ditaa" . artist)
                    ("asymptote" . asy) ("dot" . graphviz-dot) ("sqlite" . sql)
                    ("calc" . fundamental) ("C" . c) ("cpp" . c++)
                    ("screen" . shell-script)))


    (use-package org-agenda
      :if (not noninteractive)
      :defer
      :config
      (progn
        (bind-key "h" 'ibuffer org-agenda-mode-map)
        (org-clock-persistence-insinuate)))

    (defun my-term-agenda ()
      "Get a simple summary view "
      ;; (org-agenda nil "n")
      ;; (set-buffer org-agenda-buffer-name)
      ;; (princ (ansi-green (buffer-string)))

      ;; (org-agenda nil "w")
      ;; (set-buffer org-agenda-buffer-name)
      ;; (princ (ansi-red (buffer-string)))

      (org-agenda nil "a")
      (set-buffer org-agenda-buffer-name)
      (princ (buffer-string)))

    (setq org-agenda-custom-commands
          '(
            (" " "Agenda"
             ((tags "flagged"
                    ((org-agenda-overriding-header "flagged")))
              (agenda "" nil)
              (todo "WAITING"
                    ((org-agenda-overriding-header "Waiting on")
                     (org-agenda-todo-ignore-deadlines nil)
                     (org-agenda-todo-ignore-with-date nil)
                     (org-agenda-todo-ignore-scheduled nil)))
              (tags "refile"
                    ((org-agenda-overriding-header "to refile")
                     (org-tags-match-list-sublevels nil)))
              (todo "NEXT"
                    ((org-agenda-overriding-header "Next up")))
              (todo "HOLD"
                    ((org-agenda-overriding-header "On hold")))
              (todo "-NEXT-WAITING-HOLD"
                    ((org-agenda-overriding-header "The rest")))
              (todo "TODO"
                    ((org-agenda-overriding-header "todo")))
              ))
            ("r" "refile + archive"

             ((tags "ARCHIVE"
                    ((org-agenda-overriding-header "archived")
                     (org-agenda-todo-ignore-deadlines nil)
                     (org-agenda-todo-ignore-with-date nil)
                     (org-agenda-todo-ignore-scheduled nil)
                     (org-agenda-archives-mode 'tree)
                     (org-tags-match-list-sublevels nil)))
              (tags "refile"
                    ((org-agenda-overriding-header "refile")
                     (org-agenda-todo-ignore-deadlines nil)
                     (org-agenda-todo-ignore-with-date nil)
                     (org-agenda-todo-ignore-scheduled nil)
                     (org-tags-match-list-sublevels nil)))
              (todo "CANCELLED"
                    ((org-agenda-overriding-header "Cancelled")
                     (org-agenda-overriding-restriction)
                     (org-agenda-todo-ignore-deadlines nil)
                     (org-agenda-todo-ignore-with-date nil)
                     (org-agenda-todo-ignore-scheduled nil)
                     (org-tags-match-list-sublevels nil)))
              (todo "DONE"
                    ((org-agenda-overriding-header "Done")
                     (org-agenda-todo-ignore-deadlines nil)
                     (org-agenda-todo-ignore-with-date nil)
                     (org-agenda-todo-ignore-scheduled nil)
                     (org-tags-match-list-sublevels nil)))))
            ("n" "notes + someday"
             ((todo "NOTE"
                    ((org-agenda-overriding-header "Notes")
                     (org-agenda-todo-ignore-deadlines nil)
                     (org-agenda-todo-ignore-with-date nil)
                     (org-agenda-todo-ignore-scheduled nil)))
              (todo "SOMEDAY"
                    ((org-agenda-overriding-header "Someday...")
                     (org-agenda-todo-ignore-deadlines nil)
                     (org-agenda-todo-ignore-with-date nil)
                     (org-agenda-todo-ignore-scheduled nil)))))))

    (setq org-agenda-sorting-strategy
          (quote ((agenda habit-down time-up user-defined-up
                          priority-down effort-up category-keep)
                  (todo category-up priority-down effort-up)
                  (tags category-up priority-down effort-up)
                  (search category-up))))

    (defun dired-notes ()
      "Open notes directory in dired."
      (interactive)
      (dired user-notes-directory))

    (defun jump-to-org-agenda ()
      (interactive)
      (window-configuration-to-register :org-agenda-fullscreen)
      (org-agenda nil " ")
      (delete-other-windows))

    (defun org-file-from-subtree (&optional name)
      "Cut the subtree currently being edited and create a new file
from it.

If called with the universal argument, prompt for new filename,
otherwise use the subtree title."
      (interactive "P")
      (org-back-to-heading)
      (let ((filename (cond
                       (current-prefix-arg
                        (expand-file-name
                         (read-file-name "New file name: ")))
                       (t
                        (concat
                         (expand-file-name
                          (org-element-property :title
                                                (org-element-at-point))
                          default-directory)
                         ".org")))))
        (org-cut-subtree)
        (find-file-noselect filename)
        (with-temp-file filename
          (org-mode)
          (yank))))

    (defun my-org-agenda-quit-restore-window-configuration ()
      (when (get-register :org-agenda-fullscreen)
        (jump-to-register :org-agenda-fullscreen)
        (set-register :org-agenda-fullscreen nil)))

    (defadvice org-agenda-quit (after restore-window-configuration activate)
      (my-org-agenda-quit-restore-window-configuration))

    (defadvice org-agenda-Quit (after restore-window-configuration activate)
      (my-org-agenda-quit-restore-window-configuration))

    ;; (run-with-idle-timer 300 t 'jump-to-org-agenda)

    (defun myorg-update-parent-cookie ()
      (when (equal major-mode 'org-mode)
        (save-excursion
          (ignore-errors
            (org-back-to-heading)
            (org-update-parent-todo-statistics)))))

    (defadvice org-kill-line (after fix-cookies activate)
      (myorg-update-parent-cookie))

    (defadvice kill-whole-line (after fix-cookies activate)
      (myorg-update-parent-cookie))

    ))


;;;; org-import-icalendar

(use-package org-import-icalendar
  :commands org-icalendar-import-buffer)


;;;; org-screenshot

(use-package org-screenshot
  :commands org-screenshot-take)


;;;; todotxt

(use-package todotxt
  :ensure t
  :commands todotxt
  :init
  (progn
    (setq
     todotxt-file (expand-file-name "~/notes/txt/todo.txt")))
  :config
  (progn
    (defhydra todotxt-hydra
      ;; (:hint nil :foreign-keys warn :pre (todotxt) )
      (:color pink :hint nil :foreign-keys warn )
      "
^Move^       ^List^               ^Item^                 ^Other^
^^-----------^^-------------------^^---------------------^^------------------
_n_ext       ^_l_ist all           _a_dd                  (_g_) revert
_p_rev       ^h_i_de complete      toggle _c_omplete      _s_ave
^^           ^_/_ filter           _e_dit                 _q_uit
^^           _\\_ remove filter    _t_ag
^^           ^_A_rchive complete   _d_ue date
^^           ^^ ^                  p_r_iority
^^           ^^ ^                  _N_uke
"
      ;; nav
      ("<down>" next-line)
      ("n" next-line)
      ("j" next-line)
      ("<up>" previous-line)
      ("p" previous-line)
      ("k" previous-line)

      ;; list
      ("l" todotxt-unhide-all)
      ("i" todotxt-show-incomplete)
      ("/" todotxt-filter-for)
      ("\\"  todotxt-filter-out)
      ("A" todotxt-archive)

      ;; item
      ("a" todotxt-add-item)
      ("c" todotxt-complete-toggle)
      ("e" todotxt-edit-item)
      ("t" todotxt-tag-item)
      ("d" todotxt-add-due-date)
      ("r" todotxt-add-priority)
      ("N" todotxt-nuke-item)


      ;; other
      ("g" todotxt-revert)
      ("s" save-buffer)
      ;; ("u" todotxt-undo)
      ("q" todotxt-bury  :exit t))

    (defun my-todotxt-mode-hook ()
      "my todotxt mode hook"
      (todotxt-hydra/body))

    (add-hook 'todotxt-mode-hook  'my-todotxt-mode-hook)))



(provide 'init-org)

;; Local Variables:
;; eval: (outline-minor-mode 1)
;; eval: (require 'use-package)
;; End:


;;; init-org.el ends here
