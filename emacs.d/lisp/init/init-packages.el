;;; init-packages.el --- package configuration -*- lexical-binding: nil; byte-compile-warnings: (not docstrings-wide); -*-

;;; Commentary:

;; use-package configuration for all packages except org, calendar and
;; mail (init-org) and security (init-security).  That covers file and
;; buffer management, completion and search, editing helpers,
;; programming languages, lsp, flycheck, version control and anything
;; else.
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

;;;; abbrev

(use-package abbrev
  :defer
  :diminish ""
  :init
  (progn))


;;;; ac-slime

(use-package ac-slime
  :ensure t
  :commands (set-up-slime-ac)
  :init
  (progn
    (add-hook 'slime-mode-hook 'set-up-slime-ac)
    (add-hook 'slime-repl-mode-hook 'set-up-slime-ac)))


;;;; adoc-mode

(use-package adoc-mode
  :ensure t
  :mode (("\\.adoc\\'" . adoc-mode)))


;;;; ahg

(use-package ahg
  :ensure t
  :commands (ahg-log ahg-short-log ahg-status))

(eval-and-compile
  (setq magit-last-seen-setup-instructions "1.4.0"))


;;;; anchored-transpose

(use-package anchored-transpose
  :commands anchored-transpose)


;;;; ansi

(use-package ansi
  :ensure t
  :commands (with-ansi
             ansi-green
             ansi-blue
             ansi-red))


;;;; anzu

(use-package anzu
  :ensure t
  :defer 2
  :if (and
       (not noninteractive)
       )
  :commands (global-anzu-mode)
  :init (global-anzu-mode 1)
  :diminish "")


;;;; apache-mode

(use-package apache-mode
  :ensure t
  :commands apache-mode
  :mode (("\\.htaccess\\'"   . apache-mode)
         ("apache2?/httpd\\.conf\\'"  . apache-mode)
         ("apache2?/srm\\.conf\\'"    . apache-mode)
         ("apache2?/access\\.conf\\'" . apache-mode)
         ("apache2?//sites-\\(available\\|enabled\\)/" . apache-mode)))


;;;; archive-region

(use-package archive-region
  :ensure t
  :commands (archive-region))


;;;; arduino-mode

(use-package arduino-mode
  :ensure t
  :mode (("\\.ino\\'" . arduino-mode)))


;;;; auto-highlight-symbol

(use-package auto-highlight-symbol
  :ensure t
  :if (and
       (not noninteractive))
  :commands auto-highlight-symbol-mode
  :diminish auto-highlight-symbol-mode
  :init
  (progn
    (setq
     ahs-face-check-include-overlay t
     ahs-inhibit-face-list
     '(font-lock-comment-delimiter-face
       font-lock-comment-face
       font-lock-doc-face
       font-lock-doc-string-face
       font-lock-string-face
       font-lock-keyword-face
       avy-background-face
       region
       loccur-custom-buffer-grep
       isearch)
     ahs-idle-interval 1.1)
    (defun my-ahs-on ()
      ;; disable in lsp-mode modes
      (unless (or
               (eq major-mode 'js-mode)
               (eq major-mode 'js2-mode)
               (eq major-mode 'js2-jsx-mode)
               (eq major-mode 'typescript-mode)
               (eq major-mode 'typescript-ts-mode)
               (eq major-mode 'rust-mode)
               (eq major-mode 'go-mode)
               (eq major-mode 'go-ts-mode)
               (eq major-mode 'python-mode)
               (eq major-mode 'python-ts-mode)
               )
        (auto-highlight-symbol-mode))
      )
    (hook-into-modes #'my-ahs-on
                     my-prog-mode-hooks)))


;;;; autorevert

(use-package autorevert
  :if (and (not noninteractive) )
  :defer
  :init
  (progn
    (setq auto-revert-check-vc-info nil
          auto-revert-verbose nil)
    (if (not (not window-system))
        (setq auto-revert-mode-text " ◉"
              auto-revert-tail-mode-text " ◉╮")
      (setq auto-revert-mode-text " ar"
            auto-revert-tail-mode-text " ar~"))
    (defun auto-revert-turn-on-maybe ()
      (unless (current-buffer-remote-p)
        (auto-revert-mode)))
    (add-hook 'find-file-hook 'auto-revert-turn-on-maybe)))


;;;; avy

(use-package avy
  :ensure t
  :bind (
         ;; ("C-c SPC" . avy-goto-char)
         ;; ("C-c SPC" . avly-goto-word-1)
         ("C-c SPC" . avy-goto-char-timer)
         )
  :init
  (progn
    (setq avy-background t
          avy-keys '(?a ?s ?d ?f
                        ;; ?g ?h
                        ?j ?k ?l
                        ;;?q
                        ?w ?e ?r
                        ;; ?t ?y
                        ?u ?i ?o
                        ;; p
                        )
          avy-all-windows 'all-frames
          avy-highlight-first t
          avy-style 'pre
          avy-orders-alist '((avy-goto-char . avy-order-closest)
                             (avy-goto-word-1 . avy-order-closest)
                             (avy-goto-char-timer . avy-order-closest)
                             )
          )

    (use-package conf-mode
      :defer
      :config
      (progn
        (unbind-key "C-c SPC" conf-mode-map))))
  :config
  (progn


    ;;
    ;; patch start
    ;;
    ;; TODO: maybe pull request if I take the time to understand the problem.
    ;;
    ;; This switches place of avy-lead-face and avy-lead-face-0. I think this
    ;; is overall more consistent if used with (setq avy-highlight-first t). I
    (defun avy--overlay-pre (path leaf)
      "Create an overlay with PATH at LEAF.
PATH is a list of keys from tree root to LEAF.
LEAF is normally ((BEG . END) . WND)."
      (if (with-selected-window (cdr leaf)
            (bound-and-true-p visual-line-mode))
          (avy--overlay-at-full path leaf)
        (let* ((path (mapcar #'avy--key-to-char path))
               (str (propertize (apply #'string (reverse path))
                                'face 'avy-lead-face-0)))
          (when (or avy-highlight-first (> (length str) 1))
            (set-text-properties 0 1 '(face avy-lead-face) str))
          (setq str (concat
                     (propertize avy-current-path
                                 'face 'avy-lead-face-1)
                     str))
          (avy--overlay
           str
           (avy-candidate-beg leaf) nil
           (avy-candidate-wnd leaf)))))
    (defun avy--overlay-post (path leaf)
      "Create an overlay with PATH at LEAF.
PATH is a list of keys from tree root to LEAF.
LEAF is normally ((BEG . END) . WND)."
      (let* ((path (mapcar #'avy--key-to-char path))
             (str (propertize (apply #'string (reverse path))
                              'face 'avy-lead-face-0)))
        (when (or avy-highlight-first (> (length str) 1))
          (set-text-properties 0 1 '(face avy-lead-face) str))
        (setq str (concat
                   (propertize avy-current-path
                               'face 'avy-lead-face-1)
                   str))
        (avy--overlay
         str
         (avy-candidate-end leaf) nil
         (avy-candidate-wnd leaf))))
    ;;
    ;; patch end
    ;;
    ))


;;;; backline

(use-package backline
  :ensure t
  :after outline
  :config
  (progn
    (advice-add 'outline-flag-region :after 'backline-update)))


;;;; backup-walker

(use-package backup-walker
  :ensure t
  :if (not noninteractive)
  :commands (backup-walker-start))


;;;; bazel

(use-package bazel
  :ensure t
  :commands (bazel-mode)
  :mode ("drone\\.star\\'" . bazel-starlark-mode))


;;;; bf-mode

(use-package bf-mode
  :ensure t
  :commands bf-mode)


;;;; bookmark

(use-package bookmark
  :defer t
  :init
  (progn
    (setq
     bookmark-default-file (expand-file-name (workspace-prefix-file-name "bookmark" ".emacs.bmk") user-data-directory)
     bookmark-watch-bookmark-file 'silent
     bookmark-save-flag 1
     bookmark-fringe-mark nil)))


;;;; browse-kill-ring

(use-package browse-kill-ring
  :ensure t
  :bind (("M-y" . browse-kill-ring)))


;;;; buffer-move

(use-package buffer-move
  :ensure t
  :commands (buf-move-up buf-move-down buf-move-left buf-move-right))


;;;; ca65-mode

(use-package ca65-mode
  :ensure t
  :commands ca65-mode)


;;;; capture

(use-package capture
  :ensure t
  :commands (capture-mode))


;;;; clojure-mode

(use-package clojure-mode
  :ensure t
  :commands clojure-mode
  :mode (("\\.clj\\'" . clojure-mode)
         ("\\.cljs\\'" . clojure-mode))
  :init
  (progn
    (rename-modeline "clojure-mode" clojure-mode "clj")

    (use-package align-cljlet
      :ensure t
      :commands (align-cljlet))))


;;;; cmake-mode

(use-package cmake-mode
  :ensure t
  :mode (("CMakeLists\\.txt\\'" . cmake-mode)
         ("\\.cmake\\'" . cmake-mode)))


;;;; code-library

(use-package code-library
  :ensure t
  :commands code-library-save-code
  :config
  (progn
    (setq code-library-mode-file-alist '((c++-mode . "cpp.org")
                                         (emacs-lisp-mode . "elisp.org")
                                         (python-mode . "python.org")
                                         (python-ts-mode . "python.org")
                                         (perl-mode . "perl.org")
                                         (js2-mode . "javascript.org")
                                         (js-mode . "javascript.org")
                                         (js-jsx-mode . "javascript.org")
                                         (js2-jsx-mode . "javascript.org")
                                         (web-mode . "html.org")
                                         (sh-mode . "sh.org"))
          code-library-directory "~/notes/library/"
          code-library-use-tags-command nil
          code-library-downcased-org-keywords t)))


;;;; coffee-mode

(use-package coffee-mode
  :ensure t
  :commands coffee-mode
  :mode (("\\.coffee\\'" . coffee-mode)
         ("Cakefile\\'" . coffee-mode))
  :init
  (progn
    (setq
     coffee-cleanup-whitespace nil
     coffee-tab-width 2
     coffe-js-mode 'js2-mode)))


;;;; color-identifiers-mode

(use-package color-identifiers-mode
  :ensure t
  :commands color-identifiers-mode)


;;;; color-moccur

(let ((ad-redefinition-action 'accept))
  (use-package color-moccur
    :ensure t
    :if (not noninteractive)
    :commands (isearch-moccur
               isearch-moccur-all
               moccur
               moccur-grep
               moccur-grep-find
               dired-do-moccur
               occur-by-moccur
               search-buffers)
    ;; :bind (("M-s m" . moccur))
    :init
    (progn
      (bind-key "C-o" 'isearch-moccur isearch-mode-map)
      (bind-key "C-M-o" 'isearch-moccur-all isearch-mode-map))
    :config
    (progn
      (unbind-key "M-O" isearch-mode-map)
      (unbind-key "M-o" isearch-mode-map)

      (use-package moccur-edit))))


;;;; comby

(use-package comby
  :ensure t
  :commands (comby))


;;;; command-log-mode

(use-package command-log-mode
  :ensure t
  :if (not noninteractive)
  :commands (command-log-mode))


;;;; company

(use-package company
  :ensure t
  :commands company-mode
  :diminish ""
  :init
  (progn
    (setq company-tooltip-align-annotations t
          company-tooltip-limit 15
          company-tooltip-margin 1
          company-minimum-prefix-length 1
          company-idle-delay 0.3
          )
    (add-hook 'emacs-lisp-mode-hook 'company-mode)
    (add-hook 'slime-repl-mode-hook 'company-mode)
    )
  :config
  (progn
    (use-package company-yasnippet
      :commands company-yasnippet)

    (bind-key "C-<tab>" 'company-complete-common-or-cycle company-mode-map)

    (define-key company-active-map (kbd "TAB") 'company-complete-common-or-cycle)
    (define-key company-active-map (kbd "<tab>") 'company-complete-common-or-cycle)
    (define-key company-active-map (kbd "S-TAB") 'company-select-previous)
    (define-key company-active-map (kbd "<backtab>") 'company-select-previous)

    (bind-key "C-s" 'company-filter-candidates company-active-map)
    (bind-key "C-M-s" 'company-search-candidates company-active-map)
    (bind-key "M-v" 'company-previous-page company-active-map)
    (bind-key "C-v" 'company-next-page company-active-map)
    (bind-key "C-n" 'company-select-next-or-abort company-active-map)
    (bind-key "C-p" 'company-select-previous-or-abort company-active-map)

    (bind-key "M-v" 'company-previous-page company-search-map)
    (bind-key "C-v" 'company-next-page company-search-map)
    (bind-key "C-n" 'company-select-next company-search-map)
    (bind-key "C-p" 'company-select-previous company-search-map)
    (bind-key "C-c y" 'company-yasnippet)

    ;; maybe these settings
    (setq company-require-match nil)
    (defun my-company-visible-and-explicit-action-p ()
      (and (company-tooltip-visible-p)
           (company-explicit-action-p)))

    (setq company-insertion-on-trigger #'my-company-visible-and-explicit-action-p)
    (setq company-frontends '(company-echo-metadata-frontend
                              company-pseudo-tooltip-unless-just-one-frontend-with-delay
                              company-preview-frontend))

    ))


;;;; compile

(use-package compile
  :defer
  :config
  (progn
    (setq compilation-ask-about-save nil)
    (defun my-compilation-mode-hook ()
      ;; (jit-lock-defer-fontification)
      (setq truncate-lines t)
      (setq-local truncate-partial-width-windows nil))
    (add-hook 'compilation-mode-hook 'my-compilation-mode-hook)
    (add-hook 'compilation-minor-mode-hook 'my-compilation-mode-hook)
    (add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)))


;;;; conf-mode

(use-package conf-mode
  :mode "\\.env\\'")


;;;; constants

(use-package constants
  :commands (constants-get constants-insert constants-replace))


;;;; counsel

(use-package counsel
  :ensure t
  :bind (
         ("C-x C-f" . counsel-find-file)
         ("M-x" . counsel-M-x)
         ("M-o M-x" . counsel-M-x)
         ("<menu>" . counsel-M-x)
         ("<XF86Tools>" . counsel-M-x)
         ("C-h a" . counsel-apropos)
         ("C-h b" . counsel-descbinds)
         ("C-h i" . counsel-imenu)
         ("M-o M-i" . counsel-imenu)
         ("C-h r" . counsel-recentf)
         ("C-x f r" . counsel-recentf)
         ("C-x f L" . counsel-locate)
         ("C-x f p" . projectile-find-file)
         )
  :config (setq ivy-initial-inputs-alist nil)
  :commands (counsel-mode
             counsel-outline
             counsel-rg
             counsel-git-grep
             counsel-fzf))


;;;; css-mode

(use-package css-mode
  :commands css-mode
  :mode  ("\\.css\\'" . css-mode)
  :init
  (progn
    (setq css-indent-offset 2)))


;;;; csv-mode

(use-package csv-mode
  :ensure t
  :mode (("\\.csv\\'" . csv-mode)))


;;;; cua-base

(use-package cua-base
  :if (and (not noninteractive) )
  :init
  (progn
    (setq
     cua-delete-selection nil
     cua-enable-cua-keys nil
     cua-enable-cursor-indications nil
     cua-rectangle-mark-key [(control return)])
    ;; cua-mode seems to prohibit mark to be deactivated properly on the emacs-25 branch (2016-04-08 18:33)
    ;; (cua-mode t)
    ))


;;;; cuda-mode

(use-package cuda-mode
  :ensure t
  :mode (("\\.cu\\'" . cuda-mode)
         ("\\.cuh\\'" . cuda-mode)))


;;;; dart-mode

(use-package dart-mode
  ;; :disabled t
  :ensure t
  :commands (dart-mode)
  :mode ("\\.dart\\'" . dart-mode))


;;;; deadgrep

(use-package deadgrep
  :ensure t
  :commands (deadgrep)
  :config
  (progn
    (setq deadgrep-project-root-function 'project-root-function)
    )
  ;; :bind (("M-o a" . deadgrep))
  )


;;;; debbugs

(use-package debbugs
  :ensure t
  :commands (debbugs-gnu)
  :config
  (progn
    (require 'debbugs-gnu)
    (require 'debbugs-org)
    (setq debbugs-gnu-persistency-file (expand-file-name
                                        "debbugs" user-data-directory))))


;;;; delsel

(use-package delsel
  :defer t
  :init
  (progn
    (delete-selection-mode)))


;;;; diff-hl

(use-package diff-hl
  :ensure t
  :commands (diff-hl-mode
             turn-on-diff-hl-mode
             global-diff-hl-mode))


;;;; dired

(use-package dired
  :commands (dired)
  :bind (("C-x d d" . dired))
  :if (not noninteractive)
  :init
  (progn
    (setq dired-listing-switches "-alh"
          dired-isearch-filenames t
          dired-dwim-target t)

    (use-package image-dired
      :defer
      :init
      (progn
        (setq image-dired-dir (expand-file-name
                               "image-dired" user-cache-directory)
              image-dired-thumb-margin 12
              image-dired-thumb-relief 0
              image-dired-thumb-size 200))
      :config
      (progn
        (bind-keys
         :map image-dired-thumbnail-mode-map
         ("n" . image-dired-forward-image)
         ("p" . image-dired-backward-image)
         ("f" . image-dired-forward-image)
         ("b" . image-dired-backward-image))))

    (use-package dired-x
      :commands (dired-do-find-marked-files
                 dired-omit-mode)
      :bind (("C-x C-j" . dired-jump)
             ("C-x d <return>" . dired-jump))
      :init
      (progn
        (setq
         dired-omit-verbose nil
         dired-omit-files
         "^\\.?#\\|^\\.\\(DS_Store\\|localized\\|AppleDouble\\|dropbox\\|dropbox\.cache\\)$\\|org_archive$\\|^\\.\\.$"
         dired-omit-extensions completion-ignored-extensions)
        ;; (add-hook 'dired-mode-hook #'(lambda () (dired-omit-mode)))
        )
      :config
      (progn
        (unbind-key "M-o" dired-mode-map)
        ;; (bind-key "C-M-o" 'dired-omit-mode dired-mode-map)
        )))
  :config
  (progn
    (when (eq system-type 'darwin)
      (setq ls-lisp-use-insert-directory-program nil)
      (require 'ls-lisp))

    (unbind-key "l" dired-mode-map)

    (use-package dired-quick-sort
      :ensure t
      :if (and (eq system-type 'gnu/linux))
      :config
      (progn
        ;; (dired-quick-sort-setup)
        ;; Replace \"S\" with other keys to invoke the dired-quick-sort hydra.
        (define-key dired-mode-map (kbd "s")'hydra-dired-quick-sort/body)
        ;; Automatically use the sorting defined here to sort.
        (add-hook 'dired-mode-hook 'dired-quick-sort)
        ))

    (use-package wdired
      :defer
      :init
      (progn
        (bind-key "M-r" 'wdired-change-to-wdired-mode dired-mode-map)))


    (use-package dired-narrow
      :ensure t
      :commands (dired-narrow
                 dired-narrow-regexp
                 dired-narrow-fuzzy))

    (use-package dired-ranger
      :ensure t
      :commands (dired-ranger-copy dired-ranger-move dired-ranger-paste)
      :init
      (progn
        (defvar dired-ranger-map)
        (define-prefix-command 'dired-ranger-map)
        (define-key dired-mode-map (kbd "r") dired-ranger-map)
        (defun dired-ranger-show-ring ()
          (interactive)
          (describe-variable 'dired-ranger-copy-ring))
        (bind-key "c" 'dired-ranger-copy dired-ranger-map)
        (bind-key "C" 'dired-ranger-copy dired-ranger-map)
        (bind-key "M" 'dired-ranger-move dired-ranger-map)
        (bind-key "P" 'dired-ranger-paste dired-ranger-map)
        (bind-key "<SPC>" 'dired-ranger-show-ring dired-ranger-map)))


    (use-package dired-filter
      :ensure t
      :commands (dired-filter-by-name
                 dired-filter-by-regexp
                 dired-filter-by-extension
                 dired-filter-by-dot-files
                 dired-filter-by-omit
                 dired-filter-by-predicate
                 dired-filter-by-file
                 dired-filter-by-directory
                 dired-filter-by-mode
                 dired-filter-mode)
      :init
      (progn
        (setq dired-filter-save-with-custom nil
              dired-filter-verbose nil
              dired-filter-show-filters t
              dired-filter-stack '((dot-files) (omit)))
        (add-hook 'dired-mode-hook 'dired-filter-mode)
        (bind-key ")" 'dired-filter-mode dired-mode-map)))


    (use-package dired-open
      :ensure t
      :commands (dired-open-xdg
                 dired-open-guess-shell-alistv))


    (when (boundp 'dired-hide-details-mode)
      (setq dired-hide-details-hide-symlink-targets nil
            dired-hide-details-hide-information-lines t)
      (add-hook 'dired-mode-hook 'dired-hide-details-mode))

    (defun dired-sort-size ()
      "Dired sort by size."
      (interactive)
      (dired-sort-other (concat dired-listing-switches "S")))

    (defun dired-sort-extension ()
      "Dired sort by extension."
      (interactive)
      (dired-sort-other (concat dired-listing-switches "X")))

    (defun dired-sort-ctime ()
      "Dired sort by create time."
      (interactive)
      (dired-sort-other (concat dired-listing-switches "ct")))

    (defun dired-sort-utime ()
      "Dired sort by access time."
      (interactive)
      (dired-sort-other (concat dired-listing-switches "ut")))

    (defun dired-sort-time ()
      "Dired sort by time."
      (interactive)
      (dired-sort-other (concat dired-listing-switches "t")))

    (defun dired-sort-name ()
      "Dired sort by name."
      (interactive)
      (dired-sort-other (concat dired-listing-switches "")))

    (defun my-dired-goto-home ()
      (interactive)
      (dired
       (if (f-same? (dired-current-directory) "~/")
           "/" "~/")))

    (bind-key "~" 'my-dired-goto-home dired-mode-map)
    (bind-key "." 'dired-up-directory dired-mode-map)
    (bind-key "h" 'ibuffer dired-mode-map)

    (defun my-dired-create-file (file)
      "Create a file called FILE.
If FILE already exists, signal an error."
      (interactive
       (list (read-file-name "Create file: " (dired-current-directory))))
      (let* ((expanded (expand-file-name file))
             (try expanded)
             (dir (directory-file-name (file-name-directory expanded)))
             new)
        (if (file-exists-p expanded)
            (error "Cannot create file %s: file exists" expanded))
        ;; Find the topmost nonexistent parent dir (variable `new')
        (while (and try (not (file-exists-p try)) (not (equal new try)))
          (setq new try
                try (directory-file-name (file-name-directory try))))
        (when (not (file-exists-p dir))
          (make-directory dir t))
        (write-region "" nil expanded t)
        (when new
          (dired-add-file new)
          (dired-move-to-filename))))
    (define-key dired-mode-map (kbd "C-c n") 'my-dired-create-file)

    (defun my-dired-create-__init__py ()
      "Creates an __init__.py in the current directory"
      (interactive)
      (my-dired-create-file "__init__.py"))))


;;;; dired-k

(use-package dired-k
  :ensure t
  :commands dired-k)


;;;; direx

(use-package direx
  :disabled t  ;; direx is not compatible with emacs-30 (probably)
  :ensure t
  :commands (direx:jump-to-directory direx:jump-to-directory-noselect
                                     direx:jump-to-directory-other-window)
  :init
  (progn
    (setq
     direx:leaf-icon "  "
     direx:open-icon "▾ "
     direx:closed-icon "▸ "
     direx:ignored-files-regexp
     (concat "\\(?:" (regexp-opt completion-ignored-extensions) "\\|#\\)$"))

    (use-package direx-project
      :commands (direx-project:jump-to-project-root
                 direx-project:jump-to-project-root-noselect
                 direx-project:jump-to-project-root-other-window)
      :bind (("C-x C-d" . my-direx:jump-to-directory-other-window)
             ("C-x d D" . my-direx:jump-to-directory-other-window))
      :init
      (progn
        (add-hook 'direx:direx-mode-hook 'hl-line-mode)
        (setq
         direx-project:project-root-predicate-functions
         #'((lambda (dirname)
              (string= dirname (project-root-function)))))
        (defun my-direx:jump-to-directory-other-window ()
          (interactive)
          (condition-case error
              (progn
                (direx-project:jump-to-project-root-other-window))
            ('error
             (let ((message (error-message-string error)))
               (if (equal "Project root not found" message)
                   (direx:jump-to-directory-other-window)
                 (error message)))))))))
  :config
  (progn
    (let ((map direx:direx-mode-map))
      (define-key map (kbd "s") 'isearch-forward-regexp)
      (define-key map (kbd ".") 'direx:up-item)
      (define-key map (kbd "N") 'direx:next-sibling-item)
      (define-key map (kbd "P") 'direx:previous-sibling-item)
      (define-key map (kbd "j") 'direx:next-item)
      (define-key map (kbd "k") 'direx:previous-item)
      (define-key map (kbd "J") 'direx:next-sibling-item)
      (define-key map (kbd "K") 'direx:previous-sibling-item))))


;;;; docker

(use-package docker
  :ensure t
  :commands (docker))


;;;; dockerfile-mode

(use-package dockerfile-mode
  :ensure t
  :mode (("Dockerfile" . dockerfile-mode))
  :init
  (progn
    (setq-default docker-use-sudo nil)))


;;;; dpaste

(use-package dpaste
  :commands (dpaste-region dpaste-buffer dpaste-region-or-buffer)
  :ensure t)


;;;; drag-stuff

(use-package drag-stuff
  :ensure t
  :commands (drag-stuff-global-mode drag-stuff-mode))


;;;; easy-kill

(use-package easy-kill
  :ensure t
  :commands easy-kill
  :init
  (progn
    (global-set-key [remap kill-ring-save] 'easy-kill)))


;;;; ediff

(use-package ediff
  :defer
  :init
  (progn
    (setq
     ;; ediff-diff-options "-w"
     ediff-diff-options ""
     ediff-before-flag-bol "❱"
     ediff-after-flag-eol "❰"
     ediff-before-flag-mol "»»»"
     ediff-after-flag-mol "«««"
     ediff-window-setup-function 'ediff-setup-windows-plain
     ediff-split-window-function (if (or (string= (system-name) "transwhale") (string= (system-name) "flam")) 'split-window-vertically
                                   'split-window-horizontally)
     ediff-merge-split-window-function ediff-split-window-function )

    (defun command-line-diff (switch)
      "Usage: emacs -diff file1 file2"
      (let ((file1 (pop command-line-args-left))
            (file2 (pop command-line-args-left)))
        (ediff file1 file2)))
    (add-to-list 'command-switch-alist '("diff" . command-line-diff)))

  :config
  (progn
    (defun ediff-copy-both-to-C ()
      (interactive)
      (ediff-copy-diff ediff-current-difference nil 'C nil
                       (concat
                        (ediff-get-region-contents ediff-current-difference 'A ediff-control-buffer)
                        (ediff-get-region-contents ediff-current-difference 'B ediff-control-buffer))))
    (defun add-d-to-ediff-mode-map ()
      (define-key ediff-mode-map "d" 'ediff-copy-both-to-C))
    (add-hook 'ediff-keymap-setup-hook 'add-d-to-ediff-mode-map))

  (defun my/ediff-adjust-font-size (inc)
  "Adjust the font size in all active Ediff buffers by INC."
  (interactive "P")
  (let ((buffers (list ediff-buffer-A ediff-buffer-B ediff-buffer-C)))
    (dolist (buf buffers)
      (when (buffer-live-p buf)
        (with-current-buffer buf
          (text-scale-increase inc))))))

  (defun my/ediff-font-size-increase ()
    (interactive)
    (my/ediff-adjust-font-size 1))

  (defun my/ediff-font-size-decrease ()
    (interactive)
    (my/ediff-adjust-font-size -1))

  (defun my/ediff-font-size-reset ()
    (interactive)
    (my/ediff-adjust-font-size 0))


  (with-eval-after-load 'ediff-util
    (add-hook 'ediff-keymap-setup-hook
              (lambda ()
                (define-key ediff-mode-map (kbd "=") 'my/ediff-font-size-increase)
                (define-key ediff-mode-map (kbd "-") 'my/ediff-font-size-decrease)
                (define-key ediff-mode-map (kbd "0") 'my/ediff-font-size-reset)))))


;;;; edit-env

(use-package edit-env
  :commands edit-env)


;;;; edit-server

(use-package edit-server
  :ensure t
  :commands edit-server-start
  :init
  (progn
    (setq
     edit-server-new-frame-alist
     '((name . "floating-center-large")
       (minibuffer . t)
       (menu-bar-lines . t)
       (unsplittable . nil))))
  :config
  (progn

    (defun my-edit-server-start-hook ()
      "My edit-server mode hook."

      (when (string-match (rx (and line-start (* "www.") (or "github.com")))
                          edit-server-url)
        (gfm-mode))

      (when (string-match (rx (and line-start (* "www.") (or "skunk.cc" "facebook.com")))
                          edit-server-url)
        (ispell-change-dictionary "svenska"))

      (when (string-match (rx (and line-start (* "www.") (or "github.com")))
           edit-server-url)
        (ispell-change-dictionary "english"))
      (flyspell-mode 1)
      (flyspell-buffer))

    (add-hook 'edit-server-start-hook 'my-edit-server-start-hook)))


;;;; edit-var

(use-package edit-var
  :commands edit-variable)


;;;; editorconfig

(use-package editorconfig
  :commands (editorconfig-mode)
  :init
  (progn
    (hook-into-modes #'editorconfig-mode my-css-like-mode-hooks)
    (hook-into-modes #'editorconfig-mode my-prog-mode-hooks)
    (hook-into-modes #'editorconfig-mode my-html-like-mode-hooks))
  :config
  (progn
    (and (not noninteractive)
       (editorconfig-mode 1))))


;;;; eimp

(use-package eimp
  :ensure t
  :commands (eimp-mode eimp-fit-image-to-window)
  :init
  (progn

    (defmacro eimp-with-buffer-modified-unmodified (&rest body)
      "Run BODY while preserving the buffer's `buffer-modified-p' state."
      (let ((was-modified (make-symbol "was-modified")))
        `(let ((,was-modified (buffer-modified-p)))
           (unwind-protect
               (progn ,@body)
             (set-buffer-modified-p ,was-modified)))))

    (defadvice eimp-mogrify-process-sentinel (around do-not-set-modified activate)
      (eimp-with-buffer-modified-unmodified
       ad-do-it
       ))

    (use-package image-mode
      :defer
      :config
      (progn
        (bind-key "h" 'eimp-fit-image-to-window image-mode-map)))))


;;;; eldoc

(use-package eldoc
  :defer
  :diminish ""
  )


;;;; electric

(use-package electric
  :defer
  :config
  (progn
    ;; (electric-indent-mode -1)
    ))


;;;; erlang

(use-package erlang
  :ensure t
  :commands (erlang-mode))


;;;; eval-sexp-fu

(use-package eval-sexp-fu
  :ensure t
  :commands (eval-sexp-fu-flash-mode))


;;;; eww

(use-package eww
  :defer
  :init
  (progn
    (setq eww-search-prefix "http://google.com/search?q=")))


;;;; expand-region

(use-package expand-region
  :ensure t
  :commands (er/expand-region
             er/contract-region
             er/mark-inside-quotes)
  :bind (("C-=" . er/expand-region)
         ("M-h" . er/expand-region))
  :init
  (progn
    (define-key region-bindings-mode-map "h" 'er/expand-region)
    (define-key region-bindings-mode-map "j" 'er/contract-region)))


;;;; face-remap

(use-package face-remap
  :bind (("M-o m b" . buffer-face-toggle))
  :init
  (progn
    (setq text-scale-mode-step 1.15))
  :config
  (progn
    (defface fixed-pitch-terminal
      '((t :inherit fixed-pitch :font "Anonymous Pro"))
      "face for headers.")
    (setq buffer-face-mode-face 'fixed-pitch-terminal)))


;;;; feature-mode

(use-package feature-mode
  :ensure t
  :mode (("\\.feature\\'" . feature-mode)))


;;;; figlet

(use-package figlet
  :ensure t
  :commands (figlet
             figlet-comment
             figlet-preview-fonts
             figlet-figletify-region
             figlet-figletify-region-comment))


;;;; find-file

(use-package find-file
  :defer
  :bind (("C-h C-o" . cc-other-file))
  :init
  (progn
    (defun cc-other-file()
      "Toggles source/header file"
      (interactive)
      (let ((buf (current-buffer))
            (name (file-name-sans-extension (buffer-file-name)))
            (other-extens
             (cadr (assoc (concat "\\."
                                  (file-name-extension (buffer-file-name))
                                  "\\'")
                          cc-other-file-alist))))
        (dolist (e other-extens)
          (if (let ((f (concat name e)))
                (and (file-exists-p f) (find-file f)))
              (cl-return)))))

    (setq
     cc-other-file-alist
     '(("\\.cc\\'"  (".hh" ".h"))
       ("\\.hh\\'"  (".cc" ".C"))

       ("\\.c\\'"   (".h"))
       ("\\.h\\'"   (".c" ".cc" ".C" ".CC" ".cxx" ".cpp"))

       ("\\.C\\'"   (".H"  ".hh" ".h"))
       ("\\.H\\'"   (".C"  ".CC"))

       ("\\.CC\\'"  (".HH" ".H"  ".hh" ".h"))
       ("\\.HH\\'"  (".CC"))

       ("\\.c\\+\\+\\'" (".h++" ".hh" ".h"))
       ("\\.h\\+\\+\\'" (".c++"))

       ("\\.cpp\\'" (".hpp" ".hh" ".h"))
       ("\\.hpp\\'" (".cpp"))

       ("\\.cxx\\'" (".hxx" ".hh" ".h"))
       ("\\.hxx\\'" (".cxx"))


       ))))


;;;; flx

;; ivy sorts ivy--regex-fuzzy matches by flx score when flx is available.
(use-package flx
  :ensure t
  :defer t)


;;;; flycheck

(use-package flycheck
  :ensure t
  :if (not noninteractive)
  :commands (flycheck-mode
             global-flycheck-mode
             my-flycheck-list-errors
             flycheck-next-error
             flycheck-previous-error)
  :bind (("M-o e" . my-flycheck-list-errors)
         ("C-h w" . my-flycheck-list-errors))
  :init
  (progn
    (setq
     flycheck-mode-line '(:eval (my-flycheck-mode-line-status-text))
     flycheck-highlighting-mode 'lines
     flycheck-idle-change-delay 0.6
     ;; flycheck-highlighting-mode 'symbols
     flycheck-standard-error-navigation nil
     flycheck-navigation-minimum-level 'info
     flycheck-error-list-minimum-level nil)

    ;; (setq-default flycheck-disabled-checkers '(python-flake8 python-mypy python-pycompile python-pyright))
    (setq-default flycheck-disabled-checkers '(python-flake8 python-mypy python-pycompile pylsp
                                               go-gofmt go-errcheck go-staticcheck go-unconvert
                                               javascript-jshint))

    (defun my-flycheck-cycle-error-navigation-min-level ()
      (interactive)
      (message "flycheck-navigation-minimum-level: %s"
               (dring-set-next-element
                'flycheck-navigation-minimum-level
                '(info warning error))))

    (defun my-node_modules-flycheck-hook ()
      (setq-local flycheck-executable-find #'flycheck-node_modules-executable-find))

    (add-hook 'js2-mode-hook 'my-node_modules-flycheck-hook)
    (add-hook 'js-mode-hook 'my-node_modules-flycheck-hook)
    (add-hook 'web-mode-hook 'my-node_modules-flycheck-hook)

    (defun flycheck-turn-on-maybe ()
      (unless
          (or
           buffer-read-only
           (hardhat-buffer-included-p (current-buffer))
           (current-buffer-remote-p))
        (flycheck-mode)))

    (add-hook 'python-base-mode-hook 'flycheck-turn-on-maybe)
    (add-hook 'js2-mode-hook 'flycheck-turn-on-maybe)
    (add-hook 'js2-jsx-mode-hook 'flycheck-turn-on-maybe)
    (add-hook 'web-mode-hook 'flycheck-turn-on-maybe)
    (add-hook 'js-mode-hook 'flycheck-turn-on-maybe)
    (add-hook 'json-mode-hook 'flycheck-turn-on-maybe)
    (add-hook 'ruby-mode-hook 'flycheck-turn-on-maybe)
    (add-hook 'php-mode-hook 'flycheck-turn-on-maybe)
    (add-hook 'scss-mode-hook 'flycheck-turn-on-maybe)
    (add-hook 'go-mode-hook 'flycheck-turn-on-maybe)
    (add-hook 'go-ts-mode-hook 'flycheck-turn-on-maybe)
    (add-hook 'arduino-mode-hook 'flycheck-turn-on-maybe)
    (add-hook 'sh-mode-hook 'flycheck-turn-on-maybe)
    (add-hook 'haskell-mode-hook 'flycheck-turn-on-maybe)
    )

  :config
  (progn
    (defun my-flycheck-next-error (&optional n reset)
      ""
      (interactive "P")
      (flycheck-next-error n reset)
      (recenter)
      (my-pulse-rest-of-line))

    (defun my-flycheck-previous-error (&optional n)
      ""
      (interactive "P")
      (my-flycheck-next-error (- (or n 1))))

    ;; (smartrep-define-key
    ;;     flycheck-mode-map
    ;;     "C-c"
    ;;   '(("n" . my-flycheck-next-error)
    ;;     ("p" . my-flycheck-previous-error)
    ;;     ("E" . #'(lambda () (setq flycheck-navigation-minimum-level 'error) (message "level: error")))
    ;;     ("W" . #'(lambda () (setq flycheck-navigation-minimum-level 'warning) (message "level: warning")))
    ;;     ("I" . #'(lambda () (setq flycheck-navigation-minimum-level 'info) (message "leve: info")))
    ;;     )
    ;;   )

    (defhydra hydra-next-flycheck-error (flycheck-mode-map "C-c")
      "next-error"
      ("n" my-flycheck-next-error "next")
      ("p" my-flycheck-previous-error "prev" :bind nil)
      ("j" my-flycheck-next-error "next" :bind nil)
      ("k" my-flycheck-previous-error "prev" :bind nil)
      ("e" (lambda () (interactive) (setq flycheck-navigation-minimum-level 'error) (message "level: error"))
       "l:error"  :bind nil)
      ("w" (lambda () (interactive) (setq flycheck-navigation-minimum-level 'warning) (message "level: warning"))
       "l:warning" :bind nil)
      ("i" (lambda () (interactive) (setq flycheck-navigation-minimum-level 'info) (message "leve: info"))
       "l:info" :bind nil))

    (defun flycheck-node_modules-executable-find (executable)
      (or
       (let* ((base (locate-dominating-file buffer-file-name "node_modules"))
              (cmd  (if base (expand-file-name (concat "node_modules/.bin/" executable)  base))))
         (if (and cmd (file-exists-p cmd))
             cmd))
       (flycheck-default-executable-find executable)))


    (defun my-flycheck-mode-line-status-text (&optional status)
      (let ((text (pcase (or status flycheck-last-status-change)
                    (`not-checked "")
                    (`no-checker "-")
                    (`running "*")
                    (`errored "!")
                    (`finished
                     (let-alist (flycheck-count-errors flycheck-current-errors)
                       ;; (if (or .error .warning)
                       ;;     (format ":%s/%s" (or .error 0) (or .warning 0))
                       ;;   "")))
                       (if .error
                           (format ":%s" (or .error 0))
                         "")))
                    (`interrupted "-")
                    (`suspicious "?"))))
        (concat " fc" text)))

    (defun my-flycheck-error-list-goto-error (&optional pos)
      (interactive)
      (flycheck-error-list-goto-error pos)
      (recenter)
      (my-pulse-soon))

    (bind-key "RET" 'my-flycheck-error-list-goto-error flycheck-error-list-mode-map)

    (defun my-flycheck-list-errors ()
      "Save all buffers before opening list"
      (interactive)
      (if (not (flycheck-may-enable-mode))
          (error "flycheck not supported in this mode")
        (silent-save-some-buffers)
        (unless flycheck-mode
          (flycheck-mode))
        (flycheck-list-errors)
        (select-window (get-buffer-window "*Flycheck errors*"))))

    (defun my-flycheck-drop-duplicate-errors (args)
      "Drop errors in ARGS that an earlier checker in the chain already reported.
An error is a duplicate if its file, line, column and message match."
      (let* ((key (lambda (err)
                    (list (flycheck-error-filename err)
                          (flycheck-error-line err)
                          (flycheck-error-column err)
                          (flycheck-error-message err))))
             (seen (mapcar key flycheck-current-errors)))
        (list (seq-remove (lambda (err) (member (funcall key err) seen))
                          (car args)))))

    (advice-add 'flycheck-report-current-errors
                :filter-args 'my-flycheck-drop-duplicate-errors)

    (flycheck-add-mode 'javascript-eslint 'web-mode)
    ;; (setq flycheck-javascript-jshint-executable
    ;;       (cond
    ;;        ((executable-find* "jsxhint" "yarn global add jsxhint") "jsxhint")
    ;;        (t "jshint")))

    (when (fboundp 'define-fringe-bitmap)
      (require 'fringe-helper)
      (fringe-helper-define 'vertical-wave-bitmap '(center repeat)
        "...XXX."
        "...XXX."
        "..XXX.."
        "..XXX..")

      (flycheck-define-error-level 'error
        :severity 100
        :compilation-level 2
        :overlay-category 'flycheck-error-overlay
        :fringe-bitmap 'vertical-wave-bitmap
        :fringe-face 'flycheck-fringe-error
        :error-list-face 'flycheck-error-list-error)

      (flycheck-define-error-level 'warning
        :severity 10
        :compilation-level 1
        :overlay-category 'flycheck-warning-overlay
        :fringe-bitmap 'vertical-wave-bitmap
        :fringe-face 'flycheck-fringe-warning
        :error-list-face 'flycheck-error-list-warning)

      (flycheck-define-error-level 'info
        :severity -10
        :compilation-level 0
        :overlay-category 'flycheck-info-overlay
        :fringe-bitmap 'vertical-wave-bitmap
        :fringe-face 'flycheck-fringe-info
        :error-list-face 'flycheck-error-list-info)


      (advice-add 'flycheck-verify-setup :before 'package-initialize)

      (setq flycheck-flake8-error-level-alist
            '(
              ("^E501.*$" . info) ;; pep8: line too long (82 > 79 characters)
              ("^E128.*$" . info) ;; pep8:       continuation line under-indented for visual indent.
              ("^E2.*$" . info)   ;; pep8: Whitespace
              ("^E3.*$" . info)   ;; pep8: blank lines
              ("^W2.*$" . info)   ;; pep8: Whitespace
              ("^W3.*$" . info)   ;; pep8: blank lines
              ("^F401.*$". info)  ;; pyflakes: module imported but unused
              ("^E9.*$" . error)
              ("^F82.*$" . error)
              ("^F83.*$" . error)
              ("^D.*$" . info)
              ("^N.*$" . info))
            ))))


;;;; flyspell

(use-package flyspell
  :defer
  :diminish ((flyspell-mode . "fls"))
  :init
  (progn
    (setq flyspell-issue-message-flag nil)))


;;;; focus

(use-package focus
  :ensure t
  :commands focus-mode)


;;;; fold-this

(use-package fold-this
  :ensure t
  :commands (fold-this)
  :bind (("C-c C-f" . fold-this-all)
         ("C-c C-F" . fold-this)
         ("C-c M-f" . fold-this-unfold-all))
  :init
  (progn
    (setq
     ;; fold-this-overlay-text "░▒▓▒░"
     fold-this-overlay-text "╾━╳━╼")))


;;;; font-utils

(use-package font-utils
  :ensure t
  :commands (font-utils-first-existing-font))


;;;; gdscript-mode

(use-package gdscript-mode
             :ensure t
             :mode "\\.gd\\'")


;;;; git-gutter

(use-package git-gutter
  :ensure t
  :defer t
  :if (not noninteractive)
  :commands (git-gutter-mode
             global-git-gutter-mode
             hydra-git-gutter/body)
  :bind (
         ("M-o m g" . git-gutter-mode)
         ;; ("C-<f10>" . git-gutter:next-hunk)
         ;; ("C-<f9>" . git-gutter:previous-hunk)
         ("C-<f10>" . my-git-gutter:next-hunk)
         ("C-<f9>" . my-git-gutter:previous-hunk)
         )

  :diminish (git-gutter-mode)
  :init
  (progn
    (setq git-gutter:verbosity 0
          git-gutter:disabled-modes '(org-mode dired-mode wdired-mode ielm-mode)
          git-gutter:diff-option ""
          ;; git-gutter:update-interval 1.6 ;; seems to be something wrong with this
          )
    (when window-system
      (let ((symbol (char-to-string
                     (if (char-displayable-p ?∎) ?∎ ?*))))
        (setq git-gutter:added-sign symbol
              git-gutter:modified-sign symbol
              ;; git-gutter:deleted-sign "⌞"
              git-gutter:deleted-sign symbol
              git-gutter:unchanged-sign nil
              git-gutter:window-width 1)))
    (defun git-gutter-turn-on-maybe ()
      (and (buffer-file-name)
           (not (current-buffer-remote-p))
           (git-gutter-mode 1)))
    (hook-into-modes 'git-gutter-turn-on-maybe my-prog-mode-hooks))

  :config
  (progn
    (defun my-git-gutter-cycle-diff-option ()
      (interactive)
      (message "git-gutter:diff-option: \"%s\""
               (dring-set-next-element
                'git-gutter:diff-option
                '("HEAD" "" "--cached")))
      (when git-gutter-mode
        (git-gutter)))


    (defun my-git-gutter:next-hunk (&optional arg)
      (interactive)
      (let ((beg (point)))
        (git-gutter:next-hunk (or arg 1))
        (unless (eq beg (point))
          (recenter))
        (setq beg (point))
        (save-excursion
          (git-gutter:end-of-hunk)

          (if (eq beg (point))
              (my-pulse-line)
            (end-of-visual-line)
            (my-pulse-region beg (point))))))

      (defun my-git-gutter:previous-hunk ()
        (interactive)
        (my-git-gutter:next-hunk -1))

      (defhydra hydra-git-gutter (:body-pre
                                  #'(lambda () (git-gutter-mode 1) (git-gutter:update-all-windows))

                                  :hint nil)
  "
Git gutter:
  _j_: next hunk        _s_tage hunk         (_1_) diff option HEAD         _q_uit
  _k_: previous hunk    _r_evert hunk        (_2_) diff option --cached     _Q_uit and deactivate
  ^ ^                   _p_opup hunk         (_3_) clear option
  _h_: first hunk       ^ ^                  set start _R_evision
  _l_: last hunk        ^ ^                  set start _D_efault revision
"
  ("j" my-git-gutter:next-hunk)
  ("k" my-git-gutter:previous-hunk)
  ("h" (progn (goto-char (point-min))
              (git-gutter:next-hunk 1)))
  ("l" (progn (goto-char (point-min))
              (git-gutter:previous-hunk 1)))
  ("s" git-gutter:stage-hunk)
  ("r" git-gutter:revert-hunk)
  ("p" git-gutter:popup-hunk)

  ("R" (progn (call-interactively 'git-gutter:set-start-revision)
              (git-gutter:update-all-windows)))
  ("D" (progn (let ((rev (nth 1 (magit--get-default-branch))))
                (git-gutter:set-start-revision rev)
                (message "set start revision to: %s " rev))))
  ("1" (progn (setq git-gutter:diff-option "HEAD")
              (git-gutter:update-all-windows)))
  ("2" (progn (setq git-gutter:diff-option "--cached")
              (git-gutter:update-all-windows)))
  ("3" (progn (setq git-gutter:diff-option "")
              (git-gutter:update-all-windows)))

  ("q" nil :color blue)
  ("Q" (progn (git-gutter-mode -1)
              ;; git-gutter-fringe doesn't seem to
              ;; clear the markup right away
              (sit-for 0.1)
              (git-gutter:clear))
       :color blue))))


;;;; git-modes

(use-package git-modes
  :ensure t
  :mode (
         ("/\\.gitignore_global\\'" . gitignore-mode)
         ("/\\.gitignore\\'" . gitignore-mode)
         ("/\\.git/info/exclude\\'" . gitignore-mode)

         ("/\\.gitattributes\\'" . gitattributes-mode)
         ("/info/attributes\\'" . gitattributes-mode)
         ("/git/attributes\\'" . gitattributes-mode)

         ("/\\.gitconfig\\'" . gitconfig-mode)
         ("/\\.git/config\\'" . gitconfig-mode)
         ("/modules/.*/config\\'" . gitconfig-mode)
         ("/git/config\\'" . gitconfig-mode)
         ("/\\.gitmodules\\'" . gitconfig-mode)
         ("/etc/gitconfig\\'" . gitconfig-mode)

         ))


;;;; git-timemachine

(use-package git-timemachine
  :ensure t
  :commands git-timemachine)


;;;; gitlab

(use-package gitlab
  :ensure t
  :commands (gitlab-version))


;;;; glsl-mode

(use-package glsl-mode
  :ensure t
  :mode (("\\.glsl\\'" . glsl-mode)
         ("\\.vert\\'" . glsl-mode)
         ("\\.frag\\'" . glsl-mode)
         ("\\.geom\\'" . glsl-mode)))


;;;; gnuplot-mode

(use-package gnuplot-mode
  :ensure t
  :commands (gnuplot-mode)
  :mode (("\\.gp\\'" . gnuplot-mode)
         ("\\.gnuplot\\'" . gnuplot-mode)))


;;;; go-mode

(use-package go-mode
  :ensure t
  :mode (("\\.go\\'" . go-mode)
         ("go\\.mod\\'" . go-dot-mod-mode))

  :config
  (progn
    (use-package go-impl
      :ensure t
      :commands go-impl)
    (setq gofmt-command (cond
                         ;; ((executable-find* "gofumports") "gofumports")
                         ((executable-find* "goimports") "goimports")
                         (t "gofmt")))
    (defun my-go-go-command ()
      "Save all buffers, run go fmt and then flycheck, bound to C-c C-C in my go-mode."
      (interactive)
      (save-window-excursion
        (--each (buffer-list)
          (and
           (buffer-live-p it)
           (buffer-modified-p it)
           (eq major-mode 'go-mode)
           (not (eq major-mode 'messages-buffer-mode))
           (not (buffer-base-buffer it))
           (buffer-file-name it)
           (with-current-buffer it
             (save-buffer)))))
      (let ((cc (current-column)))
        (gofmt)
        (move-to-column cc))
      (save-buffer)
      (flycheck-buffer))

    (bind-key "C-c C-c" 'my-go-go-command go-mode-map)

    (use-package go-stacktracer
      :ensure t
      :commands (go-stacktracer-region))

    (require 'go-expanderr nil t)))

(use-package go-ts-mode
  :defer t
  :config
  (progn
    (use-package go-mode :ensure t)
    (bind-key "C-c C-c" 'my-go-go-command go-ts-mode-map)))


;;;;; go-traceback

(use-package go-traceback
  :commands (go-traceback)
  :mode ("goroutines\\.txt\\'" . go-traceback-mode))


;;;;; go-scratch

(use-package go-scratch
  :ensure t
  :commands go-scratch)


;;;; google-c-style

(use-package google-c-style
  :ensure t
  :commands (google-set-c-style google-make-newline-indent))


;;;; google-this

(use-package google-this
  :ensure t
  :commands (google-this
             google-this-cpp-reference
             google-this-error google-this-line google-this-maps
             google-this-mode google-this-region google-this-search
             google-this-symbol google-this-word)
  :init
  (progn
    (define-key search-map "G" 'google-this-region)))


;;;; goto-chg

(use-package undo-tree
  :ensure t
  :if (not noninteractive))

(use-package goto-chg
  :ensure t
  :if (not noninteractive)
  :commands (goto-last-change goto-last-change-flash)
  :bind ("C-c C-SPC" . goto-last-change-flash)
  :config
  (progn
    (defun goto-last-change-flash ()
      (interactive)
      (call-interactively 'goto-last-change)
      (my-pulse-later))))


;;;; graphql-mode

(use-package graphql-mode
  :ensure t
  :mode (("\\.graphql\\'" . graphql-mode)))


;;;; graphviz-dot-mode

(use-package graphviz-dot-mode
  :ensure t
  :commands graphviz-dot-mode
  :mode (("\\.dot\\'" . graphviz-dot-mode)
         ("\\.gv\\'" . graphviz-dot-mode)))


;;;; groovy-mode

(use-package groovy-mode
  :ensure t
  :mode (("\\.groovy\\'" . groovy-mode)
         ("\\.gradle\\'" . groovy-mode)))


;;;; gtpel

(use-package gptel
  :ensure t
  :config (progn
            (load "gptel-autoloads" nil t))
  :commands (gptel gptel-send)
  :init
  (progn
    (setq gptel-model 'gpt-4o)))


;;;; handlebars-mode

(use-package handlebars-mode
  :ensure t
  :commands handlebars-mode
  :mode ("\\.hb\\'" . handlebars-mode))


;;;; hardhat

(use-package hardhat
  :ensure t
  :commands (hardhat-mode
             global-hardhat-mode
             hardhat-buffer-included-p)
  :bind (("M-o m h" . hardhat-mode))
  :init
  (progn
    (setq
     hardhat-mode-lighter " "
     hardhat-basename-protected-regexps
     '(
       "-autoloads\\.el\\'"
       "\\.ix\\'"
       "\\.lock\\'"
       "\\.log\\'"
       "\\.orig\\'"
       "\\.rej\\'"
       "\\`Desktop\\.ini\\'"
       "\\`META\\.yml\\'"
       "\\`MYMETA\\.yml\\'"
       "\\`TAGS\\'"
       "\\`Thumbs\\.db\\'"
       "\\`\\.dropbox\\'"
       "\\`\\.dropbox\\.cache\\'"
       "\\`\\.emacs\\.desktop\\'"
       "\\`\\.emacs\\.desktop\\.lock\\'"
       "\\`test\\.out\\'"
       "type-break\\'"
       "~\\'"
       )
     hardhat-basename-editable-regexps
     '(
       "\\`bash-fc-[0-9]+\\'"
       "\\`bzr_log\\.[[:alnum:]]+"
       "\\`hg-editor-[[:alnum:]]+\\.txt"
       "\\`svn-commit\\.tmp\\'"
       "\\`zshecl[0-9]+"
       )
     hardhat-fullpath-protected-regexps
     '(
       "/CVS/"
       "/RCS/"
       "/SCCS/"
       "/target/generated-sources/"
       "/[._]build/"
       "/\\.bzr/"
       "/\\.coverage/"
       "/vendor/"
       "/\\.git/"
       "/\\.hg/"
       "/\\.rspec/"
       "/\\.sass-cache/"
       "/\\.svn/"
       "/\\.tox/"
       "/_MTN/"
       "/_darcs/"
       "/blib/"
       "/pm_to_blib/"
       "/test_output/"
       "~/\\.cabal/"
       "~/src/dotfiles/emacs/emacs\\.d/elpa/"
       "~/src/dotfiles/emacs/emacs\\.d/packages/"
       "~/src/dotfiles/emacs/emacs\\.d/lib/"
       "~/src/dotfiles/emacs/emacs\\.d/override/"
       "~/src/dotfiles/emacs/emacs\\.d/site-lisp/"
       "~/\\.cpan/"
       "emacs\\.d/init.el"
       "~/\\.emacs\\.d/elpa/"
       "~/\\.emacs\\.d/lib/"
       "~/\\.emacs\\.d/override/"
       "~/\\.emacs\\.d/site-lisp/"
       "~/\\.npm/"
       "~/\\.opt/"
       "~/\\.rvm/"
       "~/\\.virthualenv/"
       "~/\\.virtualenv/"
       "~/\\.virtualenvs/"
       "~/\\.local/share/virtualenvs/"
       "~/perl5/perlbrew/"
       )
     hardhat-fullpath-editable-regexps
     '("~/\\.cpan/CPAN/MyConfig\\.pm\\'"
       "/\\.git/.*\\(?:[A-Z_]*_EDITMSG\\|MERGE_MSG\\|SQUASH_MSG\\|GHI_ISSUE\\|rebase-merge/git-rebase-todo\\|description\\|hooks/\\|config\\|notes\.txt\\)\\'"
       )
     hardhat-bof-content-protected-regexps:
     '((emacs-lisp-mode . "\\`;;;;[^\n]*--- automatically extracted\\>")
       (perl-mode . "^# Changes made here will be lost when autosplit is run again\\>")
       (cperl-mode . "^# Changes made here will be lost when autosplit is run again\\>")
       "\\<THIS IS A GENERATED FILE\\>"
       "\\<automatically generated\\>"
       "\\<generated automatically\\>"
       "\\<Compiled template generated by\\>"
       "\\<do not \\(change\\|edit\\|modify\\)\\>"
       "\\<don't \\(change\\|edit\\|modify\\) this file\\>"
       "\\`;+ *Emacs Bookmark Format Version [0-9]"
       "^;+ *-+ smex-history -+" "^;+ *EIEIO PERSISTENT OBJECT\\>"
       "^;+ *Tramp connection history\\>")
     hardhat-buffer-protected-functions
     '(
       ;; hardhat-protected-by-ignoramus
       hardhat-protected-osx-homebrew
       (perl-mode  . hardhat-protected-by-perl-semantic-eof)
       (cperl-mode . hardhat-protected-by-perl-semantic-eof)))
    (when (and (not noninteractive) load-file-name)
      (global-hardhat-mode 1))))


;;;; haskell-mode

(use-package haskell-mode
  :ensure t
  :commands (haskell-mode)
  :mode ("\\.l?hs\\'" . haskell-mode)
  :preface
  (progn
    (load "haskell-mode-autoloads" t t))
  :init
  (progn
    (setq
     ;; haskell-process-type 'cabal-dev
     haskell-notify-p nil
     ;; haskell-tags-on-save t
     haskell-stylish-on-save t)
    (defun my-haskell-mode-hook ()
      (haskell-indentation-mode))
    (add-hook 'haskell-mode-hook 'my-haskell-mode-hook)))


;;;; highlight-indentation

(use-package highlight-indentation
  ;; :disabled t
  :ensure t
  :if (and
       (not noninteractive))
  :commands (highlight-indentation-mode
             highlight-indentation-current-column-mode)
  :diminish (highlight-indentation-mode
             highlight-indentation-current-column-mode)
  :init
  (progn
    (hook-into-modes #'highlight-indentation-current-column-mode
                     my-significant-whitespace-mode-hooks)
    (add-hook 'ruby-mode-hook 'highlight-indentation-current-column-mode)))


;;;; highlight-symbol

(use-package highlight-symbol
  :ensure t
  :if (and
       (not noninteractive))
  :commands (highlight-symbol-mode highlight-symbol-at-point)
  :diminish highlight-symbol-mode
  :init
  (progn
    (setq
     highlight-symbol-idle-delay 1.1)))


;;;; highlight-tail

(use-package highlight-tail
  :commands highlight-tail-mode)


;;;; htmlize

(use-package htmlize
  :ensure t
  :commands (htmlize-buffer
             htmlize-region
             htmlize-file
             htmlize-many-files
             htmlize-many-files-dired))


;;;; ibuffer

(use-package ibuffer
  :defer
  :bind (("C-h h" . ibuffer)
         ("C-h C-h" . ibuffer)
         ("<XF86Search>" . ibuffer))
  :init
  (progn
    (defvar my-ibufffer-separator " • ")
    (setq ibuffer-filter-group-name-face 'variable-pitch
          ibuffer-use-header-line nil
          ibuffer-old-time 12)

    (use-package ibuffer-vc
      :ensure t
      :commands
      (ibuffer-vc-set-filter-groups-by-vc-root
       ibuffer-vc-generate-filter-groups-by-vc-root))

    (use-package ibuffer-projectile
      :commands ibuffer-projectile-generate-filter-groups-by-projectile-root)

    (use-package ibuffer-tramp
      :ensure t
      :commands (ibuffer-tramp-generate-filter-groups-by-tramp-connection
                 ibuffer-tramp-set-filter-groups-by-tramp-connection))
    ;; Switching to ibuffer puts the cursor on the most recent buffer
    (defadvice ibuffer (around ibuffer-point-to-most-recent activate)
      "Open ibuffer with cursor pointed to most recent buffer name"
      (let ((recent-buffer-name (buffer-name)))
        ad-do-it
        (ibuffer-update nil t)
        (unless (string= recent-buffer-name "*Ibuffer*")
          (ibuffer-jump-to-buffer recent-buffer-name)))))
  :config
  (progn
    (unbind-key "M-o" ibuffer-mode-map)
    (unbind-key "M-g" ibuffer-mode-map)
    (bind-key "r" 'counsel-recentf ibuffer-mode-map)
    (bind-key "s" 'isearch-forward-regexp ibuffer-mode-map)
    (bind-key "." 'ibuffer-invert-sorting ibuffer-mode-map)

    (defun ibuffer-projectile-dired-known-projects-root (&optional arg)
      (interactive "P")

      (use-package projectile)
      (let ((project-to-switch
             (projectile-completing-read "Switch to project: "
                                         projectile-known-projects)))
        (dired project-to-switch)
        (ibuffer)))

    (bind-key "o" 'ibuffer-projectile-dired-known-projects-root ibuffer-mode-map)

    (defun ibuffer-projectile-find-file ()
      (interactive)
      (--when-let (get-buffer "*Ibuffer*")
        (with-current-buffer it
          (let* ((selected-buffer (ibuffer-current-buffer))
                 (buffer-path (with-current-buffer
                                  selected-buffer
                                (or (buffer-file-name)
                                    list-buffers-directory
                                    default-directory)))
                 (default-directory
                   (if (file-regular-p buffer-path)
                       (file-name-directory buffer-path)
                     buffer-path)))
            (projectile-find-file)))))
    (bind-key "f" 'ibuffer-projectile-find-file ibuffer-mode-map)

    (defun ibuffer-magit-status ()
      (interactive)
      (--when-let (get-buffer "*Ibuffer*")
        (with-current-buffer it
          (let* ((selected-buffer (ibuffer-current-buffer))
                 (buffer-path (with-current-buffer
                                  selected-buffer
                                (or (buffer-file-name)
                                    list-buffers-directory
                                    default-directory)))
                 (default-directory
                   (if (file-regular-p buffer-path)
                       (file-name-directory buffer-path)
                     buffer-path)))
            (magit-status default-directory)))))
    (bind-key "i" 'ibuffer-magit-status ibuffer-mode-map)
    (bind-key "G" 'ibuffer-magit-status ibuffer-mode-map)

    (setq ibuffer-directory-abbrev-alist
          (-uniq
           (-concat
            (-flatten
             (--map
              (list
               (cons
                (file-name-as-directory (expand-file-name (cdr it)))
                (format "%12s" (concat (car it) my-ibufffer-separator)))
               (cons
                (file-name-as-directory (file-truename (cdr it)))
                (format "%12s" (concat (car it) my-ibufffer-separator))))
              '(
                ("tracklib" . "~/src/gitlab.tracklib.com/")
                ("23c" . "~/src/gitlab.23c.se/23c/")
                ("tf@23c" . "~/src/gitlab.23c.se/thomasf")
                ("23c" . "~/src/gitlab.23c.se/")
                ("tf@gh" . "~/src/github.com/thomasf/")
                ("github" . "~/src/github.com/")
                ("src" . "~/src/")
                ("notes" . "~/notes/")
                ("venv" . "~/.virtualenvs/")
                ("venv" . "~/.local/share/virtualenvs/")
                (".emacsp" . "~/.emacs.d/elpa/")
                (".emacsd" . "~/.emacs.d/")
                (".config" . "~/.config/")
                ("dotfiles" . "~/src/dotfiles/")
                ("goroot" . "~/.opt/go/")
                (".opt" . "~/.opt")
                ("/usr" . "^/usr")
                ("/var" . "^/var")
                ("/etc" . "^/etc")
                ("/lib" . "^/lib")
                ("/media" . "^/media")
                ("/mnt" . "^/mnt")
                ("/srv" . "^/srv")
                ))))))

    (use-package ibuffer-git
      :ensure t)

    (use-package ibuffer-vc
      :ensure t)

    (defun ibuffer-my-abbrevs (filename)
      (if (s-starts-with? "/scp:" filename t)
          (format "%12s%s" (concat "!scp" my-ibufffer-separator)
                  (s-chop-prefix "/scp:" filename))
        (let ((directory-abbrev-alist ibuffer-directory-abbrev-alist))
          (abbreviate-file-name filename))))

    (define-ibuffer-column filename
      (:summarizer
       (lambda (strings)
         (let ((total (length (delete "" strings))))
           (cond ((zerop total) "No files")
                 ((= 1 total) "1 file")
                 (t (format "%d files" total))))))
      (ibuffer-my-abbrevs (or (ibuffer-buffer-file-name) "")))

    (define-ibuffer-column name-strip
      (:inline t
               :header-mouse-map ibuffer-name-header-map
               :props
               ('mouse-face
                'highlight 'keymap ibuffer-name-map
                'ibuffer-name-column t
                'help-echo
                '(if tooltip-mode
                     "mouse-1: mark this buffer\nmouse-2: select this buffer\nmouse-3: operate on this buffer"
                   "mouse-1: mark buffer   mouse-2: select buffer   mouse-3: operate"))
               :summarizer
               (lambda (strings)
                 (let ((bufs (length strings)))
                   (cond ((zerop bufs) "No buffers")
                         ((= 1 bufs) "1 buffer")
                         (t (format "%s buffers" bufs))))))
      (propertize
       (s-left
        (or
         (s-index-of uniquify-separator (buffer-name))
         (string-width (buffer-name)))
        (buffer-name))
       'font-lock-face (ibuffer-buffer-name-face buffer mark)))

    (define-ibuffer-column size-h
      (:name "Size" :inline t)
      (cond
       ((> (buffer-size) 1000)
        (format "%7.1fk" (/ (buffer-size) 1000.0)))
       ((> (buffer-size) 1000000)
        (format "%7.1fM" (/ (buffer-size) 1000000.0)))
       (t
        (format "%8d" (buffer-size)))))

    (require 'ibuf-ext)
    (define-ibuffer-filter filename2
        "Toggle current view to buffers with filename matching QUALIFIER."
      (:description "filename2"
                    :reader (read-from-minibuffer "Filter by filename (regexp): "))
      ;; (ibuffer-awhen (buffer-local-value 'buffer-file-name buf)
      (when-let* ((it (with-current-buffer buf
                        (or buffer-file-name
                            default-directory))))
        (string-match qualifier it)))

    (defun ibuffer-set-filter-groups-by-root  ()
      (interactive)
      (setq ibuffer-filter-groups
            (-concat
             '(("MORE"
                (or (mode . magit-log-edit-mode)
                    (name . "^\\*\\(traad-server\\|httpd\\|epc con.*\\|tramp/.*\\|Completions\\)\\*$")
                    (name . "^\\*Pymacs\\*$")
                    (name . "^\\*Compile-log\\*$")
                    (name . "^\\*magit-\\(process\\)\\*$")
                    (name . "^ "))))
             '(("EMACS"
                (or
                 (name . "^\\*scratch")
                 (name . "^\\*Messages")
                 (name . "^\\*Help")
                 )))
             (ibuffer-projectile-generate-filter-groups-by-projectile-root)
             ;; (ibuffer-vc-generate-filter-groups-by-vc-root)
             (ibuffer-tramp-generate-filter-groups-by-tramp-connection))))

    (defun toggle-ibuffer-filter-groups ()
      "DOCSTRING"
      (interactive)
      (let ((ibuf (get-buffer "*Ibuffer*")))
        (when ibuf
          (with-current-buffer ibuf
            (let ((selected-buffer (ibuffer-current-buffer)))
              (if (not ibuffer-filter-groups)
                  (ibuffer-set-filter-groups-by-root)
                (setq ibuffer-filter-groups nil))
              (pop-to-buffer ibuf)
              (ibuffer-update nil t)
              (ibuffer-jump-to-buffer (buffer-name selected-buffer )))))))
    (bind-key "h" 'toggle-ibuffer-filter-groups ibuffer-mode-map)

    (defun set-categorized-ibuffer-filter-group ()
      "DOCSTRING"
      (interactive)
      (let ((ibuf (get-buffer "*Ibuffer*")))
        (when ibuf
          (with-current-buffer ibuf
            (let ((selected-buffer (ibuffer-current-buffer)))
              (pop-to-buffer ibuf)
              (ibuffer-switch-to-saved-filter-groups "categorized")
              (ibuffer-update nil t)
              (ibuffer-jump-to-buffer (buffer-name selected-buffer )))))))

    (bind-key "H" 'set-categorized-ibuffer-filter-group ibuffer-mode-map)

    (defadvice ibuffer-invert-sorting (around ibuffer-point-to-same activate)
      "TODO"
      (let ((ibuf (get-buffer "*Ibuffer*")))
        (when ibuf
          (with-current-buffer ibuf
            (let ((selected-buffer (ibuffer-current-buffer)))
              ad-do-it
              (ibuffer-jump-to-buffer (buffer-name selected-buffer )))))))

    (defadvice ibuffer-toggle-sorting-mode (around ibuffer-point-to-same activate)
      "TODO"
      (let ((ibuf (get-buffer "*Ibuffer*")))
        (when ibuf
          (with-current-buffer ibuf
            (let ((selected-buffer (ibuffer-current-buffer)))
              ad-do-it
              (ibuffer-jump-to-buffer (buffer-name selected-buffer )))))))

    (setq
     ibuffer-default-sorting-mode 'recency
     ibuffer-eliding-string "…"
     ibuffer-compile-formats t
     ibuffer-git-column-length 4
     ibuffer-formats '(
                       (
                        mark
                        (size-h 9 -1 :right)
                        " "
                        (mode 4 4 :right :elide)
                        " "
                        read-only
                        modified
                        " "
                        (name-strip 25 25 :left :elide)
                        "  "
                        (vc-status-mini 1 1)
                        " "
                        filename-and-process)
                       (mark " " (name 16 -1) " " filename))
     ibuffer-show-empty-filter-groups nil
     ibuffer-saved-filter-groups
     (quote (("flat")
             ("categorized"
              ;; -------------------------------------------------
              ;; programming languages #1
              ("elisp" (or
                        (mode . emacs-lisp-mode)
                        ))
              ("code" (or
                       (mode . python-mode)
                       (mode . python-ts-mode)
                       (mode . ruby-mode)
                       (mode . js-mode)
                       (mode . js2-mode)
                       (mode . js2-jsx-mode)
                       (mode . java-mode)
                       (mode . sh-mode)
                       (mode . haskell-mode)
                       (mode . go-mode)
                       (mode . go-ts-mode)
                       (mode . rust-mode)
                       ))
              ;; -------------------------------------------------
              ;; programming languages #1
              ("css pre" (or
                          (mode . scss-mode)
                          ))
              ;; -------------------------------------------------
              ;; programming languages #1
              ("css" (or
                      (mode . css-mode)
                      ))
              ;; -------------------------------------------------
              ;; html and similar
              ("html" (or
                       (mode . html-mode)
                       (mode . web-mode)
                       ))
              ;; -------------------------------------------------
              ;; configuration/data files
              ("xml" (or
                      (mode . nxml-mode)
                      ))
              ;; -------------------------------------------------
              ;; text/notetaking/org
              ("org agenda" (mode . org-agenda-mode))
              ("org" (or
                      (mode . org-mode)
                      (name . "^\\*Calendar\\*$")
                      (name . "^diary$")
                      ))
              ("text misc" (or
                            (mode . text-mode)
                            (mode . rst-mode)
                            (mode . markdown-mode)
                            (mode . gfm-mode)
                            (mode . poly-gfm-mode)
                            (mode . poly-markdown-mode)
                            ))
              ;; -------------------------------------------------
              ;; media
              ("media" (or
                        (mode . image-mode)
                        ))
              ;; -------------------------------------------------
              ;; misc
              ("w3m" (mode . w3m-mode))
              ("scm" (or
                      (mode . magit-status-mode)
                      (mode . magit-log-mode)
                      (mode . vc-annotate-mode)
                      ))
              ("dired" (mode . dired-mode))
              ("help" (or
                       (mode . Info-mode)
                       (mode . help-mode)
                       (mode . Man-mode)
                       (name  . "^\\*frequencies\\*$")
                       (name . "^\\*Smex: Unbound Commands\\*$")
                       (name . "^\\*Personal Keybindings\\*$")
                       ))
              ("weechat" (mode . weechat-mode)
               )
              ;; -------------------------------------------------
              ;; *buffer* buffers
              ("*kite*" (name . "^\\*kite.*\\*")
               )
              ("MORE" (or
                       (mode . magit-log-edit-mode)
                       (name . "^\\*\\(traad-server\\|httpd\\|epc con.*\\|tramp/.*\\|Completions\\)\\*$")
                       (name . "^\\*Pymacs\\*$")
                       (name . "^\\*Compile-log\\*$")
                       (name . "^\\*magit-\\(process\\|commit\\)\\*$")
                       (name . "^ ")
                       ))
              ("*buffer*" (name . "\\*.*\\*")
               )
              ))))
    (add-hook 'ibuffer-mode-hook
              #'(lambda ()
                  (setq ibuffer-hidden-filter-groups '("MORE"))
                  (ibuffer-update nil t)
                  (hl-line-mode 1)))
    (defun ibuffer-counsel-find-file ()
      "Like `counsel-find-file', but start in the directory of the buffer at point."
      (interactive)
      (let ((buf (ibuffer-current-buffer)))
        (counsel-find-file nil (if (buffer-live-p buf)
                                   (buffer-local-value 'default-directory buf)
                                 default-directory))))
    (bind-key "C-x C-f" 'ibuffer-counsel-find-file ibuffer-mode-map)))


;;;; ielm

(use-package ielm
  :defer
  :init
  (progn
    (setq ielm-prompt "» ")))


;;;; ietf-docs


(use-package ietf-docs
  :ensure t
  :commands ietf-docs-open-at-point)


;;;; image-mode

(use-package image-mode
  :defer
  :config
  (progn
    (auto-image-file-mode +1)
    (defun image-mouse-scroll-position (start-event)
      "When bound to a down-mouse-X input START-EVENT, will smoothly
drag the viewpoint on the image buffer that the window displays."
      (interactive "e")
      (let* ((echo-keystrokes 0)
             (start-posn (event-start start-event))
             (x-offset (window-hscroll (car start-posn)))
             (y-offset (window-vscroll (car start-posn) t))
             (imframe-cwidth (frame-char-width
                              (window-frame (car start-posn))))
             (startpos (nth 8 start-posn))
             (movement-speed 10)
             event now-posn nowpos x-diff y-diff )
        (select-window (posn-window start-posn))
        (track-mouse
          (while (progn
                   ;; as used in mouse.el:mouse-drag-track
                   (setq event (read-event))
                   (or (mouse-movement-p event)
                       (memq (car-safe event) '(switch-frame select-window))))
            (setq now-posn (event-start event))
            ;; check if drag is within image on frame
            (if (and (eq (nth 7 start-posn) (nth 7 now-posn))
                     (eq (nth 1 start-posn) (nth 1 now-posn)))
                (progn
                  (setq nowpos (nth 8 now-posn))
                  (setq x-diff (- (car startpos) (car nowpos)))
                  (setq y-diff (- (cdr startpos) (cdr nowpos)))

                  (set-window-vscroll (nth 0 start-posn)
                                      (+ y-offset y-diff) t)
                  (if (< (/ imframe-cwidth 2)
                         (abs x-diff))
                      (set-window-hscroll
                       (car start-posn)
                       (+ x-offset
                          (/ (if (> x-diff 0)
                                 (+ x-diff (/ imframe-cwidth 2))
                               (- x-diff (/ imframe-cwidth 2)))
                             imframe-cwidth))))
                  (setq x-offset (window-hscroll (car now-posn)))
                  (setq y-offset (window-vscroll (car now-posn) t))))))))
    (bind-key "<down-mouse-3>" 'image-mouse-scroll-position image-mode-map)
    (bind-key "<down-mouse-1>" 'image-mouse-scroll-position image-mode-map)
    (defun image-dired-here ()
      (interactive)
      (image-dired default-directory))
    (bind-key "d" 'image-dired-here  image-mode-map)))


;;;; imenu

(use-package imenu
  :bind (("M-o i" . counsel-imenu))
  :init
  (progn
    (setq
     imenu-auto-rescan t)))


;;;; interaction-log

(use-package interaction-log
  :ensure t
  :commands interaction-log-mode)


;;;; irony

(use-package irony
  :ensure t
  :commands (irony-mode)
  :defer t
  :diminish irony-mode
  :init
  (progn
    (setq irony-user-dir (expand-file-name "irony" user-data-directory))
    (add-hook 'arduino-mode-hook 'irony-mode))
  :config
  (progn
    (add-to-list 'irony-supported-major-modes 'arduino-mode)
    (add-to-list 'irony-lang-compile-option-alist '(arduino-mode . "c++"))

    (use-package irony-cdb
      :commands (irony-cdb-autosetup-compile-options)
      :init
      (progn
        (add-hook 'arduino-mode-hook 'irony-cdb-autosetup-compile-options)))))


;;;; ivy

(use-package ivy
  :ensure t
  :if (and (not noninteractive))
  :defer t
  :commands ivy-mode
  :init
  (progn
    (setq ivy-on-del-error-function #'ignore
          ivy-height 30
          ivy-use-virtual-buffers t
          ivy-re-builders-alist
          '((swiper . ivy--regex-ignore-order)
            (swiper-isearch . ivy--regex-ignore-order)
            (counsel-rg . ivy--regex-ignore-order)
            (counsel-git-grep . ivy--regex-ignore-order)
            (t . ivy--regex-fuzzy))))
  (ivy-mode)
  :config
  (progn
    ;; Ignore spaces in fuzzy input so "find init" matches "find-init". Ivy
    ;; special-cases `ivy--regex-fuzzy' by identity for flx sorting and
    ;; highlighting, so advise it instead of using a wrapper re-builder.
    (defun my-ivy-fuzzy-strip-spaces (str)
      (replace-regexp-in-string " +" "" str))
    (advice-add 'ivy--regex-fuzzy :filter-args
                (lambda (args)
                  (cons (my-ivy-fuzzy-strip-spaces (car args)) (cdr args))))
    (advice-add 'ivy--flx-sort :filter-args
                (lambda (args)
                  (cons (my-ivy-fuzzy-strip-spaces (car args)) (cdr args))))
    (advice-add 'ivy--highlight-fuzzy :around
                (lambda (fn str)
                  (let ((ivy-text (my-ivy-fuzzy-strip-spaces ivy-text)))
                    (funcall fn str))))))


;;;; js

(use-package js
  :defer
  :init
  (progn
    (rename-modeline "js" js-mode "js"))
  :config
  (progn
    (defun js-mode-cccc ()
      (interactive)
      (silent-save-some-buffers)
      (prettier-js))

    (bind-key "C-c C-c" 'js-mode-cccc js-mode-map)))


;;;; js-comint

(use-package js-comint
  :commands inferior-js-mode
  :ensure t
  :init
  (progn
    (setq inferior-js-program-command "nodejs")))


;;;; js2-mode

(use-package js2-mode
  :ensure t
  :commands (js2-mode js2-jsx-mode js2-minor-mode)

  :init
  (progn
    (if (>= emacs-major-version 27) ;; compat
        (add-hook 'js-mode-hook 'js2-minor-mode)
      (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
      (add-to-list 'auto-mode-alist '("\\.jsx\\'" . js2-jsx-mode)))
    (rename-modeline "js2-mode" js2-mode "js2")
    (setq js2-strict-missing-semi-warning nil
          js2-strict-trailing-comma-warning nil
          js2-include-node-externs t
          js2-idle-timer-delay 0.1
          js2-highlight-level 3
          js2-mode-show-parse-errors nil ;; Let flycheck handle parse errors
          js2-mode-show-strict-warnings nil ;; Let flycheck handle parse errors
          ))

  :config
  (progn
    (defun js2-mode-cccc ()
      (interactive)
      (silent-save-some-buffers)
      (prettier-js))

    (bind-key "C-c C-c" 'js2-mode-cccc js2-mode-map)
    (bind-key "M-." 'xref-find-definitions js2-mode-map)

    (when (and (not noninteractive) window-system)
      (font-lock-add-keywords
       'js2-mode `(("\\(function *\\)("
                    (0 (progn (compose-region
                               (match-beginning 1)
                               (match-end 1) "λ") nil))))))
    (defun my-js2-maybe-jsx-mode ()
      "Switch to js2-jsx-mode if buffers seems to be using react"
      (when (eq major-mode 'js2-mode)
        (save-excursion
          (goto-char (point-min))
          (when (re-search-forward
                 (rx (or
                      "from \"react\""
                      "from 'react'"
                      "require(\"react\")"
                      "require('react')"))
                 3000 t 1)
            (js2-jsx-mode)))))
    (add-hook 'after-change-major-mode-hook 'my-js2-maybe-jsx-mode )

    (bind-key "C-<tab>" 'web-mode js2-jsx-mode-map)


    (use-package js2-imenu-extras
      :config
      (progn
        (js2-imenu-extras-setup)))

    (font-lock-add-keywords
     ;; After js2 has parsed a js file, we look for jslint globals decl comment ("/* global Fred, _, Harry */") and
     ;; add any symbols to a buffer-local var of acceptable global vars
     ;; Note that we also support the "symbol: true" way of specifying names via a hack (remove any ":true"
     ;; to make it look like a plain decl, and any ':false' are left behind so they'll effectively be ignored as
     ;; you can;t have a symbol called "someName:false"
     (add-hook 'js2-post-parse-callbacks
               (lambda ()
                 (when (> (buffer-size) 0)
                   (let ((btext (replace-regexp-in-string
                                 ": *true" " "
                                 (replace-regexp-in-string "[\n\t ]+" " " (buffer-substring-no-properties 1 (buffer-size)) t t))))
                     (mapc (apply-partially 'add-to-list 'js2-additional-externs)
                           (split-string
                            (if (string-match "/\\* *global *\\(.*?\\) *\\*/" btext) (match-string-no-properties 1 btext) "")
                            " *, *" t))
                     ))))

     'js2-mode `(("\\(function *\\)("
                  (0 (progn (compose-region
                             (match-beginning 1)
                             (match-end 1) "ƒ") nil)))))
    (font-lock-add-keywords 'js2-mode
                            '(("\\<\\(FIX\\|TODO\\|FIXME\\|HACK\\|REFACTOR\\):"
                               1 font-lock-warning-face t)))))


;;;; json-mode

(use-package json-mode
  :ensure t
  :commands json-mode
  :mode (("\\.json\\'" . json-mode)
         ("\\.ipynb\\'" . json-mode)
         ("\\.eslintrc\\'" . json-mode)
         ("\\Pipfile.lock\\'" . json-mode)))


;;;; keep-buffers

(use-package keep-buffers
  :if (and (not noninteractive))
  :commands (keep-buffers-mode)
  :init
  (progn
    (setq keep-buffers-protected-alist
          '(("\\`\\*scratch\\*\\'" . erase)
            ("\\`\\*Messages\\*\\'" . nil)
            ;; ("\\`\\*magit:.*\\*\\'" . nil)
            ))
    (keep-buffers-mode 1)))


;;;; know-your-http-well

(use-package know-your-http-well
  :ensure t
  :commands (http-header
             http-method
             http-relation
             http-status-code))


;;;; kotlin

(use-package kotlin-mode
  :ensure t
  :mode (("\\.kt\\'" . kotlin-mode)))


;;;; langtool

(use-package langtool
  :ensure t
  :commands (langtool-check langtool-correct-buffer)
  :config
  (progn
    (setq langtool-language-tool-jar "~/.opt/LanguageTool/languagetool-commandline.jar"
          langtool-disabled-rules '(
                                    "WHITESPACE_RULE"
                                    ;; "EN_UNPAIRED_BRACKETS"
                                    ;; "COMMA_PARENTHESIS_WHITESPACE"
                                    "EN_QUOTES"))))


;;;; launch

(use-package launch
  :ensure t
  :commands (global-launch-mode turn-on-launch-mode launch-file))


;;;; less-css-mode

(use-package less-css-mode
  :ensure t
  :mode "\\.less\\'")


;;;; libmpdee

(use-package libmpdee
  :ensure t
  :defer)


;;;; lisp-mode

(use-package lisp-mode
  :defer t
  :init
  (progn
    (rename-modeline "lisp-mode" emacs-lisp-mode "el"))
  :config
  (progn

    (use-package erefactor
      :ensure t
      :commands  (erefactor-lint-by-emacsen
                  erefactor-rename-symbol-in-package
                  erefactor-add-current-defun
                  erefactor-change-prefix-in-buffer
                  erefactor-dehighlight-all-symbol
                  erefactor-highlight-current-symbol
                  erefactor-lint
                  erefactor-rename-symbol-in-buffer
                  erefactor-eval-current-defun))

    (defun emacs-lisp-remove-elc-on-save ()
      "If you're saving an elisp file, likely the .elc is no longer valid."
      (make-local-variable 'after-save-hook)
      (add-hook 'after-save-hook
                (lambda ()
                  (when (and
                         buffer-file-name
                         (file-exists-p (concat buffer-file-name "c")))
                    (delete-file (concat buffer-file-name "c"))))))
    (add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
    (add-hook 'emacs-lisp-mode-hook 'emacs-lisp-remove-elc-on-save)))


;;;; loccur

(use-package loccur
  :ensure t
  :commands (loccur loccur-mode loccur-current loccur-no-highlight)
  :bind (("C-x l" . loccur-current)
         ("C-x L" . loccur-previous-match)
         ;; ("M-s l" . loccur)
         )
  :init
  (progn
    (define-key region-bindings-mode-map "l" 'loccur-current)))


;;;; logito

(use-package logito
  :ensure t
  :commands (logito-log logito-should-log logito-insert-log))


;;;; look-mode

(use-package look-mode
  :ensure t
  :commands (look-at-files look-at-this-file))


;;;; lorem-ipsum

(use-package lorem-ipsum
  :ensure t
  :commands (lorem-ipsum-insert-paragraphs
             lorem-ipsum-insert-sentences
             lorem-ipsum-insert-list)
  :config
  (progn
    (fmakunbound 'Lorem-ipsum-insert-list)
    (fmakunbound 'Lorem-ipsum-insert-paragraphs)
    (fmakunbound 'Lorem-ipsum-insert-sentences)))


;;;; lsp-mode

(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-mode)
  :init
  (progn
    (setq lsp-gopls-codelens nil
          lsp-idle-delay 0.2
          lsp-file-watch-threshold 15000
          lsp-response-timeout 25
          lsp-auto-execute-action nil
          lsp-diagnostics-provider :flycheck
          ;; lsp-diagnostics-provider :none
          lsp-diagnostics-disabled-modes '(go-mode python-mode python-ts-mode go-ts-mode)
          ;; lsp-disabled-clients '(semgrep-ls ruff pyright)
          lsp-disabled-clients '(semgrep-ls ruff)
          lsp-pylsp-configuration-sources ["flake8"]
          ;; lsp-pylsp-configuration-sources ["ruff"]
          lsp-pylsp-configuration-sources nil
          lsp-pylsp-plugins-flake8-enabled t
          lsp-pylsp-plugins-autopep8-enabled nil
          lsp-pylsp-plugins-pycodestyle-enabled nil
          lsp-pylsp-plugins-pyflakes-enabled nil
          lsp-pylsp-plugins-autopep8-enabled nil
          lsp-pylsp-plugins-pylint-enabled nil
          lsp-pylsp-plugins-yapf-enabled nil
          lsp-pylsp-plugins-pydocstyle-enabled nil)


    (defun lsp-diagnostics-toggle-major-mode ()
      "Toggles the current major mode in the lsp-diagnostics-disabled-modes list and enables or disables lsp-diagnostics-mode in all matching open buffers."
      (interactive)
      (let ((disabled (member major-mode lsp-diagnostics-disabled-modes)))
        (if disabled
            (setq lsp-diagnostics-disabled-modes (remove major-mode lsp-diagnostics-disabled-modes))
          (add-to-list 'lsp-diagnostics-disabled-modes major-mode))
        (dolist (buf (buffer-list (current-buffer)))
          (when (eq major-mode (buffer-local-value 'major-mode buf))
            (with-current-buffer buf
              (lsp-diagnostics-mode (if disabled 1 -1)))))))

    (defun flycheck-checker-lsp-add-next-errcheck ()
      (interactive)
      (flycheck-add-next-checker 'lsp 'go-errcheck))

    (defun lsp-switch-flycheck-diagnostic ()
      (interactive)
      (if (equal lsp-diagnostics-provider :none)
          (setq lsp-diagnostics-provider :flycheck)
        (setq lsp-diagnostics-provider :none))
      (message "%s" lsp-diagnostics-provider))

    (add-hook 'python-base-mode-hook #'lsp)
    (add-hook 'js2-mode-hook #'lsp)
    (add-hook 'js2-jsx-mode-hook #'lsp)
    (add-hook 'typescript-mode-hook #'lsp)
    (add-hook 'typescript-ts-mode-hook #'lsp)
    (add-hook 'js-mode-hook #'lsp)
    (add-hook 'go-dot-mod-mode-hook #'lsp)
    (add-hook 'terraform-mode-hook #'lsp)
    (add-hook 'csharp-mode-hook #'lsp)
    (add-hook 'csharp-ts-mode-hook #'lsp)

    (when (executable-find* "html-languageserver"
                            "yarn global add vscode-html-languageserver-bin")
      (add-hook 'html-mode-hook #'lsp)
      (add-hook 'web-mode-hook #'lsp))

    (when (executable-find* "gopls")
      (add-hook 'go-mode-hook #'lsp))

    (when (executable-find* "gopls")
      (add-hook 'go-ts-mode-hook #'lsp))

    (when (executable-find* "rls"
                            "rustup component add rls --toolchain stable-x86_64-unknown-linux-gnu")
      (add-hook 'rust-mode-hook #'lsp))

    (when (executable-find* "css-languageserver"
                            "yarn global add vscode-css-languageserver-bin")
      (add-hook 'scss-mode-hook #'lsp)
      (add-hook 'css-mode-hook #'lsp))

    (when (executable-find* "yaml-language-server"
                            "yarn global add yaml-language-server")
      (add-hook 'yaml-mode-hook #'lsp))

    (when (executable-find* "terraform-lsp"
                            "https://github.com/juliosueiras/terraform-lsp")
      (add-hook 'terraform-mode-hook #'lsp))

    (when (file-exists-p*
           "~/src/github.com/fwcd/kotlin-language-server/server/build/install/server/bin/kotlin-language-server")
      (use-package lsp-kotlin
        :after kotlin-mode
        :init
        (progn
         (setq lsp-clients-kotlin-server-executable (expand-file-name "~/src/github.com/fwcd/kotlin-language-server/server/build/install/server/bin/kotlin-language-server"))
          (add-hook 'kotlin-mode-hook #'lsp))))


    (use-package lsp-pyright
      ;; :disabled t
      :ensure t
      :init
      (progn
        (setq lsp-pyright-langserver-command "basedpyright")
        (defun my-lsp-pyright-hook ()
          (require 'lsp-pyright)
          (lsp))
        (add-hook 'python-base-mode-hook #'my-lsp-pyright-hook)))


    (use-package lsp-java
      :ensure t
      :after cc-mode
      :init
      (progn
        (add-hook 'java-mode-hook #'lsp)))

    )



  :config
  (progn
    (add-to-list 'lsp-file-watch-ignored-directories (rx line-start (literal backup-dir) (zero-or-more not-newline))
                 )

    (use-package lsp-headerline
      :defer
      :config
      (progn
        (set-face-attribute 'lsp-headerline-breadcrumb-symbols-face nil :font my-variable-pitch-font)
        (set-face-attribute 'lsp-headerline-breadcrumb-path-face nil :font my-variable-pitch-font)
        (set-face-attribute 'lsp-headerline-breadcrumb-project-prefix-face nil :font my-variable-pitch-font)

        (set-face-attribute 'lsp-headerline-breadcrumb-symbols-face nil :height 0.95)
        (set-face-attribute 'lsp-headerline-breadcrumb-path-face nil :height 0.95)
        (set-face-attribute 'lsp-headerline-breadcrumb-project-prefix-face nil :height 1.0)))

    (load "lsp-mode-autoloads" nil t)
    (setq
     lsp-auto-guess-root t
     lsp-prefer-flymake nil
     lsp-session-file (expand-file-name (workspace-prefix-file-name "lsp-session-v1" ".el") user-data-directory)
     lsp-restart 'auto-restart
     lsp-clients-go-library-directories
     (list
      "/usr"
      (expand-file-name "~/.opt/go")
      (expand-file-name "~/pkg/mod")
      ))))


;;;;; lsp-ui

(use-package lsp-ui
  :ensure t
  :commands (lsp-ui-mode)
  :init
  (progn
    (add-hook 'lsp-mode-hook 'lsp-ui-mode))
  :config
  (progn

    ;; (defun lsp-ui-sideline--align (&rest lengths)
    ;;   """OVerride  """
    ;;   (+ (floor (* (apply '+ lengths) 0.75))
    ;;       (if (display-graphic-p) 1 2)))

    (setq
     lsp-ui-doc-delay 0.2
     ;; lsp-ui-doc-delay 3
     lsp-ui-sideline-delay 1.1
     lsp-document-highlight-delay 1.1

     lsp-ui-doc-enable t
     lsp-ui-doc-header nil
     lsp-ui-doc-include-signature nil
     lsp-ui-doc-show-with-mouse t
     lsp-ui-doc-show-with-cursor nil

     lsp-ui-sideline-enable t
     lsp-ui-sideline-show-code-actions nil
     lsp-ui-sideline-show-hover nil
     lsp-ui-sideline-show-diagnostics nil
     lsp-ui-sideline-show-symbol t
     lsp-ui-sideline-ignore-duplicate t
     )

    (use-package lsp-ui-flycheck
      :defer t
      :config
      (progn
        ;; disable lsp-ui-flycheck hard. (might not be needed anymore)
        ;; (defun lsp-ui-flycheck-enable (_))
        ))))


;;;;; dap-mode

(use-package dap-mode
  :ensure t
  :commands (dap-debug dap-debug-edit-template)
  :init
  (progn
    (setq dap-breakpoints-file (expand-file-name
                                (workspace-prefix-file-name "dap-breakpoints" ".el")
                                user-data-directory))))


;;;; macrostep

(use-package macrostep
  :ensure t
  :commands macrostep-expand)


;;;; magit

(use-package magit
  :ensure t
  :defer 19
  :commands (magit-log magit-run-gitk magit-run-git-gui
                       magit-status magit-git-repo-p magit-list-repos find-git-notes-txt)
  :bind (("M-o G" . my-magit-status-with-prefix)
         ("M-o g" . my-magit-status)
         ("C-h g" . my-magit-status)
         )
  :init
  (progn
    (setq
     ;; magit-bury-buffer-function 'bury-buffer
     magit-bury-buffer-function 'magit-restore-window-configuration
     ;; magit-display-buffer-function 'magit-display-buffer-traditional
     magit-display-buffer-function 'magit-display-buffer-fullframe-status-v1
     magit-pre-display-buffer-hook '(magit-save-window-configuration)
     magit-post-display-buffer-hook '(magit-maybe-set-dedicated)
     ;; magit-post-display-buffer-hook nil
     magit-commit-show-diff nil
     magit-save-repository-buffers nil ;; manually saving all buffers instead (my-magit-status)
     ;; magit-completing-read-function 'magit-ido-completing-read
     magit-completing-read-function 'magit-builtin-completing-read
     magit-diff-refine-hunk 'all
     magit-log-author-date-max-length 25
     magit-log-auto-more t
     magit-auto-revert-mode t
     magit-auto-revert-mode-lighter ""
     magit-revert-buffers 'silent
     magit-revision-insert-related-refs t
     magit-revision-insert-related-refs-display-alist '((contained . nil) (merged . nil)))

    ;; (defadvice magit-version (around skipit activate)
    ;;   "900000000")

    (use-package magit-blame
      :commands (magit-blame-mode magit-blame))

    (use-package magit-wip
      :commands (magit-wip-save-mode
                 global-magit-wip-save-mode))

    (defun git-wip ()
      "run git wip"
      (interactive)
      (shell-command "git wip" nil nil))

    (defun my-magit-status ()
      (interactive)
      (silent-save-some-buffers)
      (call-interactively 'magit-status))

    (defun my-magit-status-with-prefix ()
      (interactive)
      (let ((current-prefix-arg '(4)))
        (call-interactively 'my-magit-status))))
  :config
  (progn
    (load "magit-autoloads" nil t)

    (defun find-git-notes-txt ()
      "Open notes.txt in current .git repo"
      (interactive)
      (find-file (file-name-concat (magit-gitdir) "notes.txt")))

    (use-package magit-section
      :ensure t)

    (defun my-git-commit-hook-fn ()
      "My git commit mode hook."
      (ispell-change-dictionary "english")
      (flyspell-mode 1)
      (save-place-local-mode -1))

    (add-hook 'git-commit-setup-hook 'my-git-commit-hook-fn)

    (require 'json)
    (bind-key "h" 'ibuffer magit-status-mode-map)
    (defun magit-toggle-whitespace ()
      (interactive)
      (if (member "-w" magit-diff-options)
          (magit-dont-ignore-whitespace)
        (magit-ignore-whitespace)))
    (defun magit-ignore-whitespace ()
      (interactive)
      (add-to-list 'magit-diff-options "-w")
      (magit-refresh))
    (defun magit-dont-ignore-whitespace ()
      (interactive)
      (setq magit-diff-options (remove "-w" magit-diff-options))
      (magit-refresh))
    ;; (bind-key "W" 'magit-toggle-whitespace magit-status-mode-map)
    ))


;;;; manage-minor-mode

(use-package manage-minor-mode
  :ensure t
  :commands manage-minor-mode)


;;;; markdown-mode

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("\\.slide\\'" . gfm-mode))
  :init
  (progn
    (setq markdown-command "pandoc -f markdown -t html"
          markdown-fontify-code-blocks-natively nil))
  :config
  (progn
    (unbind-key "`" gfm-mode-map)))


;;;; memory-usage

(use-package memory-usage
  :ensure t
  :commands memory-usage)


;;;; mingus

(use-package mingus
  :commands (mingus mingus-stop mingus-dired-add mingus-dired-add-and-play)
  :ensure t
  :if (not noninteractive)
  :init
  (progn
    (setq
     mingus-use-mouse-p nil))
  :config
  (progn
    (dolist
        (m (list mingus-playlist-mode-map mingus-browse-mode-map mingus-help-mode-map))
      (define-key m "s" 'isearch-forward-regexp)
      (define-key m "t" 'mingus-toggle)
      (define-key m "n" 'next-line)
      (define-key m "p" 'previous-line)
      (define-key m "h" 'ibuffer))
    (defun mingus ()
      "My mingus command"
      (interactive)
      (mingus-switch-to-playlist)
      (mingus-playlist)
      (mingus-goto-current-song)
      (recenter))))


;;;; misc

(use-package misc
  :bind (("M-z" . zap-up-to-char)))


;;;; mo-git-blame

(use-package mo-git-blame
  :ensure t
  :commands mo-git-blame-current
  :init
  (progn
    (setq mo-git-blame-blame-window-width 25)))


;;;; mouse

(use-package mouse
  :defer
  :init
  (progn
    (setq mouse-yank-at-point t)))


;;;; mouse-drag

(use-package mouse-drag
  :bind ("<down-mouse-3>" . mouse-drag-drag))


;;;; multi-term

(use-package multi-term
  :ensure t
  :commands (multi-term)
  :bind (("M-o t" . multi-term))
  :init
  (progn
    (setq multi-term-program "/bin/bash")))


;;;; multi-web-mode

(use-package multi-web-mode
  :ensure t
  :commands (multi-web-global-mode))


;;;; multiple-cursors

(use-package multiple-cursors
  :ensure t
  :commands (multiple-cursors-mode mc/edit-lines mc/mark-next-like-this
                                   mc/mark-next-word-like-this
                                   mc/mark-next-symbol-like-this
                                   mc/mark-previous-like-this
                                   mc/mark-previous-word-like-this
                                   mc/mark-previous-symbol-like-this
                                   mc/mark-more-like-this-extended
                                   mc/add-cursor-on-click
                                   mc/mark-all-like-this
                                   mc/mark-all-words-like-this
                                   mc/mark-all-symbols-like-this
                                   mc/mark-all-in-region
                                   mc/mark-all-like-this-in-defun
                                   mc/mark-all-words-like-this-in-defun
                                   mc/mark-all-symbols-like-this-in-defun
                                   mc/mark-all-like-this-dwim)
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)
         ;; ("<mouse-2>" . mc/add-cursor-on-click)
         )
  :init
  (progn
    (setq mc/list-file (expand-file-name
                        "mc-lists.el" user-lisp-directory))
    (define-key region-bindings-mode-map "a" 'mc/mark-all-like-this)
    (define-key region-bindings-mode-map "p" 'mc/mark-previous-like-this)
    (define-key region-bindings-mode-map "n" 'mc/mark-next-like-this)
    (define-key region-bindings-mode-map "m" 'mc/mark-more-like-this-extended))
  :config
  (progn
    (bind-key "C-s" 'phi-search mc/keymap)
    (bind-key "C-r" 'phi-search-backward mc/keymap)))


;;;; nginx-mode

(use-package nginx-mode
  :ensure t
  :commands nginx-mode
  :mode (("nginx/.*\\.conf\\'" . nginx-mode)
         ("nginx/.*_params\\'" . nginx-mode)
         ("nginx/sites-\\(available\\|enabled\\)/" . nginx-mode)))


;;;; notifications

(use-package notifications
  :commands notifications-notify)


;;;; nxml-mode

(use-package nxml-mode
  :defer t
  :config
  (progn
    (unbind-key "M-h" nxml-mode-map)))


;;;; openwith

(use-package openwith
  :ensure t
  :commands (openwith-mode openwith-open-unix open-terminal)
  :init
  (progn
    (setq
     openwith-associations
     '(("\\.pdf\\'" "evince" (file))
       ("\\.mp3\\'" "mpc" ("add" file))
       ("\\.ppt\\'" "libreoffice" (file))
       ("\\.pptx\\'" "libreoffice" (file))
       ("\\.\\(?:mpe?g\\|mkv\\|avi\\|wmv\\)\\'" "mplayer" ("-idx" file))
       ;; ("\\.\\(?:jp?g\\|png\\)\\'" "display" (file))
       ))
    (when (and
           (not noninteractive)
           )
      (openwith-mode)))
  :config
  (progn
    (defun open-terminal ()
      "Opens an terminal in current directory"
      (interactive)
      (openwith-open-unix "term" nil))))


;;;; osc

(use-package osc
  :defer
  :ensure t)


;;;; outline

(use-package outline
  :defer
  :diminish ((outline-minor-mode . "")))


;;;; outline-minor-faces

(use-package outline-minor-faces
  :ensure t
  :after outline
  :config
  (progn
    (add-hook 'outline-minor-mode-hook
              #'outline-minor-faces-mode)))


;;;; outshine

(use-package outshine
  :ensure t
  :commands (outshine-mode))


;;;; package-utils

(use-package package-utils
  :ensure t
  :commands package-utils-upgrade-all-and-recompile)


;;;; pandoc-mode

(use-package pandoc-mode
  :ensure t
  :commands (turn-on-pandoc
             pandoc-load-default-settings))


;;;; peep-dired

(use-package peep-dired
  :ensure t
  :commands peep-dired)


;;;; perspective

(use-package perspective
  :ensure t
  :commands persp-mode)


;;;; phi-search

(use-package phi-search
  :ensure t
  :commands (phi-search phi-search-backwards))


;;;; phi-search-dired

(use-package phi-search-dired
  :ensure t
  :commands phi-search-dired)


;;;; php-mode

(use-package php-mode
  :ensure t
  :commands php-mode
  :mode ("\\.php\\'" . php-mode))


;;;; pip-requirements

(use-package pip-requirements
  :ensure t
  :mode (("\\.pip\\'" . pip-requirements-mode)
         ("requirements\\.txt\\'" . pip-requirements-mode)))


;;;; pipenv

(use-package pipenv
  :ensure t
  :commands (pipenv-mode
             pipenv-activate
             pipenv-run))


;;;; plantuml-mode

(progn
  (setq plantuml-jar-path
        (or
         (file-exists-p* "~/.opt/plantuml.jar")
         (file-exists-p* "/usr/share/plantuml/plantuml.jar"))
        org-plantuml-jar-path plantuml-jar-path))

(use-package plantuml-mode
  :ensure t
  :commands (plantuml-mode)
  :mode (("\\.plu\\'" . plantuml-mode))
  :config
  (progn
    (setq
     plantuml-server-url nil
     plantuml-default-exec-mode 'jar
     )
    (require 'cl-lib))
  :if plantuml-jar-path)


;;;; platformio-mode

(use-package platformio-mode
  :ensure t
  :commands (platformio-mode
             platformio-build
             platformio-upload)
  :diminish platformio-mode
  :init
  (progn
    (setq platformio-mode-silent t)
    (add-hook 'arduino-mode-hook 'platformio-mode)))


;;;; poetry

(use-package poetry
  :ensure t
  :commands (poetry-venv-workon
             poetry-venv-deactivate
             poetry-venv-toggle))


;;;; point-undo

(use-package point-undo
  :if (and (not noninteractive) )
  :commands (point-undo point-redo)
  :init
  (progn
    (define-key region-bindings-mode-map "u" 'point-undo)
    (smartrep-define-key
        global-map
        "C-c"
      '(("u" . point-undo)
        ("r" . point-redo)))
    (require 'point-undo)))


;;;; polymode

(use-package polymode
  :ensure t
  :defer t)


;;;;; poly-markdown

(use-package poly-markdown
  :ensure t
  :mode (("\\.markdown\\'" . poly-gfm-mode)
          ("\\.md\\'" . poly-gfm-mode)
          ("\\.mdwn\\'" . poly-gfm-mode)
          ("\\.mkd\\'" . poly-gfm-mode)
          ("\\.mkdown\\'" . poly-gfm-mode)
          ("\\.mdtext\\'" . poly-gfm-mode)))


;;;; popup-switcher

(use-package popup-switcher
  :ensure t
  :commands (psw-switch-buffer))


;;;; popwin

(use-package popwin
  :ensure t
  :if (not noninteractive)
  :defer 0.5
  :commands (popwin-mode popwin:display-buffer popwin:popup-buffer
                         popwin:popup-buffer-tail popwin:display-last-buffer
                         popwin:find-file popwin:find-file-tail
                         popwin:select-popup-window popwin:close-popup-window
                         popwin:messages)
  :bind (("M-o p c" . popwin:close-popup-window)
         ("M-o p p" . popwin:display-last-buffer)
         ("C-x b m" . popwin:messages)
         ("M-o p s" . popwin:select-popup-window)
         ("M-o p S" . popwin:stick-popup-window)
         ("M-o p f" . popwin:find-file-tail))
  :config
  (progn
    ;; (global-set-key (kbd "C-z") popwin:keymap)
    (--each
        '(
          ("*identify*" :noselect t)
          ("*Help*" :stick t)
          (help-mode :noselect t)
          (direx:direx-mode :position left :width .35 :dedicated t)
          ;; ("*Messages*" :height .40 :tail t :stick t)
          (deadgrep-mode :height .40 :stick t)
          ;; ("*pt-search*" :height .40 :stick t)
          ("*go-traceback*" :height .40 :stick t)
          ("*xref*" :height .30)
          ("*Keys*" :height .85)
          ("*Pp Macroexpand Output*" :noselect t)
          "*Personal Keybindings*"
          ;; (flycheck-error-list-mode :stick t)
          ("*Org Select*" :position right :width 79 :noselect t)
          (" *Agenda Commands*" :position right :width 79)
          ("^\\*[Hh]elm.*\\*$" :regexp t :height 0.85)
          ;; ("*magit-commit*" :noselect t :height 0.40)
          ;; ("*magit-diff*" :noselect t :height 0.40)
          ;; ("*magit-edit-log*" :noselect t :height 0.25)
          )
      (push it popwin:special-display-config))
    (popwin-mode)))


;;;; prettier-js

(use-package prettier-js
  :ensure t
  :commands (prettier-js))


;;;; prog-mode

(use-package prog-mode
  :defer
  :init
  (progn
    (defun my-prettify-symbols-compile-patterns (patterns)
      (let ((pretty-patterns))
        (cl-loop for (glyph . pairs) in patterns do
              (cl-loop for (regexp . major-modes) in pairs do
                    (cl-loop for mode in major-modes do
                          (let* ((mode (intern (concat (symbol-name mode)
                                                       "-mode")))
                                 (assoc-pair (assoc mode pretty-patterns))

                                 (entry (cons regexp glyph)))
                            (if assoc-pair
                                (push entry (cdr assoc-pair))
                              (push (cons mode (list entry))
                                    pretty-patterns))))))
        pretty-patterns))

    (defvar my-prettify-symbols-interaction-mode-alist
      '((inferior-scheme-mode . scheme-mode)
        (lisp-interaction-mode . emacs-lisp-mode)
        (inferior-lisp-mode . lisp-mode)
        (inferior-ess-mode . ess-mode)
        (inf-haskell-mode . haskell-mode)
        (tuareg-interactive-mode . tuareg-mode)
        (inferior-python-mode . python-mode)
        (inferior-octave-mode . octave-mode)
        (inferior-ruby-mode . ruby-mode))
      "Alist mapping from inferior process interaction modes to their
  corresponding script editing modes.")


    (defvar my-prettify-symbols-patterns nil)
    (setq my-prettify-symbols-patterns
          (let* ((lispy '(scheme emacs-lisp lisp clojure))
                 (mley '(tuareg haskell sml coq))
                 (c-like '(c c++ perl sh python java ess ruby js js2 coffee go))
                 (alljs '(js js2))
                 (all `(,@lispy ,@mley ,@c-like octave latex)))
            (my-prettify-symbols-compile-patterns
             `(
               (?¬ (,(rx "not") python ,@lispy haskell coffee)
                   (,(rx "!") c c++ java ,@alljs go)
                   (,(rx "~~") coq)
                   (,(rx "\\neg") latex))
               (?≠ (,(rx "!=") ,@c-like scheme octave coq)
                   (,(rx "<>") tuareg octave)
                   (,(rx "~=") octave)
                   (,(rx "/=") haskell emacs-lisp)
                   (,(rx "\\neq") latex)
                   (,(rx "not=") clojure))
               (?≺ (,(rx "<") ,@all)
                   (,(rx "\\prec") latex))
               (?≻ (,(rx "\\succ") latex))
               (?≼ (,(rx "<=") ,@all)
                   (,(rx "\\leq") latex))
               (?≽ (,(rx ">=") ,@all)
                   (,(rx "\\geq") latex))
               (?⁑ (,(rx "**") python))
               (?∧ (,(rx "and") emacs-lisp lisp clojure python coffee)
                   (,(rx "&&") haskell c c++ java perl coq ,@alljs go)
                   (,(rx "\\wedge") latex)
                   (,(rx "\\land") latex))
               (?∨ (,(rx "or") emacs-lisp lisp clojure python coffee)
                   (,(rx "||") haskell c c++ java perl coq ,@alljs go)
                   (,(rx "\\vee") latex)
                   (,(rx "\\lor") latex))
               (?≡ (,(rx "==") ,@all)
                   (,(rx "=") clojure)
                   (,(rx "\\equiv") latex))
               (?⟵ ;;(,(rx "<-") ,@mley ess)
                (,(rx "\\leftarrow") latex))
               (?⟶ ;;(,(rx "->") ,@mley ess c c++ perl coffee)
                (,(rx "\\rightarrow") latex))
               (?↑ (,(rx "\\^") tuareg)
                   (,(rx "^+") coq))
               (?⟹ ;; (,(rx "=>") sml perl ruby haskell coq coffee)
                (,(rx "\\Rightarrow") latex))
               (?⟷ (,(rx "<->") coq)
                   (,(rx "\leftrightarrow") latex))
               (?↣ (,(rx ">->") coq))
               (?↦ (,(rx "\\mapsto") latex))
               (?⌀ (,(rx "nil") emacs-lisp clojure ruby go)
                   (,(rx "null") scheme java ,@alljs coffee)
                   (,(rx "NULL") c c++)
                   (,(rx "None") python)
                   (,(rx "set0") coq)
                   (,(rx "()") ,@mley)
                   (,(rx "\\emptyset") latex)
                   (,(rx "\\varnothing") latex))
               ;; (?… (,(rx "...") ,@all)
               ;;     (,(rx "..") haskell)
               ;;     (,(rx "\\ldots") latex))
               (?⊲ (,(rx "<|") coq))
               ;;(?√ (,(rx "sqrt") ,@all))
               (?∑ ;;(,(rx "sum") python)
                (,(rx "\\sum") coq latex)
                (,(rx "\\Sigma") latex)
                (,(rx "reduce \+") clojure))
               (?∪ (,(rx ":|:") coq))
               (?∩ (,(rx ":&:") coq))
               (?∁ (,(rx "~:") coq))
               ;; (?α (,(rx "alpha") ,@all)
               ;;     (,(rx "'a") ,@mley)
               ;;     (,(rx "\\alpha") latex))
               ;; (?β (,(rx "beta") ,@all)
               ;;     (,(rx "'b") ,@mley)
               ;;     (,(rx "\\beta") latex))
               ;; (?γ (,(rx "gamma") ,@all)
               ;;     (,(rx "'c") ,@mley)
               ;;     (,(rx "\\gamma") latex))
               ;; (?Δ (,(rx "delta") ,@all)
               ;;     (,(rx "'d") ,@mley)
               ;;     (,(rx "\\Delta") latex))
               ;; (?ε (,(rx "epsilon") ,@all)
               ;;     (,(rx "\\epsilon") latex))
               ;; (?ι (,(rx "iota") ,@all)
               ;;     (,(rx "\\iota") latex))
               ;; (?θ (,(rx "theta") ,@all)
               ;;     (,(rx "\\theta") latex))
               ;; (?ρ (,(rx "rho") ,@all)
               ;;     (,(rx "\\rho") latex))
               ;; (?σ ;;(,(rx "sigma") ,@all)
               ;; (,(rx "filter") python clojure)
               ;; (,(rx "select") clojure))
               ;; (?μ (,(rx "mu") ,@all))
               (?λ (,(rx "lambda") ,@all)
                   (,(rx "fn") sml)
                   (,(rx "fun") tuareg)
                   (,(rx "\\") haskell)
                   (,(rx "\\lambda") latex)
                   )
               (?π (,(rx "\\pi") latex))
               (?Π ;;(,(rx "Pi") @all)
                (,(rx "\\prod") latex)
                (,(rx "\\Pi") latex))
               (?ω ;;(,(rx "omega") @all)
                (,(rx "\\omega") latex))
               (?Φ ;;(,(rx "Phi") @all)
                (,(rx "\\Phi") latex))
               (?Ω ;;(,(rx "Ohm") @all)
                (,(rx "\\ohm") latex)
                (,(rx "\\Omega") latex))
               (?℧ ;;(,(rx "Mho") @all)
                (,(rx "\\mho") latex))
               (?φ ;;(,(rx "phi") ,@all)
                (,(rx "\\varphi") latex))
               (?η ;;(,(rx "eta") ,@all)
                (,(rx "\\eta") latex))

               ;;(?∞ (,(rx "HUGE_VAL") c c++))
               ;;(?∎ (,(rx "Qed.") coq))

               ;;(?∗ (,(rx "all" (? "()")) python))
               ;;(?⊢ (,(rx "assert") python))
               ;;(?≍ (,(rx "is") python))
               ;;(?𝝈 (,(rx "filter_by") python))
               ;; (?ℵ (,(rx "count") python clojure))
               ;; (?⇓(,(rx "order_by") python))
               ;; (?⤚ (,(rx "group_by") python))
               ;; (?⟶ (,(rx "def") python))

               (?⊤ (,(rx "True") python)
                   (,(rx "true") go ,@alljs)
                   )
               (?⊥ (,(rx "False") python)
                   (,(rx "false") go ,@alljs)
                   )

               (?⋂ (,(rx "intersect") python)
                   (,(rx "\\bigcap") coq)
                   (,(rx "\\cap") latex)
                   (,(rx "intersection") clojure))
               (?∏ (,(rx "\\prod") coq))
               (?⋃ (,(rx "union") python clojure)
                   (,(rx "\\bigcup") coq)
                   (,(rx "\\cup") latex))
               (?⊎ (,(rx "\\uplus") latex))
               (?ℕ (,(rx "nat") coq))
               (?∣ (,(rx "%|") coq))


               ;;(?∈ (,(rx "in") python coffee))
               (?∉ ;;(,(rx "not in") python)
                (,(rx "\\notin") coq latex))
               ;;(?⊼ (,(rx "and not") python coffee))
               ;;(?⊽ (,(rx "or not") python coffee))
               (?⊻ (,(rx "(\\+)") coq))

               (?∀ ;;(,(rx "for") python coffee)
                ;;(,(rx "forall") haskell coq)
                (,(rx "\\forall") latex))
               ;;(?∄ (,(rx "not any") python))
               ;; (?∃ (,(rx "any") python)
               ;;     (,(rx "exists") coq)
               ;;     (,(rx "\\exists") latex)
               ;;     (,(rx "some") clojure))
               (?⊂ (,(rx "\\proper") coq)
                   (,(rx "\\subset") latex))
               (?⊆ (,(rx "\\subset") coq)
                   (,(rx "\\subseteq") latex))
               (?∖ (,(rx ":\\:") coq)
                   (,(rx "\\setminus") latex)
                   (,(rx "difference") clojure))
               (?⋊ (,(rx "><|") coq))

               (?× (,(rx "\\times") latex))
               (?〈 (,(rx "\\langle") latex))
                 (?〉 (,(rx "\\rangle") latex))))))

    (defun my-prettify-symbols-hook-fn (&optional mode)
      (let* ((mode (or mode major-mode))
             (kwds (cdr-safe
                    (or (assoc mode my-prettify-symbols-patterns)
                        (assoc (cdr-safe
                                (assoc mode  my-prettify-symbols-interaction-mode-alist))
                               my-prettify-symbols-patterns)))))
        (mapc #'(lambda (v)
                  (push v prettify-symbols-alist))
              kwds))
      (prettify-symbols-mode 1))
    (unless noninteractive
      (add-hook 'prog-mode-hook 'my-prettify-symbols-hook-fn)))

  :config
  (progn
    (global-prettify-symbols-mode)))


;;;; projectile

(use-package projectile
  :ensure t
  :defer 0.2
  :commands (projectile-mode
             projectile-project-p
             projectile-sort-by-recentf-first
             projectile-sort-by-recently-active-first)
  :bind (("C-x f <SPC>" . projectile-find-file)
         ("C-x f P" . projectile-find-file-ignored)
         ("C-x d <SPC>" . projectile-find-dir)
         ("C-x d p" . projectile-switch-project)
         ("C-x b <SPC>" . projectile-switch-to-buffer))
  :diminish ""
  :init
  (progn
    (setq projectile-project-root-files-child-of
          '(
            "/lib/python[^/]*/\\(site\\|dist\\)-packages/?$"
            ;; "~/\.virtualenvs/[^/]+/\\(local/\\)?lib/python[^/]*/site-packages/?$"
            ;; "~/\.virtualenvs-cmd/[^/]+/\\(local/\\)?lib/python[^/]*/site-packages/?$"
            ;; "~/\.local/share/virtualenvs/[^/]+/\\(local/\\)?lib/python[^/]*/site-packages/?$"
            "~/\.opt/[^/]+/?$"
            "~/\.virtualenvs/[^/]+/?$"
            "~/\.virtualenvs-cmd/[^/]+/?$"
            "~/\.local/share/virtualenvs/[^/]+/?$"
            "/var/log/?$"))

    (defun projectile-root-child-of (dir &optional list)
      (projectile-locate-dominating-file
       dir
       (lambda (dir)
         (--first
          (if (and
               (s-equals? (file-remote-p it) (file-remote-p dir))
               (string-match-p (expand-file-name it) (expand-file-name dir)))
              dir)
          (or list projectile-project-root-files-child-of (list))))))

    (setq
     projectile-sort-order 'recently-active
     projectile-require-project-root t
     projectile-switch-project-action 'projectile-dired
     projectile-enable-caching nil
     projectile-verbose nil
     projectile-known-projects-file (expand-file-name
                                     "projectile-bookmarks.eld"
                                     user-data-directory)
     projectile-frecency-file (expand-file-name
                                     "projectile-frecency.eld"
                                     user-data-directory)
     projectile-cache-file (expand-file-name
                            "projectile.cache" user-cache-directory)
     projectile-file-exists-local-cache-expire nil
     projectile-file-exists-remote-cache-expire (* 15 60)
     projectile-project-root-functions
     '(projectile-root-bottom-up
       projectile-root-top-down
       projectile-root-top-down-recurring
       projectile-root-child-of)
     projectile-project-root-files
     '(
       ;; "rebar.config"
       ;; "project.clj"
       ;; "build.boot"
       ;; "deps.edn"
       "go.mod"
       "SConstruct"
       "pom.xml"
       ;; "build.sbt"
       "gradlew"
       "build.gradle"
       ;; ".ensime"
       "Gemfile"
       ;; "requirements.txt"
       "setup.py"
       "tox.ini"
       "pyproject.toml"
       ;; "composer.json"
       "Cargo.toml"
       ;; "mix.exs"
       ;; "stack.yaml"
       ;; "info.rkt"
       ;; "DESCRIPTION"
       ;; "TAGS"
       ;; "GTAGS"
       "configure.in"
       "configure.ac"
       "cscope.out"
       )
     )
    (bind-key "A" 'projectile-pt-file-pattern region-bindings-mode-map)

    (defadvice projectile-mode (before maybe-use-cache activate)
      (when
          (--any? (and it (file-remote-p it))
                  (list
                   (buffer-file-name)
                   list-buffers-directory
                   default-directory))
        (setq-local projectile-enable-caching t)))


;;;;; persp-projectile

    (use-package persp-projectile
      :ensure t
      :commands projectile-persp-switch-project)

    (defun projectile-find-file-ignored ()
      "Projectile find file without ignore."
      (interactive)
      (let ((projectile-git-command "git ls-files -zco"))
        (call-interactively 'projectile-find-file))))
  :config
  (progn
    (add-to-list 'projectile-globally-ignored-directories "vendor")
    (projectile-mode)))

(defalias 'project-root-function 'projectile-project-root)


;;;; protobuf-mode

(use-package protobuf-mode
  :ensure t
  :mode (("\\.proto\\'" . protobuf-mode)))


;;;; python

(use-package python
  :commands python-mode
  :mode ("\\.py\\'" . python-mode)
  :interpreter  (("python2" . python-mode)
                 ("python3" . python-mode)
                 ("default-python" . python-mode))
  :init
  (progn
    ;; (setq python-shell-interpreter "ipython")
    (setq python-indent-guess-indent-offset-verbose nil)
    (defun my-python-mode-hook ()
      ;; (setq-local idle-update-delay 2)
      )
    (add-hook 'python-mode-hook 'my-python-mode-hook)

    (rename-modeline "python" python-mode "py"))
  :config
  (progn

    (use-package importmagic
      :ensure t
      :init
      (progn
        (setq importmagic-be-quiet t))
      :config
      (progn
        (add-hook 'python-mode-hook 'importmagic-mode)
        (add-hook 'python-ts-mode-hook 'importmagic-mode)
        ))

    (use-package py-autopep8
      :ensure t
      :commands (py-autopep8-buffer))


    (use-package py-isort
      :ensure t
      :commands (py-isort-buffer))


    (use-package py-black
      :commands (py-black-buffer))

    (use-package py-ruff-fmt
      :commands (py-ruff-fmt-buffer))

    (defun my-python-format-buffer ()
        "format buffer with black or ruff depending on whats installed"
        (interactive)
        (cond
         ((executable-find "black" ) (py-black-buffer))
         ((executable-find "ruff" ) (py-ruff-fmt-buffer))))

    (defun python-cccc ()
      (interactive)
      (silent-save-some-buffers)
      (my-python-format-buffer)
      ;; (py-isort-buffer)
      )

    (bind-key "C-c C-c" 'python-cccc python-mode-map)
    (bind-key "C-c C-c" 'python-cccc python-ts-mode-map)

    (smartrep-define-key
        python-mode-map
        "C-c"
      '((">"   . python-indent-shift-right)
        ("<"   . python-indent-shift-left)))

    (unbind-key "C-c C-p" python-mode-map)
    (unbind-key "C-c C-j" python-mode-map)
    (unbind-key "C-c C-p" python-ts-mode-map)
    (unbind-key "C-c C-j" python-ts-mode-map)))


;;;; python-django

(use-package python-django
  :ensure t
  :commands (python-django-open-project)
  :bind (("C-x j" . python-django-open-project))
  :init
  (progn
    (setq
     python-django-ui-ignored-dirs
     '("." ".." ".bzr" ".cdv" "~.dep" "~.dot" "~.nib" "~.plst" ".git" ".hg"
       ".pc" ".svn" "_MTN" "blib" "CVS" "RCS" "SCCS" "_darcs" "_sgbak"
       "autom4te.cache" "cover_db" "_build" ".ropeproject")
     python-django-ui-allowed-extensions
     '("css" "gif" "htm" "html" "jpg" "js" "json" "mo" "png" "po" "py"
       "txt" "xml" "yaml" "scss" "less"))))


;;;; python-environment

(use-package python-environment
  :ensure t
  :defer
  :init
  (progn
    (setq python-environment-directory "~/.virtualenvs/"
          python-environment-default-root-name "emacs-default")))


;;;; quickrun

(use-package quickrun
  :ensure t
  :if (not noninteractive)
  :commands (quickrun
             quickrun-region
             quickrun-with-arg
             quickrun-with-input-file
             quickrun-compile-only)
  :bind (("M-o q" . quickrun)))


;;;; rainbow-blocks

(use-package rainbow-blocks
  :ensure t
  :commands rainbow-blocks-mode)


;;;; rainbow-delimiters

(use-package rainbow-delimiters
  :ensure t
  :commands rainbow-delimiters-mode
  :if (not noninteractive)
  :bind (("M-o m r" . rainbow-delimiters-mode))
  :init
  (progn
    ;; (hook-into-modes #'rainbow-delimiters-mode
    ;;                  '(clojure-mode-hook
    ;;                    emacs-lisp-mode-hook
    ;;                    haskell-mode-hook))
    ))


;;;; rainbow-identifiers

(use-package rainbow-identifiers
  :ensure t
  :commands rainbow-identifiers-mode
  :init
  (progn
    (defun rainbow-identifiers-turn-on-maybe ()
      (when
          (or
           (hardhat-buffer-included-p (current-buffer)))
        (rainbow-identifiers-mode)))
    ;; (add-hook 'prog-mode-hook 'rainbow-identifiers-turn-on-maybe)
    ))


;;;; rainbow-mode

(use-package rainbow-mode
  :ensure t
  :if (and
       (not (not window-system))
       (not noninteractive))
  :commands rainbow-mode
  :init
  (progn
    (hook-into-modes #'rainbow-mode
                     '(css-mode-hook)))
  :diminish ((rainbow-mode . "rb")))


;;;; realgud

(use-package realgud
  :ensure realgud
  :defer t
  :commands (realgud:pdb realgud:gdb)
  :init
  (progn
    (setq pdb-command-name "python -m pdb")))


;;;; recentf

(use-package recentf
  :if (and (not noninteractive) )
  :bind (("C-x f R" . find-recent-file))
  :defer 6
  :init
  (progn
    (setq
     recentf-save-file (expand-file-name
                        (workspace-prefix-file-name "recentf" ".el")
                        user-data-directory)
     recentf-max-saved-items 5000
     recentf-auto-cleanup 300
     recentf-keep '(file-readable-p)
     recentf-exclude '(file-remote-p
                       "ido.last"
                       "org-clock-save.el"
                       "[A-Z_]*_EDITMSG"
                       ".*-autoloads\\.el\\'"
                       "[/\\]\\.git/"))

    (defun find-recent-file ()
      "Use `completing-read' to \\[find-file] a recent file"
      (interactive)
      (recentf-mode 1)
      (if (find-file (completing-read "Find recent file: " recentf-list))
          (message "Opening file...")
        (message "Aborting")))))


;;;; regex-tool

(use-package regex-tool
  :ensure t
  :commands (regex-tool))


;;;; region-bindings-mode

(use-package region-bindings-mode
  :if (and
       (not noninteractive)
       )
  :ensure t
  :commands (region-bindings-mode-enable)
  :diminish "▒▒"
  :init
  (progn
    (setq region-bindings-mode-disabled-modes
          '(
            dired-efap-mode
            magit-status-mode
            term-mode
            transmission-mode
            transmission-files-mode
            ))
    (region-bindings-mode-enable))
  :config
  (progn

    (defun my-mark-word (N)
      (interactive "p")
      ;; (when (< (point) (mark))
      ;;   (exchange-point-and-mark))
      (forward-word N))

    (defun my-mark-word-backward (N)
      (interactive "p")
      ;; (when (< (mark) (point))
      ;;   (exchange-point-and-mark))
      (backward-word N))

    (defun my-mark-char (N)
      (interactive "p")
      (forward-char N))

    (defun my-mark-char-backward (N)
      (interactive "p")
      (backward-char N))

    (define-key region-bindings-mode-map "F" 'my-mark-char)
    (define-key region-bindings-mode-map "B" 'my-mark-char-backward)
    (define-key region-bindings-mode-map "f" 'my-mark-word)
    (define-key region-bindings-mode-map "b" 'my-mark-word-backward)
    (define-key region-bindings-mode-map "y" 'yank)
    (define-key region-bindings-mode-map "k" 'copy-region-as-kill)
    (define-key region-bindings-mode-map "w" 'kill-region)
    (define-key region-bindings-mode-map "x" 'exchange-dot-and-mark)
    (define-key region-bindings-mode-map "d" 'er/mark-defun)
    (define-key region-bindings-mode-map "g" 'keyboard-quit)
    (define-key region-bindings-mode-map "s" search-map)))


;;;; request

(use-package request
  :ensure t
  :defer
  :init
  (progn
    (setq request-storage-directory (expand-file-name
                                     "request/" user-data-directory))))


;;;; restclient

(use-package restclient
  :ensure t
  :commands restclient-mode)


;;;; revbufs

(use-package revbufs
  :commands revbufs
  ;; :bind (("M-o r" . revbufs))
  )


;;;; rg

(use-package rg
  :ensure t
  :commands (rg
             rg-dwim
             rg-project
             rg-literal
             rg-dwim-project-dir)
  :init
  (progn
    (setq rg-custom-type-aliases '(("gyp" .    "*.gyp *.gypi"))))
  :config
  (progn
    (rg-define-toggle "-g !tests/ -g !tests.py -g !test_*.py -g !testdata/ -g !*_test.go -g !conftest.py" "T")
    (rg-define-toggle "--sort-files" "S" t)

    (defun my-rg-project ()
      "rg-project and then open goto hydra directly"
      (interactive)
      (call-interactively #'rg-project)
      (call-interactively #'hydra-goto/body)))

  :bind (("M-o a" . my-rg-project)
         ("M-o A" . rg)
         :map rg-mode-map
         ("j" . next-error-no-select)
         ("k" . previous-error-no-select)))


;;;; rings

(use-package rings
  :ensure t
  :if (not noninteractive)
  :init
  (progn
    (setq rings-protect-buffers-in-rings nil)
    (defun my-rings-setup ()
      ;; f1
      (global-set-key (kbd "<f1>") (lambda nil (interactive) (rings-cycle 1)))
      (global-set-key (kbd "S-<f1>") (lambda nil (interactive) (rings-toggle-buffer 1)))
      (global-set-key (kbd "C-<f1>") (lambda nil (interactive) (rings-remove-buffer 1)))

      ;; f2
      (global-set-key (kbd "<f2>") (lambda nil (interactive) (rings-cycle 2)))
      (global-set-key (kbd "S-<f2>") (lambda nil (interactive) (rings-toggle-buffer 2)))
      (global-set-key (kbd "C-<f2>") (lambda nil (interactive) (rings-remove-buffer 2)))

      ;; f3
      (global-set-key (kbd "<f3>") (lambda nil (interactive) (rings-cycle 3)))
      (global-set-key (kbd "S-<f3>") (lambda nil (interactive) (rings-toggle-buffer 3)))
      (global-set-key (kbd "C-<f3>") (lambda nil (interactive) (rings-remove-buffer 3)))

      ;; f4
      (global-set-key (kbd "<f4>") (lambda nil (interactive) (rings-cycle 4)))
      (global-set-key (kbd "S-<f4>") (lambda nil (interactive) (rings-toggle-buffer 4)))
      (global-set-key (kbd "C-<f4>") (lambda nil (interactive) (rings-remove-buffer 4)))
      )
    (add-hook 'after-init-hook 'my-rings-setup t)))


;;;; rotate

(use-package rotate
  :ensure t
  :commands rotate-layout
  :bind (("M-o M-c" . rotate-layout)))


;;;; ruby-mode

(use-package ruby-mode
  :commands ruby-mode
  :mode (("\\.rake\\'" . ruby-mode)
         ("Rakefile\\'" . ruby-mode)
         ("\\.gemspec\\'" . ruby-mode)
         ("\\.ru\\'" . ruby-mode)
         ("Gemfile\\'" . ruby-mode)
         ("Guardfile\\'" . ruby-mode)))


;;;; rust-mode

(use-package rust-mode
  :ensure t
  :mode (("\\.rs\\'" . rust-mode)))


;;;; savehist

(use-package savehist
  :if (and (not noninteractive) )
  :init
  (progn
    (setq
     savehist-file (expand-file-name
                    (workspace-prefix-file-name "savehist" ".el")
                    user-data-directory)
     savehist-additional-variables '(search ring regexp-search-ring
                                            projectile-pt-file-pattern-history
                                            projectile-pt-file-pattern-search-history)
     savehist-autosave-interval 60))
  :config
  (progn
    (savehist-mode 1)))


;;;; saveplace

(use-package saveplace
  :if (and (not noninteractive) )
  :init
  (progn
    (setq save-place-forget-unreadable-files nil))
  :config
  (progn
    (setq save-place-file (expand-file-name
                           (workspace-prefix-file-name "saveplace" ".el")
                           user-data-directory))
    (save-place-mode 1)))


;;;; scratch

(use-package scratch
  :ensure t
  :commands (scratch))


;;;; scroll-restore (?)

(use-package scroll-restore
  :ensure t
  :if (not noninteractive)
  :commands scroll-restore-mode
  :config
  (progn
    (setq
     scroll-restore-recenter nil
     scroll-restore-commands '(handle-select-window
                               handle-switch-frame
                               mwheel-scroll
                               scroll-bar-drag
                               scroll-bar-scroll-down
                               scroll-bar-scroll-up
                               scroll-bar-toolkit-scroll
                               scroll-down scroll-down-command
                               scroll-down-command-flash
                               scroll-other-window
                               scroll-other-window-down
                               scroll-up scroll-up-command
                               my-scroll-down-command
                               my-scroll-up-command
                               ))
    (scroll-restore-mode 1)))


;;;; scss-mode

(use-package scss-mode
  :ensure t
  :commands scss-mode
  :mode "\\.scss\\'"
  :init
  (progn
    (setq scss-compile-at-save nil))
  :config
  (progn
    (unbind-key "C-c C-c" scss-mode-map)))


;;;; server

(use-package server
  :commands server-start-maybe
  :init
  (progn
    (add-hook 'after-init-hook
              'server-start-maybe))
  :config
  (progn
    (and
     (equal server-name "server")
     workspace-prefix-startup
     (setq server-name workspace-prefix-startup))
    (defun server-start-maybe ()
      (and (not (server-running-p))
           (server-start nil t)))))


;;;; sgml-mode

(use-package sgml-mode
  :commands html-mode
  :init
  (progn
    (rename-modeline "sgml-mode" html-mode "html"))
  ;; :mode (("\\.html\\'" . html-mode)
  ;;        ("\\.rhtml\\'" . html-mode)
  ;;        ("\\.mustache\\'" . html-mode))
  )


;;;; sh-script

(use-package sh-script
  :defer
  :init
  (progn
    (setq ;; sh-mode.el
     sh-basic-offset 2)))


;;;; shift-text

(use-package shift-text
  :ensure t
  :bind (("S-<down>" . shift-text-down)
         ("S-<up>" . shift-text-up)
         ;; ("S-<left>" . shift-text-left)
         ;; ("S-<right>" . shift-text-right)
         )
  :config
  (progn
    (setq st-indent-step
          (lambda ()
            (cond
             ((eq major-mode 'js-mode)
              js-indent-level)
             ((eq major-mode 'css-mode)
              css-indent-offset)
             ((memq major-mode
                    '(emacs-lisp-mode
                      lisp-mode
                      lisp-interaction-mode
                      scheme-mode))
              1)
             (t tab-width))))))


;;;; shr

(use-package shr
  :defer
  :init
  (progn
    (setq browse-url-secondary-browser-function 'browse-url-generic)))


;;;; simple-httpd

(use-package simple-httpd
  :ensure t
  :commands httpd-start
  :init
  (progn
    (setq
     httpd-servlets t
     httpd-port 7348))
  :config
  (progn
    (use-package skewer-mode)))


;;;; simpleclip

(use-package simpleclip
  :ensure t
  :commands (simpleclip-mode simpleclip-set-contents
                             simpleclip-get-contents simpleclip-cut
                             simpleclip-copy simpleclip-paste)
  :init
  (progn
    (setq
     simpleclip-cut-keystrokes
     '("s-x" "H-x" "S-<delete>")
     simpleclip-copy-keystrokes
     '("s-c" "H-c" "C-<insert>" "C-<insertchar>")
     simpleclip-paste-keystrokes
     '("s-v" "H-v" "S-<insert>" "S-<insertchar>"))
    (when window-system
      (define-key region-bindings-mode-map "c" 'simpleclip-copy)
      (simpleclip-mode 1)))
  :config
  (progn
    (defadvice simpleclip-copy (after clipboard activate)
      (if (use-region-p)
          (with-no-warnings
            (kill-ring-save (region-beginning) (region-end) t))
        (kill-ring-save (region-beginning) (region-end))))))


;;;; simplezen

(use-package simplezen
  :ensure t
  :commands (simplezen-expand))


;;;; skewer-mode

(use-package skewer-mode
  :ensure t
  :commands (skewer-mode skewer-reload-page)
  :bind (("C-x g" . skewer-reload-page))
  :init
  (progn
    (use-package skewer-repl
      :commands (skewer-repl)))
  :config
  (progn


;;;;; skewer-html

    (use-package skewer-html)


;;;;; skewer-css

    (use-package skewer-css)

    (defun skewer-reload-page ()
      "Reloads browser."
      (interactive)
      (silent-save-some-buffers)
      (skewer-eval "window.location = window.location"))
    (defun skewer-scroll-down ()
      "Scroll down"
      (interactive)
      (skewer-eval "window.scrollBy(0,200);"))
    (defun skewer-scroll-up ()
      "Scroll down"
      (interactive)
      (skewer-eval "window.scrollBy(0,-200);"))
    (defun skewer-scroll-left ()
      "Scroll down"
      (interactive)
      (skewer-eval "window.scrollBy(-100,0);"))
    (defun skewer-scroll-right ()
      "Scroll down"
      (interactive)
      (skewer-eval "window.scrollBy(100,0);"))

    (defun skewer-bind-browser-keys ()
      "DOCSTRING"
      (interactive)
      (bind-key "C-<down>" 'skewer-scroll-down)
      (bind-key "C-<up>" 'skewer-scroll-up)
      (bind-key "C-<left>" 'skewer-scroll-left)
      (bind-key "C-<right>" 'skewer-scroll-right)
      ;; (bind-key "<f5>" 'skewer-reload-page)
      )))


;;;; smart-shift

(use-package smart-shift
  :ensure t
  :commands smart-shift-mode
  :bind (("S-<left>" . smart-shift-left)
         ("S-<right>" . smart-shift-right)))


;;;; smartparens

(use-package smartparens
  :ensure t
  ;; :pin "melpa-stable"
  :if (not noninteractive)
  :commands (smartparens-mode smartparens-global-mode turn-on-smartparens-mode
                              turn-off-smartparens-mode show-smartparens-mode
                              show-smartparens-global-mode
                              smartparens-global-strict-mode
                              smartparens-strict-mode
                              turn-on-smartparens-strict-mode)
  :diminish ""
  :defer 1.4
  :init
  (progn
    (setq
     sp-show-pair-delay 0.125
     sp-show-pair-from-inside t)
    (hook-into-modes 'turn-on-smartparens-strict-mode my-lisp-mode-hooks))
  :config
  (progn
    (bind-key "8" (lambda (&optional arg)
                    (interactive "P") (sp-wrap-with-pair "*"))
              region-bindings-mode-map)
    (bind-key "9"
              (lambda (&optional arg)
                (interactive "P") (sp-wrap-with-pair "("))
              region-bindings-mode-map)
    (bind-key "q"
              (lambda (&optional arg)
                (interactive "P") (sp-wrap-with-pair "'"))
              region-bindings-mode-map)
    (bind-key "Q"
              (lambda (&optional arg)
                (interactive "P") (sp-wrap-with-pair "\""))
              region-bindings-mode-map)

    (bind-key "C-x C-r"  'sp-rewrap-sexp smartparens-mode-map)
    (setq
     sp-ignore-modes-list '(calc-mode dired-mode ibuffer-mode
                                      minibuffer-inactive-mode sr-mode)
     sp-autoescape-string-quote nil)
    (sp-pair "'" nil :unless '(sp-point-after-word-p))

    (sp-with-modes '(emacs-lisp-mode inferior-emacs-lisp-mode
                                     lisp-interaction-mode scheme-mode
                                     lisp-mode eshell-mode slime-repl-mode
                                     clojure-mode common-lisp-mode)
      (sp-local-pair "'" nil :actions nil)
      (sp-local-pair "`" "'" :when '(sp-in-string-p)))

    (sp-with-modes '(tex-mode plain-tex-mode latex-mode)
      ;; math modes, yay.  The :actions are provided automatically if
      ;; these pairs do not have global definition.
      (sp-local-pair "$" "$")
      (sp-local-pair "\\[" "\\]")
      (sp-local-pair "`" "'")
      ;; (sp-local-tag "\\b" "\\begin{_}" "\\end{_}")
      )

    (sp-with-modes '(sgml-mode html-mode web-mode)
      (sp-local-pair "<" ">")
      (sp-local-tag  "<" "<_>" "</_>" :transform 'sp-match-sgml-tags))

    (sp-with-modes '(gfm-mode gfm-mode rst-mode)
      (sp-local-pair "*" "*" :bind "C-*")
      (sp-local-tag "2" "**" "**")
      ;; (sp-local-tag "s" "```scheme" "```")
      (sp-local-tag "<"  "<_>" "</_>" :transform 'sp-match-sgml-tags))

    (sp-with-modes '(gfm-mode gfm-mode)
      (sp-local-pair "#" "#" :actions '(wrap))
      (sp-local-pair "_" "_" :actions '(wrap))
      (sp-local-pair "*" "*" :actions '(wrap)))

    (sp-with-modes '(org-mode)
      (sp-local-pair "=" "=" :actions '(wrap))
      (sp-local-pair "/" "/" :actions '(wrap))
      (sp-local-pair "*" "*" :actions '(wrap)))

    (sp-local-pair 'minibuffer-inactive-mode "'" nil :actions nil)
    (smartparens-global-mode t)
    (add-hook 'prog-mode-hook
              (lambda ()
                (when (> (buffer-size) (* 1 1024 1024))
                  (turn-off-smartparens-mode)
                  (turn-off-show-smartparens-mode))))

    (show-smartparens-global-mode t)))


;;;; smeargle

(use-package smeargle
  :ensure t
  :commands (smeargle
             smeargle-commits
             smeargle-clear))


;;;; smerge-mode

(use-package smerge-mode
  :config
  (defhydra smerge-hydra
    (:color pink :hint nil :post (smerge-auto-leave))
    "
^Move^       ^Keep^               ^Diff^                 ^Other^
^^-----------^^-------------------^^---------------------^^-------
_n_ext       _b_ase               _<_: upper/base        _C_ombine
_p_rev       _u_pper              _=_: upper/lower       _r_esolve
^^           _l_ower              _>_: base/lower        _k_ill current
^^           _a_ll                _R_efine
^^           _RET_: current       _E_diff
"
    ("n" smerge-next)
    ("p" smerge-prev)
    ("b" smerge-keep-base)
    ("u" smerge-keep-upper)
    ("l" smerge-keep-lower)
    ("a" smerge-keep-all)
    ("RET" smerge-keep-current)
    ("\C-m" smerge-keep-current)
    ("<" smerge-diff-base-upper)
    ("=" smerge-diff-upper-lower)
    (">" smerge-diff-base-lower)
    ("R" smerge-refine)
    ("E" smerge-ediff)
    ("C" smerge-combine-with-next)
    ("r" smerge-resolve)
    ("k" smerge-kill-current)
    ("ZZ" (lambda ()
            (interactive)
            (save-buffer)
            (bury-buffer))
     "Save and bury buffer" :color blue)
    ("q" nil "cancel" :color blue))
  :hook (magit-diff-visit-file . (lambda ()
                                   (when smerge-mode
                                     (smerge-hydra/body)))))


;;;; smex

;; counsel-M-x uses smex to sort commands by recent use.
(use-package smex
  :ensure t
  :defer t
  :init
  (progn
    (setq
     smex-save-file (expand-file-name
                     "smex-items" user-data-directory)
     smex-history-length 15)))


;;;; solarized-theme-utils

(use-package solarized-theme-utils
  :commands solarized-import-faces)


;;;; speedbar

(use-package speedbar
  :defer
  :init
  (progn
    (setq
     speedbar-use-images nil
     speedbar-hide-button-brackets-flag t
     speedbar-show-unknown-files t
     speedbar-smart-directory-expand-flag t
     speedbar-indentation-width 2
     speedbar-update-flag t
     speedbar-frame-parameters
     '((minibuffer)
       (width . 28)
       (border-width . 0)
       (menu-bar-lines . 0)
       (tool-bar-lines . 0)
       (unsplittable . t)
       (left-fringe . 0)
       (name . "speedbar")
       (title . "Speedbar"))))
  :config
  (progn
    (bind-key "S-<up>" 'speedbar-up-directory speedbar-mode-map)
    (bind-key "<right>" 'speedbar-flush-expand-line speedbar-mode-map)
    (bind-key "<left>" 'speedbar-contract-line speedbar-mode-map)
    (add-hook 'speedbar-mode-hook #'(lambda () (hl-line-mode 1)))

    ))


;;;; sqlite

(use-package sqlite
  :ensure t
  :defer)


;;;; string-inflection

(use-package string-inflection
  :ensure t
  :commands string-inflection-cycle)


;;;; subword

(use-package subword
  :defer t
  :diminish ""
  :init
  (progn
    (unless noninteractive
      (global-subword-mode))))


;;;; swift-mode

(use-package swift-mode
  :ensure t
  :mode (("\\.swift\\'" . swift-mode)))


;;;; swiper

(use-package swiper
  :ensure t
  :commands (swiper-isearch
             swiper-isearch-backward))


;;;; sws-mode

(use-package sws-mode
  :ensure t
  :commands sws-mode)


;;;; syslog-mode

(use-package syslog-mode
  :ensure t
  :mode (("var/log/syslog.*\\'" . syslog-mode)
         ("var/log/auth.*\\'" . syslog-mode)
         ("var/log/kern.*\\'" . syslog-mode)
         ("var/log/dmesg.*\\'" . syslog-mode)))


;;;; terraform-mode

(use-package terraform-mode
  :ensure t
  :mode (("\\.tf\\(vars\\)?\\'" . terraform-mode))
  :config
  (progn
    (defun terraform-cccc ()
      (interactive)
      (terraform-format-buffer)
      (silent-save-some-buffers))

    (bind-key "C-c C-c" 'terraform-cccc terraform-mode-map)))


;;;; tex

(use-package tex
  :ensure auctex
  :defer)


;;;; text-mode

(use-package text-mode
  :defer t
  :mode (("LICENSE\\'" . text-mode)))


;;;; thingopt

(use-package thingopt :ensure t :defer)


;;;; toggle-quotes

(use-package toggle-quotes
  :ensure t
  :commands (toggle-quotes)
  :bind ("C-'" . toggle-quotes))


;;;; toml-mode

(use-package toml-mode
  :ensure t
  :mode (("\\.toml\\'" . toml-mode)
         ("Gopkg\\.lock\\'" . toml-mode)))


;;;; traad

(use-package traad
  :ensure t
  :commands (
             traad-install-server
             traad-rename
             traad-rename-current-file
             traad-normalize-arguments
             traad-remove-argument
             traad-add-argument
             traad-extract-method
             traad-extract-variable

             )
  :init
  (progn
    (setq traad-save-unsaved-buffers 'always)))


;;;; tramp

(use-package tramp
  :ensure t
  :defer
  :init
  (progn
    (setq vc-ignore-dir-regexp
          (format "\\(%s\\)\\|\\(%s\\)"
                  vc-ignore-dir-regexp
                  tramp-file-name-regexp))
    (setq
     ;; tramp-default-method "scpx"
     tramp-persistency-file-name (expand-file-name
                                  "tramp" user-data-directory)
     tramp-backup-directory-alist backup-directory-alist
     tramp-adb-sdk-dir (getenv "ANDROID_SDK")
     tramp-remote-path '(tramp-default-remote-path
                         "/bin" "/usr/bin" "/sbin" "/usr/sbin"
                         "/usr/local/bin" "/usr/local/sbin" "/local/bin" "/local/freeware/bin"
                         "/local/gnu/bin" "/usr/freeware/bin" "/usr/pkg/bin" "/usr/contrib/bin"
                         "/opt/bin" "/opt/sbin" "/opt/local/bin"
                         "/snap/bin/")))
  :config
  (progn
    (setq tramp-completion-function-alist-ssh
          (-difference
           tramp-completion-function-alist-ssh
           '((tramp-parse-shosts "~/.ssh/known_hosts"))))))


;;;; transient

(use-package transient
  :ensure t
  :defer t
  :config
  (progn
    (setq transient-levels-file (expand-file-name (workspace-prefix-file-name "transient-levels" ".el")
                                                  user-data-directory)
          transient-history-file (expand-file-name (workspace-prefix-file-name "transient-history" ".el")
                                                   user-data-directory)
          transient-values-file (expand-file-name (workspace-prefix-file-name "transient-values" ".el")
                                                  user-data-directory))))


;;;; transmission

(use-package transmission
  :ensure t
  :commands transmission)


;;;; transpose-frame

(use-package transpose-frame
  :ensure t
  :commands (flip-frame flop-frame)
  :bind (("M-o M-f" . flop-frame)))


;;;; treemacs

(use-package treemacs
  :ensure t
  :defer t
  :commands (treemacs)
  :bind (("C-x C-d" . treemacs-select-window))
  :config
  (progn
    (load "treemacs-autoloads" nil t)
    (setq
     treemacs-select-when-already-in-treemacs 'close
     treemacs-display-in-side-window t
     treemacs-no-png-images t
     treemacs-recenter-after-file-follow t)
    (treemacs-follow-mode 1)
    (treemacs-project-follow-mode 1))
  )


;;;; truthy

(use-package truthy
  :ensure t
  :commands (truthy
             truthy-s
             truthy-l))


;;;; tuareg

(use-package tuareg
  :ensure t
  :mode ("\\.ml[ip]?\\'" . tuareg-mode))


;;;; typescript-mode

(use-package typescript-mode
  :ensure t
  :commands (typescript-mode)
  :mode (("\\.ts\\'" . typescript-mode)
         ("\\.tsx\\'" . typescript-mode))
  :config
  (progn
    (defun typescript-cccc ()
      (interactive)
      (silent-save-some-buffers)
      (prettier-js))
    (bind-key "C-c C-c" 'typescript-cccc typescript-mode-map)))


;;;; undo-tree

(use-package undo-tree
  :ensure t
  :if (not noninteractive)
  :commands (global-undo-tree-mode turn-on-undo-tree-mode)
  :diminish undo-tree-mode
  :init
  (progn
    (setq
     undo-tree-auto-save-history nil
     undo-tree-visualizer-timestamps t
     undo-tree-history-directory-alist
     (list (cons "." (expand-file-name
                      (concat "undo-tree-save/" (user-real-login-name) "/")
                      user-data-directory))))

    (global-undo-tree-mode)

    (defadvice undo-tree-insert (around pretty activate)
      (ad-set-arg 0 (cond
                     ((equal ?| (ad-get-arg 0)) ?│)
                     ((equal ?\\ (ad-get-arg 0)) ?╲)
                     ((equal ?/ (ad-get-arg 0)) ?╱)
                     ((equal ?- (ad-get-arg 0)) ?─)
                     (t (ad-get-arg 0)))())
      ad-do-it))

  :config
  (progn
    (bind-keys :map undo-tree-visualizer-mode-map
               ("u" . undo-tree-visualize-undo)
               ("r" . undo-tree-visualize-redo))))


;;;; unfill

(use-package unfill
  :ensure t
  :commands (unfill-region unfill-paragraph unfill-toggle)
  :bind ("M-q" . unfill-toggle))


;;;; uniquify

(use-package uniquify
  :if (and  (not noninteractive))
  :init
  (progn
    (setq
     uniquify-buffer-name-style 'post-forward
     uniquify-separator " • "
     uniquify-min-dir-content 3
     uniquify-after-kill-buffer-flag t
     uniquify-ignore-buffers-re "^\\*")))


;;;; vc

(use-package vc
  :defer
  :init
  (progn
    (setq vc-follow-symlinks t)
    (if noninteractive
        ;;Disable all vcs back ends (Emacs starts faster)
        (setq vc-handled-backends ())
      (setq vc-handled-backends '(Git Hg)))

    (use-package vc-annotate
      :defer
      :init
      (progn
        (defun my-vc-annotate-hook ()
          (unless
              (memq 'vc-annotate-annotation buffer-invisibility-spec)
            (vc-annotate-toggle-annotation-visibility)))
        (add-hook 'vc-annotate-mode-hook 'my-vc-annotate-hook))
      :config
      (progn
        (defun vc-annotate-get-time-set-line-props ()
          (let ((bol (point))
                (date (vc-call-backend vc-annotate-backend 'annotate-time))
                (inhibit-read-only t))
            (cl-assert (>= (point) bol))
            (put-text-property bol (point) 'invisible 'vc-annotate-annotation)
            (when (string-equal "Git" vc-annotate-backend)
              (save-excursion
                (goto-char bol)
                (search-forward "(")
                (let ((p1 (point)))
                  (re-search-forward " [0-9]")
                  (remove-text-properties p1 (1- (point)) '(invisible nil))
                  )))
            date))))
    ))


;;;; virtualenvwrapper

(use-package virtualenvwrapper
  :ensure t
  :commands (venv-workon
             venv-is-valid))


;;;; visual-regexp

(use-package visual-regexp
  :ensure t
  :commands (vr/replace vr/query-replace)
  :config
  (progn

    (use-package visual-regexp-steroids
      :ensure t)))


;;;; vkill

(use-package vkill
  :commands vkill
  :if (not noninteractive)
  :config
  (setq vkill-show-all-processes t))


;;;; volatile-highlights

(use-package volatile-highlights
  :ensure t
  :commands volatile-highlights-mode
  :diminish volatile-highlights-mode
  :init
  (progn
    (defun turn-on-volatile-highlights-mode ()
      (volatile-highlights-mode 1))
    (hook-into-modes #'turn-on-volatile-highlights-mode
                     my-prog-mode-hooks)))


;;;; web-mode

(use-package web-mode
  :ensure t
  :mode (("\\.phtml\\'" . web-mode) ("\\.erb\\'" . web-mode)
         ("\\.jsp\\'" . web-mode) ("\\.as[cp]x\\'" . web-mode)
         ("\\.erb\\'" . web-mode) ("\\.html\\'" . web-mode)
         ("\\.rhtml\\'" . web-mode) ("\\.mustache\\'" . web-mode)
         ("\\.hbs\\'" . web-mode)
         ("\\.jsx\\'" . web-mode)
         )
  :init
  (progn
    (setq
     web-mode-comment-style 2
     web-mode-indent-style 2
     web-mode-enable-whitespaces nil
     web-mode-enable-block-face t
     web-mode-enable-part-face t
     web-mode-enable-heredoc-fontification t
     web-mode-enable-comment-keywords t
     web-mode-enable-current-element-highlight t))
  :config
  (progn
    (defun my-web-mode-hook ()
      "Hooks for Web mode."
      (let ((project-root (project-root-function)))
        (when project-root
          (let* ((locate-dominating-stop-dir-regexp (regexp-quote project-root))
                 (package-json (locate-dominating-file buffer-file-name "package.json"))
                 (manage-py (locate-dominating-file buffer-file-name "manage.py"))
                 (package-json-len (length package-json))
                 (manage-py-len (length manage-py))

                (engine (cond
                         ((and (> manage-py-len 0)
                             (> manage-py-len package-json-len)
                             (string-match-p (regexp-quote "/templates/") default-directory)
                             ) "django")
                         (t nil))))
            (when engine
                  (web-mode-set-engine engine))))))
    (add-hook 'web-mode-hook  'my-web-mode-hook)

    (bind-key "C-c ;" 'web-mode-comment-or-uncomment web-mode-map)
    ;; (bind-key "C-<tab>" 'js2-jsx-mode web-mode-map)
    (unbind-key "C-c C-p" web-mode-map)
    (unbind-key "C-c C-n" web-mode-map)))


;;;; wgrep

(use-package wgrep
  :ensure t
  :commands (wgrep-setup))


;;;; whitespace

(use-package whitespace
  :bind (("M-o w" . whitespace-cleanup)))


;;;; whole-line-funcs

(use-package whole-line-funcs
  :commands (whole-line-mark-previous
             whole-line-mark-next)
  :bind (("C-x C-p" . whole-line-mark-previous)
         ("C-x C-n" . whole-line-mark-next)))


;;;; whole-line-or-region

(use-package whole-line-or-region
  :ensure t
 :commands (whole-line-or-region-mode
             whole-line-or-region-global-mode
             whole-line-or-region-comment-dwim)
  ;; :bind (
  ;;        ("C-c ;" . whole-line-or-region-comment-dwim)
  ;;        ("C-w" . whole-line-or-region-kill-region)
  ;;        ("C-y" . whole-line-or-region-yank))
  :defer 0.2
  :init
  (progn
    (define-key region-bindings-mode-map ";" 'whole-line-or-region-comment-dwim))

  :config
  (progn
    (whole-line-or-region-global-mode)))


(defun comment-or-uncomment-region-or-line ()
  "Comments or uncomments the region or the current line if there's no active region."
  (interactive)
  (if (region-active-p)
      (let ((beg (region-beginning))
            (end (region-end)))
        (save-excursion
          (goto-char beg)
          (beginning-of-line)
          (setq beg (point))
          (goto-char end)
          (end-of-line)
          (setq end (point))
          (comment-or-uncomment-region beg end)))
    (save-excursion
      (call-interactively 'comment-line))))


(define-key region-bindings-mode-map ";" 'comment-or-uncomment-region-or-line)
(bind-key "C-c ;" 'comment-or-uncomment-region-or-line)


;;;; window-layout

(use-package window-layout
  :ensure t
  :defer t)


;;;; winner

(use-package winner
  :if (and  (not noninteractive))
  :bind* (("M-N" . winner-redo)
         ("M-P" . winner-undo))
  :init
  (progn
    (setq winner-dont-bind-my-keys t
          winner-boring-buffers
          '("*Completions*"
            "*SPEEDBAR*"))
    (condition-case nil
        (winner-mode 1)
      (error (warn "winner mode startup failed!")))))


;;;; with-editor

(use-package with-editor
  :ensure t
  :defer t
  :config
  (progn
    (setq with-editor-emacsclient-executable
          (unless (getenv "SSH_TTY")
            (with-editor-locate-emacsclient)))))


;;;; ws-butler

(use-package ws-butler
  ;; :disabled t ;; FIXME something else prohibits restoring point after save
  ;; NOTE It seems like ws-butler has started working again
  :ensure t
  :if (not noninteractive)
  :commands (ws-butler-mode)
  :diminish ws-butler-mode
  :init
  (progn
    (hook-into-modes #'ws-butler-mode my-prog-mode-hooks)
    (hook-into-modes #'ws-butler-mode my-css-like-mode-hooks)
    (hook-into-modes #'ws-butler-mode my-html-like-mode-hooks)))


;;;; xref

(use-package xref
  :defer t
  :config
  (progn
    (defvar xref-current-item nil)
    (defun my-pulse-xref ()
      (pcase-let ((`(,beg . ,end)
               (save-excursion
                 (or
                  (let ((length (xref-match-length xref-current-item)))
                    (and length (cons (point) (+ (point) length))))
                  (back-to-indentation)
                  (if (eolp)
                      (cons (line-beginning-position) (1+ (point)))
                    (cons (point) (line-end-position)))))))
        (my-pulse-region beg end)))
    (remove-hook 'xref-after-jump-hook 'xref-pulse-momentarily)
    (remove-hook 'xref-after-return-hook 'xref-pulse-momentarily)
    (add-hook 'xref-after-jump-hook 'my-pulse-xref 99)
    (add-hook 'xref-after-return-hook 'my-pulse-xref 99)))


;;;; yaml-mode

(use-package yaml-mode
  :ensure t
  :commands yaml-mode
  :mode ("\\.y[a]?ml\\'" . yaml-mode)
  :init
  (progn
    (defun my-yaml-save-hook-fn ()
      (when (and buffer-file-name
                 (eq major-mode 'yaml-mode))
        (whitespace-cleanup-region (point-min) (point-max))))
    (defun my-yaml-mode-hook ()
      (add-hook 'after-save-hook 'my-yaml-save-hook-fn nil t))
    (add-hook 'yaml-mode-hook 'my-yaml-mode-hook)))


;;;; yasnippet

(use-package yasnippet
  :ensure t
  :commands (yas-reload-all yas-global-mode yas-minor-mode snippet-mode
                            yas-expand yas-expand-snippet yas-minor-mode-on
                            dired-snippets-dir yas-insert-snippet
                            yas-activate-extra-mode)
  :bind (("C-x d y" . dired-snippets-dir))
  :diminish yas-minor-mode
  :init
  (progn
    (setq ;; Yasnippet
     ;; Dont print yasnippet messages
     yas-verbosity 0
     ;; Snippet directories
     my-yas-snippets-dir (expand-file-name
                          "snippets" user-emacs-directory)
     yas-snippet-dirs (list my-yas-snippets-dir)
     ;; Disable yasnippet prompt by default
     ;; (using auto-complete to prompt)
     ;; yas-prompt-functions '(yas-popup-isearch-prompt
                            ;; yas-ido-prompt yas-completing-prompt yas-no-prompt)
     )
    (add-to-list 'auto-mode-alist
                 (cons
                  (concat (regexp-quote my-yas-snippets-dir) ".*\\'")
                  'snippet-mode))
    (add-to-list 'auto-mode-alist
                 (cons
                  (concat (regexp-quote (file-truename
                                         my-yas-snippets-dir)) ".*\\'")
                  'snippet-mode))

    (defadvice ac-fallback-command (around no-yasnippet-fallback activate)
      (let ((yas-fallback-behavior nil))
        ad-do-it))

    (bind-key "C-x i" 'yas-insert-snippet)

    (defun my-ac-git-commit-setup-hook-fn ()
      (yas-activate-extra-mode 'git-commit-mode)
      (yas-minor-mode-on))

    (add-hook 'git-commit-setup-hook #'my-ac-git-commit-setup-hook-fn )
    (hook-into-modes #'yas-minor-mode-on '(org-mode-hook))
    (hook-into-modes #'yas-minor-mode-on my-prog-mode-hooks)
    (hook-into-modes #'yas-minor-mode-on my-css-like-mode-hooks))
  :config
  (progn
    (bind-key "C-x i" 'yas-insert-snippet yas-minor-mode-map)

    (use-package popup
      :ensure t
      :commands yas-popup-isearch-prompt
      :config
      (progn

        (defun yas-popup-isearch-prompt (prompt choices &optional display-fn)
          (let ((group-max-len 0)
                (key-max-len 0)
                (fmt "")
                (popup-items))

            (dolist (choice choices)
              (when (yas--template-p choice)
                (setq group-max-len (max group-max-len
                                         (+ (length (yas--template-group choice))
                                            (apply #'+ (mapcar #'length (yas--template-group choice))))))
                (setq key-max-len (max key-max-len (length (yas--template-key choice))))))

            (setq fmt (format "%s%%%d.%ds%s%%-%d.%ds│ %%s"
                              (if (> group-max-len 0 ) "" " ")
                              group-max-len group-max-len
                              (if (> group-max-len 0 ) " > " "")
                              key-max-len key-max-len))

            (setq popup-items
                  (mapcar
                   #'(lambda (choice)
                       (popup-make-item
                        (if (yas--template-p choice)
                            (format fmt
                                    (if (yas--template-group choice)
                                        (s-join "/" (yas--template-group choice))
                                      "")
                                    (if (yas--template-key choice)
                                        (yas--template-key choice)
                                      "")
                                    (if (yas--template-name choice)
                                        (yas--template-name choice)
                                      ""))
                          (format " %s" choice))
                        :value choice))
                   choices))

            (popup-menu*
             popup-items
             :prompt prompt
             :max-width 80
             :isearch t)))))

    (defun yas-remove-recompile-reload-all ()
      (interactive)
      (let ((default-directory my-yas-snippets-dir) )
        (mapc (lambda (f)
                (delete-file f))
              (file-expand-wildcards "*.elc")))
      (f-files my-yas-snippets-dir
               (lambda (file)
                 (and
                  (equal (f-no-ext (f-filename file)) ".yas-compiled-snippets")
                  (f-delete file)))
               t)
      ;; (yas-recompile-all)
      (yas-reload-all))

    (defun my-snippet-save-hook ()
      (when (and buffer-file-name
                 (eq major-mode 'snippet-mode))
        (yas-remove-recompile-reload-all)))

    (defun my-snippet-mode-hook ()
      (add-hook 'after-save-hook 'my-snippet-save-hook nil t))
    (add-hook 'snippet-mode-hook 'my-snippet-mode-hook)

    (defun dired-snippets-dir ()
      "Open dired in the yas snippets dir."
      (interactive)
      (dired (expand-file-name
              "snippets" user-emacs-directory)))

    (yas-reload-all)))


;;;; zeal-at-point

(use-package zeal-at-point
  :ensure t
  :commands zeal-at-point
  :bind (("C-h z" . zeal-at-point))
  :init
  (progn
    (setq zeal-at-point-mode-alist
          '((arduino-mode . "arduino")
            (c++-mode . "cpp")
            (c-mode . "c")
            (clojure-mode . "clojure")
            (common-lisp-mode . "lisp")
            (cperl-mode . "perl")
            (css-mode . "css")
            (elixir-mode . "elixir")
            (emacs-lisp-mode . "emacs")
            (erlang-mode . "erlang")
            (gfm-mode . "md")
            (go-mode . "go")
            (go-ts-mode . "go")
            (groovy-mode . "groovy")
            (haskell-mode . "haskell")
            (html-mode . "html")
            (java-mode . "java")
            (js2-mode . "javascript")
            (js3-mode . "nodejs")
            (less-css-mode . "less")
            (lua-mode . "lua")
            (markdown-mode . "md")
            (objc-mode . "iphoneos")
            (perl-mode . "perl")
            (php-mode . "php")
            (processing-mode . "processing")
            (puppet-mode . "puppet")
            (python-mode . "python_2")
            (ruby-mode . "ruby")
            (scala-mode . "scala")
            (vim-mode . "vim")))))


;;;; zencoding-mode

(use-package zencoding-mode
  :ensure t
  :commands zencoding-mode
  :diminish ((zencoding-mode . "zen"))
  :init
  (progn
    (hook-into-modes #'zencoding-mode my-html-like-mode-hooks))
  :config
  (progn
    (bind-key "C-c C-c" 'zencoding-expand-line zencoding-mode-keymap)
    (unbind-key "C-j" zencoding-mode-keymap)))


;;;; zig-ts-mode

(use-package zig-ts-mode
  :ensure t
  :defer t
  :commands (zig-ts-mode)
  :mode ("\\.zig\\'" . zig-ts-mode))



(provide 'init-packages)

;; Local Variables:
;; eval: (outline-minor-mode 1)
;; eval: (require 'use-package)
;; End:


;;; init-packages.el ends here
