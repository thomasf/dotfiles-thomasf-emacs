;;; init.el --- Thomas Frössman emacs init

;;; Commentary:

;;


;;; Code:

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;; (package-initialize)


;;; bootstrap

;;;; record init-start-time

(eval-and-compile
  (defvar init-times `(("before-init" ,before-init-time)))
  (push `("init-start" ,(current-time)) init-times))


;;;; compilation utlities

(eval-when-compile
  (defmacro executable-find* (command &optional help)
    "Macro form of executable-find..."
    (let ((v (executable-find command)) )
      (unless v
        (message "executable '%s' not found: %s" command (or help "")))
      v))

  (defmacro file-exists-p* (filename)
    "Macro form of file-exists-p which expands the file name and returns the expenaded file name instead of t."
    (let ((f (expand-file-name filename)) )
      (if (file-exists-p filename) filename))))


;;;; gc

(setq-default ;; alloc.c
 gc-cons-threshold (* 20 1204 1204)
 gc-cons-percentage 0.5)


;;;; process.c

(setq ;; process.c
 read-process-output-max (* 1024 1024))


;;;; disable menu/scroll/toolbars

(and (fboundp 'menu-bar-mode)
   menu-bar-mode
   (not (eq system-type 'darwin))
   (menu-bar-mode -1))
(and (fboundp 'tool-bar-mode)
   tool-bar-mode
   (tool-bar-mode -1))
(and (fboundp 'scroll-bar-mode)
   scroll-bar-mode
   (scroll-bar-mode -1))

(setq frame-inhibit-implied-resize t
      default-frame-alist '((vertical-scroll-bars . nil)
                            (tool-bar-lines . 0)
                            (menu-bar-lines . 0)
                            (fullscreen . nil)))


;;;; define feature inhibition variable

(defvar degrade-p-minimalism nil
  "If set to non nil a lighter emacs config is used. ")


;;;; emacs version requirement check

(and
 (not noninteractive)
 (< emacs-major-version 25) ;; compat
 (warn "Use a newer version of Emacs for a full featured environment!"))


;;;; workspace-prefix

(defun workspace-prefix ()
  (let ((res (if (and
                  (eq window-system 'x)
                  (executable-find* "wsname"))
                 (shell-command-to-string "wsname -p"))))
    (and res
       (not
        (or
         (null res)
         (string= "" res)))
       res)))

(defvar workspace-prefix-startup
  (or
   (and
    (not noninteractive)

    (workspace-prefix))))

(defun workspace-prefix-file-name (name &optional ext)
  (concat name "." (or workspace-prefix-startup "DEFAULT") (or ext "")))


;;;; load-path


(eval-and-compile
  (defvar user-emacs-directory nil)
  (defvar user-data-directory nil)
  (defvar user-cache-directory nil)
  (defvar user-lisp-directory nil)
  (defvar user-site-lisp-directory nil)
  (defvar user-themes-directory nil)
  (defvar user-notes-directory nil))


(when load-file-name
  (load (expand-file-name
         "load-path" (file-name-directory load-file-name)) nil t))


;;;; verbose start up/compile logging

(eval-and-compile
  (defvar my-log-verbose nil)
  ;; (setq my-log-verbose t)
  (if my-log-verbose
      (setq byte-compile-verbose t)
    (setq ad-redefinition-action 'accept))
  (setq use-package-verbose my-log-verbose
        use-package-debug nil
        use-package-enable-imenu-support t
        use-package-minimum-reported-time 0.01))


;;;; package.el + early package install

;; set up package.el and make sure that some essential packages and features
;; are loaded early.

(eval-and-compile
  (when (and (equal emacs-version "27.2")
           (eql system-type 'darwin))
    (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")))

(eval-and-compile (push `("packages-start" ,(current-time)) init-times))

(eval-and-compile
  (setq
   package-enable-at-startup nil
   package-archives
   '(("melpa-stable" . "https://stable.melpa.org/packages/")
     ("melpa" . "https://melpa.org/packages/")
     ("nongnu" . "https://elpa.nongnu.org/nongnu/")
     ("gnu" . "https://elpa.gnu.org/packages/")
     ))

  (unless (boundp 'package-pinned-packages)
    (setq package-pinned-packages ()))

  (defun require-package (package &optional min-version no-refresh)
    "Install given PACKAGE, optionally requiring MIN-VERSION.
If NO-REFRESH is non-nil, the available package lists will not be
re-downloaded in order to locate PACKAGE."
    (if (package-installed-p package min-version)
        t
      (if (or (assoc package package-archive-contents) no-refresh)
          (package-install package)
        (progn
          (package-refresh-contents)
          (require-package package min-version t))))))


(defvar byte-compile-warnings nil)
(eval-when-compile
  (require 'package)
  (package-initialize t)
  (require-package 'use-package)
  (require 'use-package))

(require 'cl)
(require 'subr-x)

(use-package sql
  :defer t
  :init
  (progn
    (add-hook 'sql-mode-hook '(lambda () (define-key sql-mode-map (kbd "TAB") 'self-insert-command)))))

(use-package dash
  :ensure t
  :demand
  :commands (dash-enable-fontlock)
  :init
  (progn
    (add-hook 'emacs-lisp-mode-hook 'dash-enable-font-lock)))

(use-package dash-functional :ensure t :defer)
(use-package memoize :ensure t :defer)
(use-package s :ensure t)
(use-package f :ensure t)
(use-package bind-key :ensure t :config (override-global-mode 1))
(use-package smartrep :ensure t)
(use-package diminish :ensure t)
(use-package deferred :ensure t :commands (deferred:$))
(use-package let-alist :ensure t :commands (let-alist))
(use-package request-deferred :ensure t :defer)
(use-package concurrent :ensure t :defer)
(use-package load-relative :ensure t :defer)
(use-package loc-changes :ensure t :defer)
(use-package epc :ensure t :defer)
(use-package ctable :ensure t :defer)
(use-package fringe-helper :ensure t :defer)
(use-package fuzzy :ensure t :defer :disabled t)
(use-package flx : ensure t :defer :disabled t)
(use-package pcache
  :ensure t
  :defer
  :init
  (progn
    (eval-and-compile
      (setq
       pcache-directory
       (let ((dir (expand-file-name "pcache/" user-cache-directory)))
         (make-directory dir t)
         dir)))))
(use-package posframe :ensure t :defer :disabled t)
(use-package hydra
  :ensure t
  :config
  (progn

    (setq hydra-amaranth-warn-message "not bound")

    (defun hydra-compact-hint (hydra)
      (let* ((basename (symbol-name hydra))
             (hint (intern (concat basename "/hint")))
             (params (intern (concat basename "/params")))
             (heads (intern (concat basename "/heads")))
             (hydra-head-format "%s "))
        (set hint (eval (hydra--hint-heads-wocol-compact
                         (symbol-value params) (symbol-value heads))))))

    (defun hydra--hint-heads-wocol-compact (body heads)
  "Generate a hint for the echo area.
BODY, and HEADS are parameters to `defhydra'.
Works for heads without a property :column."
  (let (alist)
    (dolist (h heads)
      (let ((val (assoc (cadr h) alist))
            (pstr (hydra-fontify-head h body)))
        (if val
            (setf (cadr val)
                  (concat (cadr val) " " pstr))
          (push
           (cons (cadr h)
                 (cons pstr (cl-caddr h)))
           alist))))
    (let ((keys (nreverse (mapcar #'cdr alist)))
          (n-cols (plist-get (cddr body) :columns))
          res)
      (setq res
            (if n-cols
                (let ((n-rows (1+ (/ (length keys) n-cols)))
                      (max-key-len (apply #'max (mapcar (lambda (x) (length (car x))) keys)))
                      (max-doc-len (apply #'max (mapcar (lambda (x)
                                                          (length (hydra--to-string (cdr x)))) keys))))
                  `(concat
                    "\n"
                    (mapconcat #'identity
                               (mapcar
                                (lambda (x)
                                  (mapconcat
                                   (lambda (y)
                                     (and y
                                          (funcall hydra-key-doc-function
                                                   (car y)
                                                   ,max-key-len
                                                   (hydra--to-string (cdr y))
                                                   ,max-doc-len))) x ""))
                                ',(hydra--matrix keys n-cols n-rows))
                               "\n")))


              `(concat
                (mapconcat
                 #'hydra--eval-and-format
                 ',keys
                 " ")
                ,(if keys "." ""))))
      (if (cl-every #'stringp
                    (mapcar 'cddr alist))
          (eval res)
        res))))))

(use-package exec-path-from-shell
  :ensure t
  :commands (exec-path-from-shell-initialize)
  :init
  (progn
    (when (and (memq window-system '(mac ns))
               (not (executable-find "hsadmin")))
      (exec-path-from-shell-initialize))))


(eval-and-compile (push `("packages-end" ,(current-time)) init-times))


;;;; load private-init

(require 'private-init nil (not my-log-verbose))


;;;; startup.el

(defun display-startup-echo-area-message ())
(setq
 auto-save-list-file-prefix (expand-file-name
                             "auto-save-list/" user-data-directory)
 inhibit-startup-message t
 inhibit-splash-screen t
 inhibit-startup-buffer-menu t
 inhibit-startup-echo-area-message t
 initial-major-mode 'initial-mode
 initial-scratch-message ";;_
;;                 __         _,******
;;   ,-----,        _  _,**
;;   | Mu! |          _   ____,****
;;   ;-----;        _
;;        \\   ^__^
;;         \\  (^^)\\_______
;;          ^-(..)\\       )\\/\\/^_^
;;                ||----w |
;; __.-''*-,.,____||_____||___,_.-
;;                 ''     ''

")


;;;; initial-mode

(defvar initial-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c c") 'lisp-interaction-mode)
    (define-key map (kbd "C-c C-c") 'lisp-interaction-mode)
    map)
  "Keymap for `initial-mode'.")

(define-derived-mode initial-mode nil "Initial"
  "Major mode for start up buffer.
\\{initial-mode-map}"

  (setq-local text-mode-variant t)
  (setq-local indent-line-function 'indent-relative))


;;;; backups and auto saves

(defconst backup-dir
  (expand-file-name (concat "backups/" (user-real-login-name) "/")
                    user-data-directory)
  "Directory for Emacs backups.")

(defconst
  autosave-dir
  (expand-file-name (concat "autosaves/" (user-real-login-name) "/")
                    user-data-directory)
  "Directory for Emacs auto saves.")
(make-directory autosave-dir t)

  abbrev-file-name (expand-file-name
                    "abbrev_defs.el" user-data-directory)

(setq
 backup-directory-alist (list (cons "." backup-dir))
 auto-save-file-name-transforms `((".*" ,autosave-dir t)))


;;;; lread.c

(add-to-list 'load-suffixes ".el.gpg")


;;;; load-custom-file

(setq custom-file (expand-file-name
                   "custom-set-variables.el" user-data-directory))
(load custom-file 'noerror 'nomessage)


;;;; environment vars

(setenv "NODE_NO_READLINE" "1") ;; nodejs


;;;; enable disabled commands

(--each '(narrow-to-defun narrow-to-page narrow-to-region
                          upcase-region downcase-region
                          scroll-left dired-find-alternate-file erase-buffer)
  (put it 'disabled nil))


;;;; key bindings

;;;; global unset

(global-unset-key (kbd "M-o"))
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))


;;;; define and bind custom key maps

(defvar my-files-map)
(defvar my-dirs-map)
(defvar my-buffers-map)
(defvar my-other-map)
(define-prefix-command 'my-files-map)
(define-prefix-command 'my-dirs-map)
(define-prefix-command 'my-buffers-map)
(define-prefix-command 'my-other-map)
(global-set-key (kbd "C-x f") my-files-map)
(global-set-key (kbd "C-x d") my-dirs-map)
(global-set-key (kbd "C-x b") my-buffers-map)
(global-set-key (kbd "C-c o") my-other-map)
(global-set-key (kbd "C-h o") my-other-map)
(global-set-key (kbd "M-o") my-other-map)


(defvar region-bindings-mode-map
  (let ((region-bindings-mode-map (make-sparse-keymap)))
    region-bindings-mode-map)
  "Keymaps for command `region-bindings-mode-map'.")


;;; mode hook lists

(defmacro hook-into-modes (func modes)
  "Add hook `FUNC' to multiple `MODES'."
  `(dolist (mode-hook ,modes)
     (add-hook mode-hook ,func)))


;;;; prog modes

(defvar my-prog-mode-hooks
  '(prog-mode-hook
    clojure-mode-hook
    emacs-lisp-mode-hook
    go-mode-hook
    go-ts-mode
    groovy-mode-hook
    haskell-mode-hook
    js-mode-hook
    js2-mode-hook
    kivy-mode-hook
    pyhon-mode-hook
    qml-mode-hook
    ruby-mode-hook))


;;;; lisp modes

(defvar my-lisp-modes
  '(clojure-mode
    emacs-lisp-mode
    ielm-mode
    inferior-emacs-lisp-mode
    inferior-lisp-mode
    lisp-interaction-mode
    lisp-mode
    slime-repl-mode))

(defvar my-lisp-mode-hooks
  (mapcar (function
           (lambda (mode)
             (intern
              (concat (symbol-name mode) "-hook"))))
          my-lisp-modes))


;;;; significant whitespace

(defvar my-significant-whitespace-mode-hooks
  '(haskell-mode-hook
    kivy-mode-hook
    python-mode-hook
    python-ts-mode-hook))


;;;; markup modes

(defvar my-markup-mode-hooks-1
  '(gfm-mode-hook
    rst-mode-hook))

;; org-mode is a bit special so i dont want it among the other ones.
(defvar my-markup-mode-hooks-2
  '(org-mode-hook))


;;;; my-html-like-modes

(defvar my-html-like-modes
  '(handlebars
    html-mode
    nxml-mode
    web-mode))


(defvar my-html-like-mode-hooks
  '(handlebars-mode-hook
    html-mode-hook
    nxml-mode-hook
    web-mode-hook))


;;;; my-css-like modes

(defvar my-css-like-modes
  '(css-mode
    scss-mode))

(defvar my-css-like-mode-hooks
  '(css-mode-hook
    scss-mode-hook))


;;; core

;; configuration for things that ar built in to emacs and are not available
;; throuigh a feature that can be require'ed.


;;;; custom variables

(defvar my-normal-cursor-type '(bar . 3))


;;;; defined in elisp source

;;;;; ansi-color

(setq ansi-color-for-comint-mode t)


;;;;; apropos

(setq apropos-do-all t)


;;;;; browse-url

(when (eq 'gnu/linux system-type)
  (setq
   browse-url-browser-function 'browse-url-generic
   browse-url-generic-program "sensible-browser"))


;;;;; cedet

(setq srecode-map-save-file (expand-file-name
                             "srecode-map.el" user-data-directory))


;;;;; eshell

(setq eshell-directory-name (expand-file-name user-data-directory))


;;;;; files.el

(setq confirm-kill-emacs 'yes-or-no-p)


;;;;; font locking

;; (setq font-lock-global-modes '(not web-mode))
(unless noninteractive
  (setq font-lock-maximum-decoration t)
  ;; (global-font-lock-mode t)
  )


;;;;; isearch

(setq search-default-mode #'char-fold-to-regexp)


;;;;; jit-lock

(setq jit-lock-stealth-time nil
      jit-lock-stealth-nice 0.03
      jit-lock-stealth-load 200
      jit-lock-stealth-verbose nil
      jit-lock-chunk-size 500
      ;; jit-lock-defer-time 0.05
      )


;;;;; man

(setq Man-notify-method 'pushy)


;;;;; message (for gmail)

;; set up for gmail
(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-stream-type 'starttls
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587)


;;;;; minibuffer

(when (boundp 'completion-styles)
  (add-to-list ;; add initials to complete list
   'completion-styles 'initials t))


;;;;; mule / conding.c

(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(set-language-environment "UTF-8")
(define-coding-system-alias 'UTF-8 'utf-8)


;;;;; paragraphs

(setq sentence-end-double-space nil)


;;;;; silence successful auto saving

(defadvice do-auto-save (around do-auto-save-silent activate)
  (ad-set-arg 0 t)
  ad-do-it)


;;;;; simple

(setq column-number-mode t
      kill-whole-line nil
      shift-select-mode nil
      eval-expression-print-level nil
      idle-update-delay 0.5
      next-error-recenter '(4)
      next-error-verbose nil
      )

(and (fboundp 'x-cut-buffer-or-selection-value)
     (eq system-type 'gnu/linux)
     (setq interprogram-paste-function
           'x-cut-buffer-or-selection-value))


;;;;; subr

;; misc Emacs settings not directly related to loading a package
(defalias 'yes-or-no-p 'y-or-n-p)


;;;;; timeclock

(setq timeclock-use-display-time nil)


;;;;; tooltip

(setq tooltip-delay 0.8
      ;;tooltip-hide-delay 10
      ;;tooltip-recent-seconds 1
      x-gtk-use-system-tooltips nil)


;;;;; tracking

(setq tracking-most-recent-first t)


;;;;; url

(setq url-configuration-directory (expand-file-name
                                   "url/" user-data-directory))


;;;;; whitespace-mode

(setq-default whitespace-line-column nil)
(bind-key "M-o m w" 'whitespace-mode)


;;;;; window

(setq switch-to-buffer-preserve-window-point t)

;; (info "(elisp) Displaying Buffers")
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Displaying-Buffers.html

(setq display-buffer-alist nil)

(add-to-list 'display-buffer-alist
             `(,(rx bos "*rg*" eos)
               (
                display-buffer-reuse-window
                display-buffer-no-window
                display-buffer-at-bottom
                )
               (inhibit-same-window . t)
               (window-height . 0.4)
               (window-min-height . 30)
               (reusable-frames . visible)
               ))

(add-to-list 'display-buffer-alist
             `(,(rx bos "*Flycheck errors*" eos)
               (
                display-buffer-reuse-window
                display-buffer-in-side-window
                )
               (reusable-frames . visible)
               (side . bottom)
               (window-height . 0.15)
               (window-min-height . 10)
               ))


;;;; defined in c source

;;;;; buffer.c

(setq-default tab-width 4
              indicate-empty-lines nil
              transient-mark-mode t
              fill-column 79
              fringes-outside-margins t
              mode-line-format (list
                                "%e"
                                'mode-line-front-space
                                'mode-line-mule-info
                                ;; 'mode-line-client
                                'mode-line-modified
                                'mode-line-remote
                                " "
                                'mode-line-buffer-identification
                                "  "
                                'mode-line-position
                                'smartrep-mode-line-string
                                '(vc-mode vc-mode)
                                "  "
                                'mode-line-modes
                                ;; 'mode-line-misc-info
                                'mode-line-end-spaces
                                )
              )


;;;;; coding.c

(setq-default locale-coding-system 'utf-8)
(transient-mark-mode 1)
(make-variable-buffer-local 'transient-mark-mode)
(put 'transient-mark-mode 'permanent-local t)


;;;;; dired.c

(setq-default completion-ignored-extensions
              '("-min.css" "-min.js" ".a" ".annot" ".aux" ".bbl" ".bbl" ".bin"
              ".blg" ".blg" ".bzr/" ".class" ".cma" ".cmi" ".cmo" ".cmt"
              ".cmti" ".cmx" ".cmxa" ".cp" ".cp" ".cps" ".cps" ".d64fsl"
              ".dfsl" ".dx32fsl" ".dx64fsl" ".dxl" ".elc" ".fas" ".fasl" ".fmt"
              ".fn" ".fn" ".fns" ".fns" ".fsl" ".fx32fsl" ".fx64fsl" ".git/"
              ".glo" ".glo" ".gmo" ".hg/" ".hi" ".idx" ".idx" ".ky" ".ky"
              ".kys" ".kys" ".la" ".lbin" ".lib" ".ln" ".lo" ".lof" ".lof"
              ".lot" ".lot" ".lx32fsl" ".lx64fsl" ".map" ".mem" ".min.css"
              ".min.js" ".mo" ".o" ".p64fsl" ".pfsl" ".pg" ".pg" ".pgs" ".pgs"
              ".pyc" ".pyo" ".pyx" ".rbc" ".sass-cache" ".so" ".sparcf" ".svn/"
              ".sx32fsl" ".sx64fsl" ".test" ".tfm" ".toc" ".tp" ".tp" ".tps"
              ".tps" ".ufsl" ".vr" ".vr" ".vrs" ".vrs" ".wx32fsl" ".wx64fsl"
              ".x86f" "CVS/" "_MTN/" "_darcs/" "~"))


;;;;; dispnew.c

(setq-default visible-bell nil)


;;;;; fileio.c

(setq-default delete-by-moving-to-trash t)


;;;;; filelock.c

(setq-default create-lockfiles nil)


;;;;; fns.c

(setq-default use-dialog-box nil)


;;;;; indent.c

(setq-default indent-tabs-mode nil)


;;;;; keyboard.c

(setq-default echo-keystrokes 0.1)


;;;;; lread.c

(setq-default load-prefer-newer t)


;;;;; minibuf.c

(setq-default enable-recursive-minibuffers nil) ;; NOTE
                                                ;; enable-recursive-minibuffers
                                                ;; this can be quite confusing


;;;;; undo.c

(setq-default undo-limit (* 80 1024 1024)
              undo-strong-limit (* 200 1024 1024)
              undo-outer-limit (* 50 1024 1024))


;;;;; xdisp.c

(setq-default frame-title-format
              '((:eval (if (buffer-file-name)
                           (concat
                            (abbreviate-file-name (file-name-nondirectory
                            buffer-file-name))
                            "  ("
                            (abbreviate-file-name (file-name-directory
                            buffer-file-name))
                            ")")
                         "%b"))
                " [%m] "
                (:eval (if (buffer-modified-p) " •")))
              scroll-step 1
              scroll-margin 0
              scroll-conservatively 10000
              scroll-up-aggressively 0.01
              scroll-down-aggressively 0.01
              auto-window-vscroll nil
              truncate-partial-width-windows 50)
(setq-default
 ;; scroll-preserve-screen-position t
 scroll-preserve-screen-position 1)


;;;;; xfns.c

(setq-default x-gtk-file-dialog-help-text nil)


;;;;; xterm.c

(setq-default x-underline-at-descent-line t)


;;; key-bindings

;; (and (eq system-type 'darwin)
;;      window-system
;;      (setq
;;       mac-command-modifier 'meta
;;       mac-right-command-modifier 'super
;;       ;; mac-control-modifier
;;       ;; mac-right-control-modifier
;;       ;; mac-function-modifier
;;       mac-option-modifier 'super
;;       mac-right-option-modifier nil
;;       ))

;; This is very special because what I have done to my caps lock key.
(define-key special-event-map (kbd "<key-17>") 'ignore)
(define-key special-event-map (kbd "<M-key-17>") 'ignore)
;; (unbind-key "C-x C-l") ;; downcase region
;; (unbind-key "C-x C-u") ;; upcase region
;; (unbind-key "M-l") ;; downcase word
;; (unbind-key "M-u") ;; upcase word
;; (unbind-key "M-c") ;; capitalize word
(unbind-key "<mouse-3>")

;; make home/end behave the same as elsewhere on 'darwin
(bind-key "<home>" 'beginning-of-line)
(bind-key "<end>" 'end-of-line)
(bind-key "C-h B" 'describe-personal-keybindings)
(bind-key "C-h I" 'info)
(bind-key "C-h s" (lambda nil (interactive) (switch-to-buffer "*scratch*")))
(bind-key "C-?" 'undo)
(bind-key "C-_" 'redo)
(bind-key* "C-." 'undo) ;; NOTE this does not work in terminals
(bind-key* "C-," 'redo) ;; NOTE this does not work in terminals
(bind-key "<f5>" (lambda nil (interactive) (jump-to-register ?5)))
(bind-key "<f6>" (lambda nil (interactive) (jump-to-register ?6)))
(bind-key "<f7>" (lambda nil (interactive) (jump-to-register ?7)))
(bind-key "<f8>" (lambda nil (interactive) (jump-to-register ?8)))
(bind-key "C-<f5>" (lambda nil (interactive) (window-configuration-to-register ?5)))
(bind-key "C-<f6>" (lambda nil (interactive) (window-configuration-to-register ?6)))
(bind-key "C-<f7>" (lambda nil (interactive) (window-configuration-to-register ?7)))
(bind-key "C-<f8>" (lambda nil (interactive) (window-configuration-to-register ?8)))
(bind-key "S-<f5>" (lambda nil (interactive) (window-configuration-to-register ?5)))
(bind-key "S-<f6>" (lambda nil (interactive) (window-configuration-to-register ?6)))
(bind-key "S-<f7>" (lambda nil (interactive) (window-configuration-to-register ?7)))
(bind-key "S-<f8>" (lambda nil (interactive) (window-configuration-to-register ?8)))

;; (bind-key "<f9>" 'previous-buffer)
;; (bind-key "<f10>" 'next-buffer)
;; (bind-key "<f11>" 'switch-to-buffer)
;; (bind-key "<f9>" 'ibuffer)
;; (bind-key "<f5>" 'ibuffer)

(bind-key "C-H-n" 'forward-paragraph)
(bind-key "C-H-p" 'backward-paragraph)
(bind-key "C-s-n" 'forward-paragraph)
(bind-key "C-s-p" 'backward-paragraph)

(bind-key "M-H-n" 'next-error)
(bind-key "M-H-p" 'previous-error)
;; (bind-key "M-s-n" 'next-error)
;; (bind-key "M-s-p" 'previous-error)

;; (bind-key "S-C-<left>" 'shrink-window-horizontally)
;; (bind-key "S-C-<right>" 'enlarge-window-horizontally)
;; (bind-key "S-C-<down>" 'shrink-window)
;; (bind-key "S-C-<up>" 'enlarge-window)
(unbind-key "C-<prior>")
(unbind-key "C-<next>")

(bind-key "<M-prior>" 'previous-error)
(bind-key "<M-next>" 'next-error)

(bind-key "<f9>" 'my-flycheck-next-error)
(bind-key "<f10>" 'my-flycheck-previous-error)

(bind-key "<f11>" 'next-error)
(bind-key "<f12>" 'previous-error)


;; lisp-mode TODO: maybe move?
;;(define-key read-expression-map (kbd "TAB") 'lisp-complete-symbol)
(bind-key "M-o l"  'toggle-truncate-lines)
;; (bind-key "C-c l" "lambda" lisp-mode-shared-map)
(bind-key "RET" 'reindent-then-newline-and-indent lisp-mode-shared-map)
(bind-key "C-\\" 'lisp-complete-symbol lisp-mode-shared-map)
(bind-key "C-c v" 'eval-buffer lisp-mode-shared-map)
(bind-key* "M-j" 'my-join-line)
(bind-key "C-a" 'beginning-of-line-or-indentation)
(bind-key "H-n" 'my-scroll-other-window-up)
(bind-key "H-p" 'my-scroll-other-window-down)
(bind-key "C-x 4 n" 'clone-buffer-and-narrow-to-function)
(bind-key "C-x o" 'my-other-window)
(bind-key "C-x C-o" 'my-other-frame)

;; NOTE while being handy using smartrep with other-*
;;      functions is a bit slow for some reason.
;;
;; (smartrep-define-key
;;     global-map
;;     "C-x"
;;   '(("o" . my-other-window)
;;     ("C-o" . my-other-frame)))



(bind-key "C-x f f" 'find-file)
(bind-key "C-x b b" 'switch-to-buffer)
(bind-key "C-x C-b" 'switch-to-buffer)

(bind-key "C-x b ." 'ibuffer)

(bind-key "C-x b C-<SPC>" 'previous-buffer)
(bind-key "C-x b C-n" 'next-buffer)

;; (bind-key "C-s"  'isearch-forward-regexp)
;; (bind-key "C-r"  'isearch-backward-regexp)
(bind-key "C-s"  'isearch-forward)
(bind-key "C-r"  'isearch-backward)
(bind-key "C-S-s"  'isearch-forward-symbol-at-point)
(bind-key "C-M-s"  'isearch-forward-regexp)
(bind-key "C-M-r"  'isearch-backward-regexp)

(global-set-key (kbd "C-x C-1") 'delete-other-windows)
(global-set-key (kbd "C-x C-2") 'split-window-below)
(global-set-key (kbd "C-x C-3") 'split-window-right)
(global-set-key (kbd "C-x C-0") 'delete-window)

(unbind-key "M-t")
(bind-keys
 ;; :map undo-tree-visualizer-mode-map
 :prefix-map my-transpose-map
 :prefix "M-t"
 ("c" . transpose-chars)
 ("w" . transpose-words)
 ("t" . transpose-words)
 ("M-t" . transpose-words)
 ("l" . transpose-lines)
 ("e" . transpose-sexps)
 ("s" . transpose-sentences)
 ("p" . transpose-paragraphs))

(defun hydra-goto/pre ()

    ;; (set-frame-parameter nil 'unsplittable t))
)
(defun hydra-goto/post ()
  ;; (set-frame-parameter nil 'unsplittable nil)

  )

(defun toggle-next-error-last-buffer ()
  (interactive)
  (when next-error-last-buffer
    (let ((win (get-buffer-window next-error-last-buffer)))
    (if win
        (delete-window win)
      (display-buffer next-error-last-buffer 'display-buffer-at-bottom) t)
    ;; (set-window-dedicated-p (display-buffer next-error-last-buffer 'display-buffer-at-bottom) t))
    )))

(defhydra hydra-goto (:hint none :pre hydra-goto/pre :post hydra-goto/post)
  "
(_j_/_k_ ERR _h_ src _y_ win) (_f_/_d_ HUNK) (_u_/_i_ FLYC _o_ lvl) (_r_/_e_ BUF) (GOTO line _g_ char _c_) . _v_ recenter
(_b_/_B_ bookmark) _U_ URL . _s_ imenu . _S_ scratch . _I_ ibuffer . _R_ rg-buf . _D_ dired . _G_ git-gutter"
  ;; ("h" first-error "first-error")
  ("h" next-error-select-buffer "n-err")
  ("j" my-next-error "n-err")
  ("k" my-previous-error "p-err")
  ("y" toggle-next-error-last-buffer  "open-err" )
  ;; ("n" next-error "n-err")
  ;; ("p" previous-error "p-err")

  ("f" my-git-gutter:next-hunk "n-hunk")
  ("d" my-git-gutter:previous-hunk "p-hunk")

  ("u" my-flycheck-next-error "n-flyc")
  ("i" my-flycheck-previous-error "p-flyc")
  ("o" my-flycheck-cycle-error-navigation-min-level "level")

  ("r" switch-to-next-buffer "n-buf")
  ("e" switch-to-prev-buffer "p-buf")

  ("g" goto-line-with-feedback "line" :exit t)
  ("M-g" goto-line-with-feedback "line" :exit t)
  ("c" goto-char "char" :exit t)

  ("G" hydra-git-gutter/body :exit t)

  ("v" my-recenter-top-bottom "recenter")

  ;; goto various windows
  ("U" browse-url "browse-url" :exit t)
  ("S" scratch-buffer "scratch" :exit t)
  ("R" (lambda () (interactive) (switch-to-buffer "*rg*")) "*rg*" :exit t)
  ("I" (lambda () (interactive) (ibuffer)) "*ibuf*" :exit t)
  ("D" (lambda () (interactive) (dired ".")) "*dired*" :exit t)
  ("s" helm-imenu "imenu" :exit t)

  ("b" bookmark-jump "*bookmarks*" :exit t)
  ("B" helm-bookmarks "*bm-set*" :exit t)

  ("q" nil nil :exit t))
;; (hydra-compact-hint 'hydra-goto)
(bind-key  "M-g" 'hydra-goto/body global-map)


(defhydra hydra-search (:hint nil :foreign-keys warn :exit t)
  ""
  ;; ("" nil nil :column "rg")
  ("r" rg "rg")
  ("s" rg-project "rg-proj")
  ("f" rg-dwim-current-file "rg-dwim-current-file")
  ("R" (lambda () (interactive) (switch-to-buffer "*rg*")) "*rg*" :exit t)
  ;; ("" nil nil :column "occur")
  ("o" occur "occur")
  ("l" loccur "loccur")
  ("m" moccur "moccur")

  ;; ("" nil nil :column "misc")
  ("G" google-this "google-this")
  ("." isearch-forward-symbol-at-point "isearch-symbol")
  ;; ("q" nil nil :exit t)
  )
(hydra-compact-hint 'hydra-search)
(bind-key "M-s" 'hydra-search/body global-map)
(bind-key "s" 'hydra-search/body  my-other-map)


;;; gui packages

;; it's important to set up ui before the bulk of the settings because theme
;; and fonts should load asap


;;;; set vertical border char

(eval
 '(set-display-table-slot standard-display-table
                          'vertical-border
                          (make-glyph-code ?┃)))


;;;; theme

;;;;; define theme-bright / theme-dark

(defvar mf-theme-light nil "A light theme.")
(defvar mf-theme-dark nil "A dark theme.")


;;;;; install solarized-theme

;;;;; solarized-theme

(use-package solarized-theme
  :ensure t
  :if (or
       window-system
       (equal "xterm-24bit" (getenv-internal "TERM" initial-environment)))
  :init
  (progn
    (setq solarized-use-less-bold t
          solarized-use-more-italic t
          solarized-emphasize-indicators nil
          solarized-distinct-fringe-background nil
          solarized-high-contrast-mode-line nil))
  :config
  (progn
    (load "solarized-theme-autoloads" nil t)
    (setq mf-theme-dark 'my-solarized-dark
          mf-theme-light 'my-solarized-light)))

;; (use-package zenburn-theme
;;   :ensure t
;;   :config
;;   (progn
;;     (load "zenburn-theme-autoloads" nil t)))

;; (use-package anti-zenburn-theme
;;       :ensure t
;;       :config
;;       (load "anti-zenburn-theme-autoloads" nil t))


;;;;; post-change-theme

(defun post-change-theme ()
  "Set some stuff that"
  (set-face-inverse-video-p 'vertical-border nil)
  (set-face-background 'vertical-border (face-background 'default)))


;;;;; mf-dark-theme


(defvar mf-current nil "last loaded theme")

(defun mf-dark-theme ()
  "Switch to dark mode (dark color theme)."
  (interactive)
  (when mf-theme-dark
    (load-theme mf-theme-dark t)
    (setq mf-current 'dark)
    (post-change-theme)))


;;;;; mf-light-theme

(defun mf-light-theme ()
  "Switch to light mode (light color theme)."
  (interactive)
  (when mf-theme-light
    (load-theme mf-theme-light t)
    (setq mf-current 'light)
    (post-change-theme)))


;;;;; mf-toggle-theme

(defun mf-toggle-theme ()
  "Toggle between light and dark modes."
  (interactive)
  (if (eq mf-current 'light)
      (mf-dark-theme)
    (mf-light-theme))
  (post-change-theme))


;;;;; global darkmode

(defun mf-mode-theme (&optional force)
  (let ((m (if (dark-mode-p) 'dark 'light)))
    (if (or force (not (eq mf-current m)))
        (if  (eq m 'dark)
            (mf-dark-theme)
          (mf-light-theme)))))

(defun dark-mode-p ()
  (file-exists-p "~/.config/darkmode"))

(and (not mf-current)
   (not noninteractive)

   (mf-mode-theme))

(add-hook 'focus-in-hook
          #'(lambda ()
              (mf-mode-theme)))


;;;; simple-modeline

(use-package simple-modeline
  :ensure t
  ;; :disabled t
  :commands (simple-modeline-mode)
  :init
  (progn
    (simple-modeline-mode))
  :config
  (progn
    (setq simple-modeline--mode-line
          '((:eval
             (simple-modeline--format
              '(simple-modeline-segment-modified
                simple-modeline-segment-position
                simple-modeline-segment-buffer-name)
              '(simple-modeline-segment-minor-modes
                simple-modeline-segment-input-method
                simple-modeline-segment-eol
                simple-modeline-segment-encoding
                simple-modeline-segment-vc
                ;; simple-modeline-segment-misc-info
                simple-modeline-segment-process
                simple-modeline-segment-major-mode)))))))


;;;; smart-mode-line

;; (setq sml/theme nil)
;; (use-package smart-mode-line
;;   :disabled t
;;   :ensure t
;;   :if (and
;;        (not noninteractive)
;;        (not (not window-system))
;;        )
;;   :commands (sml/setup)
;;   :preface
;;   (progn
;;     (load "smart-mode-line-autoloads" t t))
;;   :init
;;   (progn
;;     (customize-set-variable 'sml/projectile-replacement-format ":p/%s:")
;;     (setq
;;      sml/modified-char "m"
;;      sml/read-only-char "r"
;;      sml/outside-modified-char "M"
;;      sml/mule-info ""
;;      sml/shorten-modes nil
;;      sml/replacer-regexp-list
;;      '(("^~/\.virtualenvs/\\([^/]+\\)" ":e/\\1:")
;;        ("^~/\.local/share/virtualenvs/\\([^/]+\\)" ":e/\\1:")
;;        ("^/sudo:.*:" ":su:")
;;        ("^~/dropbox/" ":db:")))
;;     (sml/setup)))


;;;; dynamic-fonts

(defvar my-monospaced-font "Pragmata Pro-12")
(defvar my-variable-pitch-font "Go-12.5")
;; (defvar my-variable-pitch-font "Input Sans Compressed-11.8")
;; (defvar my-monospaced-font "Input Mono Compressed-11.8")

(cond
 ((string-prefix-p "transwhale" system-name)
  (setq my-monospaced-font "Pragmata Pro-12"
        my-variable-pitch-font "Go-12.5"
        dynamic-fonts-preferred-monospace-point-size 12
        dynamic-fonts-preferred-proportional-point-size 12.5))

 ((string-prefix-p "prizza" system-name)
  (setq my-monospaced-font "Pragmata Pro-15"
        my-variable-pitch-font "Go-15.5"
        dynamic-fonts-preferred-monospace-point-size 15
        dynamic-fonts-preferred-proportional-point-size 15.5))

 ((string-prefix-p "fogskum" system-name)
  (setq my-monospaced-font "Pragmata Pro-15"
        my-variable-pitch-font "Go-15.5"
        dynamic-fonts-preferred-monospace-point-size 15
        dynamic-fonts-preferred-proportional-point-size 15.5))

 ((string-prefix-p "crangy" system-name)
  (setq my-monospaced-font "Pragmata Pro-17"
        my-variable-pitch-font "Go-17"
        dynamic-fonts-preferred-monospace-point-size 17
        dynamic-fonts-preferred-proportional-point-size 17))

 ((string-prefix-p "dungen" system-name)
  (setq my-monospaced-font "Pragmata Pro-16"
        my-variable-pitch-font "Go-16"
        dynamic-fonts-preferred-monospace-point-size 17
        dynamic-fonts-preferred-proportional-point-size 17))
 )

(use-package dynamic-fonts
  :ensure t
  :commands (dynamic-fonts-setup)
  :init
  (progn
    (setq
     dynamic-fonts-preferred-monospace-fonts
     '("PragmataPro" "Consolas" "Monaco" "Menlo" "DejaVu Sans Mono"
       "Droid Sans Mono Pro" "Droid Sans Mono" "Inconsolata" "Source Code Pro"
       "Lucida Console" "Envy Code R" "Andale Mono" "Lucida Sans Typewriter"
       "Lucida Typewriter" "Panic Sans" "Bitstream Vera Sans Mono"
       "Excalibur Monospace" "Courier New" "Courier" "Cousine" "Lekton"
       "Ubuntu Mono" "Liberation Mono" "BPmono" "Anonymous Pro"
       "ProFontWindows")
     dynamic-fonts-preferred-monospace-point-size 12
     dynamic-fonts-preferred-proportional-fonts
     '("Go" "PT Sans" "Lucida Grande" "Segoe UI" "DejaVu Sans" "Bitstream Vera"
       "Tahoma" "Verdana" "Helvetica" "Arial Unicode MS" "Arial")
     dynamic-fonts-preferred-proportional-point-size 12.5)



    (defun my-set-fonts  ()
      (interactive)
      (when window-system
        (condition-case nil
            (progn
              (set-face-attribute 'default nil :font my-monospaced-font)
              ;; (set-face-attribute 'default nil :font my-monospaced-font :width 'ultra-condensed :weight 'normal )
              (set-face-attribute 'fixed-pitch nil :font my-monospaced-font)
              (set-face-attribute 'variable-pitch nil :font my-variable-pitch-font)
              (set-fontset-font "fontset-default" 'unicode '("PragmataPro" . "iso10646-1")))
          (error
           (progn
             (message
              "Setting default fonts failed, running dynamic-fonts-setup...")
             (dynamic-fonts-setup))))))
    (add-hook 'after-init-hook 'my-set-fonts t)))


;;;; my-pulse

(defvar my-pulse-enabled t)

(when my-pulse-enabled
  (require 'pulse))

(defun my-pulse-p ()
  (and my-pulse-enabled
     (pulse-available-p)
     (not (window-minibuffer-p))
     (not (eq major-mode 'mu4e-headers-mode))
     (not (eq major-mode 'magit-status-mode))
     (not (eq major-mode 'image-mode))
     (not (and (boundp 'git-commit-mode) git-commit-mode))))

(defun my-pulse--setup ()
  (if (display-graphic-p) ;; flickers too much in terminals
      (setq pulse-iterations 10
            pulse-delay .04)
    (setq pulse-iterations 1
          pulse-delay .45))
    (copy-face 'pulse-highlight-start-face 'my-pulse-face))

(defun my-pulse-region (start end &optional extend)
  (when (my-pulse-p)
    (my-pulse--setup)
    (when extend
      (set-face-attribute 'my-pulse-face nil :extend t))
    (pulse-momentary-highlight-region start end 'my-pulse-face)))

(defun my-pulse-line (&rest r)
  (when (my-pulse-p)
    (my-pulse--setup)
    (set-face-attribute 'my-pulse-face nil :extend t)
    (pulse-momentary-highlight-one-line (point) 'my-pulse-face)))

(defun my-pulse-rest-of-line (&rest r)
  (when (my-pulse-p)
    (save-excursion
      (let ((beg (point)))
        (end-of-line)
        (when (not (eobp))
          (forward-char 1))
        (my-pulse-region beg (point) t)
        nil))))

(defun my-pulse-defun()
  "Flash current defun"
  (interactive)
  (save-restriction
    (narrow-to-defun)
    (my-pulse-region (point-min) (point-max))))

(defvar my-pulse-timer nil)
(defun my-pulse-cancel-timer (&rest r)
  (when
      (fboundp 'pulse-momentary-unhighlight)
    (pulse-momentary-unhighlight))
  (when my-pulse-timer
    (cancel-timer my-pulse-timer)))

(defun my-pulse-set-timer (time)
  (when (my-pulse-p)
    (my-pulse-cancel-timer)
    (setq my-pulse-timer
          (run-with-timer
           time nil
           '(lambda () (my-pulse-line))))))

(defun my-pulse-now (&rest r)
  (my-pulse-set-timer 0.02))

(defun my-pulse-soon (&rest r)
  (my-pulse-set-timer 0.2))

(defun my-pulse-later (&rest r)
  (my-pulse-set-timer 0.35))

(when my-pulse-enabled
  ;; hooks
  (add-hook 'imenu-after-jump-hook 'my-pulse-line nil t)
  (add-hook 'focus-in-hook 'my-pulse-later)
  (add-hook 'focus-out-hook 'my-pulse-cancel-timer)
  (add-hook 'projectile-find-file-hook 'my-pulse-now)
   ;; advices
  (advice-add 'ibuffer-visit-buffer :after 'my-pulse-now)
  (advice-add 'magit-diff-visit-file :after 'my-pulse-now)
  (advice-add 'ido-find-file :after 'my-pulse-now)
  (advice-add 'direx:generic-find-item :after 'my-pulse-now)
  (advice-add 'direx:generic-view-item :after 'my-pulse-now)
  ;; end
  )


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
                              (f-entries "elisp/" nil t))
                     (--map (s-chop-prefix (s-concat default-directory "/") it))
                     (mapcar #'(lambda (x)
                                 (if (s-matches-p
                                      (rx
                                       (or (and "."
                                                (or "md" "markdown" "org" "txt" "plu" "org.gpg")
                                                eol)
                                           (and bol "elisp/" (* any) (group "el" eol))))
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


;;;;; sudo-edit

(defun sudo-edit-current-file ()
  (interactive)
  (let ((my-file-name) ; fill this with the file to open
        (position))    ; if the file is already open save position
    (if (equal major-mode 'dired-mode) ; test if we are in dired-mode
        (progn
          (setq my-file-name (dired-get-file-for-visit))
          (find-alternate-file (prepare-tramp-sudo-string my-file-name)))
      (setq my-file-name (buffer-file-name); hopefully anything else is an already opened file
            position (point))
      (find-alternate-file (prepare-tramp-sudo-string my-file-name))
      (goto-char position))))

(defun prepare-tramp-sudo-string (tempfile)
  (if (file-remote-p tempfile)
      (let ((vec (tramp-dissect-file-name tempfile)))
        (tramp-make-tramp-file-name
         "sudo"
         (tramp-file-name-user nil)
         (tramp-file-name-host vec)
         (tramp-file-name-localname vec)
         (format "ssh:%s@%s|"
                 (tramp-file-name-user vec)
                 (tramp-file-name-host vec))))
    (concat "/sudo:root@localhost:" tempfile)))


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

(defun my-make-script-executable ()
  "If file starts with a shebang, make `buffer-file-name' executable"
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (when (and (looking-at "^#!")
                 (not (file-executable-p buffer-file-name)))
        (set-file-modes buffer-file-name
                        (logior (file-modes buffer-file-name) #o100))
        (message (concat "Made " buffer-file-name " executable"))))))

(when (not noninteractive)
  (add-hook 'after-save-hook 'my-make-script-executable))


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


;;;; faces, themes, fonts, looks

;;;;; adding line-prefix

(defun my-set-line-prefix ()
  (interactive)
  (setq line-prefix (propertize "│" 'face 'vertical-border)))
;; (hook-into-modes 'my-set-line-prefix my-prog-mode-hooks)


;;;;; setting fonts

;;;;;; fonts-set

(defun fonts-set (fixed-font variable-font &optional frame)
  (when window-system
    (condition-case nil
        (progn
          (set-face-attribute 'default frame :font fixed-font)
          (set-face-attribute 'fixed-pitch frame :font fixed-font))
      (error (message "Cannot set font '%s'" fixed-font)))

    (condition-case nil
        (set-face-attribute 'variable-pitch frame :font variable-font)
      (error (message "Cannot set font '%s'" variable-font)))))

(defun fonts-set-pragmata (arg)
  "Set PragmataPro/PT Sans for selected frame."
  (interactive "P")
  (let ((size (if arg arg 12)))
    (fonts-set (format "Pragmata Pro-%s" size)
               (format "Go-%s" size)
               (selected-frame))))


;;;;;; smaller fonts in certain modes

(defun my-set-text-scale-smaller ()
  (let ((amount -1))
    (when (and (or
                (not (boundp 'text-scale-mode-amount))
                (not (equal text-scale-mode-amount amount))))
      (text-scale-set amount))))

(hook-into-modes 'my-set-text-scale-smaller
                 '(ag-mode-hook
                   flycheck-error-list-mode-hook
                   go-traceback-mode-hook
                   ibuffer-mode-hook
                   magit-commit-mode-hook
                   python-django-mode-hook
                   magit-diff-mode-hook
                   magit-log-mode-hook
                   magit-status-mode-hook
                   vc-annotate-mode-hook
                   pt-search-mode-hook
                   grep-mode-hook
                   direx:direx-mode-hook
                   debugger-mode-hook
                   docker-images-mode-hook
                   compilation-mode-hook
                   docker-containers-mode-hook
                   treemacs-mode-hook))


(use-package helm
  :defer t
  :disabled t
  :commands (with-helm-buffer)
  :config
  (progn
    (add-hook 'helm-after-initialize-hook
              #'(lambda () (with-helm-buffer (my-set-text-scale-smaller))))))

(defadvice android-logcat (after smaller-font activate)
  (with-current-buffer (get-buffer "*android-logcat*")
    (my-set-text-scale-smaller)))

(defun my-set-text-scale-smaller-maybe ()
  (let ((bn (buffer-name)))
    (when
        (and
         (not my-text-scale-smaller-done)
         (s-starts-with? "*[Django: " bn))
      (setq-local my-set-text-scale-smaller-done t)
      (my-set-text-scale-smaller))))

(defvar  my-text-scale-smaller-done nil  "smaller font scale")

;; (add-hook 'window-configuration-change-hook 'my-set-text-scale-smaller-maybe)
(add-hook 'comint-mode-hook 'my-set-text-scale-smaller-maybe)


;;;;; toggle line spacing

(defun toggle-line-spacing ()
  "Toggle line spacing between no extra space to extra half line height."
  (interactive)
  (if (eq line-spacing nil)
      (setq-default line-spacing 0.2)
    (setq-default line-spacing nil))
  (redraw-display))


;;;;; jit-lock-defer-fontification

(defun jit-lock-defer-fontification ()
  (interactive)
  (make-local-variable 'jit-lock-stealth-timer)
  (make-local-variable 'jit-lock-stealth-repeat-timer)
  (make-local-variable 'jit-lock-context-timer)
  (make-local-variable 'jit-lock-defer-timer)
  (setq-local jit-lock-defer-time 0.1)
  (font-lock-mode -1)
  (font-lock-mode 1))


;;;;; scrolling

(hook-into-modes
 #'(lambda () (setq-local scroll-margin 3))
 my-prog-mode-hooks)

(defun set-browse-scrolling ()
  (setq-local scroll-margin 40)
  (setq-local scroll-step 60)
  (setq-local scroll-down-aggressively nil)
  (setq-local scroll-up-aggressively nil)
  (setq-local scroll-conservatively 0))

;; (add-hook 'magit-mode-hook 'set-browse-scrolling)


;;;;; hide mode line

(defvar hidden-mode-line-mode)
(make-variable-buffer-local 'hidden-mode-line-mode)

(define-minor-mode hidden-mode-line-mode
  "Minor mode to hide the mode-line in the current buffer."
  :init-value nil
  :global n
  :group 'editing-basics
  (if hidden-mode-line-mode
      (progn
        (setq-local hide-mode-line mode-line-format)
        (setq mode-line-format nil))
    (setq mode-line-format hide-mode-line
          hide-mode-line nil))
  (force-mode-line-update)
  ;; Apparently force-mode-line-update is not always enough to
  ;; redisplay the mode-line
  (redraw-display)
  (when (and (called-interactively-p 'interactive)
             hidden-mode-line-mode)
    (run-with-idle-timer
     0 nil 'message
     (concat "Hidden Mode Line Mode enabled.  "
             "Use M-x hidden-mode-line-mode to make the mode-line appear."))))

(defun hide-fringes ()
  "Hides the modeline for this buffer"
  (interactive)
  (setq mode-line-format nil)
  (setq header-line-format nil))


;;;;; rename-modeline

(defmacro rename-modeline (package-name mode new-name)
  `(eval-after-load ,package-name
     '(defadvice ,mode (after rename-modeline activate)
        (setq mode-name ,new-name))))


;;;;; colors

;;;;;; color processing

(defun color-blend-name (color1 color2 alpha)
  "Blends COLOR1 onto COLOR2 using alpha "
  (apply 'color-rgb-to-hex
         (-zip-with '(lambda (it other)
                       (+ (* alpha it) (* other  (- 1 alpha))))
                    (color-name-to-rgb color1)
                    (color-name-to-rgb color2))))

(defun wash-out-color-name (color &optional degree)
  "Return a colour string specifying a washed-out version of COLOUR."
  (color-blend-name color
                    (face-attribute 'default :foreground)
                    (or degree 0.8)))


;;;;;; randomize buffer background

(defun randomize-buffer-background ()
  "changes current buffer's background to a random color (close to the defualt of this face)"
  (interactive)
  (progn
    (setq face-symbol (gensym "face-"))
    (make-face face-symbol)
    (buffer-face-set face-symbol)
    (setq rgb (mapcar
               (function
                (lambda (x) (let ((y (* 0.95 (+ x (/ (- (random 100) 50) 1200.0)))))
                              (if (> y 1) (- 2 y) (if (< y 0) (- y) y)))))
               (color-name-to-rgb (face-background 'default))))
    (setq new-color (color-rgb-to-hex (car rgb) (car (cdr rgb)) (car (cdr (cdr rgb)))))
    (set-face-background face-symbol new-color) (message (concat "color changed to " new-color))))


;;;;;; wash out faces

(defun wash-out-face (face &optional degree base-color)
  "Make the foreground colour of FACE appear a bit more pale."
  (let* ((prop (if base-color :background :foreground))
         (color (face-attribute face prop)))
    (unless (eq color 'unspecified)
      (set-face-attribute face nil
                          prop (wash-out-color-name color degree)))))

(defun find-faces (regexp)
  "Return a list of all faces whose names match REGEXP."
  (delq nil
        (mapcar (lambda (face)
                  (and (string-match regexp
                                     (symbol-name face))
                       face))
                (face-list))))

(defun wash-out-fontlock-faces (degree)
  (--each (face-list)
    (let ((name (symbol-name it)))
      (cond
       ((s-match "^font-lock-comment" name) t)
       ((s-match "^isearch" name)
        (wash-out-face it (/ degree 4) t))
       ((s-match "^ahs-\\\|^sp-show-pair-.*match\\\|^font-lock-warning-face\\\|^anzu" name)
        (wash-out-face it (/ degree 4)))
       ((s-match "^font-lock\\\|^org-\\\|^web-mode" name)
        (wash-out-face it degree))))))

(defun wash-out-faces ()
  (interactive)
  (wash-out-fontlock-faces 0.9))


;;;;; cursor-style

(defun cursor-style-update-action ()
  (when (bound-and-true-p cua-normal-cursor-color)
    (let* ((current-cursor-color (cdr (assq 'cursor-color (frame-parameters))))
           (cursor-style (cond
                          ((bound-and-true-p region-bindings-mode) (list "#d33682" '(bar . 10) t))
                          ((bound-and-true-p god-local-mode) (list "#268bd2" 'box nil))
                          ((bound-and-true-p buffer-read-only) (list "#859900" 'box nil))
                          (t (list cua-normal-cursor-color my-normal-cursor-type t)))))
      (unless (equal (nth 0 cursor-style) current-cursor-color)
        (set-cursor-color (nth 0 cursor-style)))
      (unless (equal (nth 1 cursor-style) cursor-type)
        (setq cursor-type (nth 1 cursor-style)))
      (unless (equal (nth 2 cursor-style) blink-cursor-mode)
        (blink-cursor-mode (or (nth 2 cursor-style) -1))))))

(defvar cursor-style-timer nil)
(defun cursor-style-update ()
  (when cursor-style-timer
    (cancel-timer cursor-style-timer))
  (setq cursor-style-timer
        (run-with-idle-timer 0.2 nil 'cursor-style-update-action)))

(hook-into-modes 'cursor-style-update
                 '(activate-mark-hook
                   deactivate-mark-hook
                   region-bindings-mode-hook
                   window-configuration-change-hook
                   minibuffer-setup-hook
                   minibuffer-exit-hook
                   god-mode-enabled-hook
                   god-mode-disabled-hook
                   god-local-mode-hook
                   read-only-mode-hook
                   after-change-major-mode-hook
                   focus-in-hook
                   focus-out-hook
                   ))

(defadvice hardhat-local-hook (after cursor-style-update activate)
  (cursor-style-update))


;;;; windows / frames

;;;;; windows

;;;;; other-window-or-prompt

(defun other-window-or-prompt (count &optional all-frames)
  "Calls `other-window' as you'd expect from \\[C-x o] with the exception when
there is an active prompt. In this case the minibuffer
window will become active. If the prompt was active on a different frame than
the current one that frame will be gain focus."
  (interactive "p")
  (if (minibuffer-prompt)
      (unwind-protect
          (let* ((minibuf (active-minibuffer-window))
                 (minibuf-frame (window-frame minibuf)))

            (unless (equal minibuf-frame (selected-frame))
              (select-frame-set-input-focus minibuf-frame))
            ;; (select-frame minibuf-frame)
            ;; (raise-frame minibuf-frame))

            (when (window-live-p minibuf)
              (select-window minibuf))))
    (other-window count all-frames)))


;;;;;; dedicated-mode

(defvar dedicated-mode nil
  "Mode variable for dedicated minor mode.")
(make-variable-buffer-local 'dedicated-mode)

(defun dedicated-mode (&optional arg)
  "Dedicated minor mode."
  (interactive "P")
  (setq dedicated-mode (not dedicated-mode))
  (set-window-dedicated-p (selected-window) dedicated-mode)
  (if (not (assq 'dedicated-mode minor-mode-alist))
      (setq minor-mode-alist
            (cons '(dedicated-mode " D")
                  minor-mode-alist))))

(bind-key "M-o m d" 'dedicated-mode)


;;;;;; select next/prev window

(defun select-next-window ()
  "Switch to the next window"
  (interactive)
  (other-window 1))

(defun select-previous-window ()
  "Switch to the previous window"
  (interactive)
  (other-window -1))


;;;;;; my-swap-window

(defun my-swap-window (&optional other-window)
  "Swap buffers between other window."
  (interactive)
  (if (<= (count-windows) 1)
      (message "Only one window, nothing to change.")
    (let* ((master (get-largest-window))
           (current (selected-window))
           (prev-master (frame-parameter nil 'win/master))
           (prev-other (frame-parameter nil 'win/other))
           (next-master master)
           (next-other current))
      (when (eq current master)
        (cond
         ((and (eq prev-master master) prev-other)
          (setq next-master prev-other
                new-other prev-master))
         ((eq 2 (count-windows))
          (setq next-master (next-window)
                next-other current))
         (t ;; maybe it would be best to always do this
          (setq next-master (get-mru-window nil nil t)
                next-other current))))

      (set-frame-parameter nil 'win/master next-master)
      (set-frame-parameter nil 'win/other next-other)
      (window-swap-states next-master current nil))))

(bind-key "M-o M-<return>" 'my-swap-window)


;;;;; frames

;;;;;; creating frames

(defun new-floating-frame ()
  "Creates a new floating frame.
This is special to my xmonad configuration which floats windows named floating"
  (interactive)
  (make-frame '((name . "floating")
                (title . "emacs"))))

(defun make-frame-minimal ()
  "Some kind of minimal frame, for logs etc"
  (interactive)
  (let ((frame (make-frame '((name . "minimal-frame")
                             (minibuffer . nil)))))
    (select-frame frame)
    (my-set-fonts)
    (with-selected-frame frame
      (hide-fringes)
      (hidden-mode-line-mode 1))
    frame))

(defun make-frame-no-minibuffer ()
  "Some kind of minimal frame, for logs etc"
  (interactive)
  (let ((frame (make-frame '((name . "minimal-frame")
                             (minibuffer . nil)))))
    (select-frame frame)
    (my-set-fonts)
    frame))

(bind-key "C-x 5 3" 'make-frame-no-minibuffer)

(defun new-floating-center-frame ()
  "Creates a new floating frame.
This is special to my xmonad configuration which floats windows
named floating-center"
  (interactive)
  (make-frame '((name . "floating-center")
                (title . "emacs"))))

(defun new-floating-center-large-frame ()
  "Creates a new floating frame.
This is special to my xmonad configuration which floats windows
named floating-center"
  (interactive)
  (make-frame '((name . "floating-center-large")
                (title . "emacs"))))

(defun find-file-in-large-floating-frame (file)
  "Find file in large center floating frame."
  (interactive)
  (when (file-exists-p file)
    (let ((frame (new-floating-center-large-frame) ))
      (select-frame frame)
      (find-file file))))


;;;;;; intelligent close frame

(defun intelligent-close ()
  "quit a frame the same way no matter what kind of frame you are on.

This method, when bound to C-x C-c, allows you to close an emacs frame the
same way, whether it's the sole window you have open, or whether it's
a \"child\" frame of a \"parent\" frame.  If you're like me, and use emacs in
a windowing environment, you probably have lots of frames open at any given
time.  Well, it's a pain to remember to do Ctrl-x 5 0 to dispose of a child
frame, and to remember to do C-x C-x to close the main frame (and if you're
not careful, doing so will take all the child frames away with it).  This
is my solution to that: an intelligent close-frame operation that works in
all cases (even in an emacs -nw session).

  Stolen from http://www.dotemacs.de/dotfiles/BenjaminRutt.emacs.html."
  (interactive)
  (if (eq (car (visible-frame-list)) (selected-frame))
      ;;for parent/master frame...
      (if (> (length (visible-frame-list)) 1)
          ;;close a parent with children present
          (if (yes-or-no-p "Close window?")
              (delete-frame (selected-frame)))
        ;;close a parent with no children present
        (save-buffers-kill-emacs))
    ;;close a child frame
    (if (yes-or-no-p "Close window?")
        (delete-frame (selected-frame)))))


;;;;;; x urgency hint

;; let emacs blink when something interesting happens.
;; in KDE this marks the active Emacs icon in the tray.
(defun x-urgency-hint (frame arg &optional source)
  "Set the x-urgency hint for the frame to arg:

- If arg is nil, unset the urgency.
- If arg is any other value, set the urgency.

If you unset the urgency, you still have to visit the frame to
make the urgency setting disappear (at least in KDE)."
  (let* ((wm-hints (append (x-window-property
                            "WM_HINTS" frame "WM_HINTS"
                            source nil t) nil))
         (flags (car wm-hints)))
    ;; (message flags)
    (setcar wm-hints
            (if arg
                (logior flags #x00000100)
              (logand flags #x1ffffeff)))
    (x-change-window-property "WM_HINTS" wm-hints frame "WM_HINTS" 32 t)))

(defun x-urgent (&optional arg)
  "Mark the current emacs frame as requiring urgent attention.

With a prefix argument which does not equal a boolean value of
nil, remove the urgency flag (which might or might not change
display, depending on the window manager)."
  (interactive "P")
  (let (frame (car (car (cdr (current-frame-configuration)))))
    (x-urgency-hint frame (not arg))))

(when window-system
  (unbind-key "C-x C-c")
  ;; (bind-key "C-x C-c" 'intelligent-close)
  (bind-key "s-w" 'intelligent-close)
  )


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
  (set-temporary-overlay-map
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
  (while (looking-back "[-_]") (backward-word)))

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
    (display-line-numbers-mode -1)))

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
  nil nil nil
  :group 'easy-read
  :global t
  (if auto-window-margins-mode
      (progn
        (--map
         (with-selected-window it
           (auto-window-margins))
         (window-list))
        (add-hook 'window-configuration-change-hook 'auto-window-margins))
    (remove-hook  'window-configuration-change-hook 'auto-window-margins)
    (--map
     (set-window-margins it 0)
     (window-list))))

(define-minor-mode easy-read-mode
  "..."
  nil nil nil
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
  nil nil nil
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
  ;; The initial value.
  nil
  ;; The indicator for the mode line.
  nil
  ;; The minor mode bindings.
  nil
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


;;;;; helm popup frame

(defvar popup-frame--frame  nil)

(defun popup-frame--minibuffer-exit  ()
  (when (and
         popup-frame--frame
         (frame-live-p popup-frame--frame))
    (delete-frame popup-frame--frame t)
    (setq popup-frame--frame nil)))

(add-hook 'minibuffer-exit-hook
          'popup-frame--minibuffer-exit )

(defun popup-frame (f)
  (interactive)
  (setq popup-frame--frame (make-frame
                            '((name . "floating-center-large")
                              (title . "emacs popup minibuffer"))))
  (with-selected-frame popup-frame--frame
    (call-interactively f)))

(defun popup-frame-helm-for-files  ()
  (interactive)
  (popup-frame 'helm-for-files))
;; (bind-key "<menu>" 'popup-frame-helm-for-files)


;;;;; get-dwim-at-point

(defun get-dwim-at-point ()
  "If there's an active selection, return that.
Otherwise, get the symbol at point, as a string."
  (cond ((use-region-p)
         (buffer-substring-no-properties (region-beginning) (region-end)))
        ((symbol-at-point)
         (substring-no-properties
          (symbol-name (symbol-at-point))))))


;;;;; selective display menu


(defun create-temp-selective-display-keymap ()
  (set-temporary-overlay-map
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
  (format "%s:%d" (buffer-fiale-name) (line-number-at-pos)))

(defun position-to-kill-ring ()
  "Copy to the kill ring a string in the format \"file-name:line-number\"
for the current buffer's file name, and the line number at point."
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


;;; use-package

;; main package configuration section
(eval-and-compile (push `("use-package-start" ,(current-time)) init-times))


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


;;;; aes

(use-package aes
  :ensure t
  :commands (aes-insert-password))


;;;; ahg

(use-package ahg
  :ensure t
  :commands (ahg-log ahg-short-log ahg-status))

(eval-and-compile
  (setq magit-last-seen-setup-instructions "1.4.0"))


;;;; anchored-transpose

(use-package anchored-transpose
  :commands anchored-transpose)


;;;; android-mode

(use-package android-mode
  :ensure t
  :commands (android-mode android-logcat android-start-emulator
                          android-start-ddms)
  :init
  (progn
    (setq
     android-mode-sdk-dir "~/.opt/android-sdks"
     android-mode-avd "d")))


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


;;;; auth-source

(use-package auth-source
  :defer
  :init
  (progn
    (setq ;; auth.el
     auth-sources '("~/.authinfo.gpg"))))


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


;;;; bm

(use-package bm
  :ensure t
  :disabled t

  :commands (bm-next bm-previous bm-show-all bm-toggle bm-buffer-save
                     bm-buffer-save-all bm-repository-load bm-repository-save
                     bm-buffer-restore bm-buffer-restore-all bm-buffer-save
                     bm-repository-clear bm-remove-all-all-buffers)
  :bind (("C-c b n" . bm-next)
         ("C-c b p" . bm-previous)
         ("C-c b s" . bm-show-all))
  :init
  (progn


;;;;; helm-bm

    (use-package helm-bm
      :ensure t
      :disabled t
      :commands helm-bm
      :bind (
             ("C-c b b" . helm-bm)
             ("C-h u" . helm-bm))
      :config
      (progn
        (use-package bm)))
    (setq
     bm-repository-file (expand-file-name
                         "bm-repository" user-data-directory)
     bm-annotate-on-create t
     bm-cycle-all-buffers t
     bm-buffer-persistence t
     bm-repository-size 500)))


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

    (setq company-auto-complete #'my-company-visible-and-explicit-action-p)
    (setq company-frontends '(company-echo-metadata-frontend
                              company-pseudo-tooltip-unless-just-one-frontend-with-delay
                              company-preview-frontend))

    ))


;;;; compile

(use-package compile
  :defer
  :config
  (progn
    (add-to-list 'compilation-error-regexp-alist-alist
                 '(my-go . ("^\t+\\([^()\t\n]+\\):\\([0-9]+\\):? .*$" 1 2)) t)
    (setq compilation-ask-about-save nil
          compilation-error-regexp-alist
          '(
            my-go
            absoft
            ;; ada
            aix
            ant
            bash
            borland
            python-tracebacks-and-caml
            comma
            cucumber
            msft
            edg-1
            edg-2
            epc
            ftnchek
            iar
            ibm
            irix
            java
            ;; jikes-file
            ;; maven
            ;; jikes-line
            gcc-include
            ruby-Test::Unit
            gnu
            lcc
            makepp
            mips-1
            mips-2
            msft
            omake
            oracle
            perl
            php
            rxp
            ;; sparc-pascal-file
            ;; sparc-pascal-line
            ;; sparc-pascal-example
            sun
            ;; sun-ada
            ;; watcom
            4bsd
            gcov-file
            gcov-header
            gcov-nomark
            gcov-called-line
            gcov-never-called
            ;; perl--Pod::Checker
            ;; perl--Test
            ;; perl--Test2
            ;; perl--Test::Harness
            weblint
            )
          )
    (defun my-compilation-mode-hook ()
      ;; (jit-lock-defer-fontification)
      (setq truncate-lines t)
      (setq-local truncate-partial-width-windows nil))
    (add-hook 'compilation-mode-hook 'my-compilation-mode-hook)
    (add-hook 'compilation-minor-mode-hook 'my-compilation-mode-hook)
    (defun my-colorize-compilation-buffer ()
      (when (eq major-mode 'compilation-mode)

        (use-package ansi-color)
        (ansi-color-apply-on-region compilation-filter-start (point-max))))
    (add-hook 'compilation-filter-hook 'my-colorize-compilation-buffer)))


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
         )
  :config (setq ivy-initial-inputs-alist nil)
  :commands (counsel-mode
             counsel-outline
             counsel-rg
             counsel-git-grep
             counsel-fzf))


;;;; csharp-mode

(use-package csharp-mode
  ;; :ensure t
  :mode "\\.cs\\'")


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
  :bind (("C-x d d" . ido-dired))
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
              image-dired-thumb-width 200))
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
  ;; :disabled t  ;; direx is not compatible with emacs-29 (probably)
  ;; :ensure t
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
  :mode (("Dockerfile\\'" . dockerfile-mode))
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


;;;; edbi

(use-package edbi
  :disabled t
  :ensure t
  :commands edbi:open-db-viewer)


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
     ediff-split-window-function (if (string= system-name "transwhale") 'split-window-vertically
                                   'split-window-horizontally)
     ediff-merge-split-window-function ediff-split-window-function )

    (defun command-line-diff (switch)
      "Usage: emacs -diff file1 file2"
      (let ((file1 (pop command-line-args-left))
            (file2 (pop command-line-args-left)))
        (ediff file1 file2)))
    (add-to-list 'command-switch-alist '("diff" . command-line-diff))
    )

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
    (add-hook 'ediff-keymap-setup-hook 'add-d-to-ediff-mode-map)))


;;;; edit-color-stamp

(use-package edit-color-stamp
  :disabled t ;; color chooser widget does not compile atm
  :ensure t
  :commands edit-color-stamp)


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
  :commands (editorconfig-mode editorconfig-apply)
  :ensure t
  :diminish editorconfig-mode
  :init
  (progn
    (hook-into-modes #'editorconfig-mode my-css-like-mode-hooks)
    (hook-into-modes #'editorconfig-mode my-prog-mode-hooks)
    (hook-into-modes #'editorconfig-mode my-html-like-mode-hooks))
  :config
  (progn
    (require 'editorconfig-core)
    (setq editorconfig-get-properties-function 'editorconfig-core-get-properties-hash)
    (and (not noninteractive)
       (buffer-file-name)
       (editorconfig-apply))))


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


;;;; elfeed

(use-package elfeed
  :ensure t
  :commands elfeed)


;;;; erlang

(use-package erlang
  :ensure t
  :commands (erlang-mode))


;;;; eval-sexp-fu

(use-package eval-sexp-fu
  :ensure t
  :commands (eval-sexp-fu-flash-mode))


;;;; evil

(use-package evil
  :disabled t
  :ensure t
  :commands evil-mode
  :init
  (progn

    (use-package evil-matchit
      :ensure t
      :commands global-evil-machit-mode)))


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


;;;; extempore

(use-package extempore-mode
  :ensure t
  :mode ("\\.xtm\\'" . extempore-mode)
  :config
  (progn
    (setq user-extempore-directory
          (-first 'file-directory-p
                  (list
                   (expand-file-name "~/.opt/extempore/")
                   "/usr/local/opt/extempore/")))))


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


;;;; fill-column-indicator

(use-package fill-column-indicator
  :ensure t
  :commands (fci-mode
             turn-on-fci-mode)
  :init
  (progn
    (setq fci-rule-width 2)
    ;; TODO fci-mode disables visual-line-move and that causes next/prev-line to
    ;; skip multiple lines some times.. bugs
    ;; https://github.com/alpaker/Fill-Column-Indicator/issues/28
    ;; http://debbugs.gnu.org/cgi/bugreport.cgi?bug=13108

    ;; (when window-system (hook-into-modes #'turn-on-fci-mode
    ;; my-prog-mode-hooks))
    )
  :config
  (progn
    (defun fci-enabled-p ()
      (and (boundp 'fci-mode) fci-mode))
    (defvar fci-mode-suppressed nil)

    (defadvice popup-create (before suppress-fci-mode activate)
      "Suspend fci-mode while popups are visible"
      (let ((fci-enabled (fci-enabled-p)))
        (when fci-enabled
          (setq-local fci-mode-suppressed fci-enabled)
          (turn-off-fci-mode))))

    (defadvice popup-delete (after restore-fci-mode activate)
      "Restore fci-mode when all popups have closed"
      (when (and fci-mode-suppressed
                 (null popup-instances))
        (setq fci-mode-suppressed nil)
        (turn-on-fci-mode)))

    (defadvice browse-kill-ring (before suppress-fci-mode activate)
      "Suspend fci-mode while browse kill ring is active"
      (let ((fci-enabled (fci-enabled-p)))
        (when fci-enabled
          (setq-local fci-mode-suppressed fci-enabled)
          (turn-off-fci-mode))))

    (defadvice browse-kill-ring-quit (after restore-fci-mode activate)
      "Restore fci-mode when all popups have closed"
      (when fci-mode-suppressed)
      (setq fci-mode-suppressed nil)
      (turn-on-fci-mode))

    ;; Regenerate fci-mode line images after switching themes
    (defadvice enable-theme (after recompute-fci-face activate)
      (dolist (buffer (buffer-list))
        (with-current-buffer buffer
          (when (fci-enabled-p)
            (turn-on-fci-mode)))))))


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
              (return)))))

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

       ("\\.py\\'" (".kv"))
       ("\\.kv\\'" (".py"))


       ))))


;;;; floobits

(use-package floobits
  :ensure t
  :commands (floobits-join-workspace floobits-share-dir-public
                                     floobits-share-dir-private))


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
     flycheck-disabled-checkers '(javascript-jshint go-megacheck)
     flycheck-completion-system 'ido
     flycheck-standard-error-navigation nil
     flycheck-navigation-minimum-level 'info
     flycheck-error-list-minimum-level nil)
    (defun my-flycheck-cycle-error-navigation-min-level ()
      (interactive)
      (message "flycheck-navigation-minimum-level: %s"
               (dring-set-next-element
                'flycheck-navigation-minimum-level
                '('info 'warning 'error))))

    (defun my-node_modules-flycheck-hook ()
      (setq-local flycheck-executable-find #'flycheck-node_modules-executable-find))

    (add-hook 'js2-mode-hook 'my-node_modules-flycheck-hook)
    (add-hook 'js-mode-hook 'my-node_modules-flycheck-hook)
    (add-hook 'web-mode-hook 'my-node_modules-flycheck-hook)
    (add-hook 'typescript-mode 'my-node_modules-flycheck-hook)

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
    (add-hook 'haskell-mode-hook 'flycheck-turn-on-maybe))
  :config
  (progn

    ;; patch start
    ;;
    ;; TODO: patched because of https://github.com/flycheck/flycheck/issues/1856
    ;;
    (defun flycheck-error-level-interesting-p (err)
      "Check if ERR severity is >= `flycheck-navigation-minimum-level'.

ERR is also interesting (the function returns true) if there are
no errors as or more severe than `flycheck-navigation-minimum-level'."
      (when (flycheck-error-p err)
        (message "flycheck-navigation-minimum-level %S" flycheck-navigation-minimum-level )
        (message "(flycheck-error-level-severity min-level) %S" (flycheck-error-level-severity flycheck-navigation-minimum-level) )
        (message "(flycheck-error-level-severity (flycheck-error-level err)) %S" (flycheck-error-level-severity (flycheck-error-level err)) )
        ;; (message "%sh" (flycheck-error-level-severity min-level))
        (-if-let (min-level flycheck-navigation-minimum-level)
            (or (<= (flycheck-error-level-severity min-level)
                  (flycheck-error-level-severity (flycheck-error-level err))))
          t)))
    ;;
    ;; patch end
    ;;

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

    (flycheck-add-mode 'javascript-eslint 'web-mode)
    ;; (setq flycheck-javascript-jshint-executable
    ;;       (cond
    ;;        ((executable-find* "jsxhint" "yarn global add jsxhint") "jsxhint")
    ;;        (t "jshint")))

    (use-package helm-flycheck
      :ensure t
      :commands (helm-flycheck)
      :init
      (progn
        (bind-key "C-c ! h" 'helm-flycheck flycheck-mode-map)))

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
              ("^E303.*$" . info) ;; pep8: too many blank lines (3)
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
            )

      ;; this demotes go-golint from warning to info
      (flycheck-define-checker go-golint
        "A Go style checker using Golint.

See URL `https://github.com/golang/lint'."
        :command ("golint" source)
        :error-patterns
        ((info line-start (file-name) ":" line ":" column ": " (message) line-end))
        :modes (go-mode go-ts-mode)
        :next-checkers (go-vet
                        ;; Fall back, if go-vet doesn't exist
                        go-build go-test go-errcheck go-unconvert go-megacheck)))))


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
  :if (and (not noninteractive) )
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
          git-gutter:update-interval 1.6)
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
                                  '(lambda () (git-gutter-mode 1) (git-gutter:update-all-windows))

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
       :color blue))

    (use-package git-gutter-fringe
      :ensure t
      :disabled t
      :if window-system
      :init
      (progn
        (setq git-gutter-fr:side 'right-fringe))
      :config
      (progn
        (when (fboundp 'define-fringe-bitmap)
          (fringe-helper-define 'git-gutter-fr:added '(top repeat)
            ".XX.....")
          (fringe-helper-define 'git-gutter-fr:deleted '(top repeat)
            ".XX.....")
          (fringe-helper-define 'git-gutter-fr:modified '(top repeat)
            ".XX....."))))))


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
  :commands (gitlab-version)
  :init
  (progn

    (use-package helm-gitlab
      :ensure t
      :commands (helm-gitlab-issues
                 helm-gitlab-projects))))


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


;;;; god-mode

(use-package god-mode
  :disabled t
  :ensure t
  :commands (god-mode god-mode-all god-local-mode
                      god-local-mode-resume god-local-mode-pause)
  :init
  (progn
    (global-set-key (kbd "<escape>") 'god-mode-all))
  :config
  (progn
    (defun god-toggle-on-region-bindings-mode ()
      "Toggle god-mode on region-bindings-mode."
      (if (bound-and-true-p region-bindings-mode)
          (god-local-mode-pause)
        (god-local-mode-resume)))
    (add-hook 'region-bindings-mode-hook 'god-toggle-on-region-bindings-mode)
    (define-key god-local-mode-map (kbd "z") 'repeat)
    ;; (define-key god-local-mode-map (kbd "i") 'god-local-mode)
    (define-key god-local-mode-map (kbd "i") 'god-mode-all)
    ))


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
       ;; "~/\\.opt/XMonadContrib/"
       ;; "~/\\.opt/android-sdks/"
       ;; "~/\\.opt/emacs-24/"
       ;; "~/\\.opt/emacs-override/"
       ;; "~/\\.opt/emacs-site-lisp/"
       ;; "~/\\.opt/emacs/"
       ;; "~/\\.opt/go/"
       ;; "~/\\.opt/go-master/"
       ;; "~/\\.opt/xmonad/"
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
       "/\\.git/.*\\(?:[A-Z_]*_EDITMSG\\|MERGE_MSG\\|SQUASH_MSG\\|GHI_ISSUE\\|rebase-merge/git-rebase-todo\\|description\\|hooks/\\|config\\)\\'"
       ;; "~/\\.cabal/"
       ;; "~/perl5/perlbrew/"
       ;; "~/\\.npm/"
       ;; "~/\\.virtualenv/"
       ;; "~/\\.virthualenv/"
       ;; "~/\\.rvm/"
       ;; "/\\.hg/"
       ;; "/\\.svn/"
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


;;;; helm

(use-package helm
  :ensure t
  :defer 16
  :commands (helm-M-x helm-bookmarks helm-browse-code
                      helm-locate helm-mini helm-for-files helm-occur
                      helm-simple-call-tree helm-top helm-ucs helm-org-headlines
                      helm-org-keywords helm-mode helm-dired-mode
                      helm-recentf helm-find)
  :bind (("M-o M-x" . helm-M-x)
         ("C-h a" . helm-apropos)
         ;; ("M-s b" . helm-occur)
         ;; ("C-x f h" . helm-for-files)
         ;; ("<f7>" . helm-for-files)
         ("C-x f r" . helm-recentf)
         ("C-h r" . helm-recentf)
         ;; ("<f6>" . helm-recentf)
         ("C-x f L" . helm-locate))
  :preface
  (progn
    (load "helm-autoloads" t t))
  :config
  (progn
    (require 'helm-utils))
  :init
  (progn
    (setq
     helm-for-files-preferred-list
     '(
       helm-source-files-in-current-dir
       helm-source-buffers-list
       ;; helm-source-recentf
       helm-source-projectile-files-list
       ;; helm-source-projectile-buffers-list
       helm-source-projectile-recentf-list
       helm-source-file-cache
       )
     helm-prevent-escaping-from-minibuffer nil
     helm-buffer-max-length 50
     helm-full-frame t
     helm-ff-transformer-show-only-basename nil
     helm-adaptive-history-file (expand-file-name
                                 "helm-adaptive-history"
                                 user-data-directory)
     helm-adaptive-history-length 100
     helm-candidate-number-limit 100)


    (use-package helm-imenu
      :bind (("M-o M-i" . helm-imenu)
             ;; ("<f8>" . helm-imenu)
             ("C-h i" . helm-imenu))
      :config
      (progn
        (defadvice helm-imenu (before ensure-semantic-context activate)
          ;; quickly just try to to sematntic analyse before each imenu call
          (condition-case nil
              (semantic-analyze-current-context)
            (error t)))))


    (use-package helm-descbinds
      :ensure t
      :commands helm-descbinds
      :bind ("C-h b" . helm-descbinds))))


;;;; helm-chrome

(use-package helm-chrome
  :ensure t
  :commands helm-chrome-bookmarks)


;;;; helm-dash

(use-package helm-dash
  :ensure t
  :commands (helm-dash helm-dash-at-point)
  :bind (("C-h SPC" . helm-dash-at-point))
  :config
  (progn
    (setq helm-dash-browser-func
          (cond ((fboundp 'xwidget-webkit-browse-url) 'xwidget-webkit-browse-url)
                ((fboundp 'eww) 'eww)
                (t 'browse-url)))
    (when (eq system-type 'gnu/linux)
      (setq helm-dash-docsets-path (format "%s/.local/share/zeal"
                                           (getenv "HOME"))))
    (setq helm-dash-many-docsets
          '(
            "Android"
            "Appcelerator Titanium"
            "BackboneJS"
            "Bash"
            "Bootstrap_2"
            "Bootstrap_3"
            "C++"
            "CSS"
            "Clojure"
            "NET_Framework"
            "Redis"
            "Swift"
            "OS_X"
            "D3JS"
            "Django"
            "Go"
            "Glib"
            "Flask"
            "OCaml"
            "Foundation"
            "Go"
            "HTML"
            "JavaScript"
            "Lo-Dash"
            "Markdown"
            "MySQL"
            "Nginx"
            "PHP"
            "NodeJS"
            "PostgreSQL"
            "Processing"
            "Python 2"
            "Python 3"
            "SQLite"
            "SVG"
            "Sass"
            "Twisted"
            "UnderscoreJS"
            "Vagrant"
            "XSLT"
            "ZeptoJS"
            "iOS"
            "jQuery"
            "jQuery_Mobile"
            "jQuery_UI"
            ))
    (setq helm-dash-common-docsets
          '(
            ;; "Bootstrap_3"
            "CSS"
            ;; "Clojure"
            ;; "D3JS"
            "Django"
            ;; "Flask"
            ;; "Foundation"
            "Go"
            "HTML"
            "JavaScript"
            "Lo-Dash"
            "Markdown"
            "MomentJS"
            "Nginx"
            "NodeJS"
            "PostgreSQL"
            ;; "Processing"
            "Python 2"
            "Python 3"
            "SQLite"
            "SVG"
            "Sass"
            ;; "Twisted"
            ;; "Vagrant"
            ;; "ZeptoJS"
            "jQuery"
            "BackboneJS"
            ))
    ))


;;;; helm-git-grep

(use-package helm-git-grep
  :ensure t
  :commands (helm-git-grep
             helm-git-grep-1
             helm-git-grep-at-point)
  :init
  (progn
    (defun helm-git-grep-region ()
      "Helm git grep with region
if submodules exists, grep submodules too."
      (interactive)
      (let* ((symbol (buffer-substring (mark) (point)))
             (input (if symbol (concat symbol " ") nil)))
        (helm-git-grep-1 input)))
    (define-key search-map "g" 'helm-git-grep-region)
    ))


;;;; helm-orgcard

(use-package helm-orgcard
  :ensure t
  :commands helm-orgcard)


;;;; helm-projectile

(use-package helm-projectile
  :ensure t
  :commands (helm-projectile)
  :bind ("C-x f p" . helm-projectile))


;;;; helm-recoll

(use-package helm-recoll
  :ensure t
  :commands (helm-recoll helm-recoll-all)
  :bind (("M-o r" . helm-recoll-all))
  :config
  (progn
    (helm-recoll-create-source "all" "~/.recoll/")))


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


;;;; hy-mode

(use-package hy-mode
  :ensure t
  :mode "\\.hy\\'")


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
    (bind-key "r" 'helm-recentf ibuffer-mode-map)
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
      (ibuffer-awhen (with-current-buffer buf
                       (or buffer-file-name
                           default-directory))
        (string-match qualifier it)))

    (defun ibuffer-set-filter-groups-by-root  ()
      (interactive)
      (setq ibuffer-filter-groups
            (-concat
             '(("MORE"
                (or (mode . magit-log-edit-mode)
                    (name . "^\\*\\(traad-server\\|httpd\\|epc con.*\\|tramp/.*\\|Completions\\)\\*$")
                    (name . "^\\*Pymacs\\*$")
                    (name . "^\\*helm.*\\*")
                    (name . "^\\*Compile-log\\*$")
                    (name . "^\\*Ido Completions\\*$")
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
                       (mode . kivy-mode)
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
                       (name . "^\\*helm.*\\*")
                       (name . "^\\*Compile-log\\*$")
                       (name . "^\\*Ido Completions\\*$")
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
    (defun ibuffer-ido-find-file ()
      "Like `ido-find-file', but default to the directory of the buffer at point."
      (interactive
       (let ((default-directory (let ((buf (ibuffer-current-buffer)))
                                  (if (buffer-live-p buf)
                                      (with-current-buffer buf
                                        default-directory)
                                    default-directory))))
         (ido-find-file-in-dir default-directory))))
    (bind-key "C-x C-f" 'ibuffer-ido-find-file ibuffer-mode-map)))


;;;; ido

(use-package ido
  :commands ido-mode
  :if (and
       (not noninteractive))
  :init
  (progn
    (setq
     ido-enable-tramp-completion t
     ido-enable-flex-matching t
     ido-create-new-buffer 'always
     ido-use-filename-at-point nil
     ido-enable-dot-prefix t
     ido-max-prospects 50
     ido-auto-merge-work-directories-length -1
     ido-file-extensions-order
     '( ;; languages
       ".py" ".go"  ".js" ".jsx" ".rb" ".java" ".c"  ".cc" ".cpp" ".el" ".ts" ".tsx"
       ;; markup
       ".html" ".htm" ".xhtml"
       ;; styles
       ".styl" ".scss" ".sass" ".css"
       ;; documents
       ".org" ".md" ".markdowm" ".doc" ".txt" ".rst"
       ;; xml and conf
       ".yml" ".yaml" ".xml" ".ini" ".cfg" ".cnf")
     ;; ido-use-virtual-buffers t
     ido-ignore-buffers '("\\` " "*Ido Completions*" "*Completions*" "*helm*"
                          "type-break")
     ido-ignore-files '("\\`CVS/" "\\`#" "\\`.#" "\\`\\.\\./" "\\`\\./")
     ido-ignore-extensions t
     ido-default-buffer-method 'selected-window
     ido-default-file-method 'selected-window
     ido-save-directory-list-file (expand-file-name
                                   "ido.last" user-data-directory))
    (ido-mode 'both)
    (ido-everywhere 1)
    )
  :config
  (progn

    ;; (use-package ido-vertical-mode
    ;;   :ensure t
    ;;   :commands (ido-vertical-turn-on
    ;;              ido-vertical-mode)
    ;;   :init
    ;;   (progn
    ;;     (ido-vertical-turn-on)))

    (use-package flx-ido
      :ensure  t
      :commands (flx-ido-mode)
      :if (not (or
                noninteractive
                (or (not (boundp 'emacs-version)) (string< emacs-version "24.3"))))
      :init
      (progn
        (flx-ido-mode 1)))

    (defun my-ido-goto-home ()
      (interactive)
      (if (looking-back "/")
          (insert
           (if (looking-back "~/")
               "//" "~/"))
        (call-interactively 'self-insert-command)))

    (defun my-ido-setup-bindings-hook ()
      ;; (unbind-key "C-a" ido-common-completion-map)
      (bind-key "~" 'my-ido-goto-home ido-completion-map)
      (bind-key "C-a" 'beginning-of-line ido-completion-map)
      (bind-key "C-i" 'ido-toggle-ignore ido-completion-map)
      (bind-key "C-n" 'ido-next-match ido-completion-map)
      (bind-key "C-p" 'ido-prev-match ido-completion-map)
      (bind-key "<down>" 'ido-next-match ido-completion-map)
      (bind-key "<up>" 'ido-prev-match ido-completion-map)

      )
    (add-hook 'ido-setup-hook 'my-ido-setup-bindings-hook)))


;;;; ido-completing-read+

(use-package ido-completing-read+
  :disabled t
  :ensure t
  :defer 0.7
  :commands ido-ubiquitous-mode
  :config
  (progn
    (ido-ubiquitous-mode)))


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
  :bind (("M-o i" . goto-symbol))
  :init
  (progn
    (setq
     imenu-auto-rescan t))
  :config
  (progn
    (defun goto-symbol (&optional symbol-list)
      "Refresh imenu and jump to a place in the buffer using Ido."
      (interactive)
      (unless (featurep 'imenu)
        (require 'imenu nil t))
      (cond
       ((not symbol-list)
        (let ((ido-mode ido-mode)
              (ido-enable-flex-matching
               (if (boundp 'ido-enable-flex-matching)
                   ido-enable-flex-matching t))
              name-and-pos symbol-names position)
          (unless ido-mode
            (ido-mode 1)
            (setq ido-enable-flex-matching t))
          (while (progn
                   (imenu--cleanup)
                   (setq imenu--index-alist nil)
                   (ido-goto-symbol (imenu--make-index-alist))
                   (setq selected-symbol
                         (completing-read "Symbol?" symbol-names))
                   (string= (car imenu--rescan-item) selected-symbol)))
          (unless (and (boundp 'mark-active) mark-active)
            (push-mark nil t nil))
          (setq position (cdr (assoc selected-symbol name-and-pos)))
          (cond
           ((overlayp position)
            (goto-char (overlay-start position)))
           (t
            (goto-char position)))))
       ((listp symbol-list)
        (dolist (symbol symbol-list)
          (let (name position)
            (cond
             ((and (listp symbol) (imenu--subalist-p symbol))
              (ido-goto-symbol symbol))
             ((listp symbol)
              (setq name (car symbol))
              (setq position (cdr symbol)))
             ((stringp symbol)
              (setq name symbol)
              (setq position
                    (get-text-property 1 'org-imenu-marker symbol))))
            (unless (or (null position) (null name)
                        (string= (car imenu--rescan-item) name))
              (add-to-list 'symbol-names name)
              (add-to-list 'name-and-pos (cons name position))))))))))


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
           '((t . ivy--regex-ignore-order))))
  (ivy-mode))


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


;;;; jss

(use-package jss
  :ensure t
  :commands jss-connect)


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


;;;; keyfreq

(use-package keyfreq
  :ensure t
  :if (not noninteractive)
  :commands (keyfreq-mode
             keyfreq-autosave-mode)
  :init
  (progn
    (setq keyfreq-file (expand-file-name "keyfreq" user-data-directory)
          keyfreq-file-lock (expand-file-name
                             "keyfreq.lock" user-data-directory))
    ;; (keyfreq-mode)
    ;; (keyfreq-autosave-mode)
    ))


;;;; kite-mini

(use-package kite-mini
  :disabled t
  :ensure t
  :commands (kite-mini-mode)
  :init
  (progn

    (use-package kite-mini-console
      :commands kite-mini-console)))


;;;; kivy-mode

(use-package kivy-mode
  :ensure t
  :mode (("\\.kv\\'" . kivy-mode))
  :config
  (progn
    (bind-key "C-c <" 'python-indent-shift-left kivy-mode-map)
    (bind-key "C-c >" 'python-indent-shift-right kivy-mode-map)))


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


;;;; log4j-mode

(use-package log4j-mode
  :disabled t
  :mode ("\\.log\\'" . log4j-mode))


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

          lsp-diagnostics-provider :flycheck
          ;; lsp-diagnostics-provider :none
          ;; lsp-diagnostics-disabled-modes '(go-mode)
          lsp-pyls-configuration-sources ["flake8"]
          lsp-pyls-plugins-flake8-enabled t
          lsp-pyls-plugins-autopep8-enabled nil
          lsp-pyls-plugins-pycodestyle-enabled nil
          lsp-pyls-plugins-yapf-enabled nil
          lsp-pyls-plugins-pydocstyle-enabled nil)


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
    (add-hook 'js-mode-hook #'lsp)
    (add-hook 'go-dot-mod-mode-hook #'lsp)
    (add-hook 'terraform-mode-hook #'lsp)

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

    (use-package lsp-java
      :ensure t
      :after cc-mode
      :init
      (progn
        (add-hook 'java-mode-hook #'lsp)))

    (use-package lsp-haskell
      :ensure t
      :after haskell
      :init
      (progn
        (when (executable-find* "haskell-language-server"
                                "install haskell-language-server https://github.com/haskell/haskell-language-server")
          (add-hook 'haskell-mode-hook #'lsp)))))



  :config
  (progn
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
     lsp-ui-sideline-show-code-actions t
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
                       magit-status magit-git-repo-p magit-list-repos)
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
     magit-revert-buffers 'silent)

    (defadvice magit-version (around skipit activate)
      "900000000")

    ;; (use-package magit-circleci
    ;;   :ensure t
    ;;   :commands magit-circleci-mode
    ;;   :init
    ;;   (progn
    ;;     (setq magit-circleci-n-builds 15)
    ;;     (magit-circleci-mode)))

    (use-package orgit
      :ensure t
      :commands (orgit-store-link))

    (use-package magit-blame
      :commands magit-blame-mode)

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
    (use-package magit-section
      :ensure t)
    (use-package forge
      :ensure t

      :init
      (progn
        (setq forge-database-file  (expand-file-name "forge/database.sqlite" user-data-directory)
              forge-post-directory (expand-file-name "forge/posts/" user-data-directory)))
      :config
      (progn

        (use-package forge-topic
          :config
          (progn
            (defun my-forge-bug-reference-setup-condition (orig-fun &rest args)
              (when (or (eq major-mode 'forge-topic-mode)
                        (eq major-mode 'magit-staus-mode)
                        (eq major-mode 'forge-issue-list-mode)
                        (eq major-mode 'magit-log-mode))
                (apply orig-fun args)))
            (advice-add 'forge-bug-reference-setup :around #'my-forge-bug-reference-setup-condition)))
        (setq forge-alist
              '(("github.com" "api.github.com" "github.com" forge-github-repository)
                ("gitlab.com" "gitlab.com/api/v4" "gitlab.com" forge-gitlab-repository)
                ("gitlab.23c.se" "gitlab.23c.se/api/v4" "gitlab.23c.se" forge-gitlab-repository)
                ("gitlab.tracklib.com" "gitlab.tracklib.com/api/v4" "gitlab.tracklib.com" forge-gitlab-repository)
                ("salsa.debian.org" "salsa.debian.org/api/v4" "salsa.debian.org" forge-gitlab-repository)
                ("framagit.org" "framagit.org/api/v4" "framagit.org" forge-gitlab-repository)
                ("codeberg.org" "codeberg.org/api/v1" "codeberg.org" forge-gitea-repository)
                ("code.orgmode.org" "code.orgmode.org/api/v1" "code.orgmode.org" forge-gogs-repository)
                ("bitbucket.org" "api.bitbucket.org/2.0" "bitbucket.org" forge-bitbucket-repository)
                ("git.savannah.gnu.org" nil "git.savannah.gnu.org" forge-cgit*-repository)
                ("git.kernel.org" nil "git.kernel.org" forge-cgit-repository)
                ("repo.or.cz" nil "repo.or.cz" forge-repoorcz-repository)
                ("git.suckless.org" nil "git.suckless.org" forge-stagit-repository)
                ("git.sr.ht" nil "git.sr.ht" forge-srht-repository))
              forge-database-file (expand-file-name "forge-database.sqlite" user-data-directory))))

    (defun my-git-commit-hook-fn ()
      "My git commit mode hook."
      (ispell-change-dictionary "english")
      (turn-on-flyspell)
      (toggle-save-place 0))

    (add-hook 'git-commit-setup-hook 'my-git-commit-hook-fn)

    (require 'json)
    ;; (defadvice magit-status (around magit-fullscreen activate)
    ;;   (window-configuration-to-register :magit-fullscreen)
    ;;   ad-do-it
    ;;   (delete-other-windows))
    ;; (defun magit-quit-session ()
    ;;   "Restores the previous window configuration and kills the magit buffer"
    ;;   (interactive)
    ;;   ;; TODO maybe in some cases
    ;;   ;; (kill-buffer)
    ;;   (bury-buffer)
    ;;   (when (get-register :magit-fullscreen)
    ;;     (jump-to-register :magit-fullscreen)
    ;;     (set-register :magit-fullscreen nil)))
    ;; (bind-key "q" 'previous-buffer magit-status-mode-map)
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
  ;; :mode (("\\.markdown\\'" . gfm-mode)
  ;;        ("\\.md\\'" . gfm-mode)
  ;;        ("\\.mdwn\\'" . gfm-mode)
  ;;        ("\\.mkd\\'" . gfm-mode)
  ;;        ("\\.mkdown\\'" . gfm-mode)
  ;;        ("\\.mdtext\\'" . gfm-mode))
  :init
  (progn
    (setq markdown-command "pandoc -f markdown -t html"
          markdown-fontify-code-blocks-natively nil)
    (defun markdown-imenu-create-index ()
      (let* ((root '(nil . nil))
             cur-alist
             (cur-level 0)
             (pattern "^\\(\\(#+\\)[ \t]*\\(.+\\)\\|\\([^# \t\n=-].*\\)\n===+\\|\\([^# \t\n=-].*\\)\n---+\\)$")
             (empty-heading "-")
             (self-heading ".")
             hashes pos level heading)
        (save-excursion
          (goto-char (point-min))
          (while (re-search-forward pattern (point-max) t)
            (cond
             ((setq hashes (match-string-no-properties 2))
              (setq heading (match-string-no-properties 3)
                    pos (match-beginning 1)
                    level (length hashes)))
             ((setq heading (match-string-no-properties 4))
              (setq pos (match-beginning 4)
                    level 1))
             ((setq heading (match-string-no-properties 5))
              (setq pos (match-beginning 5)
                    level 2)))
            (let ((alist (list (cons heading pos))))
              (cond
               ((= cur-level level) ; new sibling
                (setcdr cur-alist alist)
                (setq cur-alist alist))
               ((< cur-level level) ; first child
                (dotimes (i (- level cur-level 1))
                  (setq alist (list (cons empty-heading alist))))
                (if cur-alist
                    (let* ((parent (car cur-alist))
                           (self-pos (cdr parent)))
                      (setcdr parent (cons
                                      (cons self-heading self-pos) alist)))
                  (setcdr root alist)) ;; primogenitor
                (setq cur-alist alist)
                (setq cur-level level))
               (t ;; new sibling of an ancestor
                (let ((sibling-alist (last (cdr root))))
                  (dotimes (i (1- level))
                    (setq sibling-alist (last (cdar sibling-alist))))
                  (setcdr sibling-alist alist)
                  (setq cur-alist alist))
                (setq cur-level level)))))
          (cdr root))))
    (add-hook 'gfm-mode-hook
              #'(lambda ()
                  (setq imenu-create-index-function
                        'markdown-imenu-create-index))))
  :config
  (progn
    (unbind-key "`" gfm-mode-map)
    )
  )


;;;; melpa-upstream-visit

(use-package melpa-upstream-visit
  :disabled t
  :ensure t
  :commands muv
  :init
  (progn
    (defadvice list-packages (before load-muv activate)
      (use-package melpa-upstream-visit))))


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
     mingus-use-mouse-p nil
     mingus-use-ido-mode-p t))
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


;;;; mml2015

(use-package mml2015
  :defer
  :init
  (progn
    (setq mml2015-encrypt-to-self t
          mml2015-sign-with-sender t
          mml2015-use 'epg)))


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


;;;; moz

(use-package moz
  :disabled t
  :ensure t
  :commands moz-minor-mode)


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


;;;; nrepl-eval-sexp-fu

(use-package nrepl-eval-sexp-fu
  :ensure t
  :disabled t
  :commands (nrepl-eval-sexp-fu-flash-mode
             turn-on-nrepl-eval-sexp-fu-flash-mode
             nrepl-eval-sexp-fu-eval-sexp-inner-list
             nrepl-eval-sexp-fu-eval-sexp-inner-sexp))


;;;; nsm

(use-package nsm
  :defer t
  :init
  (progn
    (setq nsm-settings-file (expand-file-name
                             "network-security.data" user-data-directory))))


;;;; nxml-mode

(use-package nxml-mode
  :defer t
  :config
  (progn
    (unbind-key "M-h" nxml-mode-map)))


;;;; oauth2

(use-package oauth2
  :ensure t
  ;; :commands ()
  :defer
  :init
  (progn
    (setq
     oauth2-token-file "~/.config-private/oauth2.plstore")))


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


;;;; org

(eval-and-compile
  (with-eval-after-load "package"
    (assq-delete-all 'org package--builtins)
    (assq-delete-all 'org package--builtin-versions)))

(use-package org
  :ensure org
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
      :bind ("C-c C-l" . org-annotate-file))


    (use-package org-journal
      :ensure t
      :commands (org-journal-new-entry)
      :init
      (progn
        (setq
         org-journal-dir
         (expand-file-name "org/journal/" user-notes-directory)
         org-journal-file-pattern "[0-9]\\{8\\}$"))))
  :config
  (progn
    (use-package org-contrib
      :ensure t)
    (use-package orgit
      :ensure t)
    (unbind-key "M-h" org-mode-map)
    (bind-key "C-c h" 'org-todo org-mode-map )
    (setq org-modules '(org-bbdb org-bibtex org-docview org-habit
                                 org-id org-info org-man org-w3m))
    ;; org-git-link
    ;; (require 'ox-reveal)

    (use-package orgbox
      :ensure t)

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
                    (org-show-subtree)))))

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
      (interactive (list (ido-read-file-name "csv file: ")))
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
     org-completion-use-ido t
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
     org-startup-with-inline-images noninteractive
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
     org-export-babel-evaluate nil
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


;;;; parenface-plus

(use-package parenface-plus
  :ensure t
  :disabled t
  :if (and (not noninteractive) (not (not window-system)) )
  :config
  (progn
    (defun paren-face-add-keyword-other ()
      "Adds paren-face support to the mode."
      (font-lock-add-keywords nil '(("\\[\\|\\]" . paren-face)))
      (font-lock-add-keywords nil '(("{\\|}" . paren-face))))
    (add-hook 'lisp-mode-hook 'paren-face-add-keyword)
    (add-hook 'go-mode-hook 'paren-face-add-keyword)
    (add-hook 'go-mode-hook 'paren-face-add-keyword-other)
    (add-hook 'go-ts-mode-hook 'paren-face-add-keyword)
    (add-hook 'go-ts-mode-hook 'paren-face-add-keyword-other)
    (add-hook 'python-base-mode-hook 'paren-face-add-keyword)
    (add-hook 'python-base-mode-hook 'paren-face-add-keyword-other)
    ))


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
          ("*Ido Completions*" :noselect t :position bottom)
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
        (loop for (glyph . pairs) in patterns do
              (loop for (regexp . major-modes) in pairs do
                    (loop for mode in major-modes do
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
  :defer 1.4
  :commands (projectile-mode
             projectile-global-mode
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
     ;; projectile-completion-system 'ido
     projectile-completion-system 'ivy
     projectile-require-project-root t
     projectile-switch-project-action 'projectile-dired
     projectile-enable-caching nil
     projectile-verbose nil
     projectile-known-projects-file (expand-file-name
                                     "projectile-bookmarks.eld"
                                     user-data-directory)
     projectile-cache-file (expand-file-name
                            "projectile.cache" user-cache-directory)
     projectile-file-exists-local-cache-expire nil
     projectile-file-exists-remote-cache-expire (* 15 60)
     projectile-project-root-files-functions
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
    (projectile-global-mode)))

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

    (defun python-cccc ()
      (interactive)
      (silent-save-some-buffers)
      (py-black-buffer)
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


;;;; qml-mode

(use-package qml-mode
  :ensure t
  :mode (("\\.qml\\'" . qml-mode)))


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
  :commands (realgud-pdb realgud-gdb pdb)
  :init
  (progn
    (setq pdb-command-name "python -m pdb"))
  :config
  (progn
    (defalias 'pdb 'realgud-pdb)))


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
      "Use `ido-completing-read' to \\[find-file] a recent file"
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
    (rg-define-toggle "-g !tests/ -g !tests.py -g !test_*.py -g !testdata/ -g !*_test.go" "T")
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


;;;; sclang

(use-package sclang
  :disabled t
  :commands (sclang-start
             sclang-server-boot)
  :mode ("\\.\\(sc\\|scd\\)\\'" . sclang-mode)
  :interpreter ("sclang" . sclang-mode))


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
     sh-basic-offset 2
     sh-indentation 2)))


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

(use-package smex
  :ensure t
  :if (and )
  :commands (smex smex-major-mode-commands smex-show-unbound-commands)
  :bind (
         ;; ("M-x" . smex)
         ("<menu>" . smex)
         ("<XF86Tools>" . smex)
         ;; ("<XF86Search>" . smex)
         ("M-X" . smex-major-mode-commands))
  :init
  (progn
    (setq
     smex-save-file (expand-file-name
                     "smex-items" user-data-directory)
     smex-flex-matching t
     smex-history-length 15
     smex-prompt-string "")))


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
       (title . "Speedbar")))

    (use-package sr-speedbar
      :ensure t
      :disabled t
      :commands (sr-speedbar-toggle
                 sr-speedbar-refresh-toggle
                 sr-speedbar-select-window)
      :init
      (progn
        (setq
         sr-speedbar-width 40
         sr-speedbar-width-x 40
         sr-speedbar-max-width 60
         sr-speedbar-auto-refresh t
         sr-speedbar-skip-other-window-p t
         sr-speedbar-right-side nil)

        (defvar last-selected-window (selected-window))
        (defadvice select-window (after remember-selected-window activate)
          "Remember the last selected window."
          (unless (or
                   (not (boundp 'sr-speedbar-window))
                   (eq (selected-window) sr-speedbar-window))
            (setq last-selected-window (selected-window)))))
      :config
      (progn
        (defun sr-speedbar-maybe-close ()
          (when (sr-speedbar-exist-p)
            (sr-speedbar-close)))

        (defadvice tiling-cycle (before sr-speedbar-close)
          "Close speedbar if it's open."
          (sr-speedbar-maybe-close))

        (defadvice mu4e (before sr-speedbar-close)
          "Close speedbar if it's open."
          (sr-speedbar-maybe-close))

        (defun sr-speedbar-before-visiting-file-hook ()
          "Function that hooks `speedbar-before-visiting-file-hook'."
          (select-window last-selected-window))

        (defun sr-speedbar-before-visiting-tag-hook ()
          "Function that hooks `speedbar-before-visiting-tag-hook'."
          (select-window last-selected-window))

        (defun sr-speedbar-visiting-file-hook ()
          "Function that hooks `speedbar-visiting-file-hook'."
          (select-window last-selected-window))

        (defun sr-speedbar-visiting-tag-hook ()
          "Function that hooks `speedbar-visiting-tag-hook'."
          (select-window last-selected-window)))))
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


;;;; string-edit

(use-package string-edit
  :ensure t
  :disabled t
  :commands (string-edit-at-point))


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


;;;; textile-mode

(use-package textile-mode
  :ensure t
  :commands textile-mode
  :mode ("\\.textile\\'" . textile-mode))


;;;; thingopt

(use-package thingopt :ensure t :defer)


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


;;;; tox

(use-package tox
  :ensure t
  :commands (tox-current-test tox-current-cast))


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
  :commands (unfill-region unfill-paragraph toggle-fill-unfill)
  :bind ("M-q" . toggle-fill-unfill))


;;;; uniquify

(use-package uniquify
  :if (and  (not noninteractive))
  :init
  (progn
    (setq
     uniquify-buffer-name-style 'post-forward
     uniquify-separator " • "
     uniquify-min-dir-content 3
     uniquify-after-kill-buffer-p t
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
            (assert (>= (point) bol))
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
             venv-is-valid)
  :init
  (progn
    ;; (when
    ;;     (and
    ;;      (not (getenv "VIRTUAL_ENV"))
    ;;      (eq window-system 'x))
    ;;   (let ((workspace-prefix (workspace-prefix)))
    ;;     (if (and workspace-prefix
    ;;              (venv-is-valid workspace-prefix))
    ;;         (venv-workon workspace-prefix))))
    ;; (if (getenv "VIRTUAL_ENV")
    ;;     (my-notify "emacs" (format "venv: %s" (f-base (getenv "VIRTUAL_ENV")))))
    ))


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
  :init
  (progn
    (defun vkill-and-helm-occur ()
      (interactive)
      (vkill)
      (call-interactively #'helm-occur))
    ;; (bind-key "C-x L" 'vkill-and-helm-occur)
    )

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


;;;; wakatime-mode

(use-package wakatime-mode
  :ensure t
  :if (and (not noninteractive)  (executable-find* "wakatime-cli"))
  :commands (wakatime-mode global-wakatime-mode)
  :diminish (wakatime-mode . "")
  :defer 4
  :init
  (progn
    (setq wakatime-disable-on-error t)
    ;; (setq wakatime-cli-path "~/.opt/wakatime/wakatime-cli.py")
    )
  :config
  (progn
    (when (executable-find "wakatime-cli")
      (global-wakatime-mode 1))))


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


;;;; websocket

(use-package websocket
  :ensure t
  :commands websocket-open)


;;;; weechat

(use-package weechat
  :ensure t
  :disabled t
  :commands (weechat-connect)
  :bind  (("C-x b i" . weechat-switch-buffer)
          ("C-x b I" . weechat-monitor-buffer))
  :init
  (progn
    (setq weechat-buffer-kill-buffers-on-disconnect t
          weechat-auto-monitor-new-buffers 'silent
          weechat-initial-lines 200
          weechat-auto-close-buffers t
          weechat-modules '(weechat-button weechat-complete)
          weechat-time-format "%a %H:%M"
          weechat-strip-formatting t ;; FIXME remove when weechat colors is using its color settings properly
          weechat-tracking-faces-priorities '(weechat-highlight-face))

    (defun my-weechat-mode-hook ()
      (visual-line-mode 1))
    (add-hook 'weechat-mode-hook 'my-weechat-mode-hook)

    (defun weechat-monitor-all-buffers ()
      "Monitor all weechat buffers"
      (interactive)
      (let ((weechat-auto-monitor-buffers t))
        (weechat-auto-monitor)))

    (defun weechat ()
      "Load private-weechat which is supposed to connect to weechat."
      (interactive)
      (require 'private-weechat nil t)
      (my-weechat-connect))))


;;;; wgrep

(use-package wgrep
  :ensure t
  :commands (wgrep-setup))


;;;; which-key

(use-package which-key
  :ensure t
  :disabled t
  :commands (which-key-mode)
  :defer 0.9
  :diminish ""
  :init
  (progn
    (setq which-key-idle-delay 1.0
          which-key--secondary-timer-active nil
          which-key-popup-type 'minibuffer)
    (which-key-mode)))


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
  :bind (("M-N" . winner-redo)
         ("M-P" . winner-undo))
  :init
  (progn
    (setq winner-dont-bind-my-keys t
          winner-boring-buffers
          '("*Completions*"
            "*SPEEDBAR*"
            "*helm for files*"
            "*helm imenu*"
            "*helm*"))
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
    (defvar xref--current-item nil)
    (defun my-pulse-xref ()
      (pcase-let ((`(,beg . ,end)
               (save-excursion
                 (or
                  (let ((length (xref-match-length xref--current-item)))
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


;;;; xterm-color

(use-package xterm-color
  :ensure t
  :disabled t
  :commands (xterm-color-filter xterm-color-unfontify-region)
  :init
  (progn
    (add-hook 'comint-preoutput-filter-functions 'xterm-color-filter)
    (setq comint-output-filter-functions (remove 'ansi-color-process-output comint-output-filter-functions))
    (setq font-lock-unfontify-region-function 'xterm-color-unfontify-region)))


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

            (mapcar #'(lambda (choice)
                        (when (yas--template-p choice)
                          (setq group-max-len (max group-max-len
                                                   (+ (length (yas--template-group choice) )
                                                      (apply '+ (mapcar 'length (yas--template-group choice))))))
                          (setq key-max-len (max key-max-len (length (yas--template-key choice))))))
                    choices)

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


;;; settings that might have been set by loading libraries

;;;; set cursor options

(setq-default blink-cursor-mode t
              blink-cursor-interval 0.6
              cursor-type '(bar . 3))
(blink-cursor-mode)


;;; workspace-hook

;; (defun  my-workspace-hook()
;;   "workspace specific hook function."
;;   t
;;   (and
;;    (not noninteractive)
;;    window-system

;;    (cond
;;     ((equal workspace-prefix-startup "upgrade")
;;      (call-interactively 'list-packages)))))

;; (add-hook 'after-init-hook 'my-workspace-hook t)

;; this thing just nuges emacs to adjust it's window size, some times it starts
;; in 80x35. compat

(and load-file-name
     window-system
     (eq system-type 'gnu/linux)
     (not noninteractive)
     ;; for emacs 24.4
     (not (message nil))
     ;; for emacs 25 (2015-06-21)
     (add-hook 'after-init-hook
               #'(lambda ()
                   (run-with-idle-timer
                    0 nil '(lambda ()
                             (tool-bar-mode -1))) nil)))


;;; print start up times to *emacslog*

(when (and load-file-name
           (not noninteractive))
  (add-hook 'after-init-hook
            `(lambda ()
               (eval-and-compile (push `("after-init" ,after-init-time) init-times))
               (with-current-buffer (get-buffer-create " *emacslog*")
                 (let ((end-time (current-time)))
                   (push `("end-time" ,end-time) init-times)
                   (let ((times (reverse init-times)) )
                     (let ((prev nil))
                       (mapc #'(lambda (v)
                                 (when prev
                                   (insert
                                    (format "%.3fs" (float-time (time-subtract  (nth 1 v) (nth 1 prev))))
                                    " : "
                                    (car prev)
                                    " -> "
                                    (car v)
                                    " ("
                                    (format "%.3fs" (float-time (time-subtract  (nth 1 v) before-init-time)))
                                    ")"
                                    "\n"))
                                 (setq prev v))
                             times))))))
            t))


;;; File local vars

;; Local Variables:
;; eval: (outline-minor-mode 1)
;; eval: (require 'use-package)
;; End:

(provide 'init)


;;; init.el ends here
