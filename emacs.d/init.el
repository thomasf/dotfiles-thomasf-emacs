;;; init.el --- Thomas Frössman emacs init -*- lexical-binding: nil; byte-compile-warnings: (not docstrings-wide); -*-

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


;;;; init-lib

;; The rest of the configuration lives in lisp/init/.  load-path.el adds it
;; too, but only once load-path.elc has been rebuilt after the directory was
;; created.

(eval-and-compile
  (add-to-list 'load-path
               (file-name-as-directory
                (expand-file-name "lisp/init" user-emacs-directory))))

(require 'init-lib)


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


;;;; package.el + early package install

;; set up package.el and make sure that some essential packages and features
;; are loaded early.

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

  (setq package-pinned-packages '((org . "built-in")))

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
  (require-package 'use-package))

(require 'use-package)
;; (use-package quelpa
;;   :ensure t
;;   :init (progn
;;           (use-package quelpa-use-package
;;             :ensure t)))

(require 'cl)
(require 'subr-x)

(use-package sql
  :defer t
  :init
  (progn
    (add-hook 'sql-mode-hook #'(lambda () (define-key sql-mode-map (kbd "TAB") 'self-insert-command)))))

(use-package dash
  :ensure t
  :demand
  :commands (dash-enable-fontlock)
  :init
  (progn
    (add-hook 'emacs-lisp-mode-hook 'dash-enable-font-lock)))

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


;;; load init modules

(dolist (module '(init-core
                  init-ui
                  init-commands
                  init-security
                  init-packages
                  init-org))
  (require module)
  (push `(,(symbol-name module) ,(current-time)) init-times))


;;; settings that might have been set by loading libraries

;;;; set cursor options

(setq-default blink-cursor-mode t
              blink-cursor-interval 0.6
              cursor-type '(bar . 3))
(blink-cursor-mode)




;;; print start up times to *emacslog*

(when (and load-file-name
           (not noninteractive))
  (add-hook 'after-init-hook
            #'(lambda ()
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
