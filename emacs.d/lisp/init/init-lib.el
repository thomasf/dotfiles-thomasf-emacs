;;; init-lib.el --- macros and variables shared by the init files -*- lexical-binding: nil; byte-compile-warnings: (not docstrings-wide); -*-

;;; Commentary:

;; Definitions that init.el and every init-* file depend on:
;;
;; - compile-time helper macros (`executable-find*', `file-exists-p*')
;; - `hook-into-modes' and the my-*-mode-hooks / my-*-modes lists
;; - use-package logging settings that affect how it expands
;; - `rename-modeline'
;;
;; Every init-* file requires this so that it can be byte compiled on
;; its own.  Put a macro here when it is used by more than one file;
;; a macro defined in another init-* file is not available when the
;; files using it are compiled.  Keep configuration out of here, apart
;; from settings that change how macros expand.
;;
;; init.el loads this right after load-path, before package.el.


;;; Code:

;;; compilation utilities

;; Plain defmacros instead of `eval-when-compile' so that they are also
;; available when an init-* file requires this file at compile time.

(defmacro executable-find* (command &optional help)
  "Macro form of executable-find..."
  (let ((v (executable-find command)) )
    (unless v
      (message "executable '%s' not found: %s" command (or help "")))
    v))

(defmacro file-exists-p* (filename)
  "Macro form of file-exists-p which expands the file name and returns the expenaded file name instead of t."
  (let ((f (expand-file-name filename)) )
    (if (file-exists-p filename) filename)))


;;; verbose start up/compile logging


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


;;; mode hook lists

(defmacro hook-into-modes (func modes)
  "Add hook `FUNC' to multiple `MODES'."
  `(dolist (mode-hook ,modes)
     (add-hook mode-hook ,func)))


;;;; prog modes

(defvar my-prog-mode-hooks
  '(prog-mode-hook
    csharp-mode
    csharp-ts-mode
    clojure-mode-hook
    emacs-lisp-mode-hook
    go-mode-hook
    go-ts-mode
    groovy-mode-hook
    haskell-mode-hook
    js-mode-hook
    js2-mode-hook
    pyhon-mode-hook
    ruby-mode-hook
    typescript-mode-hook
    typescript-ts-mode-hook))


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


;;; mode line

;;;; rename-modeline

(defmacro rename-modeline (package-name mode new-name)
  `(eval-after-load ,package-name
     '(defadvice ,mode (after rename-modeline activate)
        (setq mode-name ,new-name))))



(provide 'init-lib)

;; Local Variables:
;; eval: (outline-minor-mode 1)
;; eval: (require 'use-package)
;; End:


;;; init-lib.el ends here
