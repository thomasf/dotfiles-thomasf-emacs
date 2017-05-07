(setq force-load-messages nil
      use-package-verbose nil
      use-package-debug nil)

(defun ask-user-about-lock (file opponent)
  "always grab lock."
  (warn "Grabbing lock for: %s" file)
  t)

(require 'package)
(package-initialize)
(require 'use-package)
(require 'htmlize)
(require 'go-mode)
(require 'ansi-color)

(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(set-language-environment "UTF-8")
(define-coding-system-alias 'UTF-8 'utf-8)


(setq
 org-src-fontify-natively t
 org-src-preserve-indentation t
 org-src-lang-modes
 '(
   ("ocaml" . tuareg)
   ("elisp" . emacs-lisp)
   ("ditaa" . artist)
   ("asymptote" . asy)
   ("dot" . graphviz-dot)
   ("sqlite" . sql)
   ("calc" . fundamental)
   ("C" . c)
   ("cpp" . c++)
   ("json" . json)
   )
 vc-handled-backends ()
 org-confirm-babel-evaluate nil
 )


;; (setf org-html-htmlize-output-type 'css)


;; (org-babel-do-load-languages
;;  'org-babel-load-languages
;;  '(
;;    ;;(perl . t)
;;    ;;(ruby . t)
;;    (sh . t)
;;    (python . t)
;;    (emacs-lisp . t)
;;    ;;(dot . t)
;;    ;;(ditaa . t)
;;    ;;(plantuml . t)
;;    (sql . t)
;;    (http . t)
;;    ))


;;; Setup Start


(defmacro solarized-with-color-variables (variant &rest body)

  `(let* ((variant ,variant)
          (s-base03    "#002b36")
          (s-base02    "#073642")
          ;; emphasized content
          (s-base01    "#586e75")
          ;; primary content
          (s-base00    "#657b83")
          (s-base0     "#839496")
          ;; comments
          (s-base1     "#93a1a1")
          ;; background highlight light
          (s-base2     "#eee8d5")
          ;; background light
          (s-base3     "#fdf6e3")

          ;; Solarized accented colors
          (yellow    "#b58900")
          (orange    "#cb4b16")
          (red       "#dc322f")
          (magenta   "#d33682")
          (violet    "#6c71c4")
          (blue      "#268bd2")
          (cyan      "#2aa198")
          (green     "#859900")

          ;; Solarized palette names, use these instead of -fg -bg...
          (base0 (if (eq variant 'light) s-base00 s-base0))
          (base00 (if (eq variant 'light) s-base0 s-base00))
          (base1 (if (eq variant 'light) s-base01 s-base1))
          (base01 (if (eq variant 'light) s-base1 s-base01))
          (base2 (if (eq variant 'light) s-base02 s-base2))
          (base02 (if (eq variant 'light) s-base2 s-base02))
          (base3 (if (eq variant 'light) s-base03 s-base3))
          (base03 (if (eq variant 'light) s-base3 s-base03))

          ;; Line drawing color
          ;;
          ;; NOTE only use this for very thin lines that are hard to see using base02, in low
          ;; color displayes base02 might be used instead
          (s-line (if (eq variant 'light) "#cccec4" "#284b54"))
          )
     ,@body))

(defun set-solarized-colors ()
  (solarized-with-color-variables
    'light
    (custom-set-faces
;;;; Built-in
;;;;; basic coloring
     `(default ((t (:foreground ,base0 :background ,base03))))
     `(shadow ((t (:foreground ,base01))))
     `(match ((t (:background ,base02 :foreground ,base1 :weight bold))))
     `(cursor ((t (:foreground ,base03 :background ,base0 :inverse-video t))))
     `(escape-glyph ((t (:foreground ,violet))))
     `(highlight ((t (:background ,base02))))
     `(link ((t (:foreground ,yellow :underline t :weight bold))))
     `(link-visited ((t (:foreground ,yellow :underline t :weight normal))))
     `(success ((t (:foreground ,green ))))
     `(warning ((t (:foreground ,yellow ))))
     `(error ((t (:foreground ,orange))))
     `(lazy-highlight ((t (:foreground ,base03 :background ,yellow :weight normal))))
     `(widget-field ((t (:background ,base02))))
     '(button ((t (:underline t))))
     ;; ;;;;; compilation
     `(compilation-column-face ((t (:foreground ,cyan :underline nil))))
     `(compilation-column-number ((t (:inherit font-lock-doc-face :foreground ,cyan :underline nil))))
     `(compilation-enter-directory-face ((t (:foreground ,green :underline nil))))
     `(compilation-error ((t (:inherit error :underline nil))))
     `(compilation-error-face ((t (:foreground ,red : :underline nil))))
     `(compilation-face ((t (:foreground ,base0 :underline nil))))
     `(compilation-info ((t (:foreground ,base01 :underline nil :bold nil))))
     `(compilation-info-face ((t (:foreground ,blue :underline nil))))
     `(compilation-leave-directory-face ((t (:foreground ,green :underline nil))))
     `(compilation-line-face ((t (:foreground ,green :underline nil))))
     `(compilation-line-number ((t (:foreground ,green :underline nil))))
     `(compilation-warning ((t (:inherit warning :underline nil))))
     `(compilation-warning-face ((t (:foreground ,yellow :weight normal :underline nil))))

     ;; ;;;;; font lock
     `(font-lock-builtin-face ((t (:foreground ,base0 :weight bold :slant italic))))
     `(font-lock-comment-delimiter-face ((t (:foreground ,base01 :slant italic))))
     `(font-lock-comment-face ((t (:foreground ,base01))))
     `(font-lock-constant-face ((t (:foreground ,blue :weight bold))))

     ;; `(font-lock-doc-face ((t (:foreground ,violet :slant italic))))
     `(font-lock-doc-face ((t (:foreground ,cyan :slant italic))))
     `(font-lock-function-name-face ((t (:foreground ,blue))))
     `(font-lock-keyword-face ((t (:foreground ,green :weight bold))))
     `(font-lock-negation-char-face ((t (:foreground ,yellow :weight bold))))
     `(font-lock-preprocessor-face ((t (:foreground ,blue))))
     `(font-lock-regexp-grouping-construct ((t (:foreground ,yellow :weight bold))))
     `(font-lock-regexp-grouping-backslash ((t (:foreground ,green :weight bold))))
     `(font-lock-string-face ((t (:foreground ,cyan))))
     `(font-lock-type-face ((t (:foreground ,yellow))))
     `(font-lock-variable-name-face ((t (:foreground ,blue))))
     `(font-lock-warning-face ((t (:inherit error :weight bold))))
     `(c-annotation-face ((t (:inherit font-lock-constant-face))))
     )))

(defun batch-org-html-export-to-html ()
  (setq org-export-babel-evaluate nil)
  (set-solarized-colors)
  (font-lock-fontify-buffer)
  (org-html-export-to-html))
