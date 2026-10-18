;;; init-core.el --- built-in settings and global key bindings -*- lexical-binding: nil; byte-compile-warnings: (not docstrings-wide); -*-

;;; Commentary:

;; Settings for Emacs itself, independent of any package:
;;
;; - startup screen, *scratch* mode, backups and auto saves, custom-file
;; - built-in variables, grouped by the Elisp or C source defining them
;; - global key bindings, and the prefix keymaps (`my-files-map',
;;   `my-dirs-map', `my-buffers-map', `my-other-map') that the other
;;   files bind into
;;
;; Built-in features that have their own library (dired, org, vc...)
;; are configured with use-package in init-packages or init-org.
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


;;; startup

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

(define-derived-mode initial-mode text-mode "Initial"
  "Major mode for start up buffer.
\\{initial-mode-map}"

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

(setq
 backup-directory-alist (list (cons "." backup-dir))
 auto-save-file-name-transforms `((".*" ,autosave-dir t))
 abbrev-file-name (expand-file-name
                       "abbrev_defs.el" user-data-directory))


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

(define-advice do-auto-save (:around (orig-fn &optional _no-message current-only) silent)
  "Silence successful auto saving."
  (funcall orig-fn t current-only))


;;;;; simple

(setq column-number-mode t
      kill-whole-line nil
      shift-select-mode nil
      eval-expression-print-level nil
      which-func-update-delay 0.5
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
(_j_/_k_ ERR _h_ src _y_ win) (_f_/_d_ HUNK) (_u_/_i_ FLYC _o_ lvl) (_r_/_e_ BUF) (GOTO line _g_ char _c_) . _l_ recenter
(_b_/_B_ bookmark) _U_ URL . _v_ visit-file . _s_ imenu . _S_ scratch . _I_ ibuffer . _R_ rg-buf . _D_ dired . _G_ git-gutter"
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

  ("l" my-recenter-top-bottom "recenter")

  ;; goto various windows
  ("U" browse-url "browse-url" :exit t)
  ("v" visit-file "visit-file" :exit t)
  ("S" scratch-buffer "scratch" :exit t)
  ("R" (lambda () (interactive) (switch-to-buffer "*rg*")) "*rg*" :exit t)
  ("I" (lambda () (interactive) (ibuffer)) "*ibuf*" :exit t)
  ("D" (lambda () (interactive) (dired ".")) "*dired*" :exit t)
  ("s" counsel-imenu "imenu" :exit t)

  ("b" bookmark-jump "*bookmarks*" :exit t)
  ("B" counsel-bookmark "*bm-set*" :exit t)

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



(provide 'init-core)

;; Local Variables:
;; eval: (outline-minor-mode 1)
;; eval: (require 'use-package)
;; End:


;;; init-core.el ends here
