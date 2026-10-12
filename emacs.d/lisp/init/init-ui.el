;;; init-ui.el --- theme, fonts, mode line, windows and frames -*- lexical-binding: nil; byte-compile-warnings: (not docstrings-wide); -*-

;;; Commentary:

;; Appearance, and commands for faces, windows and frames:
;;
;; - theme, fonts, mode line, cursor and the pulse/highlight effects
;; - commands for faces, colors and text scaling
;; - commands for managing windows and frames
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
       (equal "xterm-24bit" (getenv-internal "TERM" initial-environment))
       (equal "alacritty" (getenv-internal "TERM" initial-environment))
       (equal "truecolor" (getenv-internal "COLORTERM" initial-environment)))
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
    ;; (setq mf-theme-dark 'my-solarized-dark
    ;;       mf-theme-light 'my-solarized-light)))

    (setq mf-theme-dark 'my-solarized-selenized-dark
          mf-theme-light 'my-solarized-selenized-light)))


;;;;; post-change-theme

(defun post-change-theme ()
  "Set some stuff that"
  (set-face-inverse-video 'vertical-border nil)
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


;;;; dynamic-fonts

(defvar my-monospaced-font "PragmataPro-12")
(defvar my-variable-pitch-font "Go-12.5")
;; (defvar my-variable-pitch-font "Input Sans Compressed-11.8")
;; (defvar my-monospaced-font "Input Mono Compressed-11.8")

(cond
 ((string-prefix-p "flam" (system-name))
  (setq my-monospaced-font "PragmataPro-12"
        my-variable-pitch-font "Go-12.5"
        dynamic-fonts-preferred-monospace-point-size 12
        dynamic-fonts-preferred-proportional-point-size 12.5))

 ((string-prefix-p "transwhale" (system-name))
  (setq my-monospaced-font "PragmataPro-12"
        my-variable-pitch-font "Go-12.5"
        dynamic-fonts-preferred-monospace-point-size 12
        dynamic-fonts-preferred-proportional-point-size 12.5))

 ((string-prefix-p "prizza" (system-name))
  (setq my-monospaced-font "PragmataPro-15"
        my-variable-pitch-font "Go-15.5"
        dynamic-fonts-preferred-monospace-point-size 15
        dynamic-fonts-preferred-proportional-point-size 15.5))

 ((string-prefix-p "fogskum" (system-name))
  (setq my-monospaced-font "PragmataPro-15"
        my-variable-pitch-font "Go-15.5"
        dynamic-fonts-preferred-monospace-point-size 15
        dynamic-fonts-preferred-proportional-point-size 15.5))

 ((string-prefix-p "crangy" (system-name))
  (setq my-monospaced-font "PragmataPro-17"
        my-variable-pitch-font "Go-17"
        dynamic-fonts-preferred-monospace-point-size 17
        dynamic-fonts-preferred-proportional-point-size 17))

 ((string-prefix-p "dungen" (system-name))
  (setq my-monospaced-font "PragmataPro-16"
        my-variable-pitch-font "Go-16"
        dynamic-fonts-preferred-monospace-point-size 17
        dynamic-fonts-preferred-proportional-point-size 17))

 ((string-prefix-p "tfr-tracklib" (system-name))
  (setq my-monospaced-font "PragmataPro-16"
        my-variable-pitch-font "Go-16"
        dynamic-fonts-preferred-monospace-point-size 17
        dynamic-fonts-preferred-proportional-point-size 17))

 ((string-prefix-p "dangus" (system-name))
  (setq my-monospaced-font "PragmataPro-16"
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
           #'(lambda () (my-pulse-line))))))

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
  (advice-add 'direx:generic-find-item :after 'my-pulse-now)
  (advice-add 'direx:generic-view-item :after 'my-pulse-now)
  ;; end
  )


;;; functions

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


(defun my-set-text-scale-smaller-maybe ()
  (let ((bn (buffer-name)))
    (when
        (and
         (not my-text-scale-smaller-done)
         (s-starts-with? "*[Django: " bn))
      (setq-local my-text-scale-smaller-done t)
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


;;;;; colors

;;;;;; color processing

(defun color-blend-name (color1 color2 alpha)
  "Blends COLOR1 onto COLOR2 using alpha "
  (apply 'color-rgb-to-hex
         (-zip-with #'(lambda (it other)
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
  (x-urgency-hint (selected-frame) (not arg)))

(when window-system
  (unbind-key "C-x C-c")
  ;; (bind-key "C-x C-c" 'intelligent-close)
  (bind-key "s-w" 'intelligent-close)
  )



(provide 'init-ui)

;; Local Variables:
;; eval: (outline-minor-mode 1)
;; eval: (require 'use-package)
;; End:


;;; init-ui.el ends here
