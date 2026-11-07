;;; -*- lexical-binding: t -*-
(require 'solarized)

(setq my-solarized-faces
      '("Personal solarized theme customization"
        (custom-theme-set-faces
         theme-name

         ;; candidates for upstream
         `(child-frame-border ((,class (:background ,s-line))))
         `(minibuffer-nonselected ((,class (:background ,yellow-2bg :foreground ,yellow-2fg :weight bold))))
         `(mode-line-highlight ((,class (:box (:line-width (-1 . -1) :color ,base01)))))
         `(separator-line ((,class (:background ,s-line :height 0.1))))


         `(diff-changed-unspecified ((,class (:background ,s-diff-C-bg :foreground ,s-diff-C-fg :extend t))))
         `(diff-error ((,class (:background ,red-2bg :foreground ,red-2fg :weight bold))))
         `(smerge-refined-changed ((,class (:background ,s-diff-fine-C-bg :foreground ,s-diff-fine-C-fg))))
         `(tab-bar-tab-highlight ((,class (:background ,base02 :foreground ,base1))))

         ;;; lso-mode
         `(lsp-installation-buffer-face ((,class (:foreground ,blue))))
         `(lsp-installation-finished-buffer-face ((,class (:foreground ,green))))
         `(lsp-modeline-code-actions-preferred-face ((,class (:foreground ,yellow :weight bold))))
         `(lsp-ui-peek-filename ((,class (:foreground ,blue))))
         `(lsp-ui-peek-footer ((,class (:inherit lsp-ui-peek-header))))
         `(lsp-ui-peek-header ((,class (:background ,base02 :foreground ,base1 :weight bold))))
         `(lsp-ui-peek-highlight ((,class (:background ,base02 :box (:line-width -1 :color ,magenta)))))
         `(lsp-ui-peek-line-number ((,class (:foreground ,base01))))
         `(lsp-ui-peek-list ((,class (:background ,base02))))
         `(lsp-ui-peek-peek ((,class (:background ,base03))))
         `(lsp-ui-peek-selection ((,class (:background ,base1 :foreground ,base03 :weight bold))))

         ;;; transient
         `(transient-key-stay            ((t (:inherit transient-key :foreground ,green))))
         `(transient-key-return          ((t (:inherit transient-key :foreground ,cyan))))
         `(transient-key-exit            ((t (:inherit transient-key :foreground ,yellow))))
         `(transient-key-recurse         ((t (:inherit transient-key :foreground ,blue))))
         `(transient-key-stack           ((t (:inherit transient-key :foreground ,blue))))
         `(transient-key-noop            ((t (:inherit transient-key :foreground ,base01 :weight normal))))
         `(transient-higher-level        ((t (:box (:line-width (-1 . -1) :color ,s-line)))))

         ;;; Core UI, Editing & Navigation
         `(ansi-color-bright-white ((,class (:background ,base3 :foreground ,base3))))
         `(ansi-color-white ((,class (:background ,base2 :foreground ,base2))))
         `(bookmark-face ((,class (:foreground ,yellow :weight bold))))
         `(confusingly-reordered ((,class (:underline (:style wave :color ,red)))))
         `(dired-broken-symlink ((,class (:foreground ,red :weight bold :underline t))))
         `(edmacro-label ((,class (:inherit bold :foreground ,blue))))
         `(epa-field-body ((,class (:foreground ,cyan :slant italic))))
         `(epa-field-name ((,class (:foreground ,blue :weight bold))))
         `(git-gutter:separator ((,class (:foreground ,s-line :weight bold))))
         `(holiday ((,class (:background ,base02 :foreground ,yellow))))
         `(homoglyph ((,class (:foreground ,cyan))))
         `(ibuffer-locked-buffer ((,class (:foreground ,base01))))
         `(icon-button ((,class (:inherit icon :background ,base02 :foreground ,base1 :box (:line-width (3 . -1) :color ,s-line :style flat-button)))))
         `(isearch-group-1 ((,class (:background ,cyan-1bg :foreground ,cyan-1fg))))
         `(isearch-group-2 ((,class (:background ,blue-1bg :foreground ,blue-1fg))))
         `(ivy-cursor ((,class (:background ,base0 :foreground ,base03))))
         `(llama-deleted-argument ((,class (:box (:line-width (-1 . -1) :color ,red :style nil)))))
         `(lv-separator ((,class (:background ,s-line))))
         `(markdown-highlighting-face ((,class (:background ,base02 :foreground ,yellow))))
         `(nobreak-hyphen ((,class (:foreground ,cyan))))
         `(pulse-highlight-face ((,class (:background ,base02 :extend t))))
         `(sp-wrap-overlay-closing-pair ((,class (:foreground ,red :weight bold))))
         `(sp-wrap-overlay-opening-pair ((,class (:foreground ,green :weight bold))))
         `(tool-bar ((,class (:background ,base02 :foreground ,base0))))
         `(tty-menu-disabled-face ((,class (:background ,base02 :foreground ,base01))))
         `(tty-menu-enabled-face ((,class (:background ,base02 :foreground ,base1 :weight bold))))
         `(tty-menu-selected-face ((,class (:background ,base1 :foreground ,base03 :weight bold))))
         `(widget-button-pressed ((,class (:foreground ,red))))
         `(widget-documentation ((,class (:foreground ,base01 :slant italic))))
         `(widget-single-line-field ((,class (:background ,base02))))

         ;;; Custom Mode
         `(custom-changed ((,class (:background ,blue-1bg :foreground ,blue-1fg))))
         `(custom-comment ((,class (:background ,base02 :foreground ,base01))))
         `(custom-invalid ((,class (:background ,red-1bg :foreground ,red-1fg :weight bold))))
         `(custom-modified ((,class (:background ,blue-1bg :foreground ,blue-1fg))))
         `(custom-rogue ((,class (:background ,magenta-1bg :foreground ,magenta-1fg))))
         `(custom-set ((,class (:background ,base02 :foreground ,blue))))
         `(custom-themed ((,class (:background ,blue-1bg :foreground ,blue-1fg))))
         `(custom-variable-obsolete ((,class (:foreground ,base01 :strike-through t))))

         ;;; Org Mode
         `(org-dispatcher-highlight ((,class (:background ,base02 :foreground ,yellow :weight bold))))
         `(org-headline-todo ((,class (:foreground ,cyan :weight bold))))
         `(org-latex-and-related ((,class (:foreground ,yellow-hc))))
         `(org-table-header ((,class (:inherit org-table :background ,base02 :foreground ,base1 :weight bold))))

         ;;; LSP Headerline & Breadcrumbs
         `(lsp-headerline-breadcrumb-path-error-face ((,class (:inherit lsp-headerline-breadcrumb-path-face :underline (:style wave :color ,red)))))
         `(lsp-headerline-breadcrumb-path-warning-face ((,class (:inherit lsp-headerline-breadcrumb-path-face :underline (:style wave :color ,yellow)))))
         `(lsp-headerline-breadcrumb-path-info-face ((,class (:inherit lsp-headerline-breadcrumb-path-face :underline (:style wave :color ,blue)))))
         `(lsp-headerline-breadcrumb-path-hint-face ((,class (:inherit lsp-headerline-breadcrumb-path-face :underline (:style wave :color ,cyan)))))
         `(lsp-headerline-breadcrumb-symbols-error-face ((,class (:inherit lsp-headerline-breadcrumb-symbols-face :underline (:style wave :color ,red)))))
         `(lsp-headerline-breadcrumb-symbols-warning-face ((,class (:inherit lsp-headerline-breadcrumb-symbols-face :underline (:style wave :color ,yellow)))))
         `(lsp-headerline-breadcrumb-symbols-info-face ((,class (:inherit lsp-headerline-breadcrumb-symbols-face :underline (:style wave :color ,blue)))))
         `(lsp-headerline-breadcrumb-symbols-hint-face ((,class (:inherit lsp-headerline-breadcrumb-symbols-face :underline (:style wave :color ,cyan)))))

         ;;; Flycheck Annotations
         `(flycheck-annotate-error-background ((,class (:background ,s-diff-A-bg :extend t))))
         `(flycheck-annotate-warning-background ((,class (:background ,yellow-1bg :extend t))))
         `(flycheck-annotate-info-background ((,class (:background ,blue-1bg :extend t))))

         ;;; Magit & Git Diffs
         `(magit-diff-added-indicator ((,class (:foreground ,green))))
         `(magit-diff-removed-indicator ((,class (:foreground ,red))))
         `(magit-diff-our-heading ((,class (:background ,s-diff-A-bg :foreground ,s-diff-A-fg :extend t))))
         `(magit-diff-their-heading ((,class (:background ,s-diff-B-bg :foreground ,s-diff-B-fg :extend t))))
         `(magit-diff-base-heading ((,class (:background ,s-diff-Ancestor-bg :foreground ,s-diff-Ancestor-fg :extend t))))
         `(magit-diff-base-indicator ((,class (:foreground ,yellow))))
         `(magit-signature-error ((,class (:foreground ,red :weight bold))))
         `(magit-signature-expired ((,class (:foreground ,yellow))))
         `(magit-signature-revoked ((,class (:foreground ,red))))

         ;;; Treemacs
         `(treemacs-fringe-indicator-face ((,class (:foreground ,base01))))
         `(treemacs-marked-file-face ((,class (:background ,base02 :foreground ,yellow :weight bold))))
         `(treemacs-on-success-pulse-face ((,class (:background ,green-1bg :extend t))))
         `(treemacs-on-failure-pulse-face ((,class (:background ,red-1bg :extend t))))
         `(treemacs-peek-mode-indicator-face ((,class (:background ,green))))

         ;;; Elisp
         `(elisp-major-mode-name ((,class (:foreground ,green))))
         `(elisp-rx ((,class (:foreground ,blue))))
         `(elisp-shorthand-font-lock-face ((,class (:inherit font-lock-keyword-face :foreground ,cyan))))
         `(elisp-symbol-at-mouse ((,class (:background ,base02))))
         `(elisp-symbol-role ((,class (:inherit font-lock-function-call-face :foreground ,blue))))
         `(elisp-symbol-role-definition ((,class (:inherit font-lock-function-name-face :foreground ,blue))))
         `(elisp-unknown-call ((,class (:inherit font-lock-function-call-face :foreground ,cyan))))
         `(elisp-condition ((,class (:foreground ,red))))


         ;;; my customizations

         `(rainbow-delimiters-depth-1-face ((,class (:foreground ,base02))))
         `(hydra-face-red ((,class (:foreground ,base1 :weight bold)))) ;; upstreamed

         `(hydra-face-blue ((,class (:foreground ,blue :weight bold))))
         `(hydra-face-amaranth ((,class (:foreground ,base1 :weight bold))))
         `(hydra-face-pink ((,class (:foreground ,magenta :weight bold))))
         `(hydra-face-teal ((,class (:foreground ,cyan :weight bold))))


         ;; tese are probably a good candidates for upstream
         `(avy-goto-char-timer-face ((,class (:inherit isearch))))
         `(avy-background-face ((,class (:background ,base02 :foreground ,base0 :extend t))))

         ;; these are personal and maybe somewhat specific to the patched version of avy functions in init.el
         `(avy-lead-face ((,class (:weight bold :foreground ,magenta))))
         `(avy-lead-face-0 ((,class (:weight bold :foreground ,blue))))
         `(avy-lead-face-1 ((,class (:weight bold :foreground ,base1))))
         `(avy-lead-face-2 ((,class (:weight bold :foreground  ,cyan))))

         )
        (custom-theme-set-variables
         theme-name
         `(org-todo-keyword-faces
           (quote (("TODO" :foreground ,cyan :weight bold :slant italic :underline nil)
                   ("NEXT" :foreground ,blue :weight bold :slant italic :underline nil)
                   ("ACTIVE" :foreground ,blue :weight bold :slant italic :underline nil)
                   ("DONE" :foreground ,base01 :weight bold :slant italic :underline nil)
                   ("WAITING" :foreground ,yellow :weight bold :slant italic :underline nil)
                   ("HOLD" :foreground ,yellow :weight bold :slant italic :underline nil)
                   ("NOTE" :foreground ,green :weight bold :slant italic :underline nil)
                   ("SOMEDAY" :foreground ,base01 :weight bold :slant italic :underline nil)
                   ("CANCELLED" :foreground ,base01 :weight bold :slant italic :underline nil)
                   ("PHONE" :foreground ,blue :weight bold :slant italicg :underline nil))))

         `(jump-char-lazy-highlight-face nil)
         `(org-tag-faces
           '(("@home"  :foreground ,green)
             ("@office" :foreground ,blue)
             ("@errand" :foreground ,cyan)
             ("@travel" :foreground ,violet)
             ("task"  :foreground ,yellow)
             ("bug"  :foreground ,yellow)
             ("enhancement"  :foreground ,yellow)
             ("work" :slant italic :foreground ,base01)
             ("personal" :slant italic :foreground ,base01)
             ("23c" :slant italic :background ,base02)
             ("bulk" :foreground ,base01)
             ("hold" :foreground ,green)
             ("note" :foreground ,base01)
             ("waiting" :foreground ,orange)
             ("cancelled" :foreground ,green)
             ("flagged" :foreground ,red))))))

(provide 'my-solarized)
