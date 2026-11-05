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
         `(transient-key-stay            ((t (:inherit transient-key :foreground ,green))))
         `(transient-key-return          ((t (:inherit transient-key :foreground ,yellow))))
         `(transient-key-exit            ((t (:inherit transient-key :foreground ,red))))
         `(transient-key-recurse         ((t (:inherit transient-key :foreground ,blue))))
         `(transient-key-stack           ((t (:inherit transient-key :foreground ,magenta))))
         `(transient-key-noop            ((t (:inherit transient-key :foreground ,base01))))
         `(transient-higher-level        ((t (:box (:line-width (-1 . -1) :color ,s-line)))))
         `(diff-changed-unspecified ((,class (:background ,s-diff-C-bg :foreground ,s-diff-C-fg :extend t))))
         `(diff-error ((,class (:background ,red-2bg :foreground ,red-2fg :weight bold))))
         `(smerge-refined-changed ((,class (:background ,s-diff-fine-C-bg :foreground ,s-diff-fine-C-fg))))
         `(tab-bar-tab-highlight ((,class (:background ,base02 :foreground ,base1))))

         ;;;

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
