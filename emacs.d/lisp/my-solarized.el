(require 'solarized)


(setq my-solarized-faces
      '("Personal solarized theme customization"
        (custom-theme-set-faces
         theme-name
         `(rainbow-delimiters-depth-1-face ((,class (:foreground ,base02))))
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
