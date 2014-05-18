;;; my-solarized.el --- Personalization of emacs24 solarized theme

;;; Commentary:
;;

(require 'solarized)

;;; Code:

(defun my-solarized-childtheme ()
  "My solarized child theme."

  (custom-theme-set-faces
   theme-name

   ;; notes -
   ;; file =      fg:normal
   ;; directory = fg:blue
   ;; hlite =
   ;;       bg:hl
   ;;       bg:hl+fg:emph+bold
   ;;       bg:hl+fg:yellow+bold
   ;;       bg:hl+fg:green+(maybe)bold

   ;; mark = fg:magenta+bold


   ;;                        _               _        _
   ;;  _____ ___ __  ___ _ _(_)_ __  ___ _ _| |_ __ _| |
   ;; / -_) \ / '_ \/ -_) '_| | '  \/ -_) ' \  _/ _` | |
   ;; \___/_\_\ .__/\___|_| |_|_|_|_\___|_||_\__\__,_|_|
   ;;         |_|
   ;; testing out ideas and maybe solving problems

   ;; TODO i don't like this. should be normal bg.
   `(ein:cell-input-area ((,class (:background ,solarized-hl))))
   `(ein:cell-output-stderr ((,class (:background ,solarized-bg :underline ,orange))))

   ;; stripe ????
   `(stripe-highlight ((,class (:background "#CCCCCC"))))
   `(stripe-hl-line ((,class (:background "#657b83" :foreground "#fdf6e3"))))



   ;; `(highlight-indentation-face
   ;;   ((,class (:background nil
   ;;                         :foreground ,base02
   ;;                         :stipple ,(list (frame-char-width) 1
   ;;                                         (string (frame-char-width)))))))

   ;; `(highlight-indentation-current-column-face
   ;;   ((,class (:background nil
   ;;                         :foreground ,s-line
   ;;                         ;; :foreground ,base01
   ;;                         :stipple ,(list (frame-char-width) 1
   ;;                                           (string (frame-char-width)))))))

   `(indent-guide-face ((,class (:foreground ,s-line))))




   ;;               _           _   _
   ;;  _____ ____ _| |_  _ __ _| |_(_)___ _ _
   ;; / -_) V / _` | | || / _` |  _| / _ \ ' \
   ;; \___|\_/\__,_|_|\_,_\__,_|\__|_\___/_||_|
   ;; might go into solarized theme at some point

   ;; org-mode
   `(org-agenda-structure
     ((,class (:inherit variable-pitch :foreground ,solarized-fg :background ,solarized-hl
                        :weight normal :slant normal :inverse-video nil :height 1.2
                        :underline nil
                        :box (:line-width 2 :color ,solarized-hl)))))

   ;; fic
   `(fic-author-face ((,class (:background nil :foreground nil
                                           :underline unspecified :slant italic :weight bold))))
   `(fic-face ((,class (:background nil :foreground nil
                                    :weight bold :slant italic))))
   `(font-lock-fic-face ((,class (:background nil :foreground nil
                                              :weight bold :slant italic))))

   `(page-break-lines ((,class (:foreground ,s-line  :slant normal :weight normal))))



   ;;                                  _
   ;;  _ __  ___ _ _ ___ ___ _ _  __ _| |
   ;; | '_ \/ -_) '_(_-</ _ \ ' \/ _` | |
   ;; | .__/\___|_| /__/\___/_||_\__,_|_|
   ;; |_|
   ;; what other people might not want


   ;; flymake
   `(flymake-infoline ((,class (:background unspecified :underline unspecified))))
   `(flymake-indicator-error ((,class (:foreground ,red :background ,solarized-bg))))
   `(flymake-indicator-warning ((,class (:foreground ,yellow :background  ,solarized-bg))))
   `(flymake-indicator-info ((,class (:foreground ,solarized-comments :background ,solarized-bg))))

   ;; flycheck
   ;; `(flycheck-fringe-error ((,class (:foreground ,red :background ,solarized-bg))))
   ;; `(flycheck-fringe-warning ((,class (:foreground ,yellow :background  ,solarized-bg))))
   ;; `(flycheck-fringe-info ((,class (:foreground ,solarized-comments :background ,solarized-bg))))

   ;; rainbow-delimiters
   `(rainbow-delimiters-depth-1-face ((,class (:foreground ,solarized-comments))))

   `(sp-show-pair-match-face
     ((,class (:background unspecified :foreground ,magenta
                           :weight unspecified))))
   `(sp-show-pair-mismatch-face
     ((,class (:foreground ,base02 :background ,red
                           :weight unspecified))))
     ;; git-gutter
     ;; `(git-gutter:added ((,class (:foreground ,solarized-comments :background ,base03
     ;;                                          :weight normal))))
     ;; `(git-gutter:deleted ((,class (:foreground ,solarized-comments :background ,base03
     ;;                                            :weight normal))))
     ;; `(git-gutter:modified ((,class (:foreground ,solarized-comments :background ,base03
     ;;                                             :weight normal))))
     ;; `(git-gutter:unchanged ((,class (:foreground ,base02
     ;;                                              :background ,base03
     ;;                                              :weight normal))))
 
   ;; org-mode
   `(org-tag ((,class (:weight unspecified :slant unspecified :inverse-video nil :foreground ,solarized-comments))))
   `(org-special-keyword ((,class (:foreground ,solarized-comments :weight unspecified :slant italic :height 0.9)))))

  (custom-theme-set-variables
   theme-name
   ;; `(pos-tip-foreground-color ,solarized-emph)
   ;; `(pos-tip-background-color ,solarized-hl)
   `(popup-isearch-match ((,class (:background ,solarized-fg :foreground ,magenta :weight bold ))))
   `(org-todo-keyword-faces
     (quote (("TODO" :foreground ,cyan :weight bold :slant italic :underline nil)
             ("NEXT" :foreground ,blue :weight bold :slant italic :underline nil)
             ("ACTIVE" :foreground ,blue :weight bold :slant italic :underline nil)
             ("DONE" :foreground ,solarized-comments :weight bold :slant italic :underline nil)
             ("WAITING" :foreground ,yellow :weight bold :slant italic :underline nil)
             ("HOLD" :foreground ,yellow :weight bold :slant italic :underline nil)
             ("NOTE" :foreground ,green :weight bold :slant italic :underline nil)
             ("SOMEDAY" :foreground ,solarized-comments :weight bold :slant italic :underline nil)
             ("CANCELLED" :foreground ,solarized-comments :weight bold :slant italic :underline nil)
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
       ("work" :slant italic :foreground ,solarized-comments)
       ("personal" :slant italic :foreground ,solarized-comments)
       ("23c" :slant italic :background ,solarized-hl)
       ("bulk" :foreground ,solarized-comments)
       ("hold" :foreground ,green)
       ("note" :foreground ,solarized-comments)
       ("waiting" :foreground ,orange)
       ("cancelled" :foreground ,green  )
       ("flagged" :foreground ,red))

     ;; custom-set-variables end
     )))

(provide 'my-solarized)

;; Local variables:
;; byte-compile-dynamic: t
;; byte-compile-warnings: (not free-vars)
;; eval: (rainbow-mode 1)
;; End:
;;; my-solarized.el ends here
