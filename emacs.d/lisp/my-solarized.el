;;; my-solarized.el --- Personalization of emacs24 solarized theme

;;; Commentary:
;;

(require 'solarized)

;;; Code:

(defun my-solarized-theme ()
  "My solarized child theme."

  (custom-theme-set-faces
   theme-name


   ;;                                  _
   ;;  _ __  ___ _ _ ___ ___ _ _  __ _| |
   ;; | '_ \/ -_) '_(_-</ _ \ ' \/ _` | |
   ;; | .__/\___|_| /__/\___/_||_\__,_|_|
   ;; |_|
   ;; what other people might not want


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
