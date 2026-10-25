;;; -*- lexical-binding: t -*-
(require 'solarized)
(require 'my-solarized)
(eval-when-compile
  (require 'solarized-palettes))

;; This files needs to be places iside the custom-theme-load-path list

(deftheme my-solarized-wombat-dark "The Solarized colour theme with the wombat palette")
(with-suppressed-warnings ((obsolete font-lock-warning-face))
  (solarized-with-color-variables-with-palette
    'dark 'my-solarized-wombat-dark
  '("#2a2a29" "#f6f3e8"           ; base03 (02 01 00 0 1 2) base3
    "#e5c06d" "#ddaa6f"           ; yellow orange
    "#ffb4ac" "#e5786d"           ; red    magenta
    "#834c98" "#a4b5e6"           ; violet blue
    "#7ec98f" "#8ac6f2"           ; cyan   green
    )
    ;; solarized-wombat-dark's own face changes, copied from it, then mine
    (append
  '((custom-theme-set-faces
     theme-name
     `(default ((,class (:foreground ,(solarized-color-blend base03 base3 0.15 2) :background ,base03))))
     `(highlight ((,class (:background ,violet))))
     `(font-lock-builtin-face ((,class (:foreground ,magenta))))
     `(font-lock-constant-face ((,class (:foreground ,blue))))
     `(font-lock-comment-face ((,class (:foreground ,base00))))
     `(mode-line
       ((,class (:foreground ,base2 :background ,(solarized-color-blend base03 base3 0.85 2)))))
     `(mode-line-inactive
       ((,class (:foreground ,base00 :background ,(solarized-color-blend base03 "black" 0.85 2)))))
     `(mode-line-buffer-id ((,class (:foreground ,base3 :weight bold))))
     `(minibuffer-prompt ((,class (:foreground ,base1))))))
     my-solarized-faces)))

(provide-theme 'my-solarized-wombat-dark)
