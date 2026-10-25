;;; -*- lexical-binding: t -*-
(require 'solarized)
(require 'my-solarized)
(eval-when-compile
  (require 'solarized-palettes))

;; This files needs to be places iside the custom-theme-load-path list

(deftheme my-solarized-gruvbox-light "The Solarized colour theme with the gruvbox light palette")
(with-suppressed-warnings ((obsolete font-lock-warning-face))
  (solarized-with-color-variables
    'light 'my-solarized-gruvbox-light solarized-gruvbox-light-color-palette-alist my-solarized-faces))

(provide-theme 'my-solarized-gruvbox-light)
