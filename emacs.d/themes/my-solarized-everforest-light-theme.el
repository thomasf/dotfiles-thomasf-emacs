;;; -*- lexical-binding: t -*-
(require 'solarized)
(require 'my-solarized)
(eval-when-compile
  (require 'solarized-palettes))

;; This files needs to be places iside the custom-theme-load-path list

(deftheme my-solarized-everforest-light "The Solarized colour theme with the Everforest light palette")
(with-suppressed-warnings ((obsolete font-lock-warning-face))
  (solarized-with-color-variables
    'dark 'my-solarized-everforest-light solarized-everforest-light-color-palette-alist my-solarized-faces))

(provide-theme 'my-solarized-everforest-light)
