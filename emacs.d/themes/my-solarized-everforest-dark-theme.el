;;; -*- lexical-binding: t -*-
(require 'solarized)
(require 'my-solarized)
(eval-when-compile
  (require 'solarized-palettes))

;; This files needs to be places iside the custom-theme-load-path list

(deftheme my-solarized-everforest-dark "The Solarized colour theme with the Everforest dark palette")
(with-suppressed-warnings ((obsolete font-lock-warning-face))
  (solarized-with-color-variables
    'dark 'my-solarized-everforest-dark solarized-everforest-dark-color-palette-alist my-solarized-faces))

(provide-theme 'my-solarized-everforest-dark)
