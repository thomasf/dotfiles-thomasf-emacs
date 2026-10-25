;;; -*- lexical-binding: t -*-
(require 'solarized)
(require 'my-solarized)
(eval-when-compile
  (require 'solarized-palettes))

;; This files needs to be places iside the custom-theme-load-path list

(deftheme my-solarized-selenized-black "The black Selenized variant of the Solarized colour theme")
(with-suppressed-warnings ((obsolete font-lock-warning-face))
  (solarized-with-color-variables
    'black 'my-solarized-selenized-black solarized-selenized-black-color-palette-alist my-solarized-faces))

(provide-theme 'my-solarized-selenized-black)
