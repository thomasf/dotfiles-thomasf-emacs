;;; -*- lexical-binding: t -*-
(require 'solarized)
(require 'my-solarized)
(require 'my-solarized-primary-palettes)

(deftheme my-solarized-primary-dark "The Solarized colour theme with the pure primary dark palette")
(with-suppressed-warnings ((obsolete font-lock-warning-face))
  (solarized-with-color-variables
    'dark 'my-solarized-primary-dark my-solarized-primary-dark-color-palette-alist my-solarized-faces))

(provide-theme 'my-solarized-primary-dark)
