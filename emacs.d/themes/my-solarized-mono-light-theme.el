;;; -*- lexical-binding: t -*-
(require 'solarized)
(require 'my-solarized)
(require 'my-solarized-mono-palettes)

(deftheme my-solarized-mono-light "The Solarized colour theme with the monochrome Paper White light palette")
(with-suppressed-warnings ((obsolete font-lock-warning-face))
  (solarized-with-color-variables
    'light 'my-solarized-mono-light my-solarized-mono-light-color-palette-alist my-solarized-faces))

(provide-theme 'my-solarized-mono-light)
