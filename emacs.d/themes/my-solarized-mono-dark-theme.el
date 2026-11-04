;;; -*- lexical-binding: t -*-
(require 'solarized)
(require 'my-solarized)
(require 'my-solarized-mono-palettes)

(deftheme my-solarized-mono-dark "The Solarized colour theme with the monochrome Paper White dark palette")
(with-suppressed-warnings ((obsolete font-lock-warning-face))
  (solarized-with-color-variables
    'dark 'my-solarized-mono-dark my-solarized-mono-dark-color-palette-alist my-solarized-faces))

(provide-theme 'my-solarized-mono-dark)
