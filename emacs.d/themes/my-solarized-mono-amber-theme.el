;;; -*- lexical-binding: t -*-
(require 'solarized)
(require 'my-solarized)
(require 'my-solarized-mono-palettes)

(deftheme my-solarized-mono-amber "The Solarized colour theme with the monochrome Amber CRT palette")
(with-suppressed-warnings ((obsolete font-lock-warning-face))
  (solarized-with-color-variables
    'dark 'my-solarized-mono-amber my-solarized-mono-amber-color-palette-alist my-solarized-faces))

(provide-theme 'my-solarized-mono-amber)
