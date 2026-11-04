;;; -*- lexical-binding: t -*-
(require 'solarized)
(require 'my-solarized)
(require 'my-solarized-mono-palettes)

(deftheme my-solarized-mono-green "The Solarized colour theme with the monochrome Green CRT palette")
(with-suppressed-warnings ((obsolete font-lock-warning-face))
  (solarized-with-color-variables
    'dark 'my-solarized-mono-green my-solarized-mono-green-color-palette-alist my-solarized-faces))

(provide-theme 'my-solarized-mono-green)
