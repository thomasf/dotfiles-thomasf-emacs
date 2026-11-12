;;; -*- lexical-binding: t -*-
(require 'solarized)
(require 'my-solarized)
(require 'my-solarized-retro-palettes)

(deftheme my-solarized-retro-cga "The Solarized colour theme with the CGA high-intensity palette")
(with-suppressed-warnings ((obsolete font-lock-warning-face))
  (solarized-with-color-variables
    'dark 'my-solarized-retro-cga my-solarized-retro-cga-color-palette-alist my-solarized-faces))

(provide-theme 'my-solarized-retro-cga)
