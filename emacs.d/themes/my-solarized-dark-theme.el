(require 'solarized)
(require 'my-solarized)
(eval-when-compile
  (require 'solarized-palettes))

;; This files needs to be places iside the custom-theme-load-path list

(deftheme my-solarized-dark "The dark variant of the Solarized colour theme")
(solarized-with-color-variables
  'dark 'my-solarized-dark solarized-dark-high-contrast-palette-alist my-solarized-faces)

(provide-theme 'my-solarized-dark)
