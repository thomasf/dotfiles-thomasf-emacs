(require 'solarized)
(require 'my-solarized)
(eval-when-compile
  (require 'solarized-palettes))

;; This files needs to be places iside the custom-theme-load-path list

(deftheme my-solarized-selenized-dark "The dark variant of the Solarized colour theme")
(solarized-with-color-variables
  'dark 'my-solarized-selenized-dark solarized-selenized-dark-color-palette-alist my-solarized-faces)

(provide-theme 'my-solarized-selenized-dark)
