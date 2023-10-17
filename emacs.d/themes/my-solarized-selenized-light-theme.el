(require 'solarized)
(require 'my-solarized)
(eval-when-compile
  (require 'solarized-palettes))

;; This files needs to be places iside the custom-theme-load-path list

(deftheme my-solarized-selenized-light
  "The light variant of the Solarized colour theme")

(solarized-with-color-variables 'dark 'my-solarized-selenized-light
 solarized-selenized-light-color-palette-alist my-solarized-faces)

(provide-theme 'my-solarized-selenized-light)
