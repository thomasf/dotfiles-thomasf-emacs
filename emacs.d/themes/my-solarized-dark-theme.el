(require 'solarized)
(require 'my-solarized)

(deftheme my-solarized-dark "The light variant of the Solarized colour theme")
(create-solarized-theme 'dark 'my-solarized-dark 'my-solarized-theme)

(provide-theme 'my-solarized-dark)
