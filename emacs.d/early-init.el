(eval-and-compile
  (setq package-enable-at-startup nil))
(setq tool-bar-mode nil
      menu-bar-mode nil
      scroll-bar-mode nil)

(setq frame-inhibit-implied-resize t
      default-frame-alist '((vertical-scroll-bars . nil)
                            (tool-bar-lines . 0)
                            (menu-bar-lines . 0)
                            (fullscreen . nil)))
