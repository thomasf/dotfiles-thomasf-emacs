(eval-and-compile
  (setq package-enable-at-startup nil
        ;; package-install-upgrade-built-in t
    package-user-dir (locate-user-emacs-file (format "elpa.%d" emacs-major-version))))

(setq tool-bar-mode nil
      menu-bar-mode nil
      scroll-bar-mode nil
      frame-inhibit-implied-resize t
      default-frame-alist '((vertical-scroll-bars . nil)
                            (tool-bar-lines . 0)
                            (menu-bar-lines . 0)
                            (fullscreen . nil)))
(setenv "LSP_USE_PLISTS" "true")
