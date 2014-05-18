(setq force-load-messages nil
      use-package-verbose nil
      use-package-debug nil)
(defun ask-user-about-lock (file opponent)
  "always grab lock."
  (warn "Grabbing lock for: %s" file)
  t)
(load (expand-file-name "init" (file-name-directory load-file-name)) nil t)
(recompile-dot-lisp)
