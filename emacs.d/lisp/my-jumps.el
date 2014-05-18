;; for now only a dummy package
(require 'jump)

;; NOTE jump are defined in this file since they cause macroexpand issues in
;; older emacs versions

(defjump
  django-toggle-app
  (("views.py"  . "models.py")
   ("models.py"  . "views.py"))
  default-directory
  "Go to the most logical model given the current location."
  nil
  (lambda () ""))

(provide 'my-jumps)
