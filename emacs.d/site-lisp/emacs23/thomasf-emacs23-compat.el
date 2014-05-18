;; a mixture of ugly hacks and other stuff...

(require 'use-package)
(require 'package)
(define-minor-mode electric-layout-mode
  "No-op version of electric-layout-mode that exists in future versions of emacs."
  :global t
  :group 'electricity
  (message "no-op electric-layout-mode from thomasf-emacs23-compat"))

;; this is just the easiest way of ensuring that modes not written
;; with emacs 23 compatibility in mind might work anyway
;; emacs23 compat
(define-derived-mode prog-mode fundamental-mode "Prog"
  "Major mode for editing programming language source code."
  (set (make-local-variable 'require-final-newline) mode-require-final-newline)
  (set (make-local-variable 'parse-sexp-ignore-comments) t)
  ;; Any programming language is always written left to right.
  (setq bidi-paragraph-direction 'left-to-right))

;; does not exist in emacs23.1
(defun string-prefix-p (str1 str2 &optional ignore-case)
  "Return non-nil if STR1 is a prefix of STR2.
If IGNORE-CASE is non-nil, the comparison is done without paying attention
to case differences."
  (eq t (compare-strings str1 nil nil
                         str2 0 (length str1) ignore-case)))


(unless (fboundp 'org-anniversary) ;; emacs23 bundled org-mode compat
  (defun org-anniversary (year month day &optional mark)
    "Like `diary-anniversary', but with fixed (ISO) order of arguments."
    (org-no-warnings
     (let ((calendar-date-style 'european) (european-calendar-style t))
       (diary-anniversary day month year mark)))))

(unless (fboundp 'org-babel-do-load-languages)
  (defun org-babel-do-load-languages (sym value)
    "No-op"
    (message "warning: invoking non existing function org-babel-do-load-languages")))

(unless (fboundp 'process-live-p)
  (defun process-live-p (process)
    "Returns non-nil if PROCESS is alive.
A process is considered alive if its status is `run', `open',
`listen', `connect' or `stop'."
    (memq (process-status process)
          '(run open listen connect stop))))

(provide 'thomasf-emacs23-compat)
