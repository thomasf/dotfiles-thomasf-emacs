;;; sheet-mode.el --- cheat sheets...

;;; Commentary:
;;

(require 'ido)

;; sheet mode
(defconst sheet-mode-font-lock-keywords
  (list
   ;; # header
   '("^\\(#+\\) \\(.*\\)"
     (1 font-lock-function-name-face)
     (2 font-lock-function-name-face))
   ;; key ,, value
   '("^\\(.*\\)\\( *,, *\\)\\(.*\\)"
     (1 font-lock-keyword-face)
     (2 font-lock-comment-face)
     (3 font-lock-doc-face))
   ;; $ command example
   ;; - list item
   '("^ *\\([-$] \\)\\(.*\\)"
     (1 font-lock-comment-face)
     (2 font-lock-keyword-face))
   '("^  \\(.*\\)"
     (1 font-lock-keyword-face))
   ;; Section::
   '("\\(.*\\)\\(::\\)$"
     (1 font-lock-type-face)
     (2 font-lock-comment-face))))

(define-derived-mode sheet-mode text-mode "Sheet"
  "Major mode for viewing and editing cheat my cheat files."
  (setq tab-width 2)
  (set (make-local-variable 'text-mode-variant) t)
  (set (make-local-variable 'indent-line-function) 'indent-relative)
  (set (make-local-variable 'font-lock-defaults)
       '(sheet-mode-font-lock-keywords)))


(provide 'sheet-mode)

;;; sheet-mode.el ends here
