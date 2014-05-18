;;; org-food.el --- Extending org-mode to handle recipe ingredients.
;;
;; Copyright (C) 2009 Erik Hetzner
;;
;; Author: Erik Hetzner <ehetzner@gmail.com>
;;
;; This file is NOT part of GNU Emacs.
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;; calc functions for brewing

(provide 'org-cook)

(require 'calc)
(require 'calc-units)

(defun org-cook-alist-append (alist-var append)
  "Append a list of cons cells to another."
  (mapc
   (lambda (elt-cons)
     (let ((existing-element (assoc (car elt-cons) (symbol-value alist-var))))
       (if existing-element
           (rplacd existing-element (cdr elt-cons))
         (set alist-var (cons elt-cons (symbol-value alist-var))))))
   append))

(defvar org-cook-unit-systems
  '((:dry .
         ((:us . (oz lb))
          (:metric . (g kg))))
    (:liquid .
            ((:us . (drop dash pinch tsp tbsp cup qt gal))
             (:metric . (ml l)))))
  "Tree of units, first unit 'type' (dry or liquid) then unit
'system' (metric or us), then ascending order list of units.")

(defvar org-cook-units-lookup-table
  nil
  "A table of information about units. Generated from
org-cook-unit-systems.")

;; Add units used in cooking to calc units.
(org-cook-alist-append 'math-additional-units
                       '((pinch "tsp / 8" "Pinch")
                         (drop "tsp / 76" "Drop")
                         (dash "drop * 6" "Dash")
                         (jigger "floz * 1.5" "Jigger")
                         (gill "floz * 4" "Gill")))

(setq math-units-table nil)

(defun org-cook-build-units-lookup-table ()
  "Build the org-cook-units-lookup-table from
org-cook-unit-systems."
  (setq org-cook-units-lookup-table '())
  (mapc (lambda (l)
          (let ((type-name (car l))
                (system-list (cdr l)))
            (mapc (lambda (s)
                    (let ((system-name (car s))
                          (unit-list (cdr s)))
                      (mapc (lambda (unit-name)
                              (org-cook-alist-append 'org-cook-units-lookup-table
                                                     `((,unit-name . (,type-name . ,system-name)))))
                            unit-list)))
                  system-list)))
        org-cook-unit-systems))

(defun org-cook-maybe-rebuild-unit-lookup-table ()
  "Rebuilds org-cook-units-lookup-table if it is nil."
  (if (null org-cook-units-lookup-table)
      (org-cook-build-units-lookup-table)))

(defmath foodNumberBadness (n)
  "Calculate the 'badness' of a particular number in terms of its
usefulness as a number for recipes. Currently not very good."
  ;; Numbers between .25 and 5 have 0 badness
  (let ((n1 (math-simplify n)))
    (if (and (>= n1 :"0.2")
             (<= n1 5))
        0
      ;; .01 has the same badness as 100, .1 as 10, etc.
      (- (if (< n1 :"0.2")
             (math-floor (/ 1 n1))
           (math-floor n1))
         5))))

(defun org-cook-make-unit-type (name)
  `(var ,name ,(intern (format "var-%s" (symbol-name name)))))

(defun org-cook-get-unit-info (expr)
  "Returns the unit info: (unit-name . (unit-type .
unit-system)), or nil."
  (org-cook-maybe-rebuild-unit-lookup-table)
  (let ((u (math-units-in-expr-p expr nil))
        (n (math-remove-units expr)))
    (if (null u)
        nil
      (assq (car u) org-cook-units-lookup-table))))

(defun org-cook-convert-units (expr new-unit)
  "Convert a unit EXPR to NEW-UNIT."
  (math-convert-units 
   expr
   (org-cook-make-unit-type new-unit)))

(defun org-cook-convert-ingredient-units (ingr unit)
  "Convert an ingredient to a new UNIX."
  (let* ((new-amount-calc
          (org-cook-convert-units (cdr (assq :amount-calc ingr)) unit))
         (new-amount (org-cook-format-unit new-amount-calc)))
    `((:amount . ,new-amount)
      (:amount-calc . ,new-amount-calc)
      (:item . ,(cdr (assq :item ingr))))))

(defun org-cook-convert-unit-system (expr new-system)
  "Convert a unit expr to a new system, e.g. :metric."
  (let ((unit-info (org-cook-get-unit-info expr)))
    (if (null unit-info)
        nil
      (let* ((u-symbol (car unit-info))
             (u-system-type (cdr unit-info))
             (u-type (car u-system-type))
             (u-system (cdr u-system-type))
             (new-units (cdr (assq new-system (cdr (assq u-type org-cook-unit-systems)))))
             (last-expr nil)
             (last-badness nil))
        (catch 'finished
          (mapc (lambda (try-unit)
                    (let* ((try-expr (org-cook-convert-units expr try-unit))
                           (try-badness (calcFunc-foodNumberBadness
                                         (math-remove-units try-expr))))
                      (if (eq 0 try-badness)
                          (throw 'finished try-expr)
                        (if (and (not (null last-badness))
                                 (> try-badness last-badness))
                            (throw 'finished last-expr)))
                      (setq last-badness try-badness)
                      (setq last-expr try-expr)))
                  new-units)
          ;; if we have got this far, the last was better than anything else
          last-expr)))))

(defun org-cook-toggle-unit-system (expr)
  "Toggles a unit expr from one system to the other, :us ->
:metric or :metric -> :us."
  (let* ((unit-type (cdr (cdr (org-cook-get-unit-info expr))))
         (new-system (if (eq unit-type :metric)
                         :us
                       :metric)))
    (org-cook-convert-unit-system expr new-system)))

(defun org-cook-get-list-item-at-point ()
  "Returns the string containing the list item currently at
point, or nil if not at an item."
  (and (org-at-item-p)
       (save-excursion
         (buffer-substring-no-properties
          (progn (org-beginning-of-item)
                 (point))
          (progn (org-end-of-item)
                 (point))))))

(defun org-cook-replace-list-item-at-point (new-item)
  ""
  (and (org-at-item-p)
       (save-excursion
         (let ((start (progn (org-beginning-of-item)
                             (point)))
               (end (progn (org-end-of-item)
                           (point))))
           (delete-region start end)
           (insert new-item)))))

(defun org-cook-parse-ingredient-at-point ()
  "Returns an alist containing information about an ingredient at
point."
  (and (org-at-item-p)
       (let ((ingr-str (org-cook-get-list-item-at-point)))
         (if (string-match "^-[[:blank:]]*\\([:\\.0-9[:blank:]\\+]+[[:blank:]]+[a-zA-Z]+\\)[[:blank:]]+\\(.*\\)$" ingr-str)
             `((:amount . ,(match-string 1 ingr-str))
               (:item . ,(match-string 2 ingr-str))
               (:amount-calc . ,(condition-case nil
                                   (math-read-expr (match-string 1 ingr-str))
                                 (error nil))))
           nil))))

(defun org-cook-format-ingredient (ingr &optional line-end)
  "Takes an ingredient alist and creates an ingredient line from
it."
  (let ((amount (cdr (assq :amount ingr)))
        (item (cdr (assq :item ingr)))
        (line-end1 (or line-end "\n")))
    (format "- %s %s%s" amount item line-end1)))

(defun org-cook-format-unit (amount)
  (let ((calc-internal-prec 3)
        (str (math-format-flat-expr amount 2)))
    (setq str (replace-regexp-in-string "\\*" "" str))
    (setq str (replace-regexp-in-string "  " " " str))
    (setq str (replace-regexp-in-string
     "^\\([^0-9]\\)" "1 \\1" str))
    (setq str (replace-regexp-in-string "\\([0-9]+\\)\\. " "\\1 " str))
    str))

(defun org-cook-round (expr prec)
  (let ((u (car (math-units-in-expr-p expr nil)))
        (n (math-remove-units expr)))
     `(* ,(math-round (math-simplify (math-remove-units expr)) prec) 
         ,(org-cook-make-unit-type u))))

(defun org-cook-round-ingredient-units (ingr prec)
  (let* ((new-amount-calc (org-cook-round (cdr (assq :amount-calc ingr)) prec))
         (new-amount (org-cook-format-unit new-amount-calc)))
    `((:amount . ,new-amount)
      (:amount-calc . ,new-amount-calc)
      (:item . ,(cdr (assq :item ingr))))))
  
(defun org-cook-round-ingredient-at-point (prec)
  (interactive "nPrecision: ")
  (let* ((old-item (org-cook-get-list-item-at-point))
         (old-ingr (org-cook-parse-ingredient-at-point))
         (new-ingr (org-cook-round-ingredient-units old-ingr prec))
         (line-end (if (string-match "\\(\n+\\)$" old-item)
                       (match-string 1 old-item)
                     "\n")))
    (org-cook-replace-list-item-at-point
     (org-cook-format-ingredient new-ingr line-end))))
        
(defun org-cook-toggle-ingredient-units (ingr)
  "Toggle the :units part of an ingredient to another system."
  (let* ((new-amount-calc (org-cook-toggle-unit-system (cdr (assq :amount-calc ingr))))
         (new-amount (org-cook-format-unit new-amount-calc)))
    `((:amount . ,new-amount)
      (:amount-calc . ,new-amount-calc)
      (:item . ,(cdr (assq :item ingr))))))

(defun org-cook-toggle-ingredient-units-at-point ()
  "Toggle the :units part of an ingredient at point to another system."
  (interactive)
  (let* ((old-item (org-cook-get-list-item-at-point))
         (old-ingr (org-cook-parse-ingredient-at-point))
         (new-ingr (org-cook-toggle-ingredient-units old-ingr))
         (line-end (if (string-match "\\(\n+\\)$" old-item)
                       (match-string 1 old-item)
                     "\n")))
    (org-cook-replace-list-item-at-point
     (org-cook-format-ingredient new-ingr line-end))))

(defun org-cook-get-convertable-units (ingr)
  (let* ((unit (org-cook-get-unit-info (cdr (assq :amount-calc ingr))))
         (unit-type (car (cdr unit)))
         (flatten (lambda  (l)
                    (if l (append (car l) (funcall flatten (cdr l))) nil))))
    (funcall flatten
             (mapcar (lambda (s) (cdr s))
                     (cdr (assq unit-type org-cook-unit-systems))))))
  
(defun org-cook-convert-ingedient-units-at-point ()
  (interactive)
  (let* ((old-item (org-cook-get-list-item-at-point))
         (line-end (if (string-match "\\(\n+\\)$" old-item)
                       (match-string 1 old-item)
                     "\n"))
         (old-ingr (org-cook-parse-ingredient-at-point))
         (old-amount-calc (cdr (assq :amount-calc old-ingr)))
         (new-ingr-strings
          (mapcar 'symbol-name
                  (org-cook-get-convertable-units old-ingr)))
         (new-unit
          (intern
           (completing-read 
            "Unit: " new-ingr-strings))))
    (org-cook-replace-list-item-at-point
     (org-cook-format-ingredient
      (org-cook-convert-ingredient-units old-ingr new-unit) line-end))))

(defun org-cook-round-ingedient-units-at-point ()
  (interactive)
  (let* ((item (org-cook-get-list-item-at-point))
         (line-end (if (string-match "\\(\n+\\)$" old-item)
                       (match-string 1 old-item)
                     "\n"))
    (org-cook-replace-list-item-at-point
     (org-cook-format-ingredient
      (org-cook-convert-ingredient-units old-ingr new-unit) line-end)))))

(defun org-cook-build-fast-lookup (var-name)
  (set var-name '())
  (org-map-entries
   (lambda ()
     (let ((custom-id (org-entry-get (point) "CUSTOM_ID")))
       (if custom-id
           (org-cook-alist-append
            var-name
            `((,custom-id . ,(point)))))))))

(defun org-cook-get-recipe-ingredient-list ()
  (save-excursion
    (org-back-to-heading)
    (let ((h (org-get-heading))
          (ingr-list '()))
      (while (and (equal h (org-get-heading))
                  (null (eobp)))
        (let ((ingr (org-cook-parse-ingredient-at-point)))
          (if ingr
              (setq ingr-list (append ingr-list (list ingr)))))
        (next-line))
      ingr-list)))

(defun org-cook-show-ingredients-only ()
  "Show only the ingredient list for the current recipe."
  (interactive)
  (save-excursion
    (org-overview)
    (org-back-to-heading)
    (org-show-entry)
    (next-line)
    (let ((h (org-get-heading))
          (hiding t)
          (last-point (point)))
      (while (and (equal h (org-get-heading))
                  (null (eobp)))
        (let ((ingr (org-cook-parse-ingredient-at-point)))
          (cond ((and hiding ingr)
                 (outline-flag-region last-point
                                      (- (point) 1) t)
                 (setq last-point (point))
                 (setq hiding nil))
                ((and (not hiding) (not ingr))
                 (setq last-point (- (point) 1))
                 (setq hiding t))))
        (next-line))
      (if hiding
          (outline-flag-region last-point
                               (- (point) 1) t)))))

(defun org-cook-toggle-recipe-units ()
  (interactive)
  (save-excursion
    (org-back-to-heading)
    (let ((h (org-get-heading)))
      (while (and (equal h (org-get-heading))
                  (null (eobp)))
        (if (org-cook-parse-ingredient-at-point)
            (org-cook-toggle-ingredient-units-at-point))
        (next-line)))))
 