;;; pretty-mode.el --- redisplay parts of the buffer as pretty symbols
;;; -*- coding: utf-8 -*-

;;; Commentary:
;;
;; Minor mode for redisplaying parts of the buffer as pretty symbols
;; originally modified from Trent Buck's version at http://paste.lisp.org/display/42335,2/raw
;; Also includes code from `sml-mode'
;; See also http://www.emacswiki.org/cgi-bin/wiki/PrettyLambda
;;
;; Released under the GPL. No implied warranties, etc. Use at your own risk.
;; Arthur Danskin <arthurdanskin@gmail.com>, March 2008
;;
;; to install:
;; (require 'pretty-mode)
;; and
;; (global-pretty-mode 1)
;; or
;; (add-hook 'my-pretty-language-hook 'turn-on-pretty-mode)

;;; Code:

(eval-when-compile
  (require 'cl))

(defvar pretty-syntax-types '(?_ ?w ?\\))
(defvar pretty-padding 2)
(defvar pretty-current-point 1)
(defvar pretty-current-line-beginning 1)
(defvar pretty-current-line-end 1)

(defvar pretty-mode-symbol-face 'pretty-mode-symbol-face)
(defface pretty-mode-symbol-face
  '((t :inherit font-lock-negation-char-face))
  "Default symbol face"
  :group 'pretty-mode)

;; modified from `sml-mode'
(defun pretty-font-lock-compose-symbol (alist)
  "Compose a sequence of ascii chars into a symbol."
  ;; Check that the chars should really be composed into a symbol.
  (let* ((start (match-beginning 0))
         (end (match-end 0))
         (match (match-string 0))
         (syntax (char-syntax (char-after start))))
    (if (or
         ;; Full line
         ;; (and (<= pretty-current-line-beginning start) (>= pretty-current-line-end end))
         ;; Border chars
         (and
          (>= pretty-current-point (- start pretty-padding))
          (<= pretty-current-point (+ end pretty-padding)))
         (or
          (if (memq syntax pretty-syntax-types)
                (or
                 (and
                    (memq (char-syntax (aref match 0)) pretty-syntax-types)
                    (memq (char-syntax (char-before start)) pretty-syntax-types))
                 (and
                    (memq (char-syntax (aref match (- (length match) 1))) pretty-syntax-types)
                   (memq (char-syntax (char-after end)) pretty-syntax-types)))
              (memq (char-syntax (char-before start)) '(?. ?\\)))
            (memq (get-text-property start 'face)
                 '(font-lock-doc-face font-lock-string-face
                                      font-lock-comment-face))))
        ;; No composition for you. Let's actually remove any composition
        ;; we may have added earlier and which is now incorrect.
        (prog1
            nil
          (decompose-region start end))
      ;; That's a symbol alright, so add the composition.
      (prog1
          pretty-mode-symbol-face
        (let ((chr nil))
          (loop for (regexp . char) in alist do
                (when (string-match (concat "^" regexp "$") match)
                  (setq chr char)))
          (compose-region start end chr))))))

(defvar pretty-interaction-mode-alist
  '((inferior-scheme-mode . scheme-mode)
    (lisp-interaction-mode . emacs-lisp-mode)
    (inferior-lisp-mode . lisp-mode)
    (inferior-ess-mode . ess-mode)
    (inf-haskell-mode . haskell-mode)
    (tuareg-interactive-mode . tuareg-mode)
    (inferior-python-mode . python-mode)
    (inferior-octave-mode . octave-mode)
    (inferior-ruby-mode . ruby-mode))
  "Alist mapping from inferior process interaction modes to their
  corresponding script editing modes.")


(defun pretty-font-lock-keywords (alist)
  "Return a `font-lock-keywords' style entry for replacing
regular expressions with symbols. ALIST has the form ((STRING .
REPLACE-CHAR) ...)."
  (when alist
    `((,(concat "\\(?:" (mapconcat 'identity (mapcar 'car alist) "\\|") "\\)")
       (0 (pretty-font-lock-compose-symbol
             ',alist))))))

(defun pretty-keywords (&optional mode)
  "Return the font-lock keywords for MODE, or the current mode if
MODE is nil. Return nil if there are no keywords."
  (let* ((mode (or mode major-mode))
         (kwds (cdr-safe
                (or (assoc mode pretty-patterns)
                    (assoc (cdr-safe
                            (assoc mode pretty-interaction-mode-alist))
                           pretty-patterns)))))
    (pretty-font-lock-keywords kwds)))

(defgroup pretty nil "Minor mode for replacing text with symbols "
  :group 'faces)

(defun pretty-line-update ()
  "Decompose current line"
  (setq pretty-current-point (point))
  (setq pretty-current-line-beginning (line-beginning-position))
  (setq pretty-current-line-end (line-end-position))
  (font-lock-fontify-buffer))

(define-minor-mode pretty-mode
  "Toggle Pretty minor mode.
With arg, turn Pretty minor mode on if arg is positive, off otherwise.

Pretty mode builds on `font-lock-mode'. Instead of highlighting
keywords, it replaces them with symbols. For example, lambda is
displayed as λ in lisp modes."
  :group 'pretty
  :lighter " λ"

  (if pretty-mode
      (progn
        (add-hook 'post-command-hook 'pretty-line-update nil t)
        (font-lock-add-keywords nil (pretty-keywords))
        (font-lock-fontify-buffer))
    (remove-hook 'post-command-hook 'pretty-line-update t)
    (font-lock-remove-keywords nil (pretty-keywords))
    (remove-text-properties (point-min) (point-max) '(composition nil))
    (font-lock-fontify-buffer)))

(defun turn-on-pretty-if-desired ()
  "Turn on `pretty-mode' if the current major mode supports it."
  (if (pretty-keywords)
      (pretty-mode 1)))

(define-globalized-minor-mode global-pretty-mode
  pretty-mode turn-on-pretty-if-desired
  :init-value t)

(defun turn-off-pretty-mode ()
  (interactive)
  (pretty-mode -1))


(defun turn-on-pretty-mode ()
  (interactive)
  (pretty-mode +1))

(defun pretty-compile-patterns (patterns)
  "Set pretty patterns in a convenient way.

PATTERNS should be of the form ((GLYPH (REGEXP MODE ...) ...)
...). GLYPH should be a character. MODE should be the name of a
major mode without the \"-mode\". Returns patterns in the form
expected by `pretty-patterns'"
  (let ((pretty-patterns))
    (loop for (glyph . pairs) in patterns do
          (loop for (regexp . major-modes) in pairs do
                (loop for mode in major-modes do
                      (let* ((mode (intern (concat (symbol-name mode)
                                                   "-mode")))
                             (assoc-pair (assoc mode pretty-patterns))

                             (entry (cons regexp glyph)))
                        (if assoc-pair
                            (push entry (cdr assoc-pair))
                          (push (cons mode (list entry))
                                pretty-patterns))))))
    pretty-patterns))

(defvar pretty-patterns nil
  "*List of pretty patterns.
Should be a list of the form ((MODE ((REGEXP . GLYPH) ...)) ...)")

(setq pretty-patterns
  (let* ((lispy '(scheme emacs-lisp lisp clojure))
         (mley '(tuareg haskell sml coq))
         (c-like '(c c++ perl sh python java ess ruby js coffee go))
         (all `(,@lispy ,@mley ,@c-like octave latex)))
    (pretty-compile-patterns
     `(
       (?¬ (,(rx "not" (? white)) python ,@lispy haskell coffee)
           (,(rx "!") c c++ java js)
           (,(rx "~~") coq)
           (,(rx "\\neg") latex))
       (?≠ (,(rx "!=") ,@c-like scheme octave coq)
           (,(rx "<>") tuareg octave)
           (,(rx "~=") octave)
           (,(rx "/=") haskell emacs-lisp)
           (,(rx "\\neq") latex)
           (,(rx "not=") clojure))
       (?≺ (,(rx "<") ,@all)
           (,(rx "\\prec") latex))
       (?≻ (,(rx ">") ,@all)
           (,(rx "\\succ") latex))
       (?≼ (,(rx "<=") ,@all)
           (,(rx "\\leq") latex))
       (?≽ (,(rx ">=") ,@all)
           (,(rx "\\geq") latex))

       ;(?× (,(rx "*[^[:alpha:]]") ,@all))
       ;; (?÷ (,(rx "/[^*/]") ,@all))
       (?− (,(rx "-") ,@all))
       (?+ (,(rx "+") ,@all))
       (?⁑ (,(rx "**") python))

       (?² (,(rx (? white) "**" (? white) "2") python tuareg octave)
           (,(rx "^2") octave haskell))
       (?³ (,(rx (? white) "**" (? white) "3") python tuareg octave)
           (,(rx "^3") octave haskell))
       (?ⁿ ;;(,(rx (? white) "**" (? white) "n") python tuareg octave)
           (,(rx "^n") octave haskell coq))

       (?∧ (,(rx "and") emacs-lisp lisp clojure python coffee)
           (,(rx "&&") haskell c c++ java perl coq js)
           (,(rx "\\wedge") latex)
           (,(rx "\\land") latex))
       (?∨ (,(rx "or") emacs-lisp lisp clojure python coffee)
           (,(rx "||") haskell c c++ java perl coq js)
           (,(rx "\\vee") latex)
           (,(rx "\\lor") latex))

       (?≡ (,(rx "==") ,@all)
           (,(rx "=") clojure)
           (,(rx "\\equiv") latex))
       (?⟵ ;;(,(rx "<-") ,@mley ess)
           (,(rx "\\leftarrow") latex))
       (?⟶ ;;(,(rx "->") ,@mley ess c c++ perl coffee)
           (,(rx "\\rightarrow") latex))
       (?↑ (,(rx "\\^") tuareg)
           (,(rx "^+") coq))
       (?⟹ ;; (,(rx "=>") sml perl ruby haskell coq coffee)
           (,(rx "\\Rightarrow") latex))
       (?⟷ (,(rx "<->") coq)
           (,(rx "\leftrightarrow") latex))
       (?↣ (,(rx ">->") coq))
       (?↦ (,(rx "\\mapsto") latex))
       (?∅ (,(rx "nil") emacs-lisp clojure ruby)
           (,(rx "null") scheme java js coffee)
           (,(rx "NULL") c c++)
           (,(rx "None") python)
           (,(rx "set0") coq)
           (,(rx "()") ,@mley)
           (,(rx "\\emptyset") latex)
           (,(rx "\\varnothing") latex))
       ;; (?… (,(rx "...") ,@all)
       ;;     (,(rx "..") haskell)
       ;;     (,(rx "\\ldots") latex))
       (?⊲ (,(rx "<|") coq))
       (?√ (,(rx "sqrt") ,@all))
       (?∑ (,(rx "sum") python)
           (,(rx "\\sum") coq latex)
           (,(rx "\\Sigma") latex)
           (,(rx "reduce \+") clojure))
       (?∪ (,(rx ":|:") coq))
       (?∩ (,(rx ":&:") coq))
       (?∁ (,(rx "~:") coq))
       ;; (?α (,(rx "alpha") ,@all)
       ;;     (,(rx "'a") ,@mley)
       ;;     (,(rx "\\alpha") latex))
       ;; (?β (,(rx "beta") ,@all)
       ;;     (,(rx "'b") ,@mley)
       ;;     (,(rx "\\beta") latex))
       ;; (?γ (,(rx "gamma") ,@all)
       ;;     (,(rx "'c") ,@mley)
       ;;     (,(rx "\\gamma") latex))
       ;; (?Δ (,(rx "delta") ,@all)
       ;;     (,(rx "'d") ,@mley)
       ;;     (,(rx "\\Delta") latex))
       ;; (?ε (,(rx "epsilon") ,@all)
       ;;     (,(rx "\\epsilon") latex))
       ;; (?ι (,(rx "iota") ,@all)
       ;;     (,(rx "\\iota") latex))
       ;; (?θ (,(rx "theta") ,@all)
       ;;     (,(rx "\\theta") latex))
       ;; (?ρ (,(rx "rho") ,@all)
       ;;     (,(rx "\\rho") latex))
       (?σ ;;(,(rx "sigma") ,@all)
           (,(rx "filter") python clojure)
           (,(rx "select") clojure))
       ;; (?μ (,(rx "mu") ,@all))
       (?λ (,(rx "lambda") ,@all)
           (,(rx "fn") sml)
           (,(rx "fun") tuareg)
           (,(rx "\\") haskell)
           (,(rx "\\lambda") latex)
           ;;(,(rx "[^\s\t]+function") js)
           )
       (?𝜆 (,(rx "lambda") python))
       ;; (?π (,(rx "pi") ,@all)
       ;;     (,(rx "M_PI") c c++)
       ;;     (,(rx "\\pi") latex)
       ;;     (,(rx "map") python clojure))
       (?Π ;;(,(rx "Pi") @all)
           (,(rx "\\prod") latex)
           (,(rx "\\Pi") latex))
       (?ω ;;(,(rx "omega") @all)
           (,(rx "\\omega") latex))
       (?Φ ;;(,(rx "Phi") @all)
           (,(rx "\\Phi") latex))
       (?Ω ;;(,(rx "Ohm") @all)
           (,(rx "\\ohm") latex)
           (,(rx "\\Omega") latex))
       (?℧ ;;(,(rx "Mho") @all)
           (,(rx "\\mho") latex))
       (?φ ;;(,(rx "phi") ,@all)
           (,(rx "\\varphi") latex))
       (?η ;;(,(rx "eta") ,@all)
           (,(rx "\\eta") latex))

       ;;(?∞ (,(rx "HUGE_VAL") c c++))
       ;;(?∎ (,(rx "Qed.") coq))

       ;;(?∗ (,(rx "all" (? "()")) python))
       ;;(?⊢ (,(rx "assert") python))
       ;;(?≍ (,(rx "is") python))
       (?𝝈 (,(rx "filter_by") python))
       (?ℵ (,(rx "count") python clojure))
       (?⇓ (,(rx "order_by") python))
       (?⤚ (,(rx "group_by") python))
       ;; (?⟶ (,(rx "def") python))

       (?⊤ (,(rx "True") python))
       (?⊥ (,(rx "False") python))

       (?⋂ (,(rx "intersect") python)
           (,(rx "\\bigcap") coq)
           (,(rx "\\cap") latex)
           (,(rx "intersection") clojure))
       (?∏ (,(rx "\\prod") coq))
       (?⋃ (,(rx "union") python clojure)
           (,(rx "\\bigcup") coq)
           (,(rx "\\cup") latex))
       (?⊎ (,(rx "\\uplus") latex))
       (?ℕ (,(rx "nat") coq))
       (?∣ (,(rx "%|") coq))


       ;;(?∈ (,(rx "in") python coffee))
       (?∉ ;;(,(rx "not in") python)
           (,(rx "\\notin") coq latex))
       ;;(?⊼ (,(rx "and not") python coffee))
       ;;(?⊽ (,(rx "or not") python coffee))
       (?⊻ (,(rx "(\\+)") coq))

       (?∀ ;;(,(rx "for") python coffee)
           ;;(,(rx "forall") haskell coq)
           (,(rx "\\forall") latex))
       ;;(?∄ (,(rx "not any") python))
       ;; (?∃ (,(rx "any") python)
       ;;     (,(rx "exists") coq)
       ;;     (,(rx "\\exists") latex)
       ;;     (,(rx "some") clojure))
       (?⊂ (,(rx "\\proper") coq)
           (,(rx "\\subset") latex))
       (?⊆ (,(rx "\\subset") coq)
           (,(rx "\\subseteq") latex))
       (?∖ (,(rx ":\\:") coq)
           (,(rx "\\setminus") latex)
           (,(rx "difference") clojure))
       (?⋊ (,(rx "><|") coq))

       (?× (,(rx "\\times") latex))
       (?〈 (,(rx "\\langle") latex))
       (?〉 (,(rx "\\rangle") latex))))))


(defun pretty-add-keywords (mode keywords)
  "Add pretty character KEYWORDS to MODE

MODE should be a symbol, the major mode command name, such as
`c-mode' or nil. If nil, pretty keywords are added to the current
buffer. KEYWORDS should be a list where each element has the
form (REGEXP . CHAR). REGEXP will be replaced with CHAR in the
relevant buffer(s)."
  (font-lock-add-keywords
   mode (mapcar (lambda (kw) `(,(car kw)
                          (0 (prog1 nil
                               (compose-region (match-beginning 0)
                                               (match-end 0)
                                               ,(cdr kw))))))
                keywords)))

(defun pretty-regexp (regexp glyph)
  "Replace REGEXP with GLYPH in buffer."
  (interactive "MRegexp to replace:
MCharacter to replace with: ")
  (pretty-add-keywords nil `((,regexp . ,(string-to-char glyph))))
  (font-lock-fontify-buffer))

(provide 'pretty-mode)
