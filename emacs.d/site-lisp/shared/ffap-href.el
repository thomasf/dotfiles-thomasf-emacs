;;; ffap-href.el --- find href URL/link anywhere in the tag

;; Copyright 2007, 2009, 2010, 2011 Kevin Ryde

;; Author: Kevin Ryde <user42@zip.com.au>
;; Version: 3
;; Keywords: files, ffap, html
;; URL: http://user42.tuxfamily.org/ffap-href/index.html
;; EmacsWiki: FindFileAtPoint

;; ffap-href.el is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation; either version 3, or (at your option) any later
;; version.
;;
;; ffap-href.el is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General
;; Public License for more details.
;;
;; You can get a copy of the GNU General Public License online at
;; <http://www.gnu.org/licenses/>.


;;; Commentary:

;; This spot of code lets M-x ffap recognise a html link, like
;;
;;     <a href="something.html">
;;
;; with point anywhere in the tag, not just on filename part (which ffap
;; handles already).

;;; Emacsen:

;; Designed for Emacs 21 up, works in XEmacs 21 and Emacs 20.

;;; Install:

;; Put ffap-href.el in one of your `load-path' directories and the following
;; in your .emacs
;;
;;     (autoload 'ffap-href-enable "ffap-href" nil t)
;;
;; To enable it when ffap loads add the following
;;
;;     (eval-after-load "ffap" '(ffap-href-enable))
;;
;; The function is interactive so `M-x ffap-href-enable' can be used if only
;; wanted sometimes, etc.
;;
;; There's an autoload cookie for `ffap-href-enable' if you know how to use
;; `update-file-autoloads' and friends, then just `eval-after-load' or
;; `M-x'.

;;; History:
;; 
;; Version 1 - the first version
;; Version 2 - emacs21 fix ffap-url-at-point when unloaded
;;           - xemacs21,emacs20 no error for unavailable unicode chars
;; Version 3 - oops, don't enable by default, only from ffap-href-enable

;;; Code:

;;;###autoload (eval-after-load "ffap" '(require 'ffap-href))

;; for `ad-find-advice' macro when running uncompiled
;; (don't unload 'advice before our -unload-function)
(require 'advice)

(require 'cl) ;; for `position'

(put  'ffap-href-ucs-string 'side-effect-free t)
(defun ffap-href-ucs-string (num)
  "Return a single-char string for unicode char NUM.
For Emacs 21 up this is simply `(string (decode-char 'ucs num))'.

For XEmacs 21 the latin-1 characters are are always converted but
higher characters require the unicode charsets, for example from
mule-ucs.  If not loaded then the return is nil."

  (cond ((eval-when-compile (fboundp 'decode-char))
         ;; emacs21 up
         (string (decode-char 'ucs num)))

        ((<= num 255)
         ;; xemacs21 and emacs20 latin1
         (decode-coding-string (string num) 'iso-8859-1))

        ((memq 'utf-16-be (coding-system-list))
         ;; xemacs21 with mule-ucs, emacs20 maybe with mule-ucs
         ;; not sure this is right on the surrogates D800 etc, but they
         ;; shouldn't occur
         (decode-coding-string (string (ash num -8)
                                       (logand num 255))
                               'utf-16-be))))

(defun ffap-href-unentitize (str)
  "Return STR with any &foo; html entities turned into plain characters.
Named entities are sought in `iso-sgml2iso' (Emacs 21 up) or
`sgml-char-names'.  Numbered entities &#247; are latin-1, or unicode if
available (Emacs 21, or XEmacs 21 with mule-ucs)."
  (if (string-match "&" str)
      (progn
        (eval-and-compile ;; quieten the byte compiler
          (require 'iso-cvt))
        (eval-and-compile ;; quieten the byte compiler
          (require 'sgml-mode))
        (with-temp-buffer
          (insert str)
          (if (fboundp 'iso-sgml2iso)
              ;; new in emacs21, not in xemacs21 or emacs20
              (iso-sgml2iso (point-min) (point-max)))
          (goto-char (point-min))
          (while (re-search-forward "&\\(#\\([0-9]+\\)\\|[^;]*\\);" nil t)
            (let* ((num (or (and (match-beginning 2) ;; numbered
                                 (string-to-number (match-string 2)))
                            (position (match-string 1)  ;; named
                                      sgml-char-names
                                      :test 'equal))))
              (when num ;; recognised &foo;, or a number &1234;
                (let* ((beg  (match-beginning 0))
                       (end  (match-end 0))
                       (repl (ffap-href-ucs-string num)))
                  (when repl ;; unicode charset available
                    (delete-region beg end)
                    (insert repl))))))
          (buffer-string)))
    str))

(defun ffap-href-at-point (&optional type)
  "Return an <a href=\"...\" URL at point, or nil if none.
In the current implementation TYPE can be symbol 'url or 'file.
A relative link is considered a file, and anything with a scheme
like \"http:\" is considered a url.  If the href at point is not
the desired TYPE then return nil.

This is designed for use from `defadvice' on `ffap-url-at-point'
and `ffap-file-at-point' so it sets the variables
`ffap-string-at-point-region' and `ffap-string-at-point' if a URL
or file is returned."

  (eval-and-compile ;; quieten the byte compiler
    (require 'ffap)) ;; for ffap-string-at-point-region
  (eval-and-compile ;; quieten the byte compiler
    (require 'thingatpt))

  ;; Narrowing to a few surrounding lines is a speedup for big buffers by
  ;; limiting the amount of searching forward and back that
  ;; `thing-at-point-looking-at' does in its workaround for the way
  ;; `re-search-backward' won't match across point.
  ;;
  ;; The match includes the closing ">" to have that highlighted in the
  ;; `ffap-string-at-point-region', but that ">" is only optional so
  ;; something unterminated still works.
  ;;
  (and (save-restriction
         (narrow-to-region (save-excursion (forward-line -5) (point))
                           (save-excursion (forward-line 5) (point)))
         (let ((case-fold-search t))
           (thing-at-point-looking-at "\
<\\(a\\|img\\)\
\\s-\
\\([^<>]+\\s-\\)?\
\\(href\\|src\\)=\\(\"\\([^\"<>]*\\)\"\\|'\\([^'<>]*\\)'\\)\
\\(?:[^<>]*>\\)?")))

       (let* ((beg (or (match-beginning 5)    ;; href=""
                       (match-beginning 6)))  ;; href=''
              (end (or (match-end 5)
                       (match-end 6)))
              (str (buffer-substring beg end)))
         (and (or (not type)
                  (eq type
                      (let ((case-fold-search t))
                        (if (string-match "\\`[a-z]+:/" str) 'url 'file))))
              (progn
                (setq ffap-string-at-point-region (list beg end))
                (setq ffap-string-at-point
                      (ffap-href-unentitize str)))))))

(defadvice ffap-url-at-point (around ffap-href disable)
  "Recognise \"<a href=\" with with point anywhere in the tag."
  (or (and (fboundp 'ffap-href-at-point) ;; in case unloaded
           (setq ad-return-value (ffap-href-at-point 'url)))
      ad-do-it))

(defadvice ffap-file-at-point (around ffap-href disable)
  "Recognise \"<a href=\" with with point anywhere in the tag."
  (or (and (fboundp 'ffap-href-at-point) ;; in case unloaded
           (setq ad-return-value (ffap-href-at-point 'file)))
      ad-do-it))

;;;###autoload
(defun ffap-href-enable ()
  "Extend `ffap' to recognise HTML <a href=\"...\"> anywhere in the tag.
`ffap' already works with point on the url proper, but not
elsewhere like on the \"<a\" or another attribute.

HTML entities like &amp; are converted to the corresponding
characters with `sgml-char-names' and `iso-sgml2iso-trans-tab'
\(though perhaps URI \"%\" escapes are more likely).

\"<a href\" is HTML-specific but `ffap-href-enable' applies it
globally since it shouldn't clash with anything else.

The `ffap-href' home page is
URL `http://user42.tuxfamily.org/ffap-href/index.html'"

  (interactive)
  (ad-enable-advice 'ffap-file-at-point 'around 'ffap-href)
  (ad-activate      'ffap-file-at-point)
  (ad-enable-advice 'ffap-url-at-point 'around 'ffap-href)
  (ad-activate      'ffap-url-at-point))

(defun ffap-href-unload-function ()
  "Remove defadvice on `ffap' bits.
This is called by `unload-feature'."

  (when (ad-find-advice 'ffap-file-at-point 'around 'ffap-href)
    (ad-remove-advice   'ffap-file-at-point 'around 'ffap-href)
    (ad-activate        'ffap-file-at-point))
  (when (ad-find-advice 'ffap-url-at-point  'around 'ffap-href)
    (ad-remove-advice   'ffap-url-at-point  'around 'ffap-href)
    (ad-activate        'ffap-url-at-point))
  nil) ;; and do normal unload-feature actions too

;; LocalWords: http html url href filename unicode ucs num latin charsets

(provide 'ffap-href)

;;; ffap-href.el ends here
