;;; whole-line-funcs.el --- triple-click like line marking functions
;;
;; Filename: whole-line-funcs.el (previously known as mark-lines.el)
;; Description:
;; Author: Le Wang
;; Maintainer:
;; Created: Mon Jan 31 10:42:39 2011 (+0800)
;; Version: 0.8
;; Last-Updated: Thu Feb  3 19:00:31 2011 (+0800)
;;           By: Le Wang
;;     Update #: 7
;; URL: https://github.com/lewang/le_emacs_whole_line_funcs
;; Keywords: convenience whole-line region editing
;; Compatibility: GNU Emacs 23.2
;;

;;; Copyright (C) 2003, 2011 Le Wang

;;;installation:

;; (require 'whole-lines-funcs)
;; (global-set-key [(control x) (control p)] 'whole-line-mark-next)
;; (global-set-key [(control x) (control n)] 'whole-line-mark-previous)

;;,----
;;| Commentary:
;;`----

;; `whole-line-funcs' provides whole line selection functionality.  The behaviour of
;; triple-clicking mouse-1 is used as a model.
;;
;;
;;
;; Also consider setting the `kill-whole-line' varaible to modify Emacs'
;; default kill-line behaviour, along with the `auto-indent' and
;; `auto-indent-lite' packages

;;,----
;;| Change Log:
;;`----

;; 0.8 [2011-02-03 Thu] (whole-line-funcs.el)
;;      Rename library to whole-line-funcs.el
;;
;; 0.7 [2011-01-31 Mon] (mark-lines.el)
;;      Incorporate some logic from textmate.el
;;
;; 0.6 some time ago (mark-lines.el)
;;      Major rewrite removing convoluted logic and always retain current line functionality.
;;
;; 0.5 Mon 17 Feb 2003 (mark-lines.el)
;;      Fixed horrible border-line case bug. (when the point is at eob, on a
;;      non-empty line).
;;
;; 0.4 Sat 15 Feb 2003 (mark-lines.el)
;;      Changed copyright to me.  ( I had originally pasted in the license
;;      w/o checking.)
;;      More documentation fixes.
;;
;; 0.3 Sat 15 Feb 2003 (mark-lines.el)
;;      Documentation updates.
;;
;; 0.2 Sat Feb 15, 2003 (mark-lines.el)
;;      Added changelog.
;;      Fixed column tracking bug with goal-column.
;;      Small documentation fixes.
;;
;; 0.1 Sat Feb 15, 2003
;;      Initial release.

;;; License

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;

;;; Related packages:

;;  * `auto-indent-lite'

;;; Code:

(eval-when-compile
  (require 'cl))

(provide 'whole-line-funcs)

(defun* whole-line-expand-region (&key (move-point nil))
  "Expand point and mark to select whole lines.

This function assumes region is ready to be used.

:MOVE-POINT can be 'beginning or 'end, or nil to keep point/mark order.
"
  (let ((point-first (= (point) (region-beginning)))
        (r-beg (region-beginning))
        (r-end (region-end)))
    (goto-char r-beg)
    (move-beginning-of-line 1)
    (push-mark (point))
    (goto-char r-end)
    (unless (eq (char-before) ?\n)
      (progn
        (move-end-of-line 1)
        (if (< (point) (point-max)) (forward-char))))
    (if point-first
        (exchange-point-and-mark))
    (cond ((eq move-point 'beginning)
           (unless point-first
             (exchange-point-and-mark)))
          ((eq move-point 'end)
           (when point-first
             (exchange-point-and-mark))))))


(defun whole-line-regionp ()
  "returns t if region only has whole lines"
  (let ((only-whole-lines t)
        (orig-p (point)))
    (goto-char (region-beginning))
    (unless (bolp)
      (setq only-whole-lines nil))
    (unless only-whole-lines
      (goto-char (region-end))
      (unless (bolp)
        (setq only-whole-lines nil)))
    (goto-char orig-p)
    only-whole-lines))

;;;###autoload
(defun whole-line-mark-previous (arg)
  "If region is active, but not whole lines, expand region to contain whole lines.

If region is already whole lines, expand backward by ARG lines.

Always ensure point is before mark.
"
  (interactive "p")
  (setq arg (- arg))
  (if (use-region-p)
      (if (whole-line-regionp)
          (progn
            (whole-line-expand-region :move-point 'beginning)
            (forward-line arg))
        (whole-line-expand-region :move-point 'beginning))
    (goto-char (point-at-bol 2))
    (if (fboundp 'cua-set-mark)
        (cua-set-mark)
      (push-mark nil t t))
    (forward-line arg)))


;;;###autoload
(defun whole-line-mark-next (arg)
  "If region is active, but not whole lines, expand region to contain whole lines.

If region is already whole lines, expand forard by ARG lines.

Always ensure point is after mark.
"
  (interactive "p")
  (if (use-region-p)
      (if (whole-line-regionp)
          (progn
            (whole-line-expand-region :move-point 'end)
            (forward-line arg))
        (whole-line-expand-region :move-point 'end))
    (goto-char (point-at-bol))
    (if (fboundp 'cua-set-mark)
        (cua-set-mark)
      (push-mark nil t t))
    (forward-line arg)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; whole-line-funcs.el ends here