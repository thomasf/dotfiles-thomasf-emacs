;;; keep-buffers.el -- Attempt to prevent named buffers from deletion.

;; Copyright (C) 2000 Steve Kemp
;; Copyright (C) 2011 Le Wang

;; Author: Steve Kemp <skx@tardis.ed.ac.uk>
;;
;; Maintainer: Le Wang
;;
;; URL: https://github.com/lewang/le_emacs_libs/blob/master/keep-buffers.el
;;
;; Keywords: extensions
;;
;; Status: Tested with Emacs 23.3+
;;
;; Created: 20/01/2000
;;
;; Last-Updated: Wed Sep 21 11:02:15 2011 (+0800)
;;           By: Le Wang

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, you can either send email to this
;; program's maintainer or write to: The Free Software Foundation,
;; Inc.; 59 Temple Place, Suite 330; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This package allows you to protect buffers, based upon their names, from
;; deletion.
;;
;; There are two ways you can use this package to protect buffers, either via
;; the customize interface, or via Lisp.
;;

;;; Installation:

;;
;; Simple Install:
;;
;;   (require 'keep-buffers)                     ;; Load the package.
;;   (keep-buffers-mode 1)
;;
;;
;; By default, "*scratch*" is protected and erased when killed, "*Messages*"
;; is never killed or erased.  You can customize easily using elisp:
;;
;;   ;; protect all buffers starting with "*scratch"
;;   (push '("\\`*scratch" . erase) keep-buffers-protected-list)
;;
;;
;; Customize:
;;
;;   M-x customize-group
;;   keep-buffers
;;

;;; History:
;;
;;   Version 1.0 -- Initial version.
;;
;;   Version 1.1 -- Added the customize entries.
;;
;;   Version 1.2 -- Added the keep-buffers-erase-on-kill function.
;;
;;   Version 1.3 -- (Le Wang)
;;
;;                   * erased lots of useless helper functions.  people are
;;                     either going to use customize or elisp.
;;
;;                   * made `keep-buffers-protected-list' an alist so each
;;                     buffer can specify whether it should be erased.
;;
;;                   * removed redundant `find-in-list' see `member'
;;
;;   Version 1.4 -- (Le Wang)
;;
;;                   * changed into prope global minor mode
;;
;;

;;; Code:

(eval-when-compile
  (require 'cl))

(define-minor-mode keep-buffers-mode
  "when active, killing protected buffers results in burying them instead.
Some may also be erased, which is undo-able."
  :init-value nil
  :global t
  :group 'keep-buffers
  :lighter ""
  :version "1.4"
  (if keep-buffers-mode
      ;; Setup the hook
      (add-hook 'kill-buffer-query-functions 'keep-buffers-query)
    (remove-hook 'kill-buffer-query-functions 'keep-buffers-query)))

(defcustom keep-buffers-protected-alist
  '(("\\`\\*scratch\\*\\'" . erase)
    ("\\`\\*Messages\\*\\'" . nil))
  "an alist '((\"regex1\" . 'erase) (\"regex2\" . nil))

CAR of each cons cell is the buffer matching regexp.  If CDR is
not nil then the matching buffer is erased then buried.

If the CDR is nil, then the buffer is only buried."
  :type '(alist)
  :group 'keep-buffers
  )


;;;###autoload
(defun keep-buffers-query ()
  "The query function that disable deletion of buffers we protect."
  (let ((crit (dolist (crit keep-buffers-protected-alist)
                (when (string-match (car crit) (buffer-name))
                  (return crit)))))
    (if crit
        (progn
          (when (cdr crit)
            (erase-buffer))
          (bury-buffer)
          nil)
      t)))


(provide 'keep-buffers)
;;; keep-buffers.el ends here
