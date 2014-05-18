;;; MRU-yank.el --- Most Recently Used stacking for kill-ring

;; Filename: MRU-yank.el
;; Description: Most Recently Used stacking for kill-ring
;; Author: Le Wang
;; Maintainer:  Le Wang (lewang.emacs!!!gmayo.com remove exclamations, correct host, hint: google mail)
;; Created: 2006/07/08 07:35:50
;;
;; Copyright (c) 2006, 2011 Le Wang

;; Version: 0.2
;; Last-Updated: Wed Apr  4 08:00:33 2012 (+0800)
;;           By: Le Wang
;;     Update #: 18
;; URL: https://github.com/lewang/le_emacs_MRU_yank
;; Keywords:
;; Compatibility: GNU Emacs 21, 23.2.1
;; Keywords: convenience editing
;; X-URL: not distributed yet

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
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; Implements MRU (Most Recently Used) stacking for kill-ring.
;;
;; By default Emacs treats the kill-ring as a "ring", I find it more useful
;; to move each yanked item to the top of a stack instead.  This way
;; recently used (inserted) stuff are always at the top -- think alt-tab in
;; Windows.
;;
;; Before using this package, you should try M-- M-y (negative prefix arg to
;; yank-pop), as it may already do what you want.

       ;;,----
       ;;| ** When you use this package, realize that you are deciding to
       ;;|     prioritize "time of last use" over "time of introduction into
       ;;|     kill-ring".
       ;;| ** You actually lose the "time of addition into kill-ring"
       ;;|     information, because this library actively modifies the
       ;;|     kill-ring.
       ;;`----

;;; Installation:

;;   Put this file into your load-path and the following into your
;;   ~/.emacs:
;;
;;   (require 'MRU-yank)
;;   (setq MRU-yank-mode t)

;;; Related packages:

;;  * `buffer-stack'
;;  * `bubble'


;;; Code:

(provide 'MRU-yank)

(eval-when-compile
  (require 'cl))

(defcustom MRU-yank-mode nil
  "*Non-nil means use use MRU order when yanking."
  :type 'boolean
  :group 'killing)

(defvar MRU-yank-count nil)
(defvar MRU-yank-count-prev nil)

(defun list-reorder (list i j)
  "move j_th elt of list to the i_th position, use i=0 for head.

return the new list."

  (let (i-after-list
        j-after-list
        j-element)
    (setq j-element (nthcdr j list)
          j-after-list (cdr j-element))

    (if (= j 0)
        (setq list (cdr list))
      (setcdr (nthcdr (- j 1) list) j-after-list))

    (setcdr j-element nil)

    ;; list ---- j-after-list --- nil

    (if (= i 0)
        (nconc j-element list)
      (setq i-after-list (nthcdr i list))
      (setcdr (nthcdr (- i 1) list) j-element)
      (nconc list i-after-list))))

(defadvice current-kill (around MRU-yank activate compile)
  "kill-ring stacking hook"
  (when (not (eq last-command 'yank))
    (setq MRU-yank-count 0)
    (setq MRU-yank-count-prev 0))

  (if (or (not MRU-yank-mode)
          (and (not (eq last-command 'yank))
               (or
                (and (= (ad-get-arg 0) 0)
                     ;; reset clipboard so it can be accessed again
                     (cond  ((memq interprogram-paste-function '(x-get-selection-value
                                                                 x-selection-value))
                             (when (funcall interprogram-paste-function)
                               (set (cond ((eq window-system 'mac)
                                           'x-last-selected-text-clipboard)
                                          ((eq window-system 'ns)
                                           'ns-last-selected-text)
                                          (t
                                           'x-last-selected-text))
                                    nil)
                               t))
                            ((null interprogram-paste-function)
                             nil)
                            (t
                             (message "MRU-yank: I don't know how to support %s" interprogram-paste-function)
                             nil)))
                (not kill-ring)      ; empty kill-ring
                )))
      ad-do-it
    (setq MRU-yank-count (+ MRU-yank-count (ad-get-arg 0)))
    (let ((n (mod MRU-yank-count
                  (length kill-ring)))
          (n-prev (mod MRU-yank-count-prev
                       (length kill-ring))))
      (when (not (= MRU-yank-count 0))
        (if (= n 0)
            (setq kill-ring (list-reorder kill-ring
                                          (- (length kill-ring)
                                             1)
                                          0))
          (setq kill-ring (list-reorder kill-ring n-prev 0)
                kill-ring (list-reorder kill-ring 0 n))))
      (setq kill-ring-yank-pointer kill-ring)
      (setq MRU-yank-count-prev MRU-yank-count)
      (setq ad-return-value (car kill-ring)))))



;;; MRU-yank.el ends here
