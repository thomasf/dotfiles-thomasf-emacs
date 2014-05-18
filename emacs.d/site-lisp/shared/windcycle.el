;;; windcycle.el --- Window cycling for Emacs

;; Copyright (C) 2011 Dmitry Geurkov

;; Author: Dmitry Geurkov <dmitry_627@mail.ru>

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;; Simple Window cycling for Emacs using C-x arrows or Meta arrows
;;; You can also swap buffers between windows using Meta-Shift arrow 
;;; And resize windows using Control-Shift arrows 
;;; To use add 
;;; (require 'windcycle) 
;;; to your .emacs

;; Windows Cycling
(defun windmove-up-cycle()
  (interactive)
  (condition-case nil (windmove-up)
    (error (condition-case nil (windmove-down)
	          (error (condition-case nil (windmove-right) (error (condition-case nil (windmove-left) (error (windmove-up))))))))))

(defun windmove-down-cycle()
  (interactive)
  (condition-case nil (windmove-down)
    (error (condition-case nil (windmove-up)
	          (error (condition-case nil (windmove-left) (error (condition-case nil (windmove-right) (error (windmove-down))))))))))

(defun windmove-right-cycle()
  (interactive)
  (condition-case nil (windmove-right)
    (error (condition-case nil (windmove-left)
	          (error (condition-case nil (windmove-up) (error (condition-case nil (windmove-down) (error (windmove-right))))))))))

(defun windmove-left-cycle()
  (interactive)
  (condition-case nil (windmove-left)
    (error (condition-case nil (windmove-right)
	          (error (condition-case nil (windmove-down) (error (condition-case nil (windmove-up) (error (windmove-left))))))))))

;; Buffer swaping
(defun buffer-up-swap()
  (interactive)
  (let ((current-window (selected-window))
	(current-buffer (buffer-name))
	(swaped-window nil)
	(swaped-buffer nil))
	(progn (windmove-up-cycle)
	 (setq swaped-window (selected-window))
	 (setq swaped-buffer (buffer-name))
	 (if (and (not (string= swaped-buffer current-buffer)))
	     (progn (set-window-buffer swaped-window current-buffer)
		    (set-window-buffer current-window swaped-buffer))))))

(defun buffer-down-swap()
  (interactive)
  (let ((current-window (selected-window))
	(current-buffer (buffer-name))
	(swaped-window nil)
	(swaped-buffer nil))
	(progn (windmove-down-cycle)
	 (setq swaped-window (selected-window))
	 (setq swaped-buffer (buffer-name))
	 (if (and (not (string= swaped-buffer current-buffer)))
	     (progn (set-window-buffer swaped-window current-buffer)
		    (set-window-buffer current-window swaped-buffer))))))

(defun buffer-right-swap()
  (interactive)
  (let ((current-window (selected-window))
	(current-buffer (buffer-name))
	(swaped-window nil)
	(swaped-buffer nil))
	(progn (windmove-right-cycle)
	 (setq swaped-window (selected-window))
	 (setq swaped-buffer (buffer-name))
	 (if (and (not (string= swaped-buffer current-buffer)))
	     (progn (set-window-buffer swaped-window current-buffer)
		    (set-window-buffer current-window swaped-buffer))))))

(defun buffer-left-swap()
  (interactive)
  (let ((current-window (selected-window))
	(current-buffer (buffer-name))
	(swaped-window nil)
	(swaped-buffer nil))
	(progn (windmove-left-cycle)
	 (setq swaped-window (selected-window))
	 (setq swaped-buffer (buffer-name))
	 (if (and (not (string= swaped-buffer current-buffer)))
	     (progn (set-window-buffer swaped-window current-buffer)
		    (set-window-buffer current-window swaped-buffer))))))

;; Window Dedicated Toggle Function
(defun toggle-dedicated-window ()
  (interactive)
  (let ((d (window-dedicated-p (selected-window))))
    (progn (set-window-dedicated-p (selected-window) (not d))
	   (if d (message "Window is not dedicated") (message "Window is now dedicated")))))

;; Switch window keybindings
(global-set-key (kbd "C-x <up>") 'windmove-up-cycle)
(global-set-key (kbd "C-x <down>") 'windmove-down-cycle)
(global-set-key (kbd "C-x <right>") 'windmove-right-cycle)
(global-set-key (kbd "C-x <left>") 'windmove-left-cycle)
(global-set-key (kbd "M-<up>") 'windmove-up-cycle)
(global-set-key (kbd "M-<down>") 'windmove-down-cycle)
(global-set-key (kbd "M-<right>") 'windmove-right-cycle)
(global-set-key (kbd "M-<left>") 'windmove-left-cycle)

;; Swap window keybindings
(global-set-key (kbd "S-M-<up>") 'buffer-up-swap)
(global-set-key (kbd "S-M-<down>") 'buffer-down-swap)
(global-set-key (kbd "S-M-<right>") 'buffer-right-swap)
(global-set-key (kbd "S-M-<left>") 'buffer-left-swap)

;; Window Resizing keybindings
(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)

;; Window Split keybindings
(global-set-key (kbd "C-x -") 'split-window-vertically)
(global-set-key (kbd "C-x |") 'split-window-horizontally)

;; Window Close keybindings
(global-set-key (kbd "C-x x") 'delete-window)

;; iTerm meta-shift-<arrows> fix
(define-key input-decode-map "\e[1;10A" [M-S-up])
(define-key input-decode-map "\e[1;10B" [M-S-down])
(define-key input-decode-map "\e[1;10C" [M-S-right])
(define-key input-decode-map "\e[1;10D" [M-S-left])

(provide 'windcycle)
