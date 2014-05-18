;;; org-import-icalendar.el --- Copy snippet to kill-ring for text/calendar part

;; Copyright (C) 2010 Vagn Johansen

;; Author: Vagn Johansen <gonz808@hotmail.com>
;; Keywords: gnus, org
;; URL: http://ozymandias.dk/emacs/emacs.html


;; This file is NOT part of Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
;; MA 02110-1301, USA.

;;; Commentary:
;;
;; Installation:
;;
;;   (require 'org-import-icalendar)
;;
;;   ;; Import to this calendar. If not set use kill-ring
;;   ;;(setq org-import-icalendar-filename "cal.org")
;;
;; Probably requires Emacs 23
;;
;; NOTE! Sets gnus-article-mime-part-function. Make sure it not set to
;; something you use first.


;;; Acknowledgements
;;
;;    Russell Adams: org-import-icalendar-process-file and 'Attendee'

;;; Code:


(require 'icalendar)

(defvar org-import-icalendar-filename nil
  "If set append to this file instead of copying to kill-ring")

(defun org-import-icalendar-iso (datetime)
  "Convert a date retrieved via `icalendar--get-event-property' to ISO format."
  (if datetime
    (format "%04d-%02d-%02d"
      (nth 5 datetime)                  ; Year
      (nth 4 datetime)                  ; Month
      (nth 3 datetime))))               ; Day

(defun org-import-icalendar-get-org-timestring (ical-element)
""
  (let* ((dtstart (icalendar--get-event-property e 'DTSTART))
          (dtstart-zone (icalendar--find-time-zone
                          (icalendar--get-event-property-attributes e 'DTSTART)
                          zone-map))
          (dtstart-dec (icalendar--decode-isodatetime dtstart nil dtstart-zone))
          (start-d (org-import-icalendar-iso dtstart-dec))
          (start-t (icalendar--datetime-to-colontime dtstart-dec))
          (dtend (icalendar--get-event-property e 'DTEND))
          (dtend-zone (icalendar--find-time-zone
                        (icalendar--get-event-property-attributes e 'DTEND)
                        zone-map))
          (dtend-dec (icalendar--decode-isodatetime dtend nil dtend-zone))
          end-d end-t

          (rrule (icalendar--get-event-property e 'RRULE))
          (rdate (icalendar--get-event-property e 'RDATE))
          (duration (icalendar--get-event-property e 'DURATION)))

    (setq end-d (if dtend-dec
                  (org-import-icalendar-iso dtend-dec)
                  start-d))
    (setq end-t (if (and
                      dtend-dec
                      (not (string=
                             (cadr
                               (icalendar--get-event-property-attributes
                                 e 'DTEND))
                             "DATE")))
                  (icalendar--datetime-to-colontime dtend-dec)
                  start-t))
    ;; Store in kill-ring
      (if (equal start-d end-d)
        (format "<%s %s-%s>" start-d start-t end-t)
        ;; else
        (format "<%s %s>--<%s %s>" start-d start-t end-d end-t))))


(defun org-import-icalendar-import-element (ical-element)
  (let* ((location (icalendar--get-event-property e 'LOCATION))
          (organizer (icalendar--get-event-property e 'ORGANIZER))
          (summary (icalendar--convert-string-for-import
                     (or (icalendar--get-event-property e 'SUMMARY)
                       "No summary")))
          (attendees (mapconcat (lambda (a) (format " - %s" a))
                       (icalendar--get-event-properties e 'ATTENDEE)
                       "\n"))
          (org-timestr (org-import-icalendar-get-org-timestring ical-element))
          ;;
          (msg (format "** TODO %s\n%s\n Location: %s\nOrganizer: %s\nAttending:\n%s\n\n"
                 summary org-timestr location (or organizer "n/a") (or attendees "n/a"))))

    (message "Added to kill-ring:\n %s" msg)
    (if (not org-import-icalendar-filename)
      ;; Store in kill-ring
      (kill-new msg)
      ;; else: append to file
      (with-temp-file org-import-icalendar-filename
        (insert-file-contents org-import-icalendar-filename)
        (goto-char (point-max))
        (insert msg)))))

(defun org-import-icalendar-import-elements (ical-list)
  (let* ((ev (icalendar--all-events ical-list))
          (error-string "")
          (event-ok t)
          (found-error nil)
          (zone-map (icalendar--convert-all-timezones ical-list))
          e diary-string)
    (while ev
      (setq e (car ev))
      (setq ev (cdr ev))
      (setq event-ok nil)
      (condition-case error-val
        (org-import-icalendar-import-element e)
        ;; FIXME: inform user about ignored event properties
        ;; handle errors
        (error
          (message "Ignoring event \"%s\"" e)
          (setq found-error t)
          (setq error-string (format "%s\n%s\nCannot handle this event: %s"
                               error-val error-string e))
          (message "%s" error-string))))
    found-error))

(defun org-icalendar-import-buffer ()
  (interactive)
  (save-current-buffer
    ;; prepare ical
    (message "Preparing icalendar...")
    (set-buffer (icalendar--get-unfolded-buffer (current-buffer)))
    (goto-char (point-min))
    (message "Preparing icalendar...done")
    (if (re-search-forward "^BEGIN:VCALENDAR\\s-*$" nil t)
        (let (ical-contents ical-errors)
          ;; read ical
          (message "Reading icalendar...")
          (beginning-of-line)
          (setq ical-contents (icalendar--read-element nil nil))
          (message "Reading icalendar...done")
          ;; convert ical
          (message "Converting icalendar...")
          (setq ical-errors (org-import-icalendar-import-elements
                              ical-contents))

          ;; return t if no error occurred
          (not ical-errors))
      (message "Current buffer does not contain icalendar contents!"))))


(defun org-import-icalendar-gnus-part (handle)
  (interactive)
  (let (fn msg)
    (when (equal (car (mm-handle-type handle)) "application/ms-tnef")
      (setq fn (make-temp-file "gnus-org-" nil ".tnef"))
      ;; Save part to file
      (mm-save-part-to-file handle fn)
      (with-temp-buffer
        ;; Load part again!
        (insert-file-contents fn)
        (goto-char (point-min))
        (if (re-search-forward "BEGIN:VCALENDAR" nil t)
          (progn
            (beginning-of-line)
            (delete-region (point-min) (point))
            (when (re-search-forward "END:VCALENDAR" nil t)
              ;; VCALENDAR in winmail.dat"
              (delete-region (point) (point-max))
              (goto-char (point-min))
              (while (search-forward "" nil t)
                (replace-match "" nil t))
              (org-icalendar-import-buffer)
              ))
          (setq msg "BEGIN:VCALENDAR not found in part.")
          (when (executable-find "tnef")
            (setq msg (concat "tnef --list ..\n"
                        (propertize (shell-command-to-string
                          (format "tnef --list %s" fn))
                          'face 'bold))))
            (message "%s" msg)
        )))

    (when (equal (car (mm-handle-type handle)) "text/calendar")
      (setq fn (make-temp-file "gnus-org-" nil ".ics"))
      ;; Save part to file
      (mm-save-part-to-file handle fn)
      (with-temp-buffer
        ;; Load part again!
        (insert-file-contents fn)
        (goto-char (point-min))
        (org-icalendar-import-buffer)))
    ;; Does not work:
    ;; (with-temp-buffer
    ;;     (insert (mm-get-part handle))
    ;;     (org-icalendar-import-buffer))
    ))


(defun org-import-icalendar-gnus ()
  "Import ics part stored as second part of alternative.

 (define-key gnus-summary-mode-map [?K ?I] 'org-import-icalendar-gnus)
"
  (interactive)
  (save-current-buffer
    (gnus-summary-show-article)
    (set-buffer gnus-article-buffer)
    (let ((handle (nth 3 (assq 1 gnus-article-mime-handle-alist))))
      (if (equal (car (mm-handle-type handle)) "text/calendar")
        (org-import-icalendar-gnus handle)
        (message "Third element not a text/calendar")))))

(defun org-import-icalendar-process-file (file)
  (interactive)
  (when (file-readable-p file)
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (org-icalendar-import-buffer))))


(eval-after-load "gnus"
  '(progn
     (setq gnus-article-mime-part-function 'org-import-icalendar-gnus-part)
     ))

(provide 'org-import-icalendar)
