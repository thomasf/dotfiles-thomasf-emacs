;;; glow-view.el --- View markdown rendered by glow -*- lexical-binding: t -*-

;;; Commentary:

;; `glow-view' renders a markdown file or buffer with the glow CLI into a
;; read-only buffer.  The text is wrapped to the width of the window showing
;; it and re-rendered when that width changes, but never narrower than
;; `glow-view-min-width'.  Point and window start stay on the same text
;; across re-renders.
;;
;; glow is given an explicit style and a pty for stdout: on a pipe it emits
;; at most the 16 ANSI colors.  COLORTERM=truecolor makes it emit the style's
;; exact colors, and TERM=dumb stops it from querying the terminal for its
;; colors.

;;; Code:

(require 'ansi-color)
(require 'goto-addr)
(require 'filenotify)

(defgroup glow-view nil
  "View markdown rendered by glow."
  :group 'markdown)

(defcustom glow-view-program "glow"
  "The glow executable."
  :type 'string)

(defcustom glow-view-min-width 80
  "Narrowest width to render at.
Windows narrower than this truncate lines instead."
  :type 'integer)

(defcustom glow-view-max-width nil
  "Widest width to render at, or nil for no limit."
  :type '(choice (const :tag "No limit" nil) integer))

(defcustom glow-view-style nil
  "Style passed to glow -s: a style name or a JSON style file.
nil means glow/style.json in the XDG config directory when it exists,
otherwise \"dark\" or \"light\" after the frame background."
  :type '(choice (const :tag "Default" nil) string))

(defcustom glow-view-resize-delay 0.1
  "Seconds to wait after a window resize before re-rendering."
  :type 'number)

(defcustom glow-view-timeout 10
  "Seconds to wait for glow before giving up."
  :type 'number)

(defvar-local glow-view--source-buffer nil
  "Buffer whose text is rendered, or nil.")

(defvar-local glow-view--source-file nil
  "File rendered when `glow-view--source-buffer' is not live.")

(defvar-local glow-view--width nil
  "Width of the current rendering.")

(defvar-local glow-view--timer nil)

(defvar-local glow-view--cache nil
  "(KEY . ALIST) of renderings by width.
KEY is the markdown and `glow-view--style-key': the renderings are of
that markdown in that style.")

(defvar glow-view--style-watch nil
  "File notification descriptor for the directory of the style file.")

(defvar glow-view--style-timer nil)

(defconst glow-view--cache-size 8
  "Number of widths to keep renderings of.")

(defconst glow-view--blank " \t\n\u00a0─-╿"
  "Characters not counted when keeping position in a block of text.
This is a `skip-chars-forward' set: whitespace, which changes with line
breaks (glow drops the no-break space padding inline code at a break),
and box drawing characters.  Glow breaks long words at different places
for different widths, so position is a count of the other characters
rather than of words.")

(defconst glow-view--table-rule-re
  "^[ \t]*[-─━]+\\(?:[+┼╋][-─━]+\\)+[ \t]*$"
  "Regexp for the rule under a table header.")

(defconst glow-view--table-separator "|│┃"
  "Characters separating table columns, as a `skip-chars-forward' set.")

(defvar glow-view--faces (make-hash-table :test #'equal)
  "Faces from glow output, so equal faces are `eq'.
Text properties only merge into one interval when their values are
`eq'; without this every escape sequence starts a new interval.")

;;;; rendering

(defun glow-view--style ()
  (or glow-view-style
      (let ((file (expand-file-name
                   "glow/style.json"
                   (or (getenv "XDG_CONFIG_HOME") "~/.config"))))
        (and (file-readable-p file) file))
      (symbol-name (frame-parameter nil 'background-mode))))

(defun glow-view--markdown ()
  "Return the markdown text to render."
  (cond
   ((buffer-live-p glow-view--source-buffer)
    (with-current-buffer glow-view--source-buffer
      (save-restriction
        (widen)
        (buffer-substring-no-properties (point-min) (point-max)))))
   (glow-view--source-file
    (let ((file glow-view--source-file))
      (with-temp-buffer
        (insert-file-contents file)
        (buffer-string))))
   (t (user-error "The markdown source of this buffer is gone"))))

(defun glow-view--run (markdown width)
  "Return MARKDOWN rendered by glow at WIDTH, with faces for its colors.
The output is processed in a scratch buffer: it is mostly escape
sequences, and deleting them in a buffer with undo and change hooks is
slow."
  (let* ((default-directory temporary-file-directory)
         (process-environment
          (append '("TERM=dumb" "COLORTERM=truecolor") process-environment))
         (out (generate-new-buffer " *glow-view-out*" t))
         (err (generate-new-buffer " *glow-view-err*" t)))
    (unwind-protect
        (let ((proc (make-process
                     :name "glow-view"
                     :buffer out
                     :stderr err
                     :command (list glow-view-program
                                    "-s" (glow-view--style)
                                    "-w" (number-to-string width)
                                    "-")
                     :connection-type '(pipe . pty)
                     :coding 'utf-8-unix
                     :noquery t
                     :sentinel #'ignore)))
          (set-process-sentinel (get-buffer-process err) #'ignore)
          (process-send-string proc markdown)
          (process-send-eof proc)
          ;; Poll with a timeout: an untimed wait warns when quit is
          ;; inhibited, as in timers, and would hang Emacs if glow did.
          (let ((stderr-proc (get-buffer-process err))
                (deadline (+ (float-time) glow-view-timeout)))
            (while (or (process-live-p proc) (process-live-p stderr-proc))
              (when (> (float-time) deadline)
                (delete-process proc)
                (error "%s took more than %s seconds" glow-view-program
                       glow-view-timeout))
              (accept-process-output nil 0.05))
            ;; Output can still be unread when the exit is seen.
            (while (accept-process-output proc 0 nil t))
            (while (accept-process-output stderr-proc 0 nil t)))
          (unless (eql (process-exit-status proc) 0)
            (error "%s failed (%s): %s" glow-view-program
                   (process-exit-status proc)
                   (with-current-buffer err (string-trim (buffer-string)))))
          (with-current-buffer out
            (goto-char (point-min))
            (while (search-forward "\r" nil t)
              (replace-match ""))
            ;; Glow pads each line to the width with spaces in escape
            ;; sequences of their own, most of the output at large widths.
            ;; Drop them before parsing; the reset keeps a color from
            ;; running into the next line.
            (goto-char (point-min))
            (while (re-search-forward "\\(?:\e\\[[0-9;]*m\\| \\)+$" nil t)
              (replace-match "\e[0m" t t))
            (let ((ansi-color-apply-face-function #'glow-view--put-face)
                  (ansi-color-context-region nil))
              (ansi-color-apply-on-region (point-min) (point-max)))
            (goto-char (point-min))
            (while (re-search-forward "[ \t]+$" nil t)
              (replace-match ""))
            (buffer-string)))
      (kill-buffer out)
      (kill-buffer err))))

(defun glow-view--put-face (beg end face)
  (when face
    (put-text-property
     beg end 'face
     (with-memoization (gethash face glow-view--faces) face))))

;;;; keeping position

;; A position is kept as the block it is in, and the place in that block.
;; Blocks are runs of non-empty lines: paragraphs, headings, lists, tables
;; and code blocks.  Their number does not change with the width.  The
;; place in a block is the count of non-blank characters before it.  In a
;; table, cells wrap side by side, so the text order changes with the
;; width but stays the same within a column; there the place is the
;; column and the count of non-blank characters above it in that column.

(defun glow-view--blocks ()
  "Return a vector of (START . END) of the blocks in the buffer."
  (save-excursion
    (goto-char (point-min))
    (let (blocks)
      (while (progn (skip-chars-forward "\n") (not (eobp)))
        (let ((start (point)))
          (if (search-forward "\n\n" nil 'move)
              (push (cons start (match-beginning 0)) blocks)
            (skip-chars-backward "\n")
            (push (cons start (point)) blocks)
            (goto-char (point-max)))))
      (vconcat (nreverse blocks)))))

(defun glow-view--table-p (block)
  (save-excursion
    (goto-char (car block))
    (re-search-forward glow-view--table-rule-re (cdr block) t)))

(defun glow-view--count-non-blank (start end)
  "Return the number of non-blank characters from START to END.
See `glow-view--blank'."
  (let ((n 0))
    (save-excursion
      (goto-char start)
      (while (< (point) end)
        (skip-chars-forward glow-view--blank end)
        (let ((from (point)))
          (skip-chars-forward (concat "^" glow-view--blank) end)
          (setq n (+ n (- (point) from))))))
    n))

(defun glow-view--skip-non-blank (n end)
  "Move past N non-blank characters, not beyond END.
Then move to the next non-blank character."
  (while (and (> n 0) (< (point) end))
    (skip-chars-forward glow-view--blank end)
    (let ((from (point)))
      (skip-chars-forward (concat "^" glow-view--blank)
                          (min end (+ (point) n)))
      (setq n (- n (- (point) from)))))
  (skip-chars-forward glow-view--blank end))

(defun glow-view--cell (column)
  "Return (START . END) of table COLUMN on the current line.
Columns count from 0.  On a line with fewer columns both are its end."
  (save-excursion
    (let ((eol (line-end-position))
          (not-separator (concat "^" glow-view--table-separator)))
      (beginning-of-line)
      (dotimes (_ column)
        (skip-chars-forward not-separator eol)
        (unless (eolp) (forward-char 1)))
      (let ((start (point)))
        (skip-chars-forward not-separator eol)
        (cons start (point))))))

(defun glow-view--column-at (pos)
  "Return the table column POS is in."
  (save-excursion
    (goto-char pos)
    (let ((n 0)
          (not-separator (concat "^" glow-view--table-separator)))
      (beginning-of-line)
      (while (progn (skip-chars-forward not-separator pos)
                    (< (point) pos))
        (forward-char 1)
        (setq n (1+ n)))
      n)))

(defun glow-view--cell-count (column &optional end)
  "Count non-blank characters of COLUMN on the current line, up to END.
The rule under the header has none."
  (if (save-excursion (beginning-of-line)
                      (looking-at-p glow-view--table-rule-re))
      0
    (let ((cell (glow-view--cell column)))
      (glow-view--count-non-blank
       (car cell) (if end (max (car cell) (min end (cdr cell))) (cdr cell))))))

(defun glow-view--anchor (pos blocks)
  "Return the place of POS in BLOCKS from `glow-view--blocks'.
It is (INDEX start), (INDEX text COUNT) or (INDEX table COLUMN COUNT)."
  (let ((i 0)
        (n (length blocks)))
    (while (and (< i n) (< (cdr (aref blocks i)) pos))
      (setq i (1+ i)))
    (if (or (= i n) (< pos (car (aref blocks i))))
        (list i 'start)
      (let ((block (aref blocks i)))
        (if (glow-view--table-p block)
            (save-excursion
              (let ((column (glow-view--column-at pos))
                    (line (progn (goto-char pos) (line-beginning-position)))
                    (count 0))
                (goto-char (car block))
                (while (< (point) line)
                  (setq count (+ count (glow-view--cell-count column)))
                  (forward-line 1))
                (list i 'table column
                      (+ count (glow-view--cell-count column pos)))))
          (list i 'text (glow-view--count-non-blank (car block) pos)))))))

(defun glow-view--anchor-pos (anchor blocks)
  "Return the position of ANCHOR from `glow-view--anchor' in BLOCKS."
  (pcase-let ((`(,i ,kind . ,data) anchor))
    (if (>= i (length blocks))
        (point-max)
      (let ((block (aref blocks i)))
        (save-excursion
          (goto-char (car block))
          (pcase kind
            ('text
             (glow-view--skip-non-blank (car data) (cdr block)))
            ((guard (and (eq kind 'table) (glow-view--table-p block)))
             (pcase-let ((`(,column ,count) data)
                         (found nil))
               (while (and (not found) (< (point) (cdr block)))
                 (let ((here (glow-view--cell-count column)))
                   (if (< count here)
                       (let ((cell (glow-view--cell column)))
                         (goto-char (car cell))
                         (glow-view--skip-non-blank count (cdr cell))
                         (setq found t))
                     (setq count (- count here))
                     (forward-line 1))))
               (unless found
                 (goto-char (cdr block))))))
          (point))))))

;;;; rendering into the buffer

(defun glow-view--style-key ()
  "Return the style and, for a style file, its modification time."
  (let ((style (glow-view--style)))
    (cons style
          (and (file-name-absolute-p style)
               (file-attribute-modification-time (file-attributes style))))))

(defun glow-view--rendering (markdown width)
  "Return MARKDOWN rendered at WIDTH, from the cache when possible."
  (let ((key (cons markdown (glow-view--style-key))))
    (unless (equal key (car glow-view--cache))
      (setq glow-view--cache (list key))))
  (or (alist-get width (cdr glow-view--cache))
      (let ((text (glow-view--run markdown width)))
        (setcdr glow-view--cache
                (take glow-view--cache-size
                      (cons (cons width text) (cdr glow-view--cache))))
        text)))

(defun glow-view--render (width)
  "Render the source at WIDTH, keeping point and window starts."
  (let* ((text (glow-view--rendering (glow-view--markdown) width))
         (blocks (glow-view--blocks))
         (point-anchor (glow-view--anchor (point) blocks))
         (windows (mapcar (lambda (win)
                            (list win
                                  (glow-view--anchor (window-point win) blocks)
                                  (glow-view--anchor (window-start win) blocks)))
                          (get-buffer-window-list nil nil t)))
         (inhibit-read-only t))
    (erase-buffer)
    (insert text)
    (setq glow-view--width width)
    (set-buffer-modified-p nil)
    (setq blocks (glow-view--blocks))
    (goto-char (glow-view--anchor-pos point-anchor blocks))
    (pcase-dolist (`(,win ,pt ,start) windows)
      (set-window-point win (glow-view--anchor-pos pt blocks))
      (set-window-start
       win (save-excursion
             (goto-char (glow-view--anchor-pos start blocks))
             (line-beginning-position))
       t))))

(defun glow-view--target-width ()
  "Width to render at for the windows showing the buffer.
The narrowest window decides, so no window needs to truncate unless it
is narrower than `glow-view-min-width'."
  (let* ((wins (get-buffer-window-list nil nil t))
         (width (if wins
                    (apply #'min (mapcar #'window-max-chars-per-line wins))
                  (or glow-view--width glow-view-min-width))))
    (when glow-view-max-width
      (setq width (min width glow-view-max-width)))
    (max width glow-view-min-width)))

(defun glow-view-refresh (&optional force)
  "Re-render if the width changed, or always with FORCE."
  (interactive (list t))
  (let ((width (glow-view--target-width)))
    (when (or force (not (eql width glow-view--width)))
      (glow-view--render width))))

(defun glow-view--revert (&rest _)
  (setq glow-view--cache nil)
  (glow-view-refresh t))

(defun glow-view--refresh-later (buffer)
  (with-current-buffer buffer
    (when (timerp glow-view--timer)
      (cancel-timer glow-view--timer))
    (setq glow-view--timer
          (run-with-timer
           glow-view-resize-delay nil
           (lambda ()
             (when (buffer-live-p buffer)
               (with-current-buffer buffer
                 (setq glow-view--timer nil)
                 (condition-case err
                     (with-local-quit
                       (glow-view-refresh))
                   (error (message "glow-view: %s" (error-message-string err)))))))))))

(defun glow-view--window-changed (window)
  "Schedule a re-render of the buffer in WINDOW if it is a glow view.
For the buffer-local values of `window-size-change-functions' and
`window-buffer-change-functions'.  These also run for a window that
switched away from the view, which then shows another buffer."
  (when (and (windowp window)
             (with-current-buffer (window-buffer window)
               (derived-mode-p 'glow-view-mode)))
    (glow-view--refresh-later (window-buffer window))))

(defun glow-view--text-scale-changed ()
  (glow-view--refresh-later (current-buffer)))

(defun glow-view--source-saved ()
  "Re-render the glow views of the current buffer."
  (let ((source (current-buffer)))
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when (and (derived-mode-p 'glow-view-mode)
                   (eq glow-view--source-buffer source))
          (glow-view-refresh t))))))

(defun glow-view--refresh-all ()
  "Re-render all glow views."
  (setq glow-view--style-timer nil)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (derived-mode-p 'glow-view-mode)
        (condition-case err
            (with-local-quit
              (glow-view-refresh t))
          (error (message "glow-view: %s" (error-message-string err))))))))

(defun glow-view--style-changed (event)
  "Schedule re-rendering all glow views if EVENT is on the style file.
The directory is watched, as installing a file often replaces it."
  (let ((style (glow-view--style)))
    (when (member style (mapcar (lambda (f) (and (stringp f) (expand-file-name f)))
                                (nthcdr 2 event)))
      (when (timerp glow-view--style-timer)
        (cancel-timer glow-view--style-timer))
      (setq glow-view--style-timer
            (run-with-timer 0.5 nil #'glow-view--refresh-all)))))

(defun glow-view--watch-style ()
  "Start watching the style file for changes, once."
  (let ((style (glow-view--style)))
    (when (and (not (file-notify-valid-p glow-view--style-watch))
               (file-name-absolute-p style)
               (file-exists-p style))
      (setq glow-view--style-watch
            (ignore-errors
              (file-notify-add-watch (file-name-directory style) '(change)
                                     #'glow-view--style-changed))))))

;;;; source positions

;; Positions are matched between the source and the rendering by the words
;; in both: glow keeps the words of the source in order, apart from markup,
;; bullets and front matter, and table cells, which it interleaves.  GNU
;; diff aligns the two lists of words, and a position maps to the matching
;; word, or the nearest word that has a match.

(defconst glow-view--word-re "[[:alnum:]]+")

(defun glow-view--words ()
  "Return the words of the buffer as a vector of (START . END)."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (let (words)
        (while (re-search-forward glow-view--word-re nil t)
          (push (cons (match-beginning 0) (match-end 0)) words))
        (vconcat (nreverse words))))))

(defun glow-view--write-words (words file)
  "Write the text of WORDS of the current buffer to FILE, one per line."
  (let ((text (mapconcat (lambda (w)
                           (downcase (buffer-substring-no-properties
                                      (car w) (cdr w))))
                         words "\n"))
        (coding-system-for-write 'utf-8-unix))
    (with-temp-buffer
      (insert text "\n")
      (write-region nil nil file nil 'silent))))

(defun glow-view--align (a-buffer a-words b-buffer b-words)
  "Align A-WORDS of A-BUFFER with B-WORDS of B-BUFFER.
Return (A-MAP . B-MAP): vectors with, for each word, the index of the
matching word on the other side, or nil."
  (let ((a-file (make-temp-file "glow-view-a"))
        (b-file (make-temp-file "glow-view-b"))
        (a-map (make-vector (length a-words) nil))
        (b-map (make-vector (length b-words) nil)))
    (unwind-protect
        (progn
          (with-current-buffer a-buffer (glow-view--write-words a-words a-file))
          (with-current-buffer b-buffer (glow-view--write-words b-words b-file))
          (with-temp-buffer
            (let* ((default-directory temporary-file-directory)
                   (status (call-process "diff" nil t nil
                                         "--old-line-format=<\n"
                                         "--new-line-format=>\n"
                                         "--unchanged-line-format==\n"
                                         a-file b-file)))
              (unless (memq status '(0 1))
                (error "diff failed: %s" (string-trim (buffer-string)))))
            (goto-char (point-min))
            (let ((i 0) (j 0))
              (while (not (eobp))
                (pcase (char-after)
                  (?= (aset a-map i j)
                      (aset b-map j i)
                      (setq i (1+ i) j (1+ j)))
                  (?< (setq i (1+ i)))
                  (?> (setq j (1+ j))))
                (forward-line 1)))))
      (delete-file a-file)
      (delete-file b-file))
    (cons a-map b-map)))

(defun glow-view--translate (pos words map other-words)
  "Return the position matching POS on the other side of an alignment.
WORDS are the words on the side of POS, MAP from `glow-view--align'
maps their indexes to OTHER-WORDS.  POS maps from the word containing
it or else the next one, keeping the offset into a word that matched."
  (let ((n (length words))
        (lo 0) hi)
    (when (> n 0)
      ;; The first word ending after POS.
      (setq hi n)
      (while (< lo hi)
        (let ((mid (/ (+ lo hi) 2)))
          (if (< (cdr (aref words mid)) pos)
              (setq lo (1+ mid))
            (setq hi mid))))
      (let ((k (min lo (1- n)))
            (d 0)
            found)
        (while (and (not found) (or (>= (- k d) 0) (< (+ k d) n)))
          (cond ((and (>= (- k d) 0) (aref map (- k d)))
                 (setq found (- k d)))
                ((and (< (+ k d) n) (aref map (+ k d)))
                 (setq found (+ k d))))
          (setq d (1+ d)))
        (when found
          (let ((other (aref other-words (aref map found))))
            (if (= found k)
                (min (cdr other)
                     (+ (car other) (max 0 (- pos (car (aref words k))))))
              (car other))))))))

(defun glow-view--call-with-source (fn)
  "Call FN with a buffer holding the source text.
For a source that is only a file, that is a temporary buffer."
  (cond
   ((buffer-live-p glow-view--source-buffer)
    (funcall fn glow-view--source-buffer))
   (glow-view--source-file
    (let ((file glow-view--source-file))
      (with-temp-buffer
        (insert-file-contents file)
        (funcall fn (current-buffer)))))
   (t (user-error "The markdown source of this buffer is gone"))))

(defun glow-view--source-position (pos)
  "Return the position in the source of POS in the rendering."
  (let ((view (current-buffer))
        (view-words (glow-view--words)))
    (glow-view--call-with-source
     (lambda (source)
       (let ((source-words (with-current-buffer source (glow-view--words))))
         (glow-view--translate
          pos view-words
          (car (glow-view--align view view-words source source-words))
          source-words))))))

(defun glow-view--view-position (source pos)
  "Return the position in the rendering of POS in buffer SOURCE."
  (let* ((view (current-buffer))
         (view-words (glow-view--words))
         (source-words (with-current-buffer source (glow-view--words)))
         (source-map (cdr (glow-view--align view view-words source source-words))))
    (with-current-buffer source
      (glow-view--translate pos source-words source-map view-words))))

(defun glow-view--window-row ()
  "Return the line of point in the selected window, counting from 0."
  (count-lines (window-start) (line-beginning-position)))

;;;; mode and commands

(defun glow-view-visit-source ()
  "Switch to the markdown source, at the text at point."
  (interactive)
  (let ((pos (glow-view--source-position (point)))
        (row (glow-view--window-row)))
    (cond
     ((buffer-live-p glow-view--source-buffer)
      (pop-to-buffer-same-window glow-view--source-buffer))
     (glow-view--source-file
      (find-file glow-view--source-file))
     (t (user-error "The markdown source of this buffer is gone")))
    (when pos
      (goto-char pos)
      (recenter row))))

(defvar-keymap glow-view-mode-map
  :parent special-mode-map
  "RET" #'glow-view-visit-source
  "e" #'glow-view-visit-source)

(define-derived-mode glow-view-mode special-mode "Glow"
  "Major mode for markdown rendered by glow.
\\{glow-view-mode-map}"
  (setq truncate-lines t)
  (setq buffer-undo-list t)
  (setq-local revert-buffer-function #'glow-view--revert)
  (add-hook 'window-size-change-functions #'glow-view--window-changed nil t)
  (add-hook 'window-buffer-change-functions #'glow-view--window-changed nil t)
  (add-hook 'text-scale-mode-hook #'glow-view--text-scale-changed nil t)
  (goto-address-mode 1))

(defun glow-view--find (source-buffer source-file)
  "Return the glow view of SOURCE-BUFFER or SOURCE-FILE, if any."
  (seq-find (lambda (buf)
              (with-current-buffer buf
                (and (derived-mode-p 'glow-view-mode)
                     (if source-buffer
                         (eq glow-view--source-buffer source-buffer)
                       (equal glow-view--source-file source-file)))))
            (buffer-list)))

(defun glow-view--markdown-buffer-p ()
  (or (derived-mode-p 'markdown-mode)
      (and buffer-file-name
           (string-match-p "\\.\\(md\\|markdown\\)\\'" buffer-file-name))))

;;;###autoload
(defun glow-view (&optional file)
  "Show markdown rendered by glow in a read-only buffer.
Render FILE, or the current buffer when FILE is nil.  When FILE is
visited by a buffer, that buffer's text is rendered, saved or not.
Interactively, render the current buffer if it is markdown, the file at
point in Dired, or else a file read from the minibuffer.  When rendering
the current buffer, point goes to the text that was at point in it."
  (interactive
   (list (cond ((glow-view--markdown-buffer-p) nil)
               ((derived-mode-p 'dired-mode) (dired-get-filename))
               (t (read-file-name "Markdown file: " nil nil t)))))
  (unless (executable-find glow-view-program)
    (user-error "Cannot find %s" glow-view-program))
  (let* ((file (and file (expand-file-name file)))
         (source-buffer (if file
                            (find-buffer-visiting file)
                          (or (buffer-base-buffer) (current-buffer))))
         (source-file (or file
                          (buffer-local-value 'buffer-file-name source-buffer)))
         (buf (or (glow-view--find source-buffer source-file)
                  (generate-new-buffer
                   (format "*glow: %s*"
                           (if source-buffer
                               (buffer-name source-buffer)
                             (file-name-nondirectory file)))))))
    (glow-view--watch-style)
    (when source-buffer
      (with-current-buffer source-buffer
        (add-hook 'after-save-hook #'glow-view--source-saved nil t)))
    (with-current-buffer buf
      (unless (derived-mode-p 'glow-view-mode)
        (glow-view-mode))
      (setq glow-view--source-buffer source-buffer
            glow-view--source-file source-file)
      (when source-file
        (setq default-directory (file-name-directory source-file))))
    (let ((source-pos (and (not file) (point)))
          (row (and (not file) (glow-view--window-row))))
      (pop-to-buffer-same-window buf)
      (glow-view-refresh t)
      (when source-pos
        (let ((pos (glow-view--view-position source-buffer source-pos)))
          (when pos
            (goto-char pos)
            (recenter row)))))))

(provide 'glow-view)

;;; glow-view.el ends here
