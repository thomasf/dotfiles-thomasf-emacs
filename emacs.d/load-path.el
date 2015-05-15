;;; load-path.el

(defconst user-emacs-directory
  (if (eq system-type 'ms-dos)
      ;; MS-DOS cannot have initial dot.
      "~/_emacs.d/"
    "~/.emacs.d/")
  "Directory beneath which additional per-user Emacs-specific files are placed.
Various programs in Emacs store information in this directory.
Note that this should end with a directory separator.
See also `locate-user-emacs-file'.")

(defconst user-data-directory
  (file-truename "~/.config/emacs-user-data"))
(defconst user-cache-directory
  (file-truename "~/.cache/emacs-user-cache"))
(defconst user-lisp-directory
  (expand-file-name "lisp" user-emacs-directory))

(defun load-path--take (n list)
  "Returns a new list of the first N items in LIST, or all items if there are fewer than N.
This is just a copy of the fully expanded macro from dash."
  (let (result)
    (let
        ((num n)
         (it 0))
      (while
          (< it num)
        (when list
          (setq result
                (cons
                 (car list)
                 result))
          (setq list
                (cdr list)))
        (setq it
              (1+ it))))
    (nreverse result)))


(defconst user-site-lisp-directory
  (expand-file-name "site-lisp/shared" user-emacs-directory))
(defconst user-themes-directory
  (expand-file-name "themes" user-emacs-directory))
(defconst user-notes-directory
  (file-truename "~/notes"))

;; These should always exist
(make-directory user-data-directory t)
(make-directory user-cache-directory t)

;; emacs23 compat
(if (boundp 'custom-theme-load-path)
    (add-to-list 'custom-theme-load-path user-themes-directory)
  (add-to-list 'load-path user-themes-directory))

(defun add-to-load-path (path &optional dir)
  (setq load-path
        (cons (expand-file-name path (or dir user-emacs-directory)) load-path)))

(defun load-path-load-path ()
  (let ((load-path load-path))
    ;; Add top-level lisp directories, in case they were not setup by the
    ;; environment.
    (require 'package)
    (package-initialize)
    (dolist (dir (nreverse
                  (list user-lisp-directory
                        user-site-lisp-directory)))
      (dolist (entry (nreverse (directory-files-and-attributes dir)))
        (and
         (cadr entry)
         (not (string= (car entry) ".."))
         (add-to-load-path (car entry) dir))))


    (mapc #'add-to-load-path
          (nreverse
           (list
            (expand-file-name "~/.config-private/emacs")
            (expand-file-name "~/.opt/extempore/extras")
            (expand-file-name "/usr/local/opt/extempore/extras")
            (concat user-site-lisp-directory "/emms/lisp")
            "/usr/local/share/emacs/site-lisp/"
            "/usr/local/share/emacs/site-lisp/mu4e/"
            "/opt/local/share/emacs/site-lisp/"
            "/usr/share/emacs/site-lisp/SuperCollider/"
            "/usr/share/emacs/site-lisp/supercollider/"
            "/var/lib/gems/1.9.1/gems/trogdoro-el4r-1.0.10/data/emacs/site-lisp/")))

    (let ((cl-p load-path))
      (while cl-p
        (setcar cl-p (file-name-as-directory
                      (expand-file-name (car cl-p))))
        (setq cl-p (cdr cl-p))))

    (when
        (or (not (boundp 'emacs-version))
           (string< emacs-version "24.3"))
      (add-to-load-path
       (expand-file-name "site-lisp/cl-lib" user-emacs-directory)))


    (delete-dups
     (delq nil (mapcar #'(lambda (x)
                         (if (file-directory-p x)
                             x
                           nil))
                     load-path)))))


(defmacro load-path-set-load-path ()
  `(progn
     (setq load-path ',(load-path-load-path))
     (let ((failed nil))
       (mapc #'(lambda (x)
                 (unless failed
                   (setq failed (not (file-directory-p x)))))
             load-path)
       (when failed
         (require 'bytecomp)
         (let ((byte-compile-verbose nil)
               (byte-compile-warnings nil)
               (use-package-verbose nil)
               (ad-redefinition-action 'accept))
           (byte-recompile-file "~/.emacs.d/load-path.el" t 0 t))))))

(load-path-set-load-path)

(eval-after-load "info"
  #'(progn
      (when (fboundp 'info-initialize)
        (info-initialize)
        (defun add-to-info-path (path &optional dir)
          (setq Info-directory-list
                (cons (expand-file-name path (or dir user-emacs-directory)) Info-directory-list)))
        (mapc #'add-to-info-path
              (nreverse
               (list
                (expand-file-name "~/.refdoc/info")))))))

(when (bound-and-true-p x-bitmap-file-path)
  (add-to-list 'x-bitmap-file-path
               (concat user-emacs-directory "/icons")))

(require 'cus-load nil t)

(provide 'load-path)

;;; load-path.el ends here
