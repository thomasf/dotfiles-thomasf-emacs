;;; makeup.el --- Tries to ensure an correct environment

;;; Commentary:
;;


;;; Code:

(defun makeup-log (msg)
  "Print `MSG' to messages."
  (message " ")
  (message "-- %s" msg))

(defvar makeup-emacsd-path
  (file-name-directory load-file-name)
  "The path to emacs.d.")


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

(require 'package)
(mkdir package-user-dir t)


(makeup-log "Delete ELPA leftovers and add elpa packages to load path.")
(let ((default-directory package-user-dir))
  (mapc (lambda (dir)
          (and
           (file-directory-p dir)
           (let ((pkgel (file-expand-wildcards (concat dir "/*-pkg.el")) ))
             (when pkgel
               (setq load-path
                     (cons (expand-file-name dir default-directory) load-path)))
             (not pkgel))
           (file-expand-wildcards (concat dir "/*-pkg.elc"))
           (delete-directory dir t)))
        (directory-files package-user-dir)))


;; require some packages
(require 'use-package)
(use-package s)
(use-package dash)
(use-package f)


(makeup-log "Delete el/elc files.")

(f-files makeup-emacsd-path
         (lambda (file)
           (and
            (f-file? file)
            (equal (f-ext file) "elc")
            (f-delete file))))

(f-files (f-join makeup-emacsd-path "snippets")
         (lambda (file)
           (when
               (or
                (equal (f-no-ext (f-filename file))
                       ".yas-compiled-snippets")
                (equal (f-ext? file) "elc"))
             (f-delete file)))
         t)

 (--each '("site-lisp" "lisp")
  (f-files (f-join makeup-emacsd-path it)
           (lambda (file)
             (when (and
                    (f-file? file)
                    (equal (f-ext file) "elc"))
               (cond ((not (f-exists? (concat (f-no-ext file ) ".el")))
                      (f-delete file))
                     ((time-less-p
                       (nth 5 (file-attributes file))
                       (nth 5 (file-attributes (concat (f-no-ext file) ".el"))))
                      (f-delete file)))))
           t))


(makeup-log "Load init.el")
(setq byte-compile-verbose nil
      byte-compile-warnings nil
      use-package-verbose nil
      ad-redefinition-action 'accept)
(load (expand-file-name "init" makeup-emacsd-path) nil t)



(defun was-compiled-p (path)
  "Does the directory at PATH contain .elc files?"
  (--any-p (s-ends-with-p ".elc" it) (directory-files path)))

(defun no-dot-directories (directories)
  "Exclude the . and .. directory from a list."
  (--remove (or (string= "." (file-name-nondirectory it))
               (string= ".." (file-name-nondirectory it))
               (string= ".git" (file-name-nondirectory it))
               (string= "archives" (file-name-nondirectory it)))
            directories))

(defun ensure-packages-compiled ()
  "If any packages installed with package.el aren't compiled yet, compile them."
  (let* ((package-files (no-dot-directories (directory-files package-user-dir t)))
         (package-directories (-filter 'file-directory-p package-files)))
    (dolist (directory package-directories)
      (unless (was-compiled-p directory)
        (byte-recompile-directory directory 0)))))

;; (makeup-log "If any packages installed with package.el aren't compiled yet, compile them.")
;; (ensure-packages-compiled)


(use-package multi-term)
(defvar user-site-lisp-directory)
(defvar user-lisp-directory)
(makeup-log "byte recompile emacsd lisp dirs")
(byte-recompile-directory user-site-lisp-directory 0)
(byte-recompile-directory user-lisp-directory 0)
(byte-recompile-directory "~/.config-private/emacs" 0)


(makeup-log "byte recompile emacsd init files and snippets")
(byte-compile-file "load-path.el" nil)
(mapc (lambda (f)
        (byte-compile-file f nil))
      (f-glob "init*.el"))
(yas-recompile-all)


(makeup-log "Done!")

;;; makeup.el ends here
