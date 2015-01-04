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

(defconst my-simplified-emacs-version-number
  (mapconcat 'identity (mapcar
                        #'(lambda (x)
                            (number-to-string x))
                        (load-path--take 3 (version-to-list emacs-version)))
             "."))

(defconst user-elpa-directory
  (expand-file-name (format "packages/%s" my-simplified-emacs-version-number) user-emacs-directory))
(defconst user-lib-directory
  (expand-file-name "lib" user-emacs-directory))
(defconst user-site-lisp-directory
  (expand-file-name "site-lisp/shared" user-emacs-directory))
(defconst user-site-lisp-version-dependent-directory
  (expand-file-name  (concat "site-lisp/emacs"
                             (if (or (not (boundp 'emacs-version))
                                    (string< emacs-version "24"))
                                 "23"
                               "24"))
                     user-emacs-directory))
(defconst user-local-site-lisp-directory
  (expand-file-name "emacs-site-lisp" "~/.opt"))
(defconst user-override-directory
  (expand-file-name "override" user-emacs-directory))
(defconst user-local-override-directory
  (expand-file-name "emacs-override" "~/.opt"))
(defconst user-themes-directory
  (expand-file-name "themes" user-emacs-directory))
(defconst user-notes-directory
  (file-truename "~/notes"))

;; These should always exist
(make-directory user-data-directory t)
(make-directory user-elpa-directory t)
(make-directory user-cache-directory t)
(make-directory user-local-override-directory t)
(make-directory user-local-site-lisp-directory t)

;; emacs23 compat
(if (boundp 'custom-theme-load-path)
    (add-to-list 'custom-theme-load-path user-themes-directory)
  (add-to-list 'load-path user-themes-directory))

(defun add-to-load-path (path &optional dir)
  (setq load-path
        (cons (expand-file-name path (or dir user-emacs-directory)) load-path)))

;; Add top-level lisp directories, in case they were not setup by the
;; environment.
(dolist (dir (nreverse
              (list user-override-directory
                    user-lisp-directory
                    user-lib-directory
                    user-elpa-directory
                    user-local-site-lisp-directory
                    user-site-lisp-directory
                    user-site-lisp-version-dependent-directory)))
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

(setq load-path (delete-dups load-path))

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
