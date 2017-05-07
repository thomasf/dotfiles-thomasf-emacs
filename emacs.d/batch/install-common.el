(require 'package)
(package-initialize)

(setq
 package-enable-at-startup nil
 package-archives
 '(("melpa-stable" . "https://stable.melpa.org/packages/")
   ("melpa" . "https://melpa.org/packages/")
   ("marmalade"   . "https://marmalade-repo.org/packages/")
   ("org"         . "http://orgmode.org/elpa/")
   ("gnu"         . "https://elpa.gnu.org/packages/")))

(defun require-package (package &optional min-version no-refresh)
    "Install given PACKAGE, optionally requiring MIN-VERSION.
If NO-REFRESH is non-nil, the available package lists will not be
re-downloaded in order to locate PACKAGE."
    (if (package-installed-p package min-version)
        t
      (if (or (assoc package package-archive-contents) no-refresh)
          (package-install package)
        (progn
         (package-refresh-contents)
          (require-package package min-version t)))))

(require-package 'use-package)
