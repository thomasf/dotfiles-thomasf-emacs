
(and (fboundp 'menu-bar-mode)
     (not (eq system-type 'darwin))
     (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))


(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)

(defun require-package (package &optional min-version no-refresh)
  (if (package-installed-p package min-version)
      t
    (if (or (assoc package package-archive-contents) no-refresh)
        (package-install package)
      (progn
        (package-refresh-contents)
        (require-package package min-version t)))))

(mapc 'require-package
      '(dash use-package flx-ido))

(setq
 gc-cons-threshold 20000000
 projectile-sort-order 'recently-active
 projectile-completion-system 'ido
 projectile-switch-project-action 'projectile-dired
 projectile-verbose nil)

(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)

(projectile-global-mode)
