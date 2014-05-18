(load (expand-file-name "load-path" (file-name-directory load-file-name)) nil t)
(load "profile-dotemacs.el")
(setq profile-dotemacs-file "~/.emacs.d/init.el")
(profile-dotemacs)
