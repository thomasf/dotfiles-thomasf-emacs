;;; archive.el --- Tries to ensure an correct environment -*- lexical-binding: nil; -*-

;;; Commentary:
;;

;;; Code:

(setq use-package-always-demand t
      noninteractive nil
      window-system t)
(require 'package)
(package-initialize)
(load (expand-file-name "init.el" (file-name-directory load-file-name)))
