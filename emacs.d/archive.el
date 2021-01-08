;;; archive.el --- Tries to ensure an correct environment

;;; Commentary:
;;

;;; Code:

(require 'package)
(package-initialize)
(require 'elpa-mirror)
(elpamr-create-mirror-for-installed (expand-file-name "elpa-mirror/archive" (file-name-directory load-file-name)) t)
