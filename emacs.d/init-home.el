;; (make-frame)
(load (expand-file-name "init" (file-name-directory load-file-name)) nil t)
;; ;; (org-agenda nil "n")
;; (with-selected-frame
;;     (next-frame)
;;   (org-agenda nil " "))

(org-agenda nil " ")
(edit-server-start)
(server-start)
;; (start-elnode-bm)

