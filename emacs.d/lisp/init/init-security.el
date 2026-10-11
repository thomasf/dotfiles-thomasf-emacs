;;; init-security.el --- credentials, encryption and network trust -*- lexical-binding: nil; byte-compile-warnings: (not docstrings-wide); -*-

;;; Commentary:

;; Everything that handles credentials, keys or network trust:
;;
;; - where credentials and tokens are stored (auth-source, oauth2)
;; - the TLS certificate/trust database (nsm)
;; - OpenPGP signing and encryption of mail (mml2015)
;; - password generation (aes)
;;
;; Settings that are part of another feature stay with it:
;; `gnutls-algorithm-priority' in the bootstrap in init.el, the .el.gpg
;; load suffix in init-core, smtpmail STARTTLS in init-core and
;; `epa-mail-mode' in the mu4e block in init-org.
;;
;; Sections are kept in alphabetical order.
;;
;; Loaded by init.el in this order: init-core, init-ui, init-commands,
;; init-security, init-packages, init-org.


;;; Code:

(require 'init-lib)
(eval-when-compile
  (require 'cl)
  (require 'subr-x)
  (require 'use-package)
  (require 'dash)
  (require 's)
  (require 'f)
  (require 'hydra)
  (require 'smartrep)
  (require 'fringe-helper))


;;; use-package

;;;; aes

(use-package aes
  :ensure t
  :commands (aes-insert-password))


;;;; auth-source

(use-package auth-source
  :defer
  :init
  (progn
    (setq ;; auth.el
     auth-sources '("~/.authinfo.gpg"))))


;;;; mml2015

(use-package mml2015
  :defer
  :init
  (progn
    (setq mml-secure-openpgp-encrypt-to-self t
          mml-secure-openpgp-sign-with-sender t
          mml2015-use 'epg)))


;;;; nsm

(use-package nsm
  :defer t
  :init
  (progn
    (setq nsm-settings-file (expand-file-name
                             "network-security.data" user-data-directory))))


;;;; oauth2

(use-package oauth2
  :ensure t
  ;; :commands ()
  :defer
  :init
  (progn
    (setq
     oauth2-token-file "~/.config-private/oauth2.plstore")))



(provide 'init-security)

;; Local Variables:
;; eval: (outline-minor-mode 1)
;; eval: (require 'use-package)
;; End:


;;; init-security.el ends here
