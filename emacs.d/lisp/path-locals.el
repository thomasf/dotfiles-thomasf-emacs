;;; path-locals.el --- Buffer setup by path or project, like dir-locals with code -*- lexical-binding: t; -*-

;;; Commentary:

;; Run buffer-local setup for files and dired buffers that match a
;; rule.  Each rule has a name and matches on `:path', `:project' or
;; both (either one matching is enough).  Re-evaluating a rule with the
;; same name replaces it.
;;
;; All matching rules run, one after another.  A rule that signals an
;; error is reported and skipped; the rest still run.  The order is:
;;
;;   1. `:priority', lowest first.  Defaults to 0, so negative values
;;      run before ordinary rules and positive values after them.
;;   2. Specificity, lowest first, among rules with the same priority.
;;      It is computed: the length of the `:path' string (for `rx'
;;      forms, the generated regexp), or 1000 for rules with only
;;      `:project'.  Shorter (broader) paths
;;      run first, so deeper paths override settings from their
;;      parents, and project rules run after path rules.
;;
;; Later rules override earlier ones but do not undo them.
;; `path-locals-doctor' shows both numbers for each matching rule.
;;
;; `:path' is always a regexp, matched against the expanded path (no
;; "~").  It matches anywhere in the path; start it with "/" to match
;; at a directory boundary, or with `bos' to anchor it at the root.
;; `path-locals-home' matches the expanded home directory.
;;
;;   ;; Everything under ~/src/example.org/acme/.
;;   (path-locals acme
;;     :path (rx bos path-locals-home "src/example.org/acme/")
;;     (setq-local fill-column 100))
;;
;;   ;; Regexp; also matches worktrees such as "widget-service@fix-login/".
;;   (path-locals widget-service
;;     :path (rx "/src/example.org/acme/"
;;               (path-locals-worktree "widget-service"))
;;     (setq-local lsp-go-build-flags ["-tags=e2e"]))
;;
;;   ;; Either of two paths, including everything below them.
;;   (path-locals example-checks
;;     :path (rx (or "/src/example.org/acme/gadget-api/"
;;                   "/src/example.org/tools/"))
;;     (setq-local compile-command "make -k check"))
;;
;;   ;; Anchored regexp: only under ~/src, any example.org repo
;;   ;; ending in "-service", not ~/backup/src/... copies.
;;   (path-locals example-services
;;     :path (rx bos path-locals-home "src/example.org/"
;;               (+ (not "/")) "-service/")
;;     (setq-local tab-width 4))
;;
;;   ;; Project names (quoted, since option values are evaluated).
;;   (path-locals gizmo
;;     :project '("gizmo" "sprocket")
;;     (setq-local indent-tabs-mode nil))
;;
;;   ;; Base layer: runs before every rule with the default priority.
;;   (path-locals home-defaults
;;     :path (rx bos path-locals-home)
;;     :priority -10
;;     (setq-local sentence-end-double-space nil))
;;
;;   ;; Per-path mode hook: rules run whenever a major mode is set, so
;;   ;; this enables Go template support only in Helm chart YAML, in
;;   ;; any chart below ~/src/example.org/.
;;   (path-locals helm-templates
;;     :path (rx bos path-locals-home "src/example.org/"
;;               (* nonl) "/chart/templates/")
;;     (when (derived-mode-p 'yaml-ts-mode 'yaml-mode)
;;       (go-template-helper-mode 1)))
;;
;; `path-locals-doctor' shows the rules matching the current buffer,
;; `path-locals-apply' re-runs them, and `path-locals-reset' clears
;; all rules before reloading the configuration.

;;; Code:

(eval-when-compile (require 'cl-lib))

(defvar path-locals-list nil
  "List of configured path-locals rules.")

(cl-defstruct (path-locals-rule (:constructor path-locals-rule-create))
  name      ; Symbol identifying the rule
  path      ; Regexp matched against the expanded path, or nil
  project   ; List of project names, or nil
  body         ; Function to run when matched
  priority     ; Manual ordering group, default 0
  specificity) ; Length of path, or 1000 (orders rules within a group)

(defun path-locals--register (name path project priority body)
  "Register rule NAME matching PATH or PROJECT, running BODY.
PRIORITY orders the rule before specificity; nil means 0."
  (let ((rule (path-locals-rule-create :name name :path path :project project
                                   :body body :priority (or priority 0)
                                   :specificity (if path (length path) 1000))))
    ;; Replace the rule with the same name if re-evaluating
    (setq path-locals-list
          (seq-remove (lambda (r) (eq (path-locals-rule-name r) name)) path-locals-list))
    (push rule path-locals-list)
    ;; Sort by priority, then specificity (shorter paths first so deeper
    ;; paths inherit and override)
    (setq path-locals-list
          (sort path-locals-list
                :key (lambda (r) (list (path-locals-rule-priority r)
                                       (path-locals-rule-specificity r)))))))

(defun path-locals--project-root ()
  "Return the project root of the current buffer, or nil."
  (let ((proj (or (and (fboundp 'project-current) (project-current))
                  (and (fboundp 'projectile-project-p) (projectile-project-p)))))
    (cond
     ;; ( (and (fboundp 'project-root) proj) (project-root proj))
     ((and (fboundp 'projectile-project-root) proj) (projectile-project-root))
     (t nil))))

(defun path-locals--match-p (rule path proj-name)
  "Return non-nil if RULE matches the expanded PATH or PROJ-NAME."
  (let ((pat (path-locals-rule-path rule))
        (names (path-locals-rule-project rule)))
    (or (and pat (string-match-p pat path))
        ;; List of project names
        (and names
             (member (downcase proj-name) (mapcar #'downcase names))))))

(defun path-locals--matching (path proj-root)
  "Return the rules matching PATH or PROJ-ROOT, in application order."
  (let ((path (expand-file-name path))
        (proj-name (if proj-root
                       (file-name-nondirectory (directory-file-name proj-root))
                     "")))
    (seq-filter (lambda (rule) (path-locals--match-p rule path proj-name))
                path-locals-list)))

(defvar-local path-locals--applied nil
  "Non-nil once rules have run since the last major mode change.
Changing the major mode kills this variable again.")

(defun path-locals-apply ()
  "Apply matching path-locals rules to the current buffer."
  (interactive)
  (setq path-locals--applied t)
  (when-let* ((file-or-dir (or (buffer-file-name) default-directory)))
    (let ((proj-root (path-locals--project-root)))
      (dolist (rule (path-locals--matching file-or-dir proj-root))
        ;; Report a failing rule and keep applying the rest; `debug'
        ;; still enters the debugger when `debug-on-error' is set.
        (condition-case err
            (funcall (path-locals-rule-body rule) proj-root file-or-dir)
          ((debug error)
           (message "path-locals %s: %s"
                    (path-locals-rule-name rule) (error-message-string err))))))))

(defun path-locals-doctor ()
  "Show which path-locals rules match the current buffer."
  (interactive)
  (let* ((path (or (buffer-file-name) default-directory))
         (proj-root (path-locals--project-root))
         (rules (path-locals--matching path proj-root)))
    (with-help-window "*path-locals-doctor*"
      (princ (format "Path:    %s\n" (expand-file-name path)))
      (princ (format "Project: %s\n\n" (or proj-root "none")))
      (if (null rules)
          (princ (format "None of the %d rules match.\n" (length path-locals-list)))
        (princ (format "%d of %d rules match, applied in this order:\n\n"
                       (length rules) (length path-locals-list)))
        (dolist (rule rules)
          (princ (format "  %s (priority %d, specificity %d)\n"
                         (path-locals-rule-name rule) (path-locals-rule-priority rule)
                         (path-locals-rule-specificity rule)))
          (when-let* ((pat (path-locals-rule-path rule)))
            (princ (format "      :path %S\n" pat)))
          (when-let* ((names (path-locals-rule-project rule)))
            (princ (format "      :project %S\n" names))))))))

(defun path-locals-reset ()
  "Remove all registered path-locals rules.
Reload the configuration afterwards to register the current rules."
  (interactive)
  (setq path-locals-list nil)
  (message "path-locals: all rules removed"))

;; Match the expanded home directory, e.g. "/home/user/".  The
;; expansion runs when the enclosing `rx' form is evaluated.
(rx-define path-locals-home
  (literal (expand-file-name "~/")))

;; Match directory NAME, optionally followed by a @<worktree> suffix.
(rx-define path-locals-worktree (name)
  (seq name (? "@" (+ (not "/"))) "/"))

(defmacro path-locals (name &rest args)
  "Define path-locals rule NAME.
ARGS starts with keyword options, followed by the body:

  :path REGEXP     Regexp matched against the expanded file path.
  :project NAMES   List of project names.
  :priority N      Ordering group, default 0; lower runs first.

At least one of :path and :project is required; the rule matches
if either does.
Option values are evaluated.  The body receives `root' (project
root) and `path' (current buffer path) as local variables.
Re-evaluating a rule with the same NAME replaces it."
  (declare (indent 1))
  (unless (symbolp name)
    (error "path-locals: NAME must be a symbol, got %S" name))
  (let (opts)
    (while (keywordp (car args))
      (let ((kw (pop args)))
        (unless (memq kw '(:path :project :priority))
          (error "path-locals %s: unknown option %S" name kw))
        (setq opts (plist-put opts kw (pop args)))))
    (unless (or (plist-member opts :path) (plist-member opts :project))
      (error "path-locals %s: needs :path or :project" name))
    `(path-locals--register
      ',name
      ,(plist-get opts :path)
      ,(plist-get opts :project)
      ,(plist-get opts :priority)
      (lambda (root path)
        (ignore root path)
        ,@args))))

(defun path-locals--after-change-major-mode ()
  "Apply path-locals rules in file-visiting and dired buffers.
Skips `fundamental-mode', which `normal-mode' sets briefly before
choosing the real mode; `path-locals--find-file' covers files that
stay in it."
  (when (and (or buffer-file-name (derived-mode-p 'dired-mode))
             (not (eq major-mode 'fundamental-mode)))
    (path-locals-apply)))

(defun path-locals--find-file ()
  "Apply path-locals rules unless a major mode change already did."
  (unless path-locals--applied
    (path-locals-apply)))

;; Run whenever a major mode is set, both when a file is opened and when
;; the mode is changed by hand, so rules can act like mode hooks
(add-hook 'after-change-major-mode-hook #'path-locals--after-change-major-mode)
(add-hook 'find-file-hook #'path-locals--find-file)

(provide 'path-locals)

;;; path-locals.el ends here
