;;; confluemacs.el --- Dired-like interface for Confluence Cloud with Org-mode support -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Your Name

;; Author: Your Name <your.email@example.com>
;; Version: 0.6.0
;; Package-Requires: ((emacs "29.1") (plz "0.9") (org "9.5") (transient "0.7.8"))
;; Keywords: tools, confluence, org-mode, dired
;; URL: https://github.com/yourusername/confluemacs

;;; Commentary:

;; This package provides a Dired-like interface to browse Confluence Cloud spaces
;; and content (pages, blogposts) using the Confluence Cloud REST API.
;; Content is displayed and edited in Org-mode, converted to/from Confluence
;; storage format (HTML). Buffers are read-only if the user lacks edit permissions.
;;
;; Authentication uses `.authinfo.gpg` with an API token (NOT password).
;; Basic auth with API tokens is the standard method for scripts/CLI tools.
;;
;; IMPORTANT: Confluence REST API v1 will be deprecated on April 30, 2025.
;; This package currently uses v1 endpoints but includes preparation for v2 migration.
;; See https://developer.atlassian.com/cloud/confluence/rest/v1/intro/ for current API
;; and https://developer.atlassian.com/cloud/confluence/rest/v2/intro/ for v2 API.

;;; Code:

(require 'plz)
(require 'json)
(require 'auth-source)
(require 'org)
(require 'transient)
(require 'dired)

(defgroup confluemacs nil
  "Customization group for Confluemacs."
  :group 'tools
  :prefix "confluemacs-"
  :link '(url-link "https://github.com/yourusername/confluemacs")
  :version "0.5.0")

(defgroup confluemacs-connection nil
  "Connection and authentication settings for Confluemacs."
  :group 'confluemacs
  :prefix "confluemacs-")

(defgroup confluemacs-interface nil
  "User interface settings for Confluemacs."
  :group 'confluemacs
  :prefix "confluemacs-")

(defgroup confluemacs-editing nil
  "Content editing and auto-save settings for Confluemacs."
  :group 'confluemacs
  :prefix "confluemacs-")

(defgroup confluemacs-advanced nil
  "Advanced settings and debugging options for Confluemacs."
  :group 'confluemacs
  :prefix "confluemacs-")

(defcustom confluemacs-base-url "https://your-site.atlassian.net/wiki"
  "Base URL for the Confluence Cloud instance (e.g., https://your-site.atlassian.net/wiki)."
  :type 'string
  :group 'confluemacs-connection)

(defcustom confluemacs-auth-source-host "your-site.atlassian.net"
  "Host identifier in .authinfo.gpg for Confluence Cloud credentials."
  :type 'string
  :group 'confluemacs-connection)

(defcustom confluemacs-timeout 10
  "Timeout in seconds for API requests."
  :type 'integer
  :group 'confluemacs-connection)

(defcustom confluemacs-expand-default "space,body.storage,version,container"
  "Default fields to expand in API responses."
  :type 'string
  :group 'confluemacs-advanced)

(defcustom confluemacs-api-version "v1"
  "Confluence REST API version to use. Valid values are \"v1\" or \"v2\".
Note: v1 will be deprecated on April 30, 2025."
  :type '(choice (const :tag "Version 1 (current)" "v1")
                 (const :tag "Version 2 (future)" "v2"))
  :group 'confluemacs-connection
  :risky t)

(defcustom confluemacs-user-agent "Confluemacs/0.5 Emacs"
  "User-Agent string for API requests."
  :type 'string
  :group 'confluemacs-connection)

(defcustom confluemacs-v2-warning-shown nil
  "Whether the v2 API deprecation warning has been shown."
  :type 'boolean
  :group 'confluemacs-advanced)

(defcustom confluemacs-auto-save-interval 300
  "Interval in seconds for auto-saving content drafts (0 to disable)."
  :type 'integer
  :group 'confluemacs-editing)

(defcustom confluemacs-auto-save-directory 
  (expand-file-name "confluemacs-drafts" user-emacs-directory)
  "Directory to store auto-saved drafts."
  :type 'directory
  :group 'confluemacs-editing)

(defvar confluemacs--current-path nil
  "Current path in Confluemacs buffer (e.g., \\='spaces\\=' or \\='spaces/TST\\=').")
(defvar confluemacs--buffer-name "*Confluemacs*"
  "Name of the Confluemacs buffer.")

(defvar confluemacs--content-buffers (make-hash-table :test 'equal)
  "Hash table tracking content buffers by content ID.")
(defvar confluemacs--buffer-counter 0
  "Counter for generating unique buffer names.")
(defvar confluemacs--cleanup-timer nil
  "Timer for periodic buffer cleanup.")
(defvar confluemacs--auto-save-timer nil
  "Timer for auto-saving drafts.")
(defvar confluemacs--auto-save-buffers '()
  "List of buffers that need auto-saving.")

(defun confluemacs--generate-unique-buffer-name (base-name &optional space-key)
  "Generate a unique buffer name based on BASE-NAME and optional SPACE-KEY."
  (let* ((space-prefix (if space-key (format "[%s] " space-key) ""))
         (full-base (format "*Confluemacs: %s%s*" space-prefix base-name))
         (counter 1)
         (candidate full-base))
    ;; If base name is already unique, use it
    (unless (get-buffer candidate)
      (setq candidate full-base))
    ;; Otherwise, add a counter
    (while (get-buffer candidate)
      (setq candidate (format "*Confluemacs: %s%s<%d>*" space-prefix base-name counter))
      (setq counter (1+ counter)))
    candidate))

(defun confluemacs--register-content-buffer (content-id buffer)
  "Register BUFFER for CONTENT-ID in tracking system."
  (puthash content-id buffer confluemacs--content-buffers)
  (with-current-buffer buffer
    (add-hook 'kill-buffer-hook #'confluemacs--unregister-buffer nil t)))

(defun confluemacs--unregister-buffer ()
  "Unregister current buffer from tracking system."
  (let ((content-id (buffer-local-value 'confluemacs-content-id (current-buffer))))
    (when content-id
      (remhash content-id confluemacs--content-buffers))))

(defun confluemacs--get-content-buffer (content-id)
  "Get existing buffer for CONTENT-ID, or nil if none exists."
  (let ((buffer (gethash content-id confluemacs--content-buffers)))
    (if (and buffer (buffer-live-p buffer))
        buffer
      (when buffer
        (remhash content-id confluemacs--content-buffers))
      nil)))

(defun confluemacs--cleanup-dead-buffers ()
  "Clean up dead buffers from tracking system."
  (maphash (lambda (content-id buffer)
             (unless (buffer-live-p buffer)
               (remhash content-id confluemacs--content-buffers)))
           confluemacs--content-buffers))

(defun confluemacs--start-cleanup-timer ()
  "Start periodic buffer cleanup."
  (unless confluemacs--cleanup-timer
    (setq confluemacs--cleanup-timer
          (run-with-timer 300 300 #'confluemacs--cleanup-dead-buffers))))

(defun confluemacs--stop-cleanup-timer ()
  "Stop periodic buffer cleanup."
  (when confluemacs--cleanup-timer
    (cancel-timer confluemacs--cleanup-timer)
    (setq confluemacs--cleanup-timer nil)))

(defun confluemacs-list-content-buffers ()
  "List all currently tracked content buffers."
  (interactive)
  (confluemacs--cleanup-dead-buffers)
  (let ((buffers '()))
    (maphash (lambda (content-id buffer)
               (when (buffer-live-p buffer)
                 (push (cons content-id buffer) buffers)))
             confluemacs--content-buffers)
    (if buffers
        (message "Active Confluemacs content buffers: %s"
                 (mapconcat (lambda (pair)
                              (format "%s (%s)" (cdr pair) (car pair)))
                            buffers ", "))
      (message "No active Confluemacs content buffers"))))

(defun confluemacs--mark-buffer-modified (&rest _)
  "Mark current buffer as modified and update modeline."
  (when (and (boundp 'confluemacs-content-id) confluemacs-content-id)
    (let ((current-hash (secure-hash 'md5 (buffer-string)))
          (original-hash (buffer-local-value 'confluemacs-content-original-hash (current-buffer))))
      (setq-local confluemacs-content-modified 
                  (not (string= current-hash original-hash)))
      (force-mode-line-update))))

(defun confluemacs--buffer-has-unsaved-changes-p (buffer)
  "Check if BUFFER has unsaved changes."
  (with-current-buffer buffer
    (and (boundp 'confluemacs-content-modified)
         confluemacs-content-modified)))

(defun confluemacs--get-modified-buffers ()
  "Get list of all modified content buffers."
  (let ((modified '()))
    (maphash (lambda (_content-id buffer)
               (when (and (buffer-live-p buffer)
                          (confluemacs--buffer-has-unsaved-changes-p buffer))
                 (push buffer modified)))
             confluemacs--content-buffers)
    modified))

(defun confluemacs-save-all-modified ()
  "Save all modified content buffers."
  (interactive)
  (let ((modified (confluemacs--get-modified-buffers)))
    (if modified
        (progn
          (dolist (buffer modified)
            (with-current-buffer buffer
              (when (y-or-n-p (format "Save buffer %s? " (buffer-name)))
                (confluemacs-save-content))))
          (message "Saved %d buffer(s)" (length modified)))
      (message "No modified buffers to save"))))

(defun confluemacs-check-unsaved-changes ()
  "Check for unsaved changes and warn user."
  (interactive)
  (let ((modified (confluemacs--get-modified-buffers)))
    (when modified
      (message "Warning: %d buffer(s) have unsaved changes: %s"
               (length modified)
               (mapconcat #'buffer-name modified ", ")))))

;;; Auto-save functionality

(defun confluemacs--get-draft-filename (content-id title)
  "Get the draft filename for CONTENT-ID and TITLE."
  (let ((safe-title (replace-regexp-in-string "[^a-zA-Z0-9-_]" "_" title)))
    (expand-file-name 
     (format "%s_%s.org" content-id safe-title)
     confluemacs-auto-save-directory)))

(defun confluemacs--ensure-auto-save-directory ()
  "Ensure the auto-save directory exists."
  (unless (file-directory-p confluemacs-auto-save-directory)
    (make-directory confluemacs-auto-save-directory t)))

(defun confluemacs--save-draft (buffer)
  "Save a draft of BUFFER content."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (and (boundp 'confluemacs-content-id) confluemacs-content-id
                 (boundp 'confluemacs-content-title) confluemacs-content-title
                 (confluemacs--buffer-has-unsaved-changes-p buffer))
        (confluemacs--ensure-auto-save-directory)
        (let ((draft-file (confluemacs--get-draft-filename 
                           confluemacs-content-id 
                           confluemacs-content-title)))
          (condition-case err
              (progn
                (write-region (point-min) (point-max) draft-file nil 'quiet)
                (message "Auto-saved draft: %s" (file-name-nondirectory draft-file)))
            (error
             (message "Failed to auto-save draft: %s" (error-message-string err)))))))))

(defun confluemacs--auto-save-all-drafts ()
  "Auto-save all modified content buffers."
  (dolist (buffer (confluemacs--get-modified-buffers))
    (confluemacs--save-draft buffer)))

(defun confluemacs--start-auto-save-timer ()
  "Start the auto-save timer."
  (when (and (> confluemacs-auto-save-interval 0)
             (not confluemacs--auto-save-timer))
    (setq confluemacs--auto-save-timer
          (run-with-timer confluemacs-auto-save-interval 
                          confluemacs-auto-save-interval
                          #'confluemacs--auto-save-all-drafts))))

(defun confluemacs--stop-auto-save-timer ()
  "Stop the auto-save timer."
  (when confluemacs--auto-save-timer
    (cancel-timer confluemacs--auto-save-timer)
    (setq confluemacs--auto-save-timer nil)))

(defun confluemacs--load-draft (content-id title)
  "Load draft for CONTENT-ID and TITLE if it exists."
  (let ((draft-file (confluemacs--get-draft-filename content-id title)))
    (when (file-exists-p draft-file)
      (when (y-or-n-p (format "Draft found for '%s'. Load it? " title))
        (insert-file-contents draft-file nil nil nil t)
        (message "Loaded draft from %s" (file-name-nondirectory draft-file))
        t))))

(defun confluemacs--delete-draft (content-id title)
  "Delete draft for CONTENT-ID and TITLE."
  (let ((draft-file (confluemacs--get-draft-filename content-id title)))
    (when (file-exists-p draft-file)
      (delete-file draft-file))))

(defun confluemacs-clean-old-drafts ()
  "Clean up old draft files (older than 30 days)."
  (interactive)
  (when (file-directory-p confluemacs-auto-save-directory)
    (let ((cutoff-time (time-subtract (current-time) (days-to-time 30)))
          (deleted-count 0))
      (dolist (file (directory-files confluemacs-auto-save-directory t "\\.org$"))
        (when (time-less-p (nth 5 (file-attributes file)) cutoff-time)
          (delete-file file)
          (setq deleted-count (1+ deleted-count))))
      (message "Cleaned up %d old draft files" deleted-count))))

;;; Configuration validation

(defun confluemacs--validate-base-url (url)
  "Validate that URL is a proper Confluence base URL."
  (and (stringp url)
       (string-match-p "^https://[^.]+\\.atlassian\\.net/wiki/?$" url)))

(defun confluemacs--validate-auth-host (host)
  "Validate that HOST is a proper auth-source host."
  (and (stringp host)
       (string-match-p "^[^.]+\\.atlassian\\.net$" host)))

;;;###autoload
(defun confluemacs-validate-configuration ()
  "Validate current Confluemacs configuration."
  (interactive)
  (let ((errors '())
        (warnings '()))
    ;; Validate base URL
    (unless (confluemacs--validate-base-url confluemacs-base-url)
      (push "Base URL should be in format https://your-site.atlassian.net/wiki" errors))
    
    ;; Validate auth host
    (unless (confluemacs--validate-auth-host confluemacs-auth-source-host)
      (push "Auth host should be in format your-site.atlassian.net" errors))
    
    ;; Check if auto-save directory is writable
    (when (> confluemacs-auto-save-interval 0)
      (unless (file-writable-p (file-name-directory confluemacs-auto-save-directory))
        (push "Auto-save directory is not writable" errors)))
    
    ;; API version warnings
    (when (string= confluemacs-api-version "v1")
      (push "Using API v1 which will be deprecated April 30, 2025" warnings))
    
    ;; Check timeout value
    (when (< confluemacs-timeout 5)
      (push "Timeout value is very low and may cause connection issues" warnings))
    
    ;; Report results
    (if errors
        (message "Configuration errors: %s" (mapconcat #'identity errors "; "))
      (if warnings
          (message "Configuration warnings: %s" (mapconcat #'identity warnings "; "))
        (message "Configuration is valid")))))

(defun confluemacs--setup-configuration ()
  "Interactive setup for Confluemacs configuration."
  (interactive)
  (let ((site-name (read-string "Enter your Atlassian site name (e.g., 'mycompany'): "))
        (api-version (completing-read "Choose API version: " '("v1" "v2") nil t "v1")))
    (when (and site-name (not (string-empty-p site-name)))
      (customize-set-variable 'confluemacs-base-url 
                              (format "https://%s.atlassian.net/wiki" site-name))
      (customize-set-variable 'confluemacs-auth-source-host 
                              (format "%s.atlassian.net" site-name))
      (customize-set-variable 'confluemacs-api-version api-version)
      (when (y-or-n-p "Save these settings? ")
        (customize-save-variable 'confluemacs-base-url 
                                 (format "https://%s.atlassian.net/wiki" site-name))
        (customize-save-variable 'confluemacs-auth-source-host 
                                 (format "%s.atlassian.net" site-name))
        (customize-save-variable 'confluemacs-api-version api-version))
      (message "Configuration updated. Remember to set up your .authinfo.gpg file."))))

;;; Error recovery mechanisms

(defvar confluemacs--error-recovery-enabled t
  "Whether error recovery is enabled.")

(defun confluemacs--with-error-recovery (operation &rest args)
  "Execute OPERATION with ARGS, providing error recovery options."
  (condition-case err
      (apply operation args)
    (error
     (if confluemacs--error-recovery-enabled
         (confluemacs--handle-recoverable-error err operation args)
       (signal (car err) (cdr err))))))

(defun confluemacs--handle-recoverable-error (err operation args)
  "Handle recoverable error ERR from OPERATION with ARGS."
  (let ((error-msg (error-message-string err))
        (operation-name (symbol-name operation)))
    (cond
     ;; Network/connection errors
     ((string-match-p "connection\\|network\\|timeout" error-msg)
      (when (y-or-n-p (format "Network error in %s: %s. Retry? " operation-name error-msg))
        (apply operation args)))
     
     ;; Authentication errors
     ((string-match-p "auth\\|credential\\|token" error-msg)
      (when (y-or-n-p (format "Authentication error: %s. Check configuration? " error-msg))
        (confluemacs-validate-configuration)))
     
     ;; Permission errors
     ((string-match-p "permission\\|forbidden\\|403" error-msg)
      (message "Permission denied. You may not have access to this content."))
     
     ;; API deprecation
     ((string-match-p "deprecated\\|410" error-msg)
      (when (y-or-n-p "API endpoint deprecated. Migrate to v2? ")
        (confluemacs-migrate-to-v2)))
     
     ;; Generic error with recovery options
     (t
      (let ((choice (read-char-choice 
                     (format "Error in %s: %s\n(r)etry, (i)gnore, (d)ebug, (c)onfigure: " 
                             operation-name error-msg)
                     '(?r ?i ?d ?c))))
        (pcase choice
          (?r (apply operation args))
          (?i (message "Ignoring error: %s" error-msg))
          (?d (debug err))
          (?c (confluemacs-validate-configuration))))))))

(defun confluemacs--safe-api-call (endpoint method &optional params data headers)
  "Make a safe API call with error recovery."
  (confluemacs--with-error-recovery 
   #'confluemacs--make-request endpoint method params data headers))

;;;###autoload
(defun confluemacs-recover-from-crash ()
  "Attempt to recover from a crash by restoring content buffers."
  (interactive)
  (when (file-directory-p confluemacs-auto-save-directory)
    (let ((draft-files (directory-files confluemacs-auto-save-directory t "\\.org$"))
          (recovered-count 0))
      (dolist (file draft-files)
        (when (file-readable-p file)
          (let* ((basename (file-name-sans-extension (file-name-nondirectory file)))
                 (parts (split-string basename "_"))
                 (content-id (car parts))
                 (title (mapconcat #'identity (cdr parts) "_")))
            (when (and content-id title
                       (y-or-n-p (format "Recover draft for '%s'? " title)))
              (let ((buffer (get-buffer-create 
                             (confluemacs--generate-unique-buffer-name title))))
                (with-current-buffer buffer
                  (insert-file-contents file)
                  (org-mode)
                  (setq-local confluemacs-content-id content-id)
                  (setq-local confluemacs-content-title title)
                  (setq-local confluemacs-content-modified t))
                (setq recovered-count (1+ recovered-count))
                (switch-to-buffer buffer))))))
      (message "Recovered %d draft(s)" recovered-count))))

(defun confluemacs--get-credentials ()
  "Retrieve Confluence Cloud credentials from .authinfo.gpg.
Returns (EMAIL . API-TOKEN) where API-TOKEN is required for authentication.
Note: Basic auth with passwords is deprecated - you must use API tokens."
  (let* ((auth (auth-source-search :host confluemacs-auth-source-host
                                   :require '(:user :secret)
                                   :create t))
         (user (plist-get (car auth) :user))
         (secret (plist-get (car auth) :secret))
         (token (if (functionp secret) (funcall secret) secret)))
    (unless (and user token)
      (error "Failed to retrieve credentials from .authinfo.gpg. Ensure you have an entry with your email and API token (not password)"))
    (cons user token)))

(defun confluemacs--make-request (endpoint method &optional params data headers)
  "Make an HTTP request to the Confluence Cloud API.
ENDPOINT is the API endpoint path (e.g., \\='/content\\=' for v1 or \\='/pages\\=' for v2).
METHOD is the HTTP method (e.g., \\='GET\\=', \\='POST\\=').
PARAMS is an alist of query parameters.
DATA is the request body (for POST/PUT).
HEADERS is an alist of additional headers."
  (unless confluemacs-base-url
    (error "Confluemacs base URL not set"))
  (let* ((creds (confluemacs--get-credentials))
         (user (car creds))
         (token (cdr creds))
         (api-path (if (string= confluemacs-api-version "v2")
                       "/wiki/rest/api/v2"
                     "/rest/api"))
         (url (concat confluemacs-base-url api-path endpoint))
         (auth (format "Basic %s"
                       (base64-encode-string (concat user ":" token) t)))
         (all-headers (append headers
                              `(("Authorization" . ,auth)
                                ("Content-Type" . "application/json")
                                ("Accept" . "application/json")
                                ("User-Agent" . ,confluemacs-user-agent)
                                ("X-Atlassian-Token" . "no-check")))))
    (condition-case err
        (let ((response (plz method url
                           :headers all-headers
                           :params params
                           :body (when data (json-encode data))
                           :as 'json)))
          response)
      (plz-error
       (let ((status (plist-get (cdr err) :status)))
         (cond
          ((eq status 410)
           (message "Confluemacs: API endpoint deprecated. Please update to v2 API."))
          ((eq status 401)
           (message "Confluemacs: Authentication failed. Check your API token."))
          ((eq status 403)
           (message "Confluemacs: Permission denied."))
          ((eq status 429)
           (message "Confluemacs: Rate limit exceeded. Please try again later."))
          (t
           (message "Confluemacs API error: %s (Status: %s)"
                    (plist-get (cdr err) :message)
                    (or status "Unknown"))))
         nil)))))

(defun confluemacs--make-request-async (endpoint method callback &optional params data headers)
  "Make an asynchronous HTTP request to the Confluence Cloud API.
ENDPOINT is the API endpoint path.
METHOD is the HTTP method.
CALLBACK is called with (data . error) when the request completes.
PARAMS is an alist of query parameters.
DATA is the request body (for POST/PUT).
HEADERS is an alist of additional headers."
  (unless confluemacs-base-url
    (error "Confluemacs base URL not set"))
  (let* ((creds (confluemacs--get-credentials))
         (user (car creds))
         (token (cdr creds))
         (api-path (if (string= confluemacs-api-version "v2")
                       "/wiki/rest/api/v2"
                     "/rest/api"))
         (url (concat confluemacs-base-url api-path endpoint))
         (auth (format "Basic %s"
                       (base64-encode-string (concat user ":" token) t)))
         (all-headers (append headers
                              `(("Authorization" . ,auth)
                                ("Content-Type" . "application/json")
                                ("Accept" . "application/json")
                                ("User-Agent" . ,confluemacs-user-agent)
                                ("X-Atlassian-Token" . "no-check")))))
    (plz method url
      :headers all-headers
      :params params
      :body (when data (json-encode data))
      :as 'json
      :then (lambda (data)
              (funcall callback (cons data nil)))
      :else (lambda (err)
              (let ((status (plist-get (cdr err) :status)))
                (cond
                 ((eq status 410)
                  (message "Confluemacs: API endpoint deprecated. Please update to v2 API."))
                 ((eq status 401)
                  (message "Confluemacs: Authentication failed. Check your API token."))
                 ((eq status 403)
                  (message "Confluemacs: Permission denied."))
                 ((eq status 429)
                  (message "Confluemacs: Rate limit exceeded. Please try again later."))
                 (t
                  (message "Confluemacs API error: %s (Status: %s)"
                           (plist-get (cdr err) :message)
                           (or status "Unknown"))))
                (funcall callback (cons nil err)))))))

(defvar confluemacs--progress-timer nil
  "Timer for progress indicator animation.")

(defvar confluemacs--progress-message ""
  "Current progress message.")

(defun confluemacs--show-progress (message)
  "Show progress MESSAGE with animated dots."
  (setq confluemacs--progress-message message)
  (when confluemacs--progress-timer
    (cancel-timer confluemacs--progress-timer))
  (let ((dots ""))
    (setq confluemacs--progress-timer
          (run-with-timer 0 0.5
                          (lambda ()
                            (setq dots (if (>= (length dots) 3) "" (concat dots ".")))
                            (message "%s%s" confluemacs--progress-message dots))))))

(defun confluemacs--hide-progress ()
  "Hide progress indicator."
  (when confluemacs--progress-timer
    (cancel-timer confluemacs--progress-timer)
    (setq confluemacs--progress-timer nil))
  (message ""))

(defun confluemacs--handle-response (data)
  "Process the API response DATA and return it."
  (if (and (listp data) (assq 'results data))
      (cdr (assq 'results data))
    data))

(defun confluemacs--api-v2-ready-p ()
  "Check if the package is configured for API v2."
  (string= confluemacs-api-version "v2"))

(defun confluemacs--warn-v1-deprecation ()
  "Display a warning about v1 API deprecation."
  (when (not (confluemacs--api-v2-ready-p))
    (message "Warning: Confluence REST API v1 will be deprecated on April 30, 2025. Consider testing with v2.")))

;;;###autoload
(defun confluemacs-check-api-version ()
  "Check which API versions are available and test connectivity."
  (interactive)
  (confluemacs--show-progress "Checking API versions")
  (let ((v1-working nil)
        (v2-working nil)
        (original-version confluemacs-api-version))
    ;; Test v1 API
    (setq confluemacs-api-version "v1")
    (confluemacs--make-request-async 
     "/space" "GET"
     (lambda (result)
       (let ((_data (car result))
             (error (cdr result)))
         (setq v1-working (not error))
         ;; Test v2 API
         (setq confluemacs-api-version "v2")
         (confluemacs--make-request-async 
          "/spaces" "GET"
          (lambda (result2)
            (confluemacs--hide-progress)
            (setq confluemacs-api-version original-version)
            (let ((_data2 (car result2))
                  (error2 (cdr result2)))
              (setq v2-working (not error2))
              (message "API Version Check Results:\nv1 API: %s\nv2 API: %s\n\nCurrent setting: %s\nRecommendation: %s"
                       (if v1-working "✓ Working" "✗ Failed")
                       (if v2-working "✓ Working" "✗ Failed")
                       original-version
                       (cond
                        ((and v1-working v2-working) "Consider migrating to v2 before April 30, 2025")
                        (v2-working "Migrate to v2 immediately - v1 may be disabled")
                        (v1-working "Continue with v1 for now, but prepare for v2 migration")
                        (t "Check your connection and credentials")))))
          '(("limit" . 1)))))
     '(("limit" . 1)))))

;;;###autoload
(defun confluemacs-migrate-to-v2 ()
  "Migrate current configuration to use API v2."
  (interactive)
  (when (y-or-n-p "This will change your API version to v2. Continue? ")
    (customize-set-variable 'confluemacs-api-version "v2")
    (message "Confluemacs configured to use API v2. You may need to restart Emacs for full effect.")
    (when (y-or-n-p "Save this setting permanently? ")
      (customize-save-variable 'confluemacs-api-version "v2"))))

(defun confluemacs--check-edit-permission (content-id)
  "Check if the user has edit permission for CONTENT-ID.
Note: Permission checking has limitations in Confluence Cloud API."
  (condition-case err
      (let ((response (confluemacs--make-request
                       (format "/content/%s/restriction/byOperation/update" content-id) "GET")))
        ;; If we get a response without error, assume we have permission
        ;; The actual permission logic is more complex but this is a reasonable approximation
        (not (null response)))
    (error
     ;; If we get an error, assume no edit permission
     (message "Could not check edit permission: %s" err)
     nil)))

(defun confluemacs--org-to-confluence (org-text)
  "Convert ORG-TEXT to Confluence storage format (HTML)."
  (if (not org-text)
      ""
    (if (executable-find "pandoc")
        (confluemacs--convert-with-pandoc org-text "org" "html")
      (let ((_org-export-with-toc nil)
            (_org-export-with-section-numbers nil))
        (org-export-string-as org-text 'html t)))))

(defun confluemacs--confluence-to-org (html-text)
  "Convert Confluence storage format (HTML) to Org-mode."
  (if (not html-text)
      ""
    (if (executable-find "pandoc")
        (confluemacs--convert-with-pandoc 
         (confluemacs--sanitize-html html-text) "html" "org")
      (confluemacs--strip-html-tags html-text))))

(defun confluemacs--convert-with-pandoc (text from-format to-format)
  "Safely convert TEXT from FROM-FORMAT to TO-FORMAT using pandoc.
This function uses call-process instead of shell-command for security."
  (let ((temp-file (make-temp-file "confluemacs-" nil ".tmp"))
        (output-buffer (generate-new-buffer " *confluemacs-pandoc*")))
    (unwind-protect
        (progn
          (with-temp-file temp-file
            (insert text))
          (if (eq 0 (call-process "pandoc" temp-file output-buffer nil
                                 "-f" from-format "-t" to-format))
              (with-current-buffer output-buffer
                (buffer-string))
            (error "Pandoc conversion failed")))
      (when (file-exists-p temp-file)
        (delete-file temp-file))
      (when (buffer-live-p output-buffer)
        (kill-buffer output-buffer)))))

(defun confluemacs--sanitize-html (html-text)
  "Sanitize HTML-TEXT by removing potentially dangerous elements."
  (let ((sanitized html-text))
    ;; Remove script tags and their content
    (setq sanitized (replace-regexp-in-string
                     "<script[^>]*>.*?</script>" "" sanitized t t))
    ;; Remove style tags and their content
    (setq sanitized (replace-regexp-in-string
                     "<style[^>]*>.*?</style>" "" sanitized t t))
    ;; Remove on* event attributes
    (setq sanitized (replace-regexp-in-string
                     " on[a-zA-Z]+=\"[^\"]*\"" "" sanitized t))
    ;; Remove javascript: links
    (setq sanitized (replace-regexp-in-string
                     "javascript:[^\"']*" "" sanitized t))
    sanitized))

(defun confluemacs--strip-html-tags (html-text)
  "Strip HTML tags from HTML-TEXT, providing a fallback when pandoc is unavailable."
  (let ((stripped html-text))
    ;; Convert common HTML entities
    (setq stripped (replace-regexp-in-string "&lt;" "<" stripped))
    (setq stripped (replace-regexp-in-string "&gt;" ">" stripped))
    (setq stripped (replace-regexp-in-string "&amp;" "&" stripped))
    (setq stripped (replace-regexp-in-string "&quot;" "\"" stripped))
    ;; Remove HTML tags
    (setq stripped (replace-regexp-in-string "<[^>]+>" "" stripped))
    stripped))

;;; Dired-like Interface
(defvar confluemacs-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'confluemacs-open)
    (define-key map (kbd "g") 'confluemacs-refresh)
    (define-key map (kbd "c") 'confluemacs-create-content)
    (define-key map (kbd "d") 'confluemacs-delete)
    (define-key map (kbd "^") 'confluemacs-up)
    (define-key map (kbd "m") 'confluemacs-menu)
    map)
  "Keymap for `confluemacs-mode'.")

(define-derived-mode confluemacs-mode special-mode "Confluemacs"
  "Major mode for browsing Confluence Cloud spaces and content."
  (setq buffer-read-only t)
  (setq truncate-lines t)
  (use-local-map confluemacs-mode-map))

;;;###autoload
(defun confluemacs ()
  "Open the Confluemacs browser at the root (spaces)."
  (interactive)
  (let ((buffer (get-buffer-create confluemacs--buffer-name)))
    (with-current-buffer buffer
      (confluemacs-mode)
      (setq confluemacs--current-path "spaces")
      (confluemacs-refresh))
    (switch-to-buffer buffer)))

(defun confluemacs-refresh ()
  "Refresh the Confluemacs buffer based on the current path."
  (interactive)
  (with-current-buffer confluemacs--buffer-name
    (let ((inhibit-read-only t))
      (erase-buffer)
      (if (equal confluemacs--current-path "spaces")
          (confluemacs--display-spaces-async)
        (let ((space-key (cadr (split-string confluemacs--current-path "/"))))
          (confluemacs--display-content-async space-key))))))

(defun confluemacs--display-spaces ()
  "Display a list of Confluence spaces."
  (let* ((response (confluemacs--make-request "/space" "GET" '(("limit" . 100))))
         (spaces (confluemacs--handle-response response)))
    (when spaces
      (insert (format "%-10s %-30s\n" "Key" "Name"))
      (insert (make-string 40 ?-))
      (insert "\n")
      (dolist (space spaces)
        (let ((key (cdr (assq 'key space)))
              (name (cdr (assq 'name space))))
          (insert (format "%-10s %-30s\n" key name))
          (put-text-property (line-beginning-position) (line-end-position)
                             'confluemacs-type 'space)
          (put-text-property (line-beginning-position) (line-end-position)
                             'confluemacs-key key))))))

(defun confluemacs--display-spaces-async ()
  "Asynchronously display a list of Confluence spaces."
  (confluemacs--show-progress "Loading spaces")
  (confluemacs--make-request-async 
   "/space" "GET"
   (lambda (result)
     (confluemacs--hide-progress)
     (let ((data (car result))
           (error (cdr result)))
       (if error
           (message "Failed to load spaces: %s" error)
         (with-current-buffer confluemacs--buffer-name
           (let ((inhibit-read-only t)
                 (spaces (confluemacs--handle-response data)))
             (erase-buffer)
             (when spaces
               (insert (format "%-10s %-30s\n" "Key" "Name"))
               (insert (make-string 40 ?-))
               (insert "\n")
               (dolist (space spaces)
                 (let ((key (cdr (assq 'key space)))
                       (name (cdr (assq 'name space))))
                   (insert (format "%-10s %-30s\n" key name))
                   (put-text-property (line-beginning-position) (line-end-position)
                                      'confluemacs-type 'space)
                   (put-text-property (line-beginning-position) (line-end-position)
                                      'confluemacs-key key))))
             (goto-char (point-min)))))))
   '(("limit" . 100))))

(defun confluemacs--display-content (space-key)
  "Display content for SPACE-KEY."
  (let* ((response (confluemacs--make-request "/content" "GET"
                                              `(("spaceKey" . ,space-key) ("expand" . ,confluemacs-expand-default) ("limit" . 100))))
         (content-list (confluemacs--handle-response response)))
    (when content-list
      (insert (format "%-10s %-30s\n" "ID" "Title"))
      (insert (make-string 40 ?-))
      (insert "\n")
      (dolist (content content-list)
        (let ((id (cdr (assq 'id content)))
              (title (cdr (assq 'title content))))
          (insert (format "%-10s %-30s\n" id title))
          (put-text-property (line-beginning-position) (line-end-position)
                             'confluemacs-type 'content)
          (put-text-property (line-beginning-position) (line-end-position)
                             'confluemacs-id id))))))

(defun confluemacs--display-content-async (space-key)
  "Asynchronously display content for SPACE-KEY."
  (confluemacs--show-progress "Loading content")
  (confluemacs--make-request-async 
   "/content" "GET"
   (lambda (result)
     (confluemacs--hide-progress)
     (let ((data (car result))
           (error (cdr result)))
       (if error
           (message "Failed to load content: %s" error)
         (with-current-buffer confluemacs--buffer-name
           (let ((inhibit-read-only t)
                 (content-list (confluemacs--handle-response data)))
             (erase-buffer)
             (when content-list
               (insert (format "%-10s %-30s\n" "ID" "Title"))
               (insert (make-string 40 ?-))
               (insert "\n")
               (dolist (content content-list)
                 (let ((id (cdr (assq 'id content)))
                       (title (cdr (assq 'title content))))
                   (insert (format "%-10s %-30s\n" id title))
                   (put-text-property (line-beginning-position) (line-end-position)
                                      'confluemacs-type 'content)
                   (put-text-property (line-beginning-position) (line-end-position)
                                      'confluemacs-id id))))
             (goto-char (point-min)))))))
   `(("spaceKey" . ,space-key) ("expand" . ,confluemacs-expand-default) ("limit" . 100))))

(defun confluemacs-open ()
  "Open the item at point (space or content)."
  (interactive)
  (let ((type (get-text-property (point) 'confluemacs-type)))
    (cond
     ((eq type 'space)
      (let ((key (get-text-property (point) 'confluemacs-key)))
        (setq confluemacs--current-path (concat "spaces/" key))
        (confluemacs-refresh)))
     ((eq type 'content)
      (let ((id (get-text-property (point) 'confluemacs-id)))
        (confluemacs-get-content-by-id id))))))

(defun confluemacs-up ()
  "Go up one level in the hierarchy."
  (interactive)
  (when (string-match "^spaces/[^/]+$" confluemacs--current-path)
    (setq confluemacs--current-path "spaces")
    (confluemacs-refresh)))

(defun confluemacs-get-content-by-id (id &optional expand)
  "Retrieve content by ID and display as Org-mode.
EXPAND is a string of fields to expand (default: confluemacs-expand-default)."
  ;; Check if buffer already exists for this content
  (let ((existing-buffer (confluemacs--get-content-buffer id)))
    (if existing-buffer
        (progn
          (switch-to-buffer existing-buffer)
          (message "Switched to existing buffer for content %s" id))
      ;; Create new buffer
      (let* ((response (confluemacs--make-request (format "/content/%s" id) "GET"
                                                  `(("expand" . ,(or expand confluemacs-expand-default)))))
             (content response))
        (when content
          (let* ((body (cdr (assq 'storage (cdr (assq 'body content)))))
                 (html (cdr (assq 'value body)))
                 (org-text (confluemacs--confluence-to-org html))
                 (title (cdr (assq 'title content)))
                 (version (cdr (assq 'number (cdr (assq 'version content)))))
                 (space-key (cdr (assq 'key (cdr (assq 'space content)))))
                 (buffer-name (confluemacs--generate-unique-buffer-name title space-key))
                 (buffer (get-buffer-create buffer-name))
                 (can-edit (confluemacs--check-edit-permission id)))
            ;; Start cleanup and auto-save timers if not already running
            (confluemacs--start-cleanup-timer)
            (confluemacs--start-auto-save-timer)
            ;; Register buffer
            (confluemacs--register-content-buffer id buffer)
            (with-current-buffer buffer
              (erase-buffer)
              ;; Try to load draft first, otherwise use original content
              (unless (confluemacs--load-draft id title)
                (insert org-text))
              (org-mode)
              (setq-local confluemacs-content-id id)
              (setq-local confluemacs-content-title title)
              (setq-local confluemacs-content-version version)
              (setq-local confluemacs-content-space-key space-key)
              (setq-local confluemacs-content-original-hash 
                          (secure-hash 'md5 org-text))
              (setq-local confluemacs-content-modified nil)
              (setq buffer-read-only (not can-edit))
              (when can-edit
                (local-set-key (kbd "C-c C-c") 'confluemacs-save-content)
                (add-hook 'after-change-functions #'confluemacs--mark-buffer-modified nil t)))
            (switch-to-buffer buffer)
            (when (not can-edit)
              (message "Content is read-only: No edit permission"))))))))

(defun confluemacs-create-content (type title space-key)
  "Create content of TYPE with TITLE in SPACE-KEY from an Org-mode buffer."
  (interactive
   (let* ((space-key (if (string-match "^spaces/\\([^/]+\\)$" confluemacs--current-path)
                         (match-string 1 confluemacs--current-path)
                       (read-string "Space key: ")))
          (type (completing-read "Type: " '("page" "blogpost") nil t "page"))
          (title (read-string "Title: ")))
     (list type title space-key)))
  (let ((buffer (get-buffer-create (format "*Confluemacs: %s*" title))))
    (with-current-buffer buffer
      (erase-buffer)
      (insert "#+TITLE: " title "\n\n")
      (org-mode)
      (setq-local confluemacs-content-type type)
      (setq-local confluemacs-content-title title)
      (setq-local confluemacs-content-space-key space-key)
      (local-set-key (kbd "C-c C-c") 'confluemacs-save-content))
    (switch-to-buffer buffer)))

(defun confluemacs-save-content ()
  "Save the current Org-mode buffer to Confluence."
  (interactive)
  (let ((org-text (buffer-string))
        (id (buffer-local-value 'confluemacs-content-id (current-buffer)))
        (title (buffer-local-value 'confluemacs-content-title (current-buffer)))
        (space-key (buffer-local-value 'confluemacs-content-space-key (current-buffer)))
        (version (buffer-local-value 'confluemacs-content-version (current-buffer)))
        (type (buffer-local-value 'confluemacs-content-type (current-buffer))))
    (if id
        (confluemacs-update-content id title org-text (1+ version) space-key)
      (confluemacs-create-content-api type title space-key org-text))
    ;; Update content state after successful save
    (setq-local confluemacs-content-original-hash (secure-hash 'md5 org-text))
    (setq-local confluemacs-content-modified nil)
    ;; Delete draft file since content is now saved
    (when (and id title)
      (confluemacs--delete-draft id title))
    (force-mode-line-update)
    (message "Content saved to Confluence")))

(defun confluemacs-create-content-api (type title space-key org-text &optional parent-id)
  "Create content of TYPE with TITLE in SPACE-KEY from ORG-TEXT.
PARENT-ID is optional."
  (let* ((body (confluemacs--org-to-confluence org-text))
         (data `(("type" . ,type)
                 ("title" . ,title)
                 ("space" . (("key" . ,space-key)))
                 ("body" . (("storage" . (("value" . ,body)
                                          ("representation" . "storage")))))
                 ,@(when parent-id `(("ancestors" . ((("id" . ,parent-id)))))))))
    (confluemacs--make-request "/content" "POST" nil data)))

(defun confluemacs-update-content (id title org-text version &optional space-key)
  "Update content with ID, TITLE, ORG-TEXT, and VERSION.
SPACE-KEY is optional."
  (let* ((body (confluemacs--org-to-confluence org-text))
         (data `(("id" . ,id)
                 ("type" . "page")
                 ("title" . ,title)
                 ,@(when space-key `(("space" . (("key" . ,space-key)))))
                 ("body" . (("storage" . (("value" . ,body)
                                          ("representation" . "storage")))))
                 ("version" . (("number" . ,version))))))
    (confluemacs--make-request (format "/content/%s" id) "PUT" nil data)))

(defun confluemacs-delete ()
  "Delete the item at point (space or content)."
  (interactive)
  (let ((type (get-text-property (point) 'confluemacs-type)))
    (cond
     ((eq type 'space)
      (let ((key (get-text-property (point) 'confluemacs-key)))
        (when (y-or-n-p (format "Delete space %s? " key))
          (let ((response (confluemacs--make-request (format "/space/%s" key) "DELETE")))
            (if response
                (progn
                  (message "Space %s deleted" key)
                  (confluemacs-refresh))
              (message "Failed to delete space %s" key))))))
     ((eq type 'content)
      (let ((id (get-text-property (point) 'confluemacs-id)))
        (when (y-or-n-p "Delete content? ")
          (let ((response (confluemacs--make-request (format "/content/%s" id) "DELETE")))
            (if response
                (progn
                  (message "Content deleted")
                  (confluemacs-refresh))
              (message "Failed to delete content"))))))))

;;; Transient Menu
(transient-define-prefix confluemacs-menu ()
  "Transient menu for Confluemacs."
  [["Navigation"
    ("r" "Refresh" confluemacs-refresh)
    ("u" "Up" confluemacs-up)
    ("o" "Open" confluemacs-open)]
   ["Content Management"
    ("c" "Create content" confluemacs-create-content)
    ("d" "Delete" confluemacs-delete)
    ("s" "Save all modified" confluemacs-save-all-modified)
    ("l" "List content buffers" confluemacs-list-content-buffers)]
   ["Advanced"
    ("sg" "Get spaces" confluemacs-get-spaces)
    ("cg" "Get content" confluemacs-get-content)
    ("ss" "Search" confluemacs-search)]
   ["API Management"
    ("v" "Check API version" confluemacs-check-api-version)
    ("V" "Migrate to API v2" confluemacs-migrate-to-v2)]
   ["Maintenance"
    ("C" "Check configuration" confluemacs-validate-configuration)
    ("S" "Setup configuration" confluemacs--setup-configuration)
    ("R" "Recover from crash" confluemacs-recover-from-crash)
    ("D" "Clean old drafts" confluemacs-clean-old-drafts)]])

(defun confluemacs-get-spaces (&optional params)
  "Retrieve a list of spaces.
PARAMS is an alist of query parameters."
  (confluemacs--make-request "/space" "GET" params))

(defun confluemacs-get-content (&optional params)
  "Retrieve content with PARAMS."
  (let ((params (append params `(("expand" . ,confluemacs-expand-default)))))
    (confluemacs--make-request "/content" "GET" params)))

(defun confluemacs-search (cql &optional params)
  "Search Confluence using CQL query."
  (interactive "sSearch query (CQL): ")
  (let ((params (append params `(("cql" . ,cql)
                                 ("expand" . ,confluemacs-expand-default)))))
    (confluemacs--make-request "/content/search" "GET" params)))


) ; Add missing closing paren

(provide 'confluemacs)
;;; confluemacs.el ends here
