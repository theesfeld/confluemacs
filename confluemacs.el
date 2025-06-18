;;; confluemacs.el --- Dired-like interface for Confluence Cloud with Org-mode support -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Your Name

;; Author: Your Name <your.email@example.com>
;; Version: 0.5
;; Package-Requires: ((emacs "27.1") (request "0.3.3") (org "9.4") (transient "0.7.8"))
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

(require 'request)
(require 'json)
(require 'auth-source)
(require 'org)
(require 'transient)
(require 'dired)

(defgroup confluemacs nil
  "Customization group for Confluemacs."
  :group 'tools
  :prefix "confluemacs-")

(defcustom confluemacs-base-url "https://your-site.atlassian.net/wiki"
  "Base URL for the Confluence Cloud instance (e.g., https://your-site.atlassian.net/wiki)."
  :type 'string
  :group 'confluemacs)

(defcustom confluemacs-auth-source-host "your-site.atlassian.net"
  "Host identifier in .authinfo.gpg for Confluence Cloud credentials."
  :type 'string
  :group 'confluemacs)

(defcustom confluemacs-timeout 10
  "Timeout in seconds for API requests."
  :type 'integer
  :group 'confluemacs)

(defcustom confluemacs-expand-default "space,body.storage,version,container"
  "Default fields to expand in API responses."
  :type 'string
  :group 'confluemacs)

(defcustom confluemacs-api-version "v1"
  "Confluence REST API version to use. Valid values are \"v1\" or \"v2\".
Note: v1 will be deprecated on April 30, 2025."
  :type '(choice (const :tag "Version 1 (current)" "v1")
                 (const :tag "Version 2 (future)" "v2"))
  :group 'confluemacs)

(defcustom confluemacs-user-agent "Confluemacs/0.5 Emacs"
  "User-Agent string for API requests."
  :type 'string
  :group 'confluemacs)

(defvar confluemacs--current-path nil
  "Current path in Confluemacs buffer (e.g., \\='spaces\\=' or \\='spaces/TST\\=').")
(defvar confluemacs--buffer-name "*Confluemacs*"
  "Name of the Confluemacs buffer.")

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
         (default-headers `(("Authorization" . ,auth)
                            ("Content-Type" . "application/json")
                            ("Accept" . "application/json")
                            ("User-Agent" . ,confluemacs-user-agent)
                            ("X-Atlassian-Token" . "no-check")))
         (all-headers (append headers default-headers))
         (response-data nil)
         (response-status nil)
         (error-thrown nil))
    (request url
      :type method
      :params params
      :data (when data (json-encode data))
      :headers all-headers
      :parser 'json-read
      :timeout confluemacs-timeout
      :sync t
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  (setq response-data data)))
      :error (cl-function
              (lambda (&key error-thrown response &allow-other-keys)
                (setq response-status (request-response-status-code response))
                (cond
                 ((eq response-status 410)
                  (message "Confluemacs: API endpoint deprecated. Please update to v2 API."))
                 ((eq response-status 401)
                  (message "Confluemacs: Authentication failed. Check your API token."))
                 ((eq response-status 403)
                  (message "Confluemacs: Permission denied."))
                 ((eq response-status 429)
                  (message "Confluemacs: Rate limit exceeded. Please try again later."))
                 (t
                  (message "Confluemacs API error: %s (Status: %s)"
                           (or error-thrown "Unknown error")
                           (or response-status "Unknown"))))))
      :complete (cl-function
                 (lambda (&key response &allow-other-keys)
                   (setq response-status (request-response-status-code response)))))
    (cond
     ((and response-status (>= response-status 400))
      nil)
     (response-data response-data)
     (t nil))))

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
    (with-temp-buffer
      (insert org-text)
      (if (executable-find "pandoc")
          (progn
            (shell-command-on-region
             (point-min) (point-max)
             "pandoc -f org -t html"
             (current-buffer) t)
            (buffer-string))
        (let ((org-export-with-toc nil)
              (org-export-with-section-numbers nil))
          (org-export-string-as org-text 'html t))))))

(defun confluemacs--confluence-to-org (html-text)
  "Convert Confluence storage format (HTML) to Org-mode."
  (if (not html-text)
      ""
    (with-temp-buffer
      (insert html-text)
      (if (executable-find "pandoc")
          (progn
            (shell-command-on-region
             (point-min) (point-max)
             "pandoc -f html -t org"
             (current-buffer) t)
            (buffer-string))
        (let ((html (replace-regexp-in-string "<[^>]+>" "" html-text)))
          html)))))

;;; Dired-like Interface
(define-derived-mode confluemacs-mode special-mode "Confluemacs"
  "Major mode for browsing Confluence Cloud spaces and content."
  (setq buffer-read-only t)
  (setq truncate-lines t)
  (use-local-map confluemacs-mode-map))

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
          (confluemacs--display-spaces)
        (let ((space-key (cadr (split-string confluemacs--current-path "/"))))
          (confluemacs--display-content space-key)))
      (goto-char (point-min)))))

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
  (let* ((response (confluemacs--make-request (format "/content/%s" id) "GET"
                                              `(("expand" . ,(or expand confluemacs-expand-default)))))
         (content response))
    (when content
      (let* ((body (cdr (assq 'storage (cdr (assq 'body content)))))
             (html (cdr (assq 'value body)))
             (org-text (confluemacs--confluence-to-org html))
             (title (cdr (assq 'title content)))
             (version (cdr (assq 'number (cdr (assq 'version content)))))
             (buffer (get-buffer-create (format "*Confluemacs: %s*" title)))
             (can-edit (confluemacs--check-edit-permission id)))
        (with-current-buffer buffer
          (erase-buffer)
          (insert org-text)
          (org-mode)
          (setq-local confluemacs-content-id id)
          (setq-local confluemacs-content-title title)
          (setq-local confluemacs-content-version version)
          (setq-local confluemacs-content-space-key
                      (cdr (assq 'key (cdr (assq 'space content)))))
          (setq buffer-read-only (not can-edit))
          (when can-edit
            (local-set-key (kbd "C-c C-c") 'confluemacs-save-content)))
        (switch-to-buffer buffer)
        (when (not can-edit)
          (message "Content is read-only: No edit permission"))))))

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
   ["Content"
    ("c" "Create content" confluemacs-create-content)
    ("d" "Delete" confluemacs-delete)]
   ["Advanced"
    ("sg" "Get spaces" confluemacs-get-spaces)
    ("cg" "Get content" confluemacs-get-content)
    ("ss" "Search" confluemacs-search)
    ("v" "Check API version" confluemacs-check-api-version)]])

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
  (let ((params (append params `(("cql" . ,cql)
                                 ("expand" . ,confluemacs-expand-default)))))
    (confluemacs--make-request "/content/search" "GET" params)))

(defun confluemacs-check-api-version ()
  "Check the current API version configuration and deprecation status."
  (interactive)
  (let ((current-version confluemacs-api-version)
        (base-url confluemacs-base-url))
    (message "Confluemacs API Configuration:
- Current API version: %s
- Base URL: %s
- Deprecation: v1 will be removed on April 30, 2025
- Status: %s"
             current-version
             base-url
             (if (string= current-version "v2")
                 "Ready for v2 (experimental)"
               "Using v1 (migration recommended)"))))

(provide 'confluemacs)
;;; confluemacs.el ends here
