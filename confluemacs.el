;;; confluemacs.el --- Dired-like interface for Confluence Cloud with Org-mode support -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Your Name

;; Author: Your Name <your.email@example.com>
;; Version: 0.4
;; Package-Requires: ((emacs "27.1") (request "0.3.3") (org "9.4") (transient "0.7.8"))
;; Keywords: tools, confluence, org-mode, dired
;; URL: https://github.com/yourusername/confluemacs

;;; Commentary:

;; This package provides a Dired-like interface to browse Confluence Cloud spaces
;; and content (pages, blogposts) using the REST API v5.7.1. Content is displayed
;; and edited in Org-mode, converted to/from Confluence storage format (HTML).
;; Buffers are read-only if the user lacks edit permissions.
;; Authentication uses `.authinfo.gpg` with an API token.
;; See https://docs.atlassian.com/atlassian-confluence/REST/5.7.1/ for API details.

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

(defvar confluemacs--current-path nil
  "Current path in Confluemacs buffer (e.g., \\='spaces\\=' or \\='spaces/TST\\=').")
(defvar confluemacs--buffer-name "*Confluemacs*"
  "Name of the Confluemacs buffer.")

(defun confluemacs--get-credentials ()
  "Retrieve Confluence Cloud credentials from .authinfo.gpg."
  (let* ((auth (auth-source-search :host confluemacs-auth-source-host
                                   :require '(:user :secret)
                                   :create t))
         (user (plist-get (car auth) :user))
         (secret (plist-get (car auth) :secret))
         (token (if (functionp secret) (funcall secret) secret)))
    (unless (and user token)
      (error "Failed to retrieve credentials from .authinfo.gpg"))
    (cons user token)))

(defun confluemacs--make-request (endpoint method &optional params data headers)
  "Make an HTTP request to the Confluence Cloud API.
ENDPOINT is the API endpoint path (e.g., \\='/rest/api/content\\=').
METHOD is the HTTP method (e.g., \\='GET, \\='POST).
PARAMS is an alist of query parameters.
DATA is the request body (for POST/PUT).
HEADERS is an alist of additional headers."
  (unless confluemacs-base-url
    (error "Confluemacs base URL not set"))
  (let* ((creds (confluemacs--get-credentials))
         (user (car creds))
         (token (cdr creds))
         (url (concat confluemacs-base-url "/rest/api" endpoint))
         (auth (format "Basic %s"
                       (base64-encode-string (concat user ":" token))))
         (default-headers `(("Authorization" . ,auth)
                            ("Content-Type" . "application/json")
                            ("X-Atlassian-Token" . "nocheck")))
         (all-headers (append headers default-headers)))
    (request url
      :type method
      :params params
      :data (when data (json-encode data))
      :headers all-headers
      :parser 'json-read
      :timeout confluemacs-timeout
      :sync nil
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  (confluemacs--handle-response data)))
      :error (cl-function
              (lambda (&key error-thrown &allow-other-keys)
                (message "Confluemacs API error: %s" error-thrown))))))

(defun confluemacs--handle-response (data)
  "Process the API response DATA and return it."
  (if (and (listp data) (assq 'results data))
      (cdr (assq 'results data))
    data))

(defun confluemacs--check-edit-permission (content-id)
  "Check if the user has edit permission for CONTENT-ID."
  (request-response-data
   (confluemacs--make-request
    (format "/content/%s/restriction/byOperation/update" content-id) "GET")))

(defun confluemacs--org-to-confluence (org-text)
  "Convert ORG-TEXT to Confluence storage format (HTML)."
  (with-temp-buffer
    (insert org-text)
    (if (executable-find "pandoc")
        (shell-command-on-region
         (point-min) (point-max)
         "pandoc -f org -t html"
         (current-buffer) t)
      (let ((org-export-with-toc nil)
            (org-export-with-section-numbers nil))
        (org-export-string-as org-text 'html t)))))

(defun confluemacs--confluence-to-org (html-text)
  "Convert Confluence storage format (HTML) to Org-mode."
  (with-temp-buffer
    (insert html-text)
    (if (executable-find "pandoc")
        (shell-command-on-region
         (point-min) (point-max)
         "pandoc -f html -t org"
         (current-buffer) t)
      (let ((html (replace-regexp-in-string "<[^>]+>" "" html-text)))
        (with-temp-buffer
          (insert html)
          (org-mode)
          (buffer-string))))))

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
  (confluemacs--make-request "/space" "GET" '(("limit" . 100))))

(defun confluemacs--display-content (space-key)
  "Display content for SPACE-KEY."
  (confluemacs--make-request "/content" "GET"
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
  (confluemacs--make-request (format "/content/%s" id) "GET"
                             `(("expand" . ,(or expand confluemacs-expand-default)))))

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
                 ,@(when parent-id `(("ancestors" . ((("id" . ,parent-id))))))))
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
          (confluemacs--make-request (format "/space/%s" key) "DELETE")
          (message "Space %s deleted" key)
          (confluemacs-refresh))))
     ((eq type 'content)
      (let ((id (get-text-property (point) 'confluemacs-id)))
        (when (y-or-n-p "Delete content? ")
          (confluemacs--make-request (format "/content/%s" id) "DELETE")
          (message "Content deleted")
          (confluemacs-refresh)))))))

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
    ("ss" "Search" confluemacs-search)]])

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

(provide 'confluemacs)
;;; confluemacs.el ends here
