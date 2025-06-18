;;; test-confluemacs.el --- Test suite for Confluemacs -*- lexical-binding: t; -*-

;;; Commentary:

;; Simple test functions to verify Confluemacs API functionality.
;; Run these tests after configuring your Confluence instance.

;;; Code:

(require 'confluemacs)

(defun confluemacs-test-authentication ()
  "Test authentication to Confluence Cloud API."
  (interactive)
  (condition-case err
      (let ((creds (confluemacs--get-credentials)))
        (if (and (car creds) (cdr creds))
            (message "Authentication credentials found: %s" (car creds))
          (error "No credentials found")))
    (error
     (message "Authentication test failed: %s" err))))

(defun confluemacs-test-api-connection ()
  "Test basic API connection by fetching spaces."
  (interactive)
  (let ((response (confluemacs--make-request "/space" "GET" '(("limit" . 1)))))
    (if response
        (message "API connection successful. Found %d space(s)."
                 (length (confluemacs--handle-response response)))
      (message "API connection failed. Check your configuration."))))

(defun confluemacs-test-v2-endpoint ()
  "Test v2 API endpoint (experimental)."
  (interactive)
  (let ((original-version confluemacs-api-version))
    (unwind-protect
        (progn
          (setq confluemacs-api-version "v2")
          (let ((response (confluemacs--make-request "/spaces" "GET" '(("limit" . 1)))))
            (if response
                (message "v2 API test successful")
              (message "v2 API test failed - this is expected as v2 support is experimental"))))
      (setq confluemacs-api-version original-version))))

(defun confluemacs-test-all ()
  "Run all Confluemacs tests."
  (interactive)
  (message "Starting Confluemacs tests...")
  (confluemacs-test-authentication)
  (sit-for 1)
  (confluemacs-test-api-connection)
  (sit-for 1)
  (confluemacs-test-v2-endpoint)
  (message "Confluemacs tests completed."))

(provide 'test-confluemacs)
;;; test-confluemacs.el ends here