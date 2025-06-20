;;; test-use-package.el --- Test use-package syntax

;; This file contains the exact use-package configuration for testing

(use-package confluemacs
  :vc (:url "https://github.com/yourusername/confluemacs.git"
       :rev :newest)
  :ensure t
  :commands (confluemacs
             confluemacs-check-api-version
             confluemacs-validate-configuration
             confluemacs-recover-from-crash)
  :bind (("C-c f c" . confluemacs)
         ("C-c f v" . confluemacs-check-api-version)
         ("C-c f r" . confluemacs-recover-from-crash))
  :hook ((confluemacs-mode . hl-line-mode)
         (kill-emacs . confluemacs-check-unsaved-changes))
  :custom
  (confluemacs-base-url "https://yourcompany.atlassian.net/wiki")
  (confluemacs-auth-source-host "yourcompany.atlassian.net")
  (confluemacs-api-version "v1")
  (confluemacs-timeout 15)
  (confluemacs-auto-save-interval 300)
  (confluemacs-auto-save-directory 
   (expand-file-name "confluemacs-drafts" user-emacs-directory))
  (confluemacs-expand-default "space,body.storage,version,container,ancestors")
  :config
  (confluemacs-validate-configuration)
  (add-hook 'confluemacs-mode-hook
            (lambda ()
              (setq mode-line-format
                    (append mode-line-format
                            '(" " (:eval (when (bound-and-true-p confluemacs-content-modified)
                                          " [Modified]"))))))))

;;; test-use-package.el ends here