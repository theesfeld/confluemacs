;;; Simple test configuration

(use-package confluemacs
  :vc (:url "https://github.com/yourusername/confluemacs.git"
       :rev :newest)
  :ensure t
  :commands (confluemacs)
  :custom
  (confluemacs-base-url "https://yourcompany.atlassian.net/wiki")
  (confluemacs-auth-source-host "yourcompany.atlassian.net"))