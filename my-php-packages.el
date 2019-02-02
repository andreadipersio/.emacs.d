(defvar required-php-packages
  '(
    ede-php-autoload
    ede-php-autoload-composer-installers
    ))

(install-missing-packages required-php-packages)

;;
;; PHP
;;

(use-package ac-php
  :ensure t
  :after company)

(use-package company-php
  :ensure t
  :after ac-php)

(use-package php-mode
  :ensure t
  :after company-php
  
  :mode "\\.php$"
  
  :requires (ac-php company-php)
  :after company  
  
  :config
  (add-hook 'php-mode-hook 'auto-complete-mode)
  (add-hook 'php-mode-hook 'company-mode)
  (add-hook 'php-mode-hook 'subword-mode)
  
  (ac-php-core-eldoc-setup)  
  (make-local-variable 'company-backends)
  (add-to-list 'company-backends 'company-ac-php-backend)
  
  :bind
  (("<C-tab>" . counsel-company)
   ("C-]" . ac-php-find-symbol-at-point)
   ("C-t" . ac-php-location-stack-back)))
