(defvar required-rust-packages
  '(
    flycheck-rust
    ))

(install-missing-packages required-rust-packages)

;;
;; Rust
;;

(use-package rust-mode
  :ensure t
  
  :init
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)
  
  :config
  (add-hook 'rust-mode-hook #'racer-mode)
  (add-hook 'racer-mode-hook #'eldoc-mode)
  (add-hook 'racer-mode-hook #'company-mode)
  (add-hook 'rust-mode-hook #'flycheck-mode)

  :bind
  ("<C-tab>" . 'company-common)

  :config
  (setq company-tooltip-align-annotations t))
