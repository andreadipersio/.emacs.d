(use-package go-mode
  :ensure t
  :init (add-hook 'go-mode-hook
                  (lambda ()
                    (setq gofmt-command "goimports")
                    (add-hook 'before-save-hook 'gofmt-before-save)
                    (setq truncate-lines t)
                    (setq indent-tabs-mode t)
                    (setq tab-width 4)
		    (flycheck-mode))))

(use-package go-eldoc
  :ensure t
  :init (add-hook 'go-mode-hook 'go-eldoc-setup))

(use-package go-guru
  :ensure t
  :init (add-hook 'go-mode-hook 'go-guru-hl-identifier-mode))
