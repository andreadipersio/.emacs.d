(use-package go-mode
  :ensure t
  :hook ((go-mode . lsp-deferred)
         (before-save . lsp-format-buffer)
         (before-save . lsp-organize-imports))
  :bind (("C-c C-j" . lsp-find-definition)
         ("C-c C-d" . lsp-describe-thing-at-point))
  :init (add-hook 'go-mode-hook
                  (lambda ()
                    (setq truncate-lines t)
                    (setq indent-tabs-mode t)
                    (setq tab-width 4))))
