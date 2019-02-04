(use-package fsharp-mode
  :ensure t)

(use-package dotnet
  :ensure t
  :init
  (add-hook 'fsharp-mode-hook 'dotnet-mode))
