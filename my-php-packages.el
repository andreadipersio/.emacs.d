;;
;; PHP
;;

(use-package php-mode
  :ensure t

  :after dumb-jump

  :mode "\\.php$"

  :config
  (add-hook 'php-mode-hook 'subword-mode)

  :bind
  (:map php-mode-map
	("C-]" . dumb-jump-go)))
