;;
;; PHP
;;

(use-package php-mode
  :ensure t

  :after dumb-jump

  :mode "\\.php$"

  :config
  (add-hook 'php-mode-hook 'subword-mode)

  :hook my-php-mode

  :bind
  (:map php-mode-map
	("C-]" . dumb-jump-go)))

(defun my-php-mode-hook ()
  "My PHP mode configuration."
  (setq indent-tabs-mode nil
        tab-width 4
        c-basic-offset 4))
