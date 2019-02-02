(defvar required-ruby-packages
  '(
    yard-mode
    robe
    ))

(install-missing-packages required-ruby-packages)

;;
;; Ruby
;;

(use-package enh-ruby-mode
  :ensure t
  :mode "\\.rb\\'"
  :mode "Rakefile\\'"
  :mode "Gemfile\\'"
  :mode "Berksfile\\'"
  :mode "Vagrantfile\\'"
  
  :interpreter "ruby"

  :init
  (push 'company-robe company-backends)
  (add-hook 'enh-ruby-mode-hook 'robe-mode)
  (add-hook 'enh-ruby-mode-hook 'yard-mode)
  (add-hook 'ehn-ruby-mode-hook 'inf-ruby-minor-mode)
  (add-hook 'ehn-ruby-mode-hook 'company-mode))

(use-package inf-ruby
  :ensure t
  :init
  (add-hook 'ruby-mode-hook 'inf-ruby-minor-mode))

(use-package chruby
  :ensure t
  :init
  (chruby "2.5.3"))
