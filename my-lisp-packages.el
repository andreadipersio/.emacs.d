(use-package sly
  :ensure t
  :init
  (unless (boundp 'inferior-lisp-program)
    (setq inferior-lisp-program "/usr/bin/sbcl")))

(use-package sly-quicklisp
  :ensure t)

(use-package sly-asdf
  :ensure t)
