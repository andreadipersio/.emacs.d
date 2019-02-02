(package-initialize)

(load "~/.emacs.d/my.el")

;;
;; General purpose packages
;;
(load "~/.emacs.d/my-packages.el")

;;
;; Language specific packages
;;
(load "~/.emacs.d/my-rust-packages.el")
;; (load "~/.emacs.d/my-ruby-packages.el")
;; (load "~/.emacs.d/my-php-packages.el")

;;
;; Site specific packages
;;
(setq my-local-packages-fullpath (expand-file-name "my-local-packages.el" user-emacs-directory))

(if (file-exists-p my-local-packages-fullpath)
  (load my-local-packages-fullpath))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(if (file-exists-p custom-file)
  (load custom-file))
