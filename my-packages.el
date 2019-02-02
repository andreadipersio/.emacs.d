(require 'package)

;;
;; Package Archives
;;
(add-to-list 'package-archives
	     '("gnu" . "http://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
	     '("marmalade" . "http://marmalade-repo.org/packages/") t)

;;
;; Required Packages
;;

(defvar required-packages
  '(
    ;; foundamentals
    let-alist
    magit
    cider

    use-package

    ;; Completion
    flx

    ;; project navigation
    ag

    ;; syntax checking
    flycheck    

    ;; various modes
    yaml-mode
    markdown-mode
    ) "a list of packages to ensure are installed at launch.")

(require 'cl)

;; Ensure required packages are installed

(defun packages-installed-p (packages)
  (loop for p in packages
	when (not (package-installed-p p)) do (return nil)
	finally (return t)))

(defun install-missing-packages (packages)
  (unless (packages-installed-p packages)
    (message "%s" "runnning package-refresh-contents...")
    (package-refresh-contents)
    (message "%s" " done")
    (dolist (p packages)
      (when (not (package-installed-p p))
	(package-install p)))))

(install-missing-packages required-packages)

;;
;; Packages Configuration
;;

(require 'magit)
(require 'cider)
(require 'company)
(require 'cl)

;;
;; ivy
;;

(use-package counsel
  :ensure t
  :config
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "C-x C-f") 'counsel-find-file)
  (global-set-key (kbd "C-x b") 'counsel-ibuffer)  
  (global-set-key (kbd "C-h f") 'counsel-describe-function)
  (global-set-key (kbd "C-h v") 'counsel-describe-variable))

(use-package swiper
  :ensure t
  :after ivy
  :bind (("C-s" . swiper)
	 ("C-r" . swiper)))

(use-package ivy
  :ensure t
  :config
  (with-eval-after-load 'ido
    (ido-mode -1)
    (ivy-mode 1))

  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (setq ivy-count-format "(%d/%d) ")
  (setq ivy-initial-input-alist nil)
  (setq ivy-display-style 'fancy)

  ;; Enable fuzzy matching
  (setq ivy-re-builders-alist
	'((t . ivy--regex-fuzzy))))

;;
;; Projectile
;;

(use-package projectile
  :ensure t
  
  :init
  (projectile-mode)
  (setq projectile-completion-system 'ivy)
  
  :bind (("C-c p" . projectile-command-map)))

(use-package company
  :ensure t)

;;
;; Temporary highlight changes from yankink, etc
;;
(use-package volatile-highlights
  :ensure t
  :config
  (volatile-highlights-mode +1))
