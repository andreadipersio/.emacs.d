(require 'package)

;;
;; Package Archives
;;
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives
	     '("org" . "http://orgmode.org/elpa/") t)
(add-to-list 'package-archives
	     '("gnu" . "http://elpa.gnu.org/packages/") t)

(setq package-archive-priorities
      '(("melpa" .  3)
        ("org" . 2)
        ("gnu" . 1)))

;; Initialise the packages, avoiding a re-initialisation.
(unless (bound-and-true-p package--initialized)
  (setq package-enable-at-startup nil)
  (package-initialize))

(setq load-prefer-newer t)              ; Always load newer compiled files
(setq ad-redefinition-action 'accept)   ; Silence advice redefinition warnings

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

;;
;; Packages Configuration
;;

(use-package magit
  :ensure t
  :config
  (global-set-key (kbd "C-x g") 'magit-status))

(use-package git-link
  :ensure t
  :bind (("C-c g l" . git-link)
	 ("C-c g c" . git-link-commit)
	 ("C-c g h" . git-link-homepage)))

(use-package let-alist
  :ensure t)

;;
;; Fuzzy matching
;;
(use-package flx
  :ensure t)

;;
;; The Silver Searcher
;;
(use-package ag
  :ensure t)

;;
;; Theme
;;
(use-package doom-themes
  :ensure t
  :init
  (load-theme 'doom-city-lights t))

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
  :config
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
	'((swiper . ivy--regex-plus)
	  (t . ivy--regex-fuzzy))))

;;
;; Projectile
;;

(use-package projectile
  :ensure t
  :after ivy
  :init
  (projectile-mode)
  (setq projectile-completion-system 'ivy)

  :bind (("C-c p" . projectile-command-map)))

(use-package company
  :ensure t
  :defer t
  :bind (:map company-mode-map
	      ("C-<tab>" . company-complete))
  :init (global-company-mode)
  :config
  (setq company-idle-delay nil)
  (setq tab-always-indent 'complete)
  (setq company-dabbrev-downcase nil)
  (setq company-tooltip-limit 10)
  (setq company-minimum-prefix-length 1)
  (setq company-selection-wrap-around t)
  (setq company-tooltip-align-annotations t)
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
  (define-key company-active-map (kbd "C-d") 'company-show-doc-buffer)
  (setq company-transformers '(company-sort-by-occurrence)))

;;
;; Temporary highlight changes from yanking, etc
;;
(use-package volatile-highlights
  :ensure t
  :config
  (volatile-highlights-mode +1))

;;
;; Neotree
;;
(use-package neotree
  :ensure t
  :config
  (setq neo-theme 'ascii)
  (setq neo-window-fixed-size nil)
  :bind (("<f8>" . neotree-project-dir)))

(defun neotree-project-dir ()
  "Open NeoTree using the git root."
  (interactive)
  (let ((project-dir (projectile-project-root))
        (file-name (buffer-file-name)))
    (neotree-toggle)
    (if project-dir
        (if (neo-global--window-exists-p)
            (progn
              (neotree-dir project-dir)
              (neotree-find file-name)))
      (message "Could not find git project root."))))

;;
;; Flycheck
;;
(use-package flycheck
  :ensure t
  :config
  (setq flycheck-highlighting-mode nil)
  (setq flycheck-indication-mode nil))

;;
;; GraphQL
;;
(use-package graphql-mode
  :ensure t)

;;
;; Ace packages
;;
(use-package ace-window
  :ensure t
  :init
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  :bind (("C-x o" . ace-window))
  :config
  (set-face-attribute
     'aw-leading-char-face nil
     :weight 'bold
     :height 3.0))

(use-package ace-jump-mode
  :ensure t
  :bind (("C-c SPC" . ace-jump-mode)))

;;
;; Dumb Jump
;;
(use-package dumb-jump
  :ensure t
  :init
  (dumb-jump-mode)
  (setq dumb-jump-selector 'ivy))

;;
;; YAML Mode
;;
(use-package yaml-mode
  :ensure t
  :mode "\\.y?ml\\'")

;;
;; Visualize regexp matches on current buffer
;;
(use-package visual-regexp
  :ensure t
  :bind (("C-c v r" . vr/replace)
         ("C-c v q" . vr/query-replace)
         ("C-c v m" . vr/mc-mark)))

;;
;; Which Key
;;
(use-package which-key
  :ensure t
  :hook (after-init . which-key-mode))

;;
;;
;;
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "markdown"))

;;
;; JSON Mode
;;
(use-package json-mode
  :ensure t)

;;
;;
;;
(use-package itail
  :ensure t)

;;
;; Quelpa
;;
(use-package quelpa
  :ensure t)

(use-package quelpa-use-package
  :ensure t
  :init (setq quelpa-checkout-melpa-p nil))

;;
;; Language Server Protocol
;;
(use-package lsp-mode
  :ensure t
  :hook (
	 (ruby-mode . lsp)
	 (enh-ruby-mode . lsp)
         (php-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

(use-package lsp-ivy
  :ensure t
  :commands lsp-ivy-workspace-symbol)

;;
;; Configure flymake
;;

(setq flymake-fringe-indicator-position nil)
