(require 'package)

(setq package-native-compile t)

;;
;; Package Archives
;;
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives
	     '("org" . "https://orgmode.org/elpa/") t)
(add-to-list 'package-archives
	     '("gnu" . "https://elpa.gnu.org/packages/") t)

(setq package-archive-priorities
      '(("melpa" .  3)
        ("org" . 2)
        ("gnu" . 2)))

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
;; Ripgrep
;;
(use-package rg
  :ensure t)

;;
;; Theme
;;
;; (use-package doom-themes
;;   :ensure t
;;   :init
;;   (load-theme 'doom-city-lights t))

(use-package modus-themes
  :ensure t
  :init
  (load-theme 'modus-vivendi t))

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
;; Flymake
;;
(setq flymake-fringe-indicator-position nil)

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
  (setq aw-keys '(?a ?s ?d ?f ?x ?c ?v ?b ?q))
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
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)  
  (setq dumb-jump-selector 'ivy))

;;
;; YAML
;;
(use-package yaml
  :ensure t)

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
         ("\\.mdx\\'" . markdown-mode)	 
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "markdown"))

;;
;; JSON Mode
;;
(use-package json-mode
  :ensure t)

;;
;; Language Server Protocol
;;
(use-package eglot
  :ensure t)

(use-package lsp-ui
  :ensure
  :commands lsp-ui-mode
  :custom
  (lsp-ui-peek-always-show nil)
  (lsp-ui-sideline-show-hover nil)
  (lsp-ui-doc-enable nil)
  (lsp-headerline-breadcrumb-enable nil))

;;
;; Ivy
;;
(use-package lsp-ivy
  :ensure t
  :commands lsp-ivy-workspace-symbol)

;;
;; JS2 Mode
;;
(use-package js2-mode
  :ensure t
  :init
  (setq-default js2-indent-level 2))

;;
;; Enable line wrap in org mode
;;
(add-hook 'org-mode-hook #'(lambda ()
                             (visual-line-mode)
                             (org-indent-mode)))

;; Enable richer annotations using the Marginalia package
(use-package marginalia
  :ensure t
  ;; Either bind `marginalia-cycle` globally or only in the minibuffer
  :bind (("M-A" . marginalia-cycle)
         :map minibuffer-local-map
         ("M-A" . marginalia-cycle))

  ;; The :init configuration is always executed (Not lazy!)
  :init

  ;; Must be in the :init section of use-package such that the mode gets
  ;; enabled right away. Note that this forces loading the package.
  (marginalia-mode))

;;
;; V-Term
;;
(when (not (eq system-type 'windows-nt))
    (use-package vterm
      :ensure t))

;;
;; Evil - Vi Layer
;;
(use-package evil
  :ensure t
  :config (evil-mode 1))
