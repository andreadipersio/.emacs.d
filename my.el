;; Less noise

(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(setq inhibit-startup-screen t)
(setq ring-bell-function 'ignore)

;;
;; Paths
;;
(setq
 backup-by-copying t
 backup-directory-alist '(("." . "~/.emacs.d/backup/"))
 delete-old-versions t
 kept-new-versions 5
 kept-old-versions 2
 version-control t)

(setq my-auto-save-path (expand-file-name "auto-save" user-emacs-directory))

(if (not (file-directory-p my-auto-save-path))
  (make-directory my-auto-save-path))

(setq auto-save-file-name-transforms
  `((".*" ,(file-name-as-directory my-auto-save-path) t)))

;;
;; Aspect
;;
(add-to-list 'load-path "~/.emacs.d/themes")
(require 'material-theme)
(load-theme 'material t)

(add-to-list 'default-frame-alist
	     '(font . "Fira Code Medium-12"))

(add-to-list 'default-frame-alist	     
	     '(width . 120))

(add-to-list 'default-frame-alist
	     '(height . 65))

(show-paren-mode 1)

(blink-cursor-mode 0)

;; Hide all minor modes
(setq mode-line-modes
      (mapcar (lambda (elem)
                (pcase elem
                  (`(:propertize (,_ minor-mode-alist . ,_) . ,_)
                   "")
                  (t elem)))
              mode-line-modes))

;;
;; Aliases and Global Keybindings
;;
(defalias 'yes-or-no-p 'y-or-n-p)

(global-set-key (kbd "S-<f1>") 'racer-describe-tooltip)

;;
;; Functions
;;
(setq sqlformat-command "sqlformat --reindent --keywords upper --identifiers lower -")

(defun format-sql ()
  "Format SQL in the current buffer"
  (interactive)
  (mark-whole-buffer)
  (shell-command-on-region (point-min) (point-max) sqlformat-command (current-buffer)))
