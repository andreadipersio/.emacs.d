;;
;; Prevent boot being slowdown by GC and determining file handlers,
;; will restore at the end
;;
(setq gc-cons-threshold 402653184
      gc-cons-percentage 0.6)

(defvar file-name-handler-alist-original file-name-handler-alist)
(setq file-name-handler-alist nil)

;;
;; Don't care about warning, mostly related to packages
;;
(setq warning-suppress-types nil)

(setq package-enable-at-startup nil)

;;
;; Less noise
;;
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(setq inhibit-startup-screen t)
(setq ring-bell-function 'ignore)
(setq frame-inhibit-implied-resize t)

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

(setq my-auto-save-path (expand-file-name "auto-save/" user-emacs-directory))

(if (not (file-directory-p my-auto-save-path))
  (make-directory my-auto-save-path))

(setq auto-save-file-name-transforms
  `((".*" ,(file-name-as-directory my-auto-save-path) t)))

;; Hide all minor modes
(setq mode-line-modes
      (mapcar (lambda (elem)
                (pcase elem
                  (`(:propertize (,_ minor-mode-alist . ,_) . ,_)
                   "")
                  (_ elem)))
              mode-line-modes))

;;
;; Aliases and Global Keybindings
;;
(defalias 'yes-or-no-p 'y-or-n-p)

;;
;; When saving ensure there are no trailing whitespaces
;;
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;;
;; Setup EPA (Encryption)
;;
(require 'epa-file)
(epa-file-enable)

(setq epa-pinentry-mode 'loopback)

;;
;; Restart Garbage Collection
;;
(add-hook 'after-init-hook
          `(lambda ()
             (setq gc-cons-threshold 67108864 ; 64mb
                   gc-cons-percentage 0.1
                   file-name-handler-alist file-name-handler-alist-original)
             (garbage-collect)) t)
