;; -*- lexical-binding: t; -*-

(unless (>= emacs-major-version 27)
  (message "Early init: Emacs Version < 27.0")
  (load (expand-file-name "early-init.el" user-emacs-directory)))


;;
;; Customizations that relies specifically in the graphic mode being loaded
;;
(load "~/.emacs.d/my.el")

;;
;; General purpose packages
;;
(load "~/.emacs.d/my-packages.el")

;;
;; Package Bundles
;;
;; - Read the EMACS_BUNDLES environment variable which must be a
;;   space separated list of bundle names
;; - For each bundle load a file named 'my-{bundle name}-packages.el'
;;
(if-let ((bundles-string (getenv "EMACS_BUNDLES"))
	 (bundles (split-string bundles-string " "))
	 (load-suffixes (push ".gpg" load-suffixes))
	 (make-bundle-path (lambda (bundle) (format "~/.emacs.d/my-%s-packages" bundle))))
  (mapcar
   (lambda (bundle)
     (load (funcall make-bundle-path bundle) nil t nil nil))
   bundles))

;;
;; Site specific configuration
;;
;; - Load a file named my-{system-type}.el
;;
;; "/" in system-type is replaced with "-"
;; ex. gnu/linux => gnu-linux
;;
(let* ((site-config-filename (format "my-%s-site.el" (s-replace "/" "-" (symbol-name system-type))))
       (site-config-fullpath (expand-file-name site-config-filename user-emacs-directory)))
  (when (file-exists-p site-config-fullpath)
    (message "Loading %s site configuration" system-type)
    (load site-config-fullpath)))

;;
;; Emacs Customizations
;;
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(if (file-exists-p custom-file)
  (load custom-file))
