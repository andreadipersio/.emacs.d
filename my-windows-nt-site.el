(set-frame-position (selected-frame) 0 0)

(add-to-list 'default-frame-alist
	     '(font . "Consolas-14"))

(add-to-list 'default-frame-alist
	     '(width . 120))

(add-to-list 'default-frame-alist
	     '(height . 60))

(setq powershell-path "pwsh.exe")
(setq inferior-lisp-program "sbcl.exe")

(setq explicit-shell-file-name powershell-path)
(setq shell-file-name powershell-path)
(setenv "SHELL" "pwsh.exe")
