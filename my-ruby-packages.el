;;
;; Ruby
;;

(use-package chruby
  :ensure t
  :hook (enh-ruby-mode . chruby-use-corresponding))

(use-package enh-ruby-mode
  :ensure t
  :mode "\\.rb\\'"
  :mode "Rakefile\\'"
  :mode "Gemfile\\'"
  :mode "Berksfile\\'"
  :mode "Vagrantfile\\'"
  :mode "\\.erb\\'"

  :interpreter "ruby"

  :bind
  (:map enh-ruby-mode-map
	("C-r !! d" . ruby-dig-disasm)
	("C-r !! s" . ruby-dig-sexp)))

(use-package yard-mode
  :ensure t
  :hook enh-ruby-mode)

(use-package inf-ruby
  :ensure t
  :hook ruby-mode)

(use-package rubocop
  :ensure t
  :hook (enh-ruby-mode . rubocop-mode))

(use-package minitest
  :ensure t
  :hook (enh-ruby-mode . minitest-mode))

(use-package projectile-rails
  :ensure t
  :after projectile
  :hook (enh-ruby-mode . projectile-rails-mode))

;;
;; Ruby Dig
;;
;; A set of interactive commands to dig into Ruby Internals.
;;

(defvar ruby-dig-disasm-instruction
  "puts RubyVM::InstructionSequence.compile(File.read('%s')).disasm"
  "A format string, it accepts the filename of the script we want to disassemble.")

(defvar ruby-dig-sexp-instruction
  "require 'ripper'; require 'pp'; pp Ripper.sexp(File.read('%s'))"
  "A format string, it accepts the filename of the script we want to transform into
a Symbolic Expression Tree.")


(defvar ruby-dig-ruby-executable "ruby")

(defun ruby-dig-make-command (script)
  "Invoke ruby and pass a single line script using -e."
  (format "%s -e \"%s\"" ruby-dig-ruby-executable script))

(defun ruby-dig-create-temp-file (content)
  "The name of the file is the sha1 hash of CONTENT prefixed
with 'ruby-mri-disasm-'."
  (let* ((content-hash (secure-hash 'sha1 content))
	 (temp-filename (format "ruby-mri-disasm-%s" content-hash)))
    (with-temp-file temp-filename
      (insert content))
    temp-filename))

(defmacro ruby-dig-with-temp-file (file-content &rest body)
  "Create a temporary filename with the content of FILE-CONTENT and
ensure it is is deleted after BODY is executed."
  `(let ((temp-filename (ruby-dig-create-temp-file ,file-content)))
     ,@body
     (delete-file temp-filename)))

(defun ruby-dig (content instruction)
  (let ((output-buffer-name "*ruby-dig*"))
    (ruby-dig-with-temp-file content
      (let* ((command (ruby-dig-make-command (format instruction temp-filename)))
	     (command-output (shell-command-to-string command)))
	(message "[ruby-dig] %s" command)
	(with-current-buffer-window output-buffer-name 'display-buffer-pop-up-window nil
	  (insert command-output)
	  (help-mode))
	(select-window (get-buffer-window output-buffer-name))))))

(defun ruby-dig-sexp ()
  "Use Ripper to parse the current region into its Symbolic Expressions Tree."
  (interactive)
  (ruby-dig (buffer-substring (mark) (point)) ruby-dig-sexp-instruction))

(defun ruby-dig-disasm ()
  "Compile and disassemble the code within the current region."
  (interactive)
  (ruby-dig (buffer-substring (mark) (point)) ruby-dig-disasm-instruction))
