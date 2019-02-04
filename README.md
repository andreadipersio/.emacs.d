# My Emacs Configuration

This is my ever changing emacs configuration.

### Highlights
- It works best on Emacs >=27.
- Uses `ivy` for interactive completion.
- Packages are managed with `use-package`.
- You can customize the behaviour on different system types (windows, osx, etc) creating a file
named `my-{system-type}-site.el`, it would load automatically after all
the other scripts.
- You can manage bundles of packages by creating a `my-{package-name}-packages.el` file.
To automatically load this bundles, set the `EMACS_BUNDLES` env variable (it is a space separate list
of package-name, example: `EMACS_BUNDLES=lisp fsharp` will load `my-lisp-packages.el` and `my-fsharp-packages.el`.
- Backups and auto saves are stored in their own directories in .emacs.d.

### Boot Sequence
- `early-init.el`: Includes some boot tweaks (like gc tuning) and all package agnostic settings.
- `init.el`: Load `my.el`, `my-packages.el`, bundles and site-specific files.
- `my.el`: Doesn't do much, I'll probably remove it soon.
- `my-packages.el`: Load packages of generic use like `ivy`, `magit`, etc.
