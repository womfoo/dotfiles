(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(set-face-attribute 'default nil :font "DejaVu Sans Mono:pixelsize=14:foundry=unknown:weight=normal:slant=0:width=normal:spacing=100:scalable=true")

(add-to-list 'load-path "~/git/github.com/chriskempson/tomorrow-theme/GNU Emacs/")
(require 'color-theme-tomorrow)
(color-theme-tomorrow--define-theme night)

(add-to-list 'load-path "/nix/var/nix/profiles/default/share/emacs/site-lisp/")
(require 'haskell-mode-autoloads)

(global-set-key [C-mouse-4] 'text-scale-increase)
(global-set-key [C-mouse-5] 'text-scale-decrease)

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(desktop-save-mode 1)
(setq backup-inhibited t)
