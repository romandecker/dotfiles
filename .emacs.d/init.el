;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(require 'package)

;; make sure emacs finds all my-*.el files
(add-to-list 'load-path "~/.emacs.d/lib")

(require 'my-setup)            ; initial setup, libraries and basic stuff
(require 'my-config)           ; Some basic configuration settings
(require 'my-backup)
(require 'my-general)
(require 'my-utils)
(require 'my-evil)
(require 'my-helm)
(require 'my-projectile)
(require 'my-org)
(require 'my-dired)
(require 'my-term)
(require 'my-javascript)
(require 'my-editorconfig)
(require 'my-flycheck)
(require 'my-yasnippet)
(require 'my-company)
(require 'my-hydra)
(require 'my-bookmarks)
(require 'my-markdown)
(require 'my-rainbow-delimiters)
(require 'my-magit)
(require 'my-origami)
(require 'my-modeline)
(require 'my-prog)
(require 'my-elisp)
(require 'my-visuals)
(require 'my-text-objects)
(require 'my-undo-tree)
(require 'my-misc)
(require 'my-realgud)
(require 'my-writing)
(require 'my-docview)
(require 'my-collaboration)
(require 'my-html)
(require 'my-raml)
(require 'my-docker)
(require 'my-pomodoro)
(require 'my-workgroups)
