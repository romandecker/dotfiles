;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
;; (setq user-full-name "John Doe"
;;       user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
(setq doom-font (font-spec :family "monospace" :size 14))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Dropbox/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;

(map! "C-l" #'evil-window-right)
(map! "C-h" #'evil-window-left)
(map! "C-j" #'evil-window-down)
(map! "C-k" #'evil-window-up)
(map! :leader "w |" #'evil-window-vsplit)
(map! :leader "w -" #'evil-window-split)

(map! :leader "p /" #'+default/search-project)

;; automatically set the search pattern after doing a project-wide search, so
;; you can continue searching in the opened file by just pressing `n'
(defun ++default/search-project-advice (orig-fun &rest args)
  (let* ((result (apply orig-fun args))
         (last-search (substring-no-properties (car counsel-git-grep-history))))
    (setq evil-ex-search-pattern `(,last-search t t))
    result))
(advice-add '+default/search-project :around #'++default/search-project-advice)

(map! :n "g z s" #'evil-mc-skip-and-goto-next-match)
(map! :n "M-n" #'evil-mc-make-and-goto-next-match)
(map! :n "M-N" #'evil-mc-make-and-goto-prev-match)
(map! :n "M-s" #'evil-mc-skip-and-goto-next-match)
(map! :n "M-S" #'evil-mc-skip-and-goto-prev-match)

(map! :i "C-l" #'evil-delete-char)

(map! :leader "w o" #'delete-other-windows)

(global-subword-mode)

(setq doom-modeline-buffer-file-name-style 'truncate-with-project)

(setq fill-column 100)

;; First element in `counsel-projectile-switch-project-action' signifies
;; the index of the default action to take after switching projects,
;; set it to 14, which is to invoke magit
(setcar counsel-projectile-switch-project-action 14)

(add-to-list 'auto-mode-alist '("\\.ejs\\'" . web-mode))


(let ((local-config "~/.emacs.local.el"))
  (when (file-readable-p local-config)
    (load-file local-config)))


;; TO get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c g k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c g d') to jump to their definition and see how
;; they are implemented.
