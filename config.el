;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
(setq user-full-name "Owen Price-Skelly"
      user-mail-address "Owen.Price.Skelly@gmail.com"
      ;; ranger-override-dired t
      ;; +mu4e-backend 'offlineimap TODO
      iedit-occurrence-context-lines 1
      fill-column 88
      company-idle-delay nil
      +workspaces-on-switch-project-behavior t)

(use-package! zone
  :defer-incrementally t
  :config
  (zone-when-idle 600))

(use-package! evil-textobj-line
  :demand t)

(use-package! lispyville
  :when (featurep! :editor evil)
  :hook (lispy-mode . lispyville-mode)
  :config
  (lispy-set-key-theme '(lispy c-digits))
  (lispyville-set-key-theme
   '(operators
     c-w
     prettify
     text-objects
     (atom-movement normal visual)
     (additional-movement normal visual motion)
     commentary
     slurp/barf-cp
     ;; slurp/barf-lispy
     additional
     additional-insert
     escape)))

(use-package! evil-snipe
  :init
  (setq evil-snipe-scope                     'whole-visible
        evil-snipe-spillover-scope           'whole-buffer
        evil-snipe-repeat-scope              'buffer
        evil-snipe-repeat-keys               t
        evil-snipe-override-evil-repeat-keys t)
  :config
  ;; when f/t/s searching, interpret open/close square brackets to be any
  ;; open/close delimiters, respectively
  (push '(?\[ "[[{(]") evil-snipe-aliases)
  (push '(?\] "[]})]") evil-snipe-aliases)
  (evil-snipe-override-mode +1))

(load! "+extras/ui")
(load! "+extras/bindings")
(after! lsp (load! "+extras/lsp"))
(after! org (load! "+extras/org"))
(toggle-frame-fullscreen)
