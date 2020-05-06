;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
(setq user-full-name "Owen Price-Skelly"
      user-mail-address "Owen.Price.Skelly@gmail.com"
      ranger-override-dired t
      iedit-occurrence-context-lines 1
      fill-column 88
      company-idle-delay nil
      +workspaces-on-switch-project-behavior t)

(use-package! zone
  :defer-incrementally t
  :config
  (zone-when-idle 300))

(use-package! evil-textobj-line
  :demand t)

(load! "+extras/ui")
(load! "+extras/bindings")
(after! lsp (load! "+extras/lsp"))
(after! org (load! "+extras/org"))
