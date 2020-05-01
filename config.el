;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
(setq user-full-name "Owen Price-Skelly"
      user-mail-address "Owen.Price.Skelly@gmail.com"
      ranger-override-dired t
      iedit-occurrence-context-lines 1
      fill-column 88
      +workspaces-on-switch-project-behavior t)

(use-package! zone
  :defer-incrementally t
  :config
  (zone-when-idle 300))

(use-package! evil-textobj-line
  :demand t)

(use-package! mathpix.el
  :commands (mathpix-screenshot)
  :config (setq
            mathpix-app-id "owenpriceskelly_gmail_com_2bbd51"
            mathpix-app-key "0b3d8ae26f3762b4d5b8"
            mathpix-screenshot-method "screencapture -i %s"))


;; TODO configure doom-modeline in ui.el
(load! "+extras/ui")
;; TODO evil-snipe config
(load! "+extras/bindings")
(after! lsp (load! "+extras/lsp"))
;; TODO configure org-mode line numbers hook (currently just dir-locals)
(after! org (load! "+extras/org"))
