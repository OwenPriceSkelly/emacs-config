;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
(setq user-full-name "Owen Price-Skelly"
      user-mail-address "Owen.Price.Skelly@gmail.com"

      doom-theme 'doom-gruvbox
      doom-font (font-spec :family "Iosevka Extended" :size 16)
      doom-variable-pitch-font (font-spec :family "Iosevka Etoile" :size 16)

      solaire-mode-auto-swap-bg t
      solaire-mode-remap-line-numbers t

      which-key-side-window-location 'bottom
      which-key-sort-order 'which-key-key-order-alpha

      display-line-numbers-type 'relative
      ranger-override-dired t

      iedit-occurrence-context-lines 1
      fill-column 88
      which-key-max-description-length nil
      +workspaces-on-switch-project-behavior t

      +latex-viewers '(pdf-tools)
      +pretty-code-enabled-modes '(org-mode)

      doom-leader-key "SPC"
      doom-leader-alt-key "C-SPC"
      doom-localleader-key ","
      doom-localleader-alt-key "C-,"
      evil-split-window-below t
      evil-vsplit-window-right t)

(setq +pretty-code-enabled-modes '(org-mode))

(use-package! rainbow-mode
  :defer-incrementally t)

(use-package! zone
  :defer-incrementally t
  :config
  (zone-when-idle 300))

(use-package! expand-region
  :config
  (setq expand-region-contract-fast-key "V"))

(use-package! evil-textobj-line
  :demand t)

(use-package! mathpix.el
  :commands (mathpix-screenshot)
  :config (setq
            mathpix-app-id "owenpriceskelly_gmail_com_2bbd51"
            mathpix-app-key "0b3d8ae26f3762b4d5b8"
            mathpix-screenshot-method "screencapture -i %s"))


;; persist frame size/fullscreen across sessions
(when-let (dims (doom-cache-get 'last-frame-size))
  (cl-destructuring-bind ((left . top) width height fullscreen) dims
    (setq initial-frame-alist
          (append initial-frame-alist
                  `((left . ,left)
                    (top . ,top)
                    (width . ,width)
                    (height . ,height)
                    (fullscreen . ,fullscreen))))))
(defun save-frame-dimensions ()
  (doom-cache-set 'last-frame-size
                  (list (frame-position)
                        (frame-width)
                        (frame-height)
                        (frame-parameter nil 'fullscreen))))
(add-hook 'kill-emacs-hook  #'save-frame-dimensions)

(load! "+extras/dashboard")
(load! "+extras/lsp")
(load! "+extras/org")
(load! "+extras/bindings")
