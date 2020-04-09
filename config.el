;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
(setq user-full-name "Owen Price-Skelly"
      user-mail-address "Owen.Price.Skelly@gmail.com"
      doom-theme 'doom-gruvbox
      doom-font (font-spec :family "Iosevka" :size 16)
      doom-variable-pitch-font (font-spec :family "Iosevka Sparkle" :size 18)

      solaire-mode-auto-swap-bg t
      solaire-mode-remap-line-numbers t

      which-key-side-window-location 'bottom
      which-key-sort-order 'which-key-key-order-alpha

      display-line-numbers-type 'relative
      ranger-override-dired t

      iedit-occurrence-context-lines 1
      fill-column 88
      which-key-max-description-length nil
      undo-tree-visualizer-diff nil
      +workspaces-on-switch-project-behavior t
      +latex-viewers '(pdf-tools)
      doom-leader-key "SPC"
      doom-leader-alt-key "C-SPC"
      doom-localleader-key ","
      doom-localleader-alt-key "C-,"
      evil-split-window-below t
      evil-split-window-right t
      +pretty-code-enabled-modes (list 'not 'python-mode))
(set-pretty-symbols! 'python-mode nil)
;; (remove-hook '+doom-dashboard-functions #'doom-dashboard-widget-shortmenu)
(use-package! expand-region
  :config
  (setq expand-region-contract-fast-key "V"))

(use-package! evil-textobj-line
  :demand t)

(use-package! mathpix.el
  :commands (mathpix-screenshot)
  :custom ((mathpix-app-id "owenpriceskelly_gmail_com_2bbd51")
           (mathpix-app-key "0b3d8ae26f3762b4d5b8")
           (mathpix-screenshot-method "screencapture -i %s")))

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

(load! "+dashboard")
(load! "+keybindings")
(after! org (load! "+org"))
(after! lsp (load! "+lsp"))
;; (+default/restart-server)
