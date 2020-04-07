;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
(setq user-full-name "Owen Price-Skelly"
      user-mail-address "Owen.Price.Skelly@gmail.com"
      doom-theme 'doom-solarized-light
      doom-font (font-spec :family (if IS-MAC "Iosevka" "monospace") :size 14)
      doom-variable-pitch-font (font-spec :family (if IS-MAC "ETBookOT"))
      ;; doom-unicode-font (font-spec :family (if IS-MAC "Iosevka Sparkle"))
      ;; solaire-mode-auto-swap-bg t
      solaire-mode-remap-line-numbers t
      highlight-indent-guides-method 'character
      highlight-indent-guides-responsive 'stack
      highlight-indent-guides-delay 0.2
      which-key-side-window-location 'bottom
      which-key-sort-order 'which-key-key-order-alpha
      display-line-numbers-type t
      ranger-override-dired t
      iedit-occurrence-context-lines 1
      fill-column 88
      which-key-max-description-length nil
      undo-tree-visualizer-diff nil
      +workspaces-on-switch-project-behavior 'non-empty
      +latex-viewers '(pdf-tools)
      doom-leader-key "SPC"
      doom-leader-alt-key "C-SPC"
      doom-localleader-key ","
      doom-localleader-alt-key "C-,")

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
(after! org (load! "+notes"))
(after! lsp (load! "+lsp"))
;; (+default/restart-server)
