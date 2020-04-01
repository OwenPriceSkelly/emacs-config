;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
(setq user-full-name "Owen Price-Skelly"
      user-mail-address "Owen.Price.Skelly@gmail.com"

      doom-theme 'doom-rouge
      doom-font (font-spec :family (if IS-MAC "Anonymous Pro" "monospace") :size 14)
      solaire-mode-auto-swap-bg t
      solaire-mode-remap-line-numbers t

      highlight-indent-guides-method 'character
      highlight-indent-guides-responsive 'stack
      highlight-indent-guides-delay 0.2


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
      doom-localleader-alt-key "C-,")

(use-package! expand-region
  :config
  (setq expand-region-contract-fast-key "V"))

(use-package! evil-textobj-line
  :demand t)


(defun +python-lsp-config ()
  (when (and (featurep! :tools lsp)
             (featurep! :lang python +lsp))
    (setq lsp-pyls-plugins-pylint-enabled t
          lsp-pyls-plugins-pylint-args ["--disable=C,logging-format-interpolation,useless-return"]
          lsp-pyls-plugins-pycodestyle-enabled nil
          lsp-pyls-plugins-flake8-enabled nil
          lsp-pyls-plugins-pyflakes-enabled nil)
    (when (featurep! :tools lsp +peek)
      (after! lsp-ui
        (lsp-ui-doc-enable t)
        (setq lsp-ui-doc-position 'top
              lsp-ui-doc-max-height 35
              lsp-ui-doc-max-width 35
              lsp-ui-doc-delay 0.4)))))

(after! python (+python-lsp-config))
(after! org (load! "+org-config.el"))
(load! "+keybindings")

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
