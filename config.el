;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
(defun personal/variables ()
  (setq user-full-name "Owen Price-Skelly"
        user-mail-address "Owen.Price.Skelly@gmail.com"

        doom-theme 'doom-gruvbox
        doom-font (font-spec :family "monospace" :size 14)

        org-directory "~/.doom.d/org/"
        deft-directory org-directory

        which-key-side-window-location 'bottom
        display-line-numbers-type 'relative
        ranger-override-dired t

        ;; (add-hook! 'prog-mode-hook
        ;;   (setq-local +word-wrap-extra-indent 'double)
        ;;   (+word-wrap-mode +1))

        doom-leader-key "SPC"
        doom-localleader-key ","))


(defun personal/use-packages ()
  (use-package! treemacs
    :defer-incrementally t)
  (use-package! evil-iedit-state
    :demand t)
  (use-package! hercules
    :demand t)
  (use-package expand-region
    :config (setq expand-region-contract-fast-key "V"))
  (use-package! evil-textobj-line
    :demand t))
;; -----------------------------------------------------------------------------
;; - Keybinding
;; -----------------------------------------------------------------------------
(defun personal/bind-keys ()
  (general-auto-unbind-keys)
  ;; Leader-key bindings
  (map! :leader
        :desc "Ivy M-x"                "SPC"     #'counsel-M-x
        :desc "M-x"                    ":"       #'execute-extended-command
        :desc "Search project"         "/"       #'+default/search-project
        :desc "Visual expand"          "v"       #'er/expand-region

        (:when (featurep! :ui treemacs)
          :desc "Project sidebar"      "0"       #'treemacs-select-window)
        (:when (featurep! :ui workspaces)
          (:prefix "TAB"
            :desc "Switch workspace"   "TAB"     #'+workspace/switch-to))

        (:when (featurep! :ui tabs)
          :desc "Forward tab"   "]" #'centaur-tabs-forward-tab
          :desc "Backward tab"  "[" #'centaur-t)
        (:prefix "b"
          :desc "Fallback buffer"      "h"       #'+doom-dashboard/open))
  ;; Search/replace bindings
  (map! (:when (featurep! :completion ivy)
          :map ivy-minibuffer-map
          (:prefix "C-c"
            :desc "wgrep"              "e"       #'+ivy/woccur)))

  (map! :v "R" #'evil-iedit-state/iedit-mode-from-expand-region
        :n "R" #'evil-iedit-state/iedit-mode)

  (hercules-def
   :show-funs '(evil-iedit-state/iedit-mode evil-iedit-state/iedit-mode-from-expand-region)
   :hide-funs #'evil-iedit-state/quit-iedit-mode
   :keymap 'evil-iedit-state-map)



  ;; (hercules-def
  ;;  :show-funs #'evil-multiedit-state
  ;;  :keymap 'evil-multiedit-state-map
  ;;  :transient t)

  ;; (hercules-def
  ;;  :show-funs #'evil-multiedit-insert-state
  ;;  :keymap 'evil-multiedit-insert-state-map
  ;;  :transient t)

  ;; major mode bindings
  (map! :after python
        :map python-mode-map
        :localleader
        (:prefix ("r" . "repl")
          :desc "python"  "p" #'+python/open-repl
          :desc "ipython" "i" #'+python/open-ipython-repl
          :desc "jupyter" "j" #'+python/open-jupyter-repl)
        (:prefix ("=" . "format")
          :desc "buffer" "=" #'+format/buffer))

  (map! :after ein
        :map ein:notebook-mode-map
        :localleader
        "," #'+ein/hydra/body))

;; -----------------------------------------------------------------------------
;; Misc. quality of life snippets
;; -----------------------------------------------------------------------------
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

(add-hook 'kill-emacs-hook #'save-frame-dimensions)

(personal/variables)
(personal/use-packages)
(personal/bind-keys)

;; - `load!' for loading external *.el files relative to this one
;; -----------------------------------------------------------------------------
;; - `use-package' `use-package!'for configuring packages
;; -----------------------------------------------------------------------------
;; - `after!' for running code after a package has loaded
;; -----------------------------------------------------------------------------
;; - `add-load-path!' for adding directories to the `load-path', where Emacs
;;   looks when you load packages with `require' or `use-package'.
