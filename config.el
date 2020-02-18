;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
(defun personal/variables ()
  (setq user-full-name "Owen Price-Skelly"
        user-mail-address "Owen.Price.Skelly@gmail.com"

        doom-theme 'doom-horizon
        doom-font (font-spec :family "monospace" :size 14)

        org-directory "~/.doom.d/org/"
        deft-directory org-directory

        which-key-side-window-location 'bottom
        display-line-numbers-type 'relative
        ranger-override-dired t


        doom-leader-key "SPC"
        doom-localleader-key ","))

;; - `load!' for loading external *.el files relative to this one
;; -----------------------------------------------------------------------------
;; - `use-package' `use-package!'for configuring packages
(defun personal/use-packages ()
  (use-package! treemacs
    :defer-incrementally t)
  (use-package! hercules
    :demand t)
  (use-package! evil-textobj-line
    :demand t))
;; -----------------------------------------------------------------------------
;; - `after!' for running code after a package has loaded
;; -----------------------------------------------------------------------------
;; - `add-load-path!' for adding directories to the `load-path', where Emacs
;;   looks when you load packages with `require' or `use-package'.
;; -----------------------------------------------------------------------------
;; - Keybinding
;; -----------------------------------------------------------------------------
(defun personal/bind-keys ()
  (general-auto-unbind-keys)
  (map! :leader
        :desc "Ivy M-x"                "SPC"     #'counsel-M-x
        :desc "M-x"            ":"       #'execute-extended-command
        :desc "Search project"     "/"       #'+default/search-project)

  (map! (:when (featurep! :completion ivy)
          :map ivy-minibuffer-map
          (:prefix "C-c"
            :desc "Search/Replace buffer" "e" #'+ivy/woccur)))

  (map! :leader
        :desc "Visual expand"      "v"       #'er/expand-region)

  (map! :leader
        (:prefix "b"
          :desc "Fallback buffer" "h" #'+doom-dashboard/open))

  (map! :leader
        (:when (featurep! :ui treemacs)
          :desc "Project sidebar" "0" #'treemacs-select-window))

  (map! :leader
        (:when (featurep! :ui workspaces)
          (:prefix "TAB"
            :desc "Switch workspace" "TAB" #'+workspace/switch-to
            "." nil)))

  (map! :after python
        :map python-mode-map
        :localleader
        (:prefix ("r" . "repl")
          :desc "python" "p" #'+python/open-repl
          :desc "python" "i" #'+python/open-ipython-repl
          :desc "python" "j" #'+python/open-jupyter-repl)
        (:prefix ("=" . "format")
          :desc "buffer" "=" #'+format/buffer))

  (map! :after ein
        :map ein:notebook-mode-map
        :localleader
        "," #'+ein/hydra/body)

  (map! :leader
        (:when (featurep! :ui tabs)
          :n "]" #'centaur-tabs-forward-tab
          :n "[" #'centaur-tabs-backward-tab))

  ;;TODO: toggle-funs vs show-funs??
  (hercules-def
   :toggle-funs #'evil-multiedit-state
   :keymap 'evil-multiedit-state-map)

  (hercules-def
   :toggle-funs #'evil-multiedit-insert-state
   :keymap 'evil-multiedit-insert-state-map))

;; -----------------------------------------------------------------------------
;; Misc. quality of life snippets
;; -----------------------------------------------------------------------------

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
