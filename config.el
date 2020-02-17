;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
(setq user-full-name "Owen Price-Skelly"
      user-mail-address "Owen.Price.Skelly@gmail.com"

      doom-theme 'doom-horizon
      doom-font (font-spec :family "monospace" :size 14)

      org-directory "~/org/"
      deft-directory org-directory

      which-key-side-window-location 'bottom
      display-line-numbers-type 'relative
      ranger-override-dired t


      doom-leader-key "SPC"
      doom-localleader-key ",")

;; - `load!' for loading external *.el files relative to this one
;; -----------------------------------------------------------------------------
;; - `use-package' `use-package!'for configuring packages
(use-package! treemacs
  :defer-incrementally t)
(use-package! hercules
  :demand t)
(use-package! evil-textobj-line
  :demand t)
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
        :desc "M-x"                "SPC"     #'execute-extended-command
        :desc "Ivy M-x"            ":"       #'counsel-M-x
        :desc "Search project"     "/"       #'+default/search-project)

  (map! :leader
        :desc "visual expand"      "v"       #'er/expand-region)

  (map! :leader
        (:prefix "b"
          :desc "Fallback Buffer" "h" #'+doom-dashboard/open))

  (map! :leader
        (:when (featurep! :ui treemacs)
          :desc "project sidebar" "0" #'treemacs-select-window))

  (map! :leader
        (:when (featurep! :ui workspaces)
          (:prefix "TAB"
            :desc "Switch workspace" "TAB" #'+workspace/switch-to
            "." nil)))

  (map! :leader
        (:when (featurep! :ui tabs)
          :n "]" #'centaur-tabs-forward-tab
          :n "[" #'centaur-tabs-backward-tab)))


;; -----------------------------------------------------------------------------
;; Misc. quality of life snippets
;; -----------------------------------------------------------------------------
(hercules-def
 :toggle-funs #'evil-multiedit-state
 :keymap 'evil-multiedit-state-map)

(hercules-def
 :toggle-funs #'evil-multiedit-insert-state
 :keymap 'evil-multiedit-insert-state-map)

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

;; (set-variables)
(personal/bind-keys)
