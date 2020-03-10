;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
(defun personal/variables ()
  (setq user-full-name "Owen Price-Skelly"
        user-mail-address "Owen.Price.Skelly@gmail.com"

        doom-theme 'doom-gruvbox
        doom-font (font-spec :family "monospace" :size 14)

        org-directory "~/.doom.d/org/"
        deft-directory org-directory
        deft-recursive t
        deft-use-filter-string-for-filename t
        org-bullets-bullet-list '( "▶" "◉" "○" "▸" "✸" "•" "★")

        which-key-side-window-location 'bottom
        display-line-numbers-type 'relative
        ranger-override-dired t

        iedit-occurrence-context-lines 1
        fill-column 88
        which-key-max-description-length nil

        doom-leader-key "SPC"
        doom-leader-alt-key "C-SPC"
        doom-localleader-key ","
        doom-localleader-alt-key "C-,"))


(defun personal/use-packages ()
  (use-package! treemacs
    :defer-incrementally t)
  (use-package! org-roam
    :commands
    (org-roam org-roam-find-file org-roam-insert)
    :init
    (setq org-roam-directory (concat org-directory "roam/work/")
          org-roam-buffer-width 0.30)
    (map! :leader
          :prefix "n"
          (:prefix ("r" . "roam")
            :desc "toggle org-roam buffer" "r"         #'org-roam
            :desc "find org-roam file"   "f"           #'org-roam-find-file
            :desc "insert org-roam file" "i"           #'org-roam-insert
            :desc "show graph in browser"  "g"         #'org-roam-show-graph
            :desc "find today's org-roam file"  "t"    #'org-roam-today
            :desc "find tomorrow's org-roam file"  "T" #'org-roam-tomorrow
            :desc "find yesterday's org-roam file" "y" #'org-roam-yesterday))
    :config
    (org-roam-mode +1))

  (use-package! expand-region
    :config
    (setq expand-region-contract-fast-key "V"))

  (use-package! evil-textobj-line
    :demand t))
;; -----------------------------------------------------------------------------
;; - Keybinding
;; -----------------------------------------------------------------------------
(defun personal/bind-keys ()
  (general-auto-unbind-keys)
  ;; Leader-key bindings
  (map! :leader
        :desc "Ivy M-x"                "SPC"           #'counsel-M-x
        :desc "M-x"                    ":"             #'execute-extended-command
        :desc "Search project"         "/"             #'+default/search-project
        :desc "Visual expand"          "v"             #'er/expand-region

        (:when (featurep! :ui treemacs)
          :desc "Project sidebar"      "0"             #'treemacs-select-window)
        (:when (featurep! :ui workspaces)
          (:prefix "TAB"
            :desc "Switch workspace"   "TAB"           #'+workspace/switch-to))

        (:prefix "w"
          "w"                                          #'other-window)


        (:prefix "b"
          :desc "Fallback buffer"        "h"           #'+doom-dashboard/open
          :desc "Messages buffer"        "m"           #'view-echo-area-messages
          :desc "ibuffer (other window)" "I"           #'ibuffer-other-window))

  (map! (:when (featurep! :ui tabs)
          :n "L"                                       #'centaur-tabs-forward-tab
          :n "C-S-l"                                   #'centaur-tabs-forward-tab-other-window
          :n "H"                                       #'centaur-tabs-backward-tab
          :n "C-S-h"                                   #'centaur-tabs-backward-tab-other-window))

  ;; Search/replace bindings
  (map! :map ivy-minibuffer-map
        (:prefix "C-c"
          :desc "Edit and replace"              "e"    #'+ivy/woccur))

  ;; evil-multiedit
  (map! :nv "R"                                        #'evil-multiedit-match-all
        :n "C-n"                                       #'evil-multiedit-match-symbol-and-next
        :n "C-S-n"                                     #'evil-multiedit-match-symbol-and-prev
        :v "C-n"                                       #'evil-multiedit-match-and-next
        :v "C-S-n"                                     #'evil-multiedit-match-and-prev
        :nv "C-M-n"                                    #'evil-multiedit-restore
        (:after evil-multiedit
          (:map evil-multiedit-state-map
            "n"                                             #'evil-multiedit-next
            "N"                                             #'evil-multiedit-prev
            "C-n"                                           #'evil-multiedit-match-and-next
            "C-S-n"                                         #'evil-multiedit-match-and-prev
            "V"                                             #'iedit-show/hide-unmatched-lines))))

;; major mode bindings
(defun personal/python-config ()
  (map! :map python-mode-map
        :localleader
        (:prefix ("e" . "[pip]env"))
        (:prefix ("r" . "repl")
          :desc "default" "r"                          #'+eval/open-repl-other-window
          ;; :desc "python"  "p"                       #'+python/open-repl
          :desc "ipython" "i"                          #'+python/open-ipython-repl
          :desc "jupyter" "j"                          #'+python/open-jupyter-repl
          :desc "send to repl" "s"                     #'+eval/send-region-to-repl)))


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

(add-hook 'kill-emacs-hook                             #'save-frame-dimensions)

(personal/variables)
(personal/use-packages)
(personal/bind-keys)
(after! python
  (personal/python-config))

;; - `load!' for loading external *.el files relative to this one
;; -----------------------------------------------------------------------------
;; - `use-package' `use-package!'for configuring packages
;; -----------------------------------------------------------------------------
;; - `after!' for running code after a package has loaded
;; -----------------------------------------------------------------------------
;; - `add-load-path!' for adding directories to the `load-path', where Emacs
;;   looks when you load packages with `require' or `use-package'.
