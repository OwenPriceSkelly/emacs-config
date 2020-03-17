;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
(defun +my/variables ()
  (setq user-full-name "Owen Price-Skelly"
        user-mail-address "Owen.Price.Skelly@gmail.com"

        doom-theme 'doom-moonlight
        doom-font (font-spec :family "monospace" :size 14)
        solaire-mode-auto-swap-bg t
        solaire-mode-remap-line-numbers t

        org-directory "~/.doom.d/org/"
        deft-directory org-directory
        deft-recursive t
        deft-use-filter-string-for-filename t
        org-bullets-bullet-list '( "▶" "◉" "▸" "○" "✸" "•" "★")

        which-key-side-window-location 'bottom
        display-line-numbers-type 'relative
        ranger-override-dired t

        iedit-occurrence-context-lines 1
        fill-column 88
        which-key-max-description-length nil

        undo-tree-visualizer-diff nil

        doom-leader-key "SPC"
        doom-leader-alt-key "C-SPC"
        doom-localleader-key ","
        doom-localleader-alt-key "C-,"))

;; -----------------------------------------------------------------------------
;; --------------------------- use-package configs -----------------------------
;; -----------------------------------------------------------------------------
(defun +my/use-packages ()
  (use-package! org-roam
    :commands
    (org-roam org-roam-find-file org-roam-insert
              org-roam-show-graph org-roam-today
              org-roam-tomorrow org-roam-yesterday)
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

(defun +my/treemacs-sidebar ()
  "Hacky; Found myself selecting window after changing project and needing to
  close and reopen for treemacs to update to the next project's directory"
  (interactive)
  (require 'treemacs)
  (treemacs-select-window)
  (+treemacs/toggle)
  (+treemacs/toggle))


(defun +my/neotree-sidebar ()
  (interactive)
  (require 'neotree)
  (if (neo-global--window-exists-p)
      (neotree-refresh)
    (neotree-dir (or (doom-project-root
                      default-directory)))))


;; -----------------------------------------------------------------------------
;; ----------------------------- Keybinding ------------------------------------
;; -----------------------------------------------------------------------------
(defun +my/bind-keys ()
  (general-auto-unbind-keys)
  ;; Leader-key bindings
  (map! :leader
        :desc "Ivy M-x"                "SPC"           #'counsel-M-x
        :desc "M-x"                    ":"             #'execute-extended-command
        :desc "Search project"         "/"             #'+default/search-project
        :desc "Visual expand"          "v"             #'er/expand-region
        :desc "Project sidebar"        "0"             #'+treemacs/find-file
        :desc "Undo Tree"              "U"             #'undo-tree-visualize

        (:when (featurep! :ui workspaces)
          (:prefix "TAB"
            :desc "Switch workspace"   "TAB"           #'+workspace/switch-to))

        (:when (featurep! :completion company)
          (:prefix "i"
            :desc "Show Completions"   "i"             #'+company/complete))


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
            "n"                                        #'evil-multiedit-next
            "N"                                        #'evil-multiedit-prev
            "C-n"                                      #'evil-multiedit-match-and-next
            "C-S-n"                                    #'evil-multiedit-match-and-prev
            "V"                                        #'iedit-show/hide-unmatched-lines))))

;; major mode bindings
(defun +my/python-config ()
  (map! :map python-mode-map
        :localleader
        (:prefix ("e" . "[pip]env"))
        (:prefix ("r" . "repl")
          :desc "default"     "r"                          #'+eval/open-repl-other-window
          :desc "ipython"     "i"                          #'+python/open-ipython-repl
          :desc "jupyter"     "j"                          #'+python/open-jupyter-repl
          :desc "send region" "s"                          #'+eval/send-region-to-repl)
        (:prefix ("s" . "skeletons")
          :desc "if"     "i"  #'python-skeleton-if
          :desc "def"    "d"  #'python-skeleton-def
          :desc "for"    "f"  #'python-skeleton-for
          :desc "try"    "t"  #'python-skeleton-try
          :desc "class"  "c"  #'python-skeleton-class
          :desc "while"  "w"  #'python-skeleton-while
          :desc "import" "m"  #'python-skeleton-import))
  (setq python-fill-docstring-style 'django
        python-skeleton-autoinsert t)
  (when (featurep! :tools lsp)
    (map! :map lsp-ui-peek-mode-map
          "C-j" #'lsp-ui-peek--select-next
          "C-h" #'lsp-ui-peek--select-prev-file
          "C-l" #'lsp-ui-peek--select-next-file
          "C-k" #'lsp-ui-peek--select-prev)
    (setq lsp-pyls-plugins-pylint-enabled t
          lsp-pyls-plugins-pycodestyle-enabled nil
          lsp-pyls-plugins-flake8-enabled nil
          lsp-pyls-plugins-pylint-args '("--rcfile=~/.config/pylintrc"))))

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

(+my/variables)
(+my/use-packages)
(+my/bind-keys)
(after! python (+my/python-config))
