;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
(defun +my/variables ()
  (setq user-full-name "Owen Price-Skelly"
        user-mail-address "Owen.Price.Skelly@gmail.com"

        doom-theme 'doom-dark+

        doom-vibrant-lighter-modeline t
        doom-vibrant-brighter-modeline t
        ;; doom-acario-light-brighter-modeline t
        ;; doom-acario-light-comment-bg t

        doom-font (font-spec :family "monospace" :size 14)

        solaire-mode-auto-swap-bg t
        solaire-mode-remap-line-numbers t

        org-directory "~/.doom.d/org/"
        deft-directory org-directory
        deft-recursive t
        deft-use-filter-string-for-filename t
        org-bullets-bullet-list '( "▶" "◉" "▸" "○" "✸" "•" "★")

        which-key-side-window-location 'bottom
        which-key-sort-order 'which-key-key-order-alpha

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
            :desc "toggle org-roam buffer"         "r" #'org-roam
            :desc "find org-roam file"             "f" #'org-roam-find-file
            :desc "insert org-roam file"           "i" #'org-roam-insert
            :desc "show graph in browser"          "g" #'org-roam-show-graph
            :desc "find today's org-roam file"     "t" #'org-roam-today
            :desc "find tomorrow's org-roam file"  "T" #'org-roam-tomorrow
            :desc "find yesterday's org-roam file" "y" #'org-roam-yesterday))
    (map! :map org-roam-backlinks-mode-map
          "TAB"  #'org-next-link
          [tab]  #'org-next-link)
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

(defun +my/project-sidebar ()
  (interactive)
  (cond (featurep! :ui treemacs)
        (progn (require 'treemacs)
               (treemacs-select-window)
               (+treemacs/toggle)
               (+treemacs/toggle))

        (featurep! :ui neotree)
        (progn (require 'neotree)
               (if (neo-global--window-exists-p)
                   (neotree-refresh)
                 (neotree-dir (or (doom-project-root default-directory)))))))

;; -----------------------------------------------------------------------------
;; ----------------------------- Keybinding ------------------------------------
;; -----------------------------------------------------------------------------
(defun +my/keybindings ()
  (general-auto-unbind-keys)
  ;; Leader-key bindings
  (map! :leader
        :desc "Ivy M-x"                "SPC"           #'counsel-M-x
        :desc "M-x"                    ":"             #'execute-extended-command
        :desc "Search project"         "/"             #'+default/search-project
        :desc "Visual expand"          "v"             #'er/expand-region
        :desc "Project sidebar"        "0"             #'+my/treemacs-sidebar
        :desc "Undo Tree"              "U"             #'undo-tree-visualize

        (:when (featurep! :ui workspaces)
          (:prefix "TAB"
            :desc "Switch workspace"   "TAB"           #'+workspace/switch-to))

        (:when (featurep! :completion company)
          (:prefix "i"
            :desc "Show Completions"   "i"             #'+company/complete))


        (:prefix "w"
                                         "w"           #'other-window)

        (:prefix "b"
          :desc "Fallback buffer"        "h"           #'+doom-dashboard/open
          :desc "Messages buffer"        "m"           #'view-echo-area-messages
          :desc "ibuffer (other window)" "I"           #'ibuffer-other-window))

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
            "V"                                        #'iedit-show/hide-unmatched-lines)))

  (map! :m  "SPC j"                                    #'+evil/easymotion))

;; major mode bindings
(defun +my/python-config ()
  (map! :map python-mode-map
        :localleader
        (:prefix "e"
         "a"      nil
         "d"      nil
         "i"      nil
         "l"      nil
         "o"      nil
         "r"      nil
         "s"      nil
         "u"      nil)
        (:prefix ("p" . "pipenv")
          :desc "activate"    "a" #'pipenv-activate
          :desc "deactivate"  "d" #'pipenv-deactivate
          :desc "install"     "i" #'pipenv-install
          :desc "lock"        "l" #'pipenv-lock
          :desc "open module" "o" #'pipenv-open
          :desc "run"         "r" #'pipenv-run
          :desc "shell"       "s" #'pipenv-shell
          :desc "uninstall"   "u" #'pipenv-uninstall)
        (:prefix ("r" . "repl")
          :desc "default"              "r"              #'+python/open-repl
          :desc "ipython"              "i"              #'+python/open-ipython-repl
          :desc "jupyter"              "j"              #'+python/open-jupyter-repl)
        (:prefix ("s" . "skeletons")
          :desc "if"                   "i"              #'python-skeleton-if
          :desc "def"                  "d"              #'python-skeleton-def
          :desc "for"                  "f"              #'python-skeleton-for
          :desc "try"                  "t"              #'python-skeleton-try
          :desc "class"                "c"              #'python-skeleton-class
          :desc "while"                "w"              #'python-skeleton-while
          :desc "import"               "m"              #'python-skeleton-import))
  (setq python-fill-docstring-style 'django)

  (when (and (featurep! :tools lsp)
             (featurep! :lang python +lsp))
    (setq lsp-pyls-plugins-pylint-enabled t
          lsp-pyls-plugins-pycodestyle-enabled nil
          lsp-pyls-plugins-flake8-enabled nil
          lsp-pyls-plugins-pyflakes-enabled nil
          lsp-pyls-plugins-pylint-args ["--rcfile=~/.config/pylintrc"
                                        (concat "--disable="
                                                "print-statement,"
                                                "line-too-long,"
                                                "missing-module-doctring,"
                                                "bad-continuation,"
                                                "c-extension-no-member")])
    (when (featurep! :tools lsp +peek)
      (map! :map lsp-ui-peek-mode-map
            "C-j" #'lsp-ui-peek--select-next
            "C-h" #'lsp-ui-peek--select-prev-file
            "C-l" #'lsp-ui-peek--select-next-file
            "C-k" #'lsp-ui-peek--select-prev)
      (after! lsp-ui
        (lsp-ui-doc-enable t)
        (setq lsp-ui-doc-position 'top
              lsp-ui-doc-max-height 35
              lsp-ui-doc-max-width 35
              lsp-ui-doc-delay 0.4)))))


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
(+my/keybindings)
(after! python (+my/python-config))
