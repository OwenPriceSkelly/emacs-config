;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
(defun +my/variables ()
  (setq user-full-name "Owen Price-Skelly"
        user-mail-address "Owen.Price.Skelly@gmail.com"

        doom-theme 'doom-city-lights
        doom-font (font-spec :family "monospace" :size 14)
        solaire-mode-auto-swap-bg t
        solaire-mode-remap-line-numbers t

        org-directory "~/.doom.d/org/"
        org-agenda-files '("~/.doom.d/"
                           "~/.doom.d/org/"
                           "~/.doom.d/org/roam/work/")

        which-key-side-window-location 'bottom
        which-key-sort-order 'which-key-key-order-alpha

        display-line-numbers-type 'relative

        ranger-override-dired t

        iedit-occurrence-context-lines 1
        fill-column 88
        which-key-max-description-length nil

        undo-tree-visualizer-diff nil
        +workspaces-on-switch-project-behavior t

        doom-leader-key "SPC"
        doom-leader-alt-key "C-SPC"
        doom-localleader-key ","
        doom-localleader-alt-key "C-,"))

(defun +my/org-config ()
  (setq deft-directory org-directory
        deft-recursive t
        deft-use-filter-string-for-filename t
        deft-default-extension "org"
        org-bullets-bullet-list '( "▶" "◉" "▸" "○" "✸" "•" "★")
        org-todo-keywords '((sequence "[ ](t)"     ; A subtask
                                      "[~](p!)"    ; Subtask currently in-progress
                                      "[*](w@)"    ; Subtask is being held up or paused
                                      "|"
                                      "[X](d!)")   ; Subtask was completed
                            (sequence "TODO(T)"    ; A task that needs doing & is ready to do
                                      ;; "START(S)"   ; Start a larger task that cannot be completed in one step
                                      "PROG(P!)"   ; Mark a task as in-progress
                                      "WAIT(W@)"   ; Something is holding up this task or it is paused
                                      "|"
                                      "DONE(D!)"   ; Task successfully completed
                                      "DROP(K@)")) ; Task was cancelled or is no longer applicable
        org-todo-keyword-faces '(("[~]"   . +org-todo-active)
                                 ("[*]"   . +org-todo-onhold)
                                 ;; ("START" . org-agenda-date)
                                 ("PROG"  . +org-todo-active)
                                 ("WAIT"  . +org-todo-onhold)))
  (map! :map org-mode-map
        :localleader
        :desc "Sort"     "S"     #'org-sort))

;; -----------------------------------------------------------------------------
;; --------------------------- use-package configs -----------------------------
;; -----------------------------------------------------------------------------
(defun +my/use-packages ()

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
;; -----------------------------------------------------------------------------
;; ----------------------------- Keybinding ------------------------------------
;; -----------------------------------------------------------------------------
(defun +my/keybindings ()
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
  (setq python-fill-docstring-style 'django)
  (map! :map python-mode-map
        :localleader
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
          :desc "jupyter"              "j"              #'+python/open-jupyter-repl))
  (when (and (featurep! :tools lsp)
             (featurep! :lang python +lsp))
    (setq lsp-pyls-plugins-pylint-enabled t
          lsp-pyls-plugins-pylint-args ["--disable=C,logging-format-interpolation,useless-return"]
          lsp-pyls-plugins-pycodestyle-enabled nil
          lsp-pyls-plugins-flake8-enabled nil
          lsp-pyls-plugins-pyflakes-enabled nil)

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
(defun +my/persist-frame-size ()
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
  (add-hook 'kill-emacs-hook  #'save-frame-dimensions))

(general-auto-unbind-keys)

(after! python (+my/python-config))
(after! org (+my/org-config))

(+my/persist-frame-size)
(+my/variables)
(+my/use-packages)
(+my/keybindings)
