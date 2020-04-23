;;; $DOOMDIR/modules/personal/config/+keybindings.el -*- lexical-binding: t; -*-

(defun +toplevel-bindings ()
  (map!
   (:when (featurep! :completion company)
     :i "C-i"                                       #'+company/complete)
   ;; multiedit
   (:when (featurep! :editor multiple-cursors)
      :nv "R"                                       #'evil-multiedit-match-all
      :n "C-n"                                      #'evil-multiedit-match-symbol-and-next
      :n "C-S-n"                                    #'evil-multiedit-match-symbol-and-prev
      :v "C-n"                                      #'evil-multiedit-match-and-next
      :v "C-S-n"                                    #'evil-multiedit-match-and-prev
      :nv "C-M-n"                                   #'evil-multiedit-restore
      (:after evil-multiedit
        (:map evil-multiedit-state-map
          "n"                                       #'evil-multiedit-next
          "N"                                       #'evil-multiedit-prev
          "C-n"                                     #'evil-multiedit-match-and-next
          "C-S-n"                                   #'evil-multiedit-match-and-prev
          "V"                                       #'iedit-show/hide-unmatched-lines))
      ;; multiple cursors
      (:prefix "gz"
        :nv "m"                                     #'evil-mc-make-all-cursors
        :nv "n"                                     #'evil-mc-make-and-goto-next-match
        :nv "N"                                     #'evil-mc-make-and-goto-prev-match
        :nv "d"                                     #'evil-mc-make-and-goto-next-cursor
        :nv "D"                                     #'evil-mc-make-and-goto-last-cursor
        :nv "j"                                     #'evil-mc-make-cursor-move-next-line
        :nv "k"                                     #'evil-mc-make-cursor-move-prev-line
        :nv "p"                                     #'evil-mc-make-and-goto-prev-cursor
        :nv "P"                                     #'evil-mc-make-and-goto-first-cursor
        :nv "q"                                     #'evil-mc-undo-all-cursors
        :nv "t"                                     #'+multiple-cursors/evil-mc-toggle-cursors
        :nv "u"                                     #'evil-mc-undo-last-added-cursor
        :nv "z"                                     #'+multiple-cursors/evil-mc-make-cursor-here
        :v  "I"                                     #'evil-mc-make-cursor-in-visual-selection-beg
        :v  "A"                                     #'evil-mc-make-cursor-in-visual-selection-end))
    ;; wgrep
   (:when (featurep! :completion ivy)
      (:map ivy-minibuffer-map
        (:prefix "C-c"
          :desc "Edit and replace"              "e" #'+ivy/woccur)))
   (:when (featurep! :tools lsp +peek)
      :map lsp-ui-peek-mode-map
      "C-j"                                         #'lsp-ui-peek--select-next
      "C-h"                                         #'lsp-ui-peek--select-prev-file
      "C-l"                                         #'lsp-ui-peek--select-next-file
      "C-k"                                         #'lsp-ui-peek--select-prev)))

(defun +localleader-key-bindings ()
  (map! :localleader
        (:when (featurep! :lang org)
          (:map org-mode-map
            :desc "Sort"     "S"                    #'org-sort
            :desc "preview fragments" "L"           #'org-latex-preview
            :desc "toggle pretty entities" "p"      #'+org-pretty-mode))
        (:when (featurep! :lang python)
          (:map python-mode-map
            (:prefix ("p" . "pipenv")
              :desc "activate"    "a"               #'pipenv-activate
              :desc "deactivate"  "d"               #'pipenv-deactivate
              :desc "install"     "i"               #'pipenv-install
              :desc "lock"        "l"               #'pipenv-lock
              :desc "open module" "o"               #'pipenv-open
              :desc "run"         "r"               #'pipenv-run
              :desc "shell"       "s"               #'pipenv-shell
              :desc "uninstall"   "u"               #'pipenv-uninstall)
            (:prefix ("r" . "repl")
              :desc "default"              "r"      #'+python/open-repl
              :desc "ipython"              "i"      #'+python/open-ipython-repl
              :desc "jupyter"              "j"      #'+python/open-jupyter-repl)
            "e" nil))))


(defun +leader-key-bindings ()
  (map! (:leader
          :desc "Search project"         "/"        #'+default/search-project
          :desc "Visual expand"          "v"        #'er/expand-region

          (:prefix ("w" . "window")
            :desc "Switch to last window" "w"       #'evil-window-mru)

          (:prefix ("b" . "buffer")
            :desc "Fallback buffer"        "h"      #'+doom-dashboard/open
            :desc "Messages buffer"        "m"      #'view-echo-area-messages
            :desc "ibuffer (other window)" "I"      #'ibuffer-other-window)

          (:when (featurep! :emacs undo +tree)
            :desc "Undo Tree"              "U"        #'undo-tree-visualize)
         
          (:when (featurep! :ui treemacs)
            :desc "Project sidebar"        "0"      #'+treemacs/toggle)

          (:when (featurep! :ui workspaces)
            (:prefix "TAB"
              :desc "Main workspace"       "`"      #'+workspace/switch-to-0
              :desc "Previous workspace"   "TAB"    #'+workspace/other))

          (:when (featurep! :completion ivy)
            :desc "Ivy M-x"                "SPC"    #'counsel-M-x)

          (:when (featurep! :lang org +roam)
            (:prefix ("n" . "notes")
              :desc "roam buffer"        "r"        #'org-roam
              :desc "find/new"           "f"        #'org-roam-find-file
              :desc "find/new"           "n"        #'org-roam-find-file
              :desc "insert/new"         "i"        #'org-roam-insert
              :desc "today's file"       "t"        #'org-roam-dailies-today
              :desc "tomorrow's file"    "T"        #'org-roam-dailies-tomorrow
              :desc "yesterday's file"   "y"        #'org-roam-dailies-yesterday
              :desc "<date>'s file"      "d"        #'org-roam-dailies-date
              :desc "mathpix.el"         "m"        #'mathpix-screenshot
              (:prefix ("g" . "graph")
                :desc "graph all"       "g" #'org-roam-graph-show
                :desc "graph connected" "c" #'org-roam-graph-show-connected-component))))))

(general-auto-unbind-keys)
(+toplevel-bindings)
(+localleader-key-bindings)
(+leader-key-bindings)
