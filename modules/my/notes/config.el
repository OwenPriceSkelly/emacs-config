
(use-package! org-roam
  :defer-incrementally t
  :commands
  (org-roam org-roam-find-file
            org-roam-insert
            org-roam-graph-show
            org-roam-today
            org-roam-tomorrow
            org-roam-yesterday
            org-roam-date)
  :init
  (setq org-directory "~/.org.d/"
        org-roam-directory org-directory
        org-roam-db-location (concat org-roam-directory "org-roam.db")
        org-roam-buffer-width 0.30
        org-roam-completion-fuzzy-match t
        org-roam-date-title-format "%Y-%m-%d-%A"
        org-roam-graph-max-title-length 50
        ;; org-roam-graph-viewer "open"
        org-roam-capture-templates '(("d" "default" entry (function org-roam-capture--get-point)
                                      "%?"
                                      :file-name "%<%Y-%m-%d>-${slug}"
                                      :head "#+TITLE: ${title}\n* Tags:\n** From: [[file:%F][${captured-from}]]\n** Category: \n* Description: "
                                      :unnarrowed t))
        org-capture-templates org-roam-capture-templates)
  :config
  (general-auto-unbind-keys)
  (map! :leader
        (:prefix ("n" . "notes")
          :desc "roam buffer"        "r"   #'org-roam
          :desc "find/new"           "f"   #'org-roam-find-file
          :desc "find/new"           "n"   #'org-roam-find-file
          :desc "insert/new"         "i"   #'org-roam-insert
          :desc "graph"              "g"   #'org-roam-graph-show
          :desc "today's file"       "t"   #'org-roam-today
          :desc "tomorrow's file"    "T"   #'org-roam-tomorrow
          :desc "yesterday's file"   "y"   #'org-roam-yesterday
          :desc "<date>'s file"      "d"   #'org-roam-date))
  (map! :map org-roam-backlinks-mode-map
        :m "TAB"  #'org-next-link
        :m [tab]  #'org-next-link)
  (org-roam-mode +1))

(use-package! org-roam-protocol
  :after org-protocol)

(use-package! company-org-roam
  :when (featurep! :completion company)
  :after org-roam
  :config
  (set-company-backend! 'org-mode '(company-org-roam company-yasnippet company-dabbrev)))
