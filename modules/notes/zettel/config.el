(use-package! org-roam
  :commands
  (org-roam
   org-roam-today
   org-roam-switch-to-buffer
   org-roam-show-graph
   org-roam-find-file
   org-roam-insert)

  :init
  (setq org-directory "~/.doom.d/org/"
        org-roam-directory org-directory
        org-roam-buffer-width 0.33
        org-roam-graph-node-shape "")
  (map! :leader
        (:prefix ("z" . "zettel")
          :desc "roam buffer"         "r" #'org-roam
          :desc "find"             "f" #'org-roam-find-file
          :desc "insert"           "i" #'org-roam-insert
          :desc "graph"          "g" #'org-roam-show-graph
          ;; :desc "capture"                        "x" #'org-roam-capture
          :desc "find today's org-roam file"     "t" #'org-roam-today
          :desc "find tomorrow's org-roam file"  "T" #'org-roam-tomorrow
          :desc "find yesterday's org-roam file" "y" #'org-roam-yesterday
          :desc "find <date> org-roam file"      "d" #'org-roam-date))
  (map! :map org-roam-backlinks-mode-map
        "TAB"  #'org-next-link
        [tab]  #'org-next-link)

  :config
  (defun org-roam--build-graph ()
    "Overwrites the function to build graphviz string"
    (org-roam--db-ensure-built)
    (org-roam--with-temp-buffer
      (let* ((node-query `[:select [file titles]
                                   :from titles
                                   ,@(org-roam--graph-expand-matcher 'file t)])
             (nodes (org-roam-sql node-query))
             (edges-query `[:select :distinct [file-to file-from]
                                    :from file-links
                                    ,@(org-roam--graph-expand-matcher 'file-to t)
                                    ,@(org-roam--graph-expand-matcher 'file-from t t)])
             (edges (org-roam-sql edges-query)))
        (insert "digraph \"org-roam\" {\n")
        (dolist (option org-roam-graphviz-extra-options)
          (insert (concat (car option)
                          "="
                          (cdr option)
                          ";\n")))
        (dolist (node nodes)
          (let* ((file (xml-escape-string (car node)))
                 (title (or (caadr node)
                            (org-roam--path-to-slug file)))
                 (shortened-title (s-truncate org-roam-graph-max-title-length title)))
            (insert
             (format "  \"%s\" [label=\"%s\", shape=%s, URL=\"org-protocol://roam-file?file=%s\", tooltip=\"%s\"];\n"
                     file
                     (s-replace "\"" "\\\"" shortened-title)
                     org-roam-graph-node-shape
                     (url-hexify-string file)
                     (xml-escape-string title)))))
        (dolist (edge edges)
          (insert (format "  \"%s\" -- \"%s\";\n"
                          (xml-escape-string (car edge))
                          (xml-escape-string (cadr edge)))))
        (insert "}")
        (buffer-string))))
  (org-roam-mode +1))

(use-package! org-roam-protocol
  :after org-protocol)

(use-package company-org-roam
  :when (featurep! :completion company)
  :after org-roam
  :config
  (set-company-backend! 'org-mode '(company-org-roam company-yasnippet company-dabbrev)))
