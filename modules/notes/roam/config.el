(defun +org-roam-show-graph (&optional prefix)
  "Generate and displays the Org-roam graph using `org-roam-graph-viewer'.
If PREFIX, then the graph is generated but the viewer is not invoked."
  (interactive "P")
  (declare (indent 0))
  (unless org-roam-graphviz-executable
    (user-error "Can't find graphviz executable.  Please check if it is in your path"))
  (let ((temp-dot (expand-file-name "graph.dot" temporary-file-directory))
        (temp-graph (expand-file-name "graph.svg" temporary-file-directory))
        (graph (+org-roam--build-graph)))
    (with-temp-file temp-dot
      (insert graph))
    (call-process org-roam-graphviz-executable nil 0 nil temp-dot "-Tsvg" "-o" temp-graph)
    (unless prefix
      (if (and org-roam-graph-viewer (executable-find org-roam-graph-viewer))
          (call-process org-roam-graph-viewer nil 0 nil temp-graph)
        (view-file temp-graph)))))

(defun +org-roam--build-graph ()
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
      (insert "graph \"org-roam\" {\n")
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

(use-package! org-roam
  :defer-incrementally t
  :commands
  (org-roam org-roam-find-file
            org-roam-insert
            org-roam-show-graph
            org-roam-today
            org-roam-tomorrow
            org-roam-yesterday
            org-roam-date)
  :init
  (setq org-directory "~/.doom.d/org/"
        org-roam-directory org-directory
        org-roam-buffer-width 0.30
        org-roam-fuzzy-match t
        org-roam-date-title-format "%Y-%m-%d-%A"
        org-roam-graph-max-title-length 50
        org-roam-graph-viewer "open "
        org-roam-capture-templates '(("d" "default" entry (function org-roam--capture-get-point)
                                      "%?"
                                      :file-name "%<%Y-%m-%d>-${slug}"
                                      :head "#+TITLE: ${title}\n*"
                                      :unnarrowed t
                                      :immediate-finish t)))

  (map! :leader
        (:prefix ("z" . "zettel")
          :desc "roam buffer"      "r"    #'org-roam
          :desc "find/create"      "f"    #'org-roam-find-file
          :desc "insert/create"    "i"    #'org-roam-insert
          :desc "graph"            "g"    #'+org-roam-show-graph
          :desc "today's file"     "t"    #'org-roam-today
          :desc "tomorrow's file"  "T"    #'org-roam-tomorrow
          :desc "yesterday's file" "y"    #'org-roam-yesterday
          :desc "<date> file"      "d"    #'org-roam-date))
  (map! :map org-roam-backlinks-mode-map
        "TAB"  #'org-next-link
        [tab]  #'org-next-link)

  :config
  (general-auto-unbind-keys)
  (map! :leader
        (:prefix ("r" . "roam")
          :desc "roam buffer"      "r"    #'org-roam
          :desc "find/create"      "f"    #'org-roam-find-file
          :desc "insert/create"    "i"    #'org-roam-insert
          :desc "graph"            "g"    #'+org-roam-show-graph
          :desc "today's file"     "t"    #'org-roam-today
          :desc "tomorrow's file"  "T"    #'org-roam-tomorrow
          :desc "yesterday's file" "y"    #'org-roam-yesterday
          :desc "<date> file"      "d"    #'org-roam-date))
  (map! :map org-roam-backlinks-mode-map
        "TAB"  #'org-next-link
        [tab]  #'org-next-link)
  (org-roam-mode +1))

(use-package! org-roam-protocol
  :after org-protocol)

(use-package! company-org-roam
  :when (featurep! :completion company)
  :after org-roam
  :config
  (set-company-backend! 'org-mode '(company-org-roam company-yasnippet company-dabbrev)))
