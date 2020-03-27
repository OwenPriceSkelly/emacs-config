;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/modules/notes/zettel/packages.el
(package! org-roam :pin nil)
(when (featurep! :completion +company) 
  (package! company-org-roam
      :recipe (:host github :repo "jethrokuan/company-org-roam")
      :pin "eb01e12174"))
