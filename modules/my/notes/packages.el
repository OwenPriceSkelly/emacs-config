;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/modules/notes/zettel/packages.el

(package! company-org-roam
  :recipe (:host github :repo "jethrokuan/org-roam")
  :pin nil)

(when (featurep! :completion company)
  (package! company-org-roam
      :recipe (:host github :repo "jethrokuan/company-org-roam")
      :pin nil))
