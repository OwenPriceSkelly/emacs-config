;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el
;; (package! hercules)
(package! evil-textobj-line)
(package! doom-themes :pin nil)
(package! org-roam
  :recipe (:host github :repo "jethrokuan/org-roam")
  :pin "1b13c42")
(package! company-org-roam
  :recipe (:host github :repo "jethrokuan/company-org-roam")
  :pin nil)
(package! mathpix.el
  :recipe (:host github :repo "jethrokuan/mathpix.el"))
