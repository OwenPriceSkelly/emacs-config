;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el
(package! hercules)
(package! evil-textobj-line)
(package! doom-themes :pin nil)
(package! org-roam
  :recipe (:host github :repo "jethrokuan/org-roam"))
(package! company-org-roam
  :recipe (:host github :repo "jethrokuan/company-org-roam"))
(package! mathpix.el
  :recipe (:host github :repo "jethrokuan/mathpix.el"))
