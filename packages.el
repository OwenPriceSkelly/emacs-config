;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el
;; (package! hercules)
(package! evil-textobj-line)
(package! doom-themes
  :pin nil)
(package! org-roam
  :recipe (:host github :repo "jethrokuan/org-roam")
  :pin nil)
(package! company-org-roam
  :recipe (:host github :repo "jethrokuan/company-org-roam")
  :pin nil)
(package! mathpix.el
  :recipe (:host github :repo "jethrokuan/mathpix.el"))
(package! org-superstar
  :recipe (:host github :repo "integral-dw/org-superstar-mode"))
(package! org-pdftools
  :recipe (:host github :repo "fuxialexander/org-pdftools"))
(package! rainbow-mode)
