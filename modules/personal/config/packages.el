;; -*- no-byte-compile: t; -*-
;;; personal/config/packages.el

(package! lsp-mode)
(package! lsp-ui)
(package! org)
(package! org-roam
  :recipe (:host github :repo "jethrokuan/org-roam") :pin nil)
(package! company-org-roam
  :recipe (:host github :repo "jethrokuan/company-org-roam") :pin nil)
