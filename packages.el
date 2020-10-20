;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el
(package! evil-textobj-line
  :pin "3d401b6831bdbeec967ec8e64177a8950251e812")
(package! mathpix.el
  :recipe (:host github :repo "jethrokuan/mathpix.el")
  :pin "52bc9a6acd34d2282555ac576e905af3c5a0e767")
(package! visual-basic-mode
  :recipe (:host github :repo "emacsmirror/visual-basic-mode") :pin "79689e97d9dc0f90388c4111c5409d544a173631")
(package! lsp-jedi :pin "10c782261b20ad459f5d2785592c4f46f7088126")
(package! org-roam-server)

;; existing packages pinned separately
(when (featurep! :tools lsp +eglot)
  (unpin! eglot))
(unpin! org-roam org-roam-server doom-themes)
(package! darktooth-theme)
(package! creamsody-theme)
;; (package! key-chord)

;; (package! tree-sitter
;;   :recipe (:host github :repo "ubolonton/emacs-tree-sitter" :files ("lisp/*.el" )))
;; (package! tree-sitter-langs
;;   :recipe (:host github :repo "ubolonton/emacs-tree-sitter" :files ("langs/*.el" "langs/queries")))
