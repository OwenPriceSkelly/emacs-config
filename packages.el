;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el
(package! evil-textobj-line
  :pin "3d401b6831bdbeec967ec8e64177a8950251e812")
(package! mathpix.el
  :recipe (:host github :repo "jethrokuan/mathpix.el")
  :pin "02016ca4aee9ffce32e730372f45de35c00a3657")
;; (package! zoom :pin "a373e7eed59ad93315e5ae88c816ca70404d2d34")
(package! visual-basic-mode
  :recipe (:host github :repo "emacsmirror/visual-basic-mode") :pin "79689e97d9dc0f90388c4111c5409d544a173631")
(package! lsp-jedi :pin "10c782261b20ad459f5d2785592c4f46f7088126")
(package! org-roam-server  :pin "8d1d143f5db415864c008b8e42e4d92279df9a81")

;; existing packages pinned separately
(unpin! org-roam)
(unpin! doom-themes)
(unpin! eglot)
(package! darktooth-theme)
(package! creamsody-theme)

;; (package! tree-sitter
;;   :recipe (:host github :repo "ubolonton/emacs-tree-sitter" :files ("lisp/*.el" )))
;; (package! tree-sitter-langs
;;   :recipe (:host github :repo "ubolonton/emacs-tree-sitter" :files ("langs/*.el" "langs/queries")))
