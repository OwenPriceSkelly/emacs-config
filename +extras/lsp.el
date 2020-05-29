;; ;;; $DOOMDIR/+extras/lsp.el  -*- lexical-binding: t; -*-
(use-package! lsp
  :defer-incrementally lsp-mode
  :init
  (setq lsp-pyls-plugins-pylint-enabled t
        lsp-pyls-plugins-pylint-args ["--disable=C,line-too-long,logging-format-interpolation,useless-return"]
        lsp-pyls-plugins-pycodestyle-enabled nil
        lsp-pyls-plugins-flake8-enabled nil
        lsp-pyls-plugins-pyflakes-enabled nil))

(use-package! lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :init
  (setq lsp-ui-doc-enable t
        lsp-ui-sideline-show-hover t
        lsp-ui-doc-position 'top
        lsp-ui-doc-max-height 35
        lsp-ui-doc-max-width 35))
