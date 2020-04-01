;;; $DOOMDIR/modules/personal/config/+lsp.el  -*- lexical-binding: t; -*-

(use-package! lsp-mode
  :config
  (setq lsp-pyls-plugins-pylint-enabled t
        lsp-pyls-plugins-pylint-args ["--disable=C,logging-format-interpolation,useless-return"]
        lsp-pyls-plugins-pycodestyle-enabled nil
        lsp-pyls-plugins-flake8-enabled nil
        lsp-pyls-plugins-pyflakes-enabled nil))

(use-package! lsp-ui
  :when (featurep! :tools lsp +peek)
  :config
  (lsp-ui-doc-enable t)
  (setq lsp-ui-doc-position 'top
        lsp-ui-doc-max-height 35
        lsp-ui-doc-max-width 35))
