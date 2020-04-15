;;; ~/.doom.d/+extras/doom-gruvbox-light-theme.el -*- lexical-binding: t; -*-
;; doom-gruvbox-light-theme.el --- inspired by morhetz Gruvbox -*- no-byte-compile: t; -*-
(require 'doom-themes)

;; Compiler pacifier
(defvar modeline-bg)

;;
(defgroup doom-gruvbox-light-theme nil
  "Options for doom-gruvbox-light."
  :group 'doom-themes)

(defcustom doom-gruvbox-light-brighter-comments nil
  "If non-nil, comments will be highlighted in more vivid colors."
  :group 'doom-gruvbox-light-theme
  :type 'boolean)

(defcustom doom-gruvbox-light-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line. Can be an integer to
determine the exact padding."
  :group 'doom-gruvbox-light-theme
  :type '(choice integer boolean))

;;
(def-doom-theme doom-gruvbox-light
  "Dark theme with pastel 'retro groove' colors."

  ;; name        gui       256       16
  ((bg         '("#fbf1c7" "#fbf1c7" "brightwhite"))
   (bg-alt     '("#f2e5bc" "#f2e5bc" "brightwhite"))
   (fg         '("#282828" "#282828"  nil))
   (fg-alt     '("#3c3836" "#3c3836" "brightblack"))

   (base0      '("#f9f5d7" "#f9f5d7" "brightwhite"))
   (base1      '("#fbf1c7" "#fbf1c7" "brightwhite"))
   (base2      '("#f2e5bc" "#f2e5bc" "brightwhite"))
   (base3      '("#d5c4a1" "#d5c4a1" "brightblack"))
   (base4      '("#bdae93" "#bdae93" "brightwhite"))
   (accent     '("#a89984" "#a89984" "brightblack"))
   (base5      '("#665c54" "#665c54" "brightblack"))
   (base6      '("#504945" "#504945" "brightblack"))
   (base7      '("#3c3836" "#3c3836" "brightblack"))
   (base8      '("#282828" "#282828"  "black"))

   (red        '("#cc241d" "#cc241d" "red"))
   (green      '("#b8bb26" "#b8bb26" "green"))
   (olive      '("#98971a" "#98971a" "green"))
   (yellow     '("#fabd2f" "#fabd2f" "yellow"))
   (blue       '("#076678" "#076678" "cyan"))
   (violet     '("#8f3f71" "#8f3f71" "brightmagenta"))
   (magenta     '("#b16286" "#b16286" "magenta"))
   (aqua       '("#689d6a" "#689d6a" "green"))
   (grey       '("#928374" "#928374" "brightblack"))
   (orange     '("#d65d0e" "#d65d0e" "orange"))


   (cyan       '("#458588" "#458588" "brightcyan"))
   (dark-cyan       '("#458588" "#458588" "brightcyan"))
   (dark-red        '("#9d0006" "#9d0006" "red"))
   (dark-yellow     '("#b57614" "#b57614" "yellow"))
   (dark-blue blue)
   (teal       aqua)

   ;; face categories
   (highlight      yellow)
   (vertical-bar   grey)
   (selection      accent)
   (builtin        orange)
   (comments       (if doom-gruvbox-light-brighter-comments magenta base6))
   (doc-comments   (if doom-gruvbox-light-brighter-comments (doom-lighten magenta 0.2) (doom-lighten fg-alt 0.25)))
   (constants      yellow)
   (functions      green)
   (keywords       red)
   (methods        green)
   (operators      blue)
   (type           green)
   (strings        olive)
   (variables      blue)
   (numbers        violet)
   (region         accent)
   (error          red)
   (warning        yellow)
   (success        green)

   (vc-modified    accent)
   (vc-added       (doom-darken green 0.15))
   (vc-deleted     red)

   ;; custom categories
   (-modeline-pad
    (when doom-gruvbox-light-padded-modeline
      (if (integerp doom-gruvbox-light-padded-modeline)
          doom-gruvbox-light-padded-modeline
        4)))

   (org-quote `(,(doom-lighten (car bg) 0.05) "#1f1f1f")))

  ;; --- extra faces ------------------------
  (
   ;;;;;;;; Editor ;;;;;;;;
   (cursor :background "white")
   (hl-line :background bg-alt)
   ((line-number-current-line &override) :background grey :foreground "white" :bold t)
   ((line-number &override) :foreground grey)

   ;; Vimish-fold
   ((vimish-fold-overlay &override) :inherit 'font-lock-comment-face :background accent :weight 'light)
   ((vimish-fold-mouse-face &override) :foreground "white" :background yellow :weight 'light)
   ((vimish-fold-fringe &override) :foreground magenta :background magenta)

   ;;;;;;;; Doom-modeline ;;;;;;;;
   (mode-line
    :background accent :foreground (doom-lighten fg-alt 0.25)
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color base3)))

   (mode-line-inactive
    :background bg :foreground base4
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color base2)))

   ;; File-name
   (doom-modeline-project-dir :bold t :foreground blue)
   (doom-modeline-buffer-path :inherit 'bold :foreground green)
   (doom-modeline-buffer-file :inherit 'bold :foreground fg)
   (doom-modeline-buffer-modified :inherit 'bold :foreground yellow)
   ;; Misc
   (doom-modeline-error :background bg)
   (doom-modeline-buffer-major-mode :foreground green :bold t)
   (doom-modeline-warning :foreground red :bold t)
   (doom-modeline-info :bold t :foreground blue)
   (doom-modeline-bar :background aqua)
   (doom-modeline-panel :background aqua :foreground fg)

   ;;;;;;;; Search ;;;;;;;;
   ;; /find
   (isearch :foreground base0 :background yellow)
   (evil-search-highlight-persist-highlight-face :background orange)
   (lazy-highlight :background yellow :foreground base0 :distant-foreground base0 :bold bold)
   (evil-ex-substitute-replacement :foreground yellow :inherit 'evil-ex-substitute-matches)

   ;; evil-snipe
   (evil-snipe-first-match-face :foreground "white" :background yellow)
   (evil-snipe-matches-face     :foreground yellow :bold t :underline t)

   ;;;;;;;; Mini-buffers ;;;;;;;;
   (minibuffer-prompt :foreground green)
   (solaire-hl-line-face :background accent)

   ;; ivy
   (ivy-current-match :background accent)
   (ivy-subdir :background nil :foreground blue)
   (ivy-action :background nil :foreground blue)
   (ivy-grep-line-number :background nil :foreground blue)
   (ivy-minibuffer-match-face-1 :background nil :foreground yellow)
   (ivy-minibuffer-match-face-2 :background nil :foreground yellow)
   (ivy-minibuffer-match-highlight :foreground olive)
   (counsel-key-binding :foreground green)

   ;; ivy-posframe
   (ivy-posframe :background bg-alt)
   (ivy-posframe-border :background base1)

   ;; neotree
   (neo-root-dir-face   :foreground green)
   (doom-neotree-dir-face :foreground blue)
   (neo-dir-link-face   :foreground blue)
   (doom-neotree-file-face :foreground fg)
   (doom-neotree-hidden-file-face :foreground (doom-lighten fg-alt 0.25))
   (doom-neotree-media-file-face :foreground (doom-lighten fg-alt 0.25))
   (neo-expand-btn-face :foreground magenta)

   ;; dired
   (dired-directory :foreground blue)
   (dired-marked :foreground yellow)
   (dired-symlink :foreground green)

   ;; term
   (term-color-blue :background blue :foreground blue)
   (term-color-cyan :background green :foreground green)
   (term-color-green :background olive :foreground olive)

   ;;;;;;;; Brackets ;;;;;;;;
   ;; Rainbow-delimiters
   (rainbow-delimiters-depth-1-face :foreground red)
   (rainbow-delimiters-depth-2-face :foreground yellow)
   (rainbow-delimiters-depth-3-face :foreground green)
   (rainbow-delimiters-depth-4-face :foreground red)
   (rainbow-delimiters-depth-5-face :foreground yellow)
   (rainbow-delimiters-depth-6-face :foreground green)
   (rainbow-delimiters-depth-7-face :foreground red)
   ;; Bracket pairing
   ((show-paren-match &override) :foreground nil :background fg-alt :bold t)
   ((show-paren-mismatch &override) :foreground nil :background "red")

   ;;;;;;;; which-key ;;;;;;;;
   (which-func :foreground green)
   (which-key-command-description-face :foreground fg)
   (which-key-group-description-face :foreground (doom-lighten fg-alt 0.25))
   (which-key-local-map-description-face :foreground blue)

   ;;;;;;;; Company ;;;;;;;;
   (company-preview-common :foreground green)
   (company-tooltip-common :foreground green)
   (company-tooltip-common-selection :foreground green)
   (company-tooltip-annotation :foreground blue)
   (company-tooltip-annotation-selection :foreground blue)
   (company-scrollbar-bg :background fg)
   (company-scrollbar-fg :background green)
   (company-tooltip-selection :background accent)
   (company-tooltip-mouse :background accent :foreground nil)

   ;;;;;;;; Misc ;;;;;;;;
   (+workspace-tab-selected-face :background aqua :foreground "white")

   ;; Undo tree
   (undo-tree-visualizer-active-branch-face :foreground green)
   (undo-tree-visualizer-current-face :foreground yellow)

   ;; General UI
   (button :foreground green :underline t :bold t)

   ;; ediff
   (ediff-fine-diff-A    :background (doom-blend red bg 0.3) :weight 'bold)
   (ediff-current-diff-A :background (doom-blend red bg 0.1))

   ;; flycheck
   (flycheck-error   :underline `(:style wave :color ,red)    :background base3)
   (flycheck-warning :underline `(:style wave :color ,yellow) :background base3)
   (flycheck-info    :underline `(:style wave :color ,green)  :background base3)

   ;; helm
   (helm-swoop-target-line-face :foreground magenta :inverse-video t)

   ;; magit
   (magit-section-heading             :foreground yellow :weight 'bold)
   (magit-branch-current              :underline blue :inherit 'magit-branch-local)
   (magit-diff-hunk-heading           :background base3 :foreground fg-alt)
   (magit-diff-hunk-heading-highlight :background accent :foreground fg)
   (magit-diff-context                :foreground bg-alt :foreground fg-alt)


   ;;;;;;;; Major mode faces ;;;;;;;;
   ;; css-mode / scss-mode
   (css-proprietary-property :foreground keywords)

   ;; markdown-mode
   (markdown-blockquote-face :inherit 'italic :foreground blue)
   (markdown-list-face :foreground red)
   (markdown-url-face :foreground red)
   (markdown-pre-face  :foreground green)
   (markdown-link-face :inherit 'bold :foreground blue)
   ((markdown-code-face &override) :background (doom-lighten base2 0.045))

   ;; org-mode
   ((outline-1 &override) :foreground yellow)
   ((outline-2 &override) :foreground blue)
   ((outline-3 &override) :foreground green)
   (org-ellipsis :underline nil :foreground orange)
   (org-tag :foreground yellow :bold nil)
   ((org-quote &override) :inherit 'italic :foreground base7 :background org-quote)
   (org-todo :foreground yellow :bold 'inherit)
   (org-list-dt :foreground yellow)))

  ;; --- extra variables --------------------
  ;; ()

;;; doom-gruvbox-light-theme.el ends here
