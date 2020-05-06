;;; $DOOMDIR/+extra/org.el -*- lexical-binding: t; -*-
;;; Configures org, org-roam for notes

(use-package! org
  :init
  (setq org-directory                   (if IS-MAC "~/.org/" "~/.org.d/"))
  (sp-local-pair '(org-mode) "$" "$") ; For inline latex stuff
  (add-hook! (org-mode) #'(+org-pretty-mode  variable-pitch-mode))
  ;; TODO make other capital letters just bold caps
  (setq org-entities-user
        ;; org |latex |mathp|html         |ascii|latin1|utf-8
        '(("Z"   "\\mathbb{Z}" t "&#x2124;"  "Z" "Z"  "ℤ")
          ("C"   "\\mathbb{C}" t "&#x2102;"  "C" "C"  "ℂ")
          ;; ("H"   "\\mathbb{H}" t "&#x210D;"  "H" "H"  "ℍ")
          ("N"   "\\mathbb{N}" t "&#x2115;"  "N" "N"  "ℕ")
          ("P"   "\\mathbb{P}" t "&#x2119;"  "P" "P"  "ℙ")
          ("Q"   "\\mathbb{Q}" t "&#x211A;"  "Q" "Q"  "ℚ")
          ("R"   "\\mathbb{R}" t "&#x211D;"  "R" "R"  "ℝ")))
  (setq org-startup-folded              'content
        org-startup-with-latex-preview nil
        org-highlight-latex-and-related nil
        org-preview-latex-default-process 'dvisvgm
        org-format-latex-options '(:foreground default :background default :scale 1.0
                                   :html-foreground "Black" :html-background "Transparent"
                                   :html-scale 1.0 :matchers ("begin" "$1" "$" "$$" "\\(" "\\["))
        ;; org-agenda-files (list org-directory)
        org-ellipsis " ▾ "
        org-superstar-headline-bullets-list '("☰" "☱" "☵" "☳" "☴" "☲" "☶" "☷")
        org-todo-keywords '((sequence "[ ](t)" "[~](p)" "[*](w)" "|"
                                      "[X](d)" "[-](k)")
                            (sequence "TODO(T)" "PROG(P)" "WAIT(W)" "|"
                                      "DONE(D)" "DROP(K)"))
        org-todo-keyword-faces '(("[~]"   . +org-todo-active)
                                 ("[*]"   . +org-todo-onhold)
                                 ("PROG"  . +org-todo-active)
                                 ("WAIT"  . +org-todo-onhold))))

(setq +my/default-roam-header (concat "#+TITLE: ${title}\n"
                                      "* Tags:\n" "- Tag:  \n" "- Tag:  \n" "- Tag:  \n" "* Description: \n" "* Related: \n")
      +my/math-roam-header (concat "#+TITLE: ${title}\n"
                                   "* Tags:\n" "- Tag: [[file:2020-04-02-math.org][::math]]\n" "- Tag:  \n" "- Tag:  \n" "* Description: ")
      +my/work-roam-header (concat "#+TITLE: ${title}\n"
                                   "* Tags:\n" "- Sprint: \n" "- Category: \n" "- Project: \n" "* Description: ")
      +my/roam-ref-header (concat "#+TITLE: ${title}\n"
                                  "#+ROAM_KEY: ${ref}\n"
                                  "* Tags:\n" "- Tag:  \n" "- Tag:  \n" "- Tag:  \n" "* Description: ")
      +my/org-roam-ref-templates (list (list "r" "ref" 'entry (list 'function #'org-roam-capture--get-point)
                                             "%?"
                                             :file-name "${slug}"
                                             :head +my/roam-ref-header
                                             :unnarrowed t))
      +my/org-roam-capture-templates (list (list "d" "default" 'entry (list 'function #'org-roam-capture--get-point)
                                                 "%?"
                                                 :file-name "%<%Y-%m-%d>-${slug}"
                                                 :head +my/default-roam-header
                                                 :unnarrowed t)
                                           (if IS-MAC
                                               (list "m" "math" 'entry (list 'function #'org-roam-capture--get-point)
                                                     "%?"
                                                     :file-name "%<%Y-%m-%d>-${slug}"
                                                     :head +my/math-roam-header
                                                     :unnarrowed t)
                                             (list "w" "work" 'entry (list 'function #'org-roam-capture--get-point)
                                                   "%?"
                                                   :file-name "%<%Y-%m-%d>-${slug}"
                                                   :head +my/work-roam-header
                                                   :unnarrowed t))))
(use-package! org-roam
  :custom
  (org-roam-directory                      org-directory)
  (org-roam-index-file "./index.org")
  (org-roam-verbose                        t)
  (org-roam-buffer-prepare-hook            '(hide-mode-line-mode
                                             org-roam-buffer--insert-title
                                             org-roam-buffer--insert-backlinks)) ;;org-roam-buffer--insert-citelinks
  (org-roam-buffer-position                'right)
  (org-roam-buffer-width                   0.27)
  (org-roam-buffer-no-delete-other-windows t)
  (org-roam-completion-system              'ivy)
  (org-roam-graph-viewer                   (if IS-MAC "open" (executable-find "firefox"))) ;; the osx `open' executable just finds system default for .svg
  (org-roam-graph-max-title-length         40)
  (org-roam-graph-exclude-matcher          '("old/" "Sunday" "Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday" "journal"))
  :init
  (setq
   ;; org-roam-db-location            (concat org-roam-directory "org-roam.db")
   org-roam-capture-ref-templates  +my/org-roam-ref-templates
   org-roam-capture-templates      +my/org-roam-capture-templates
   org-roam-dailies-capture-templates '(("d" "daily" plain (function org-roam-capture--get-point) ""
                                         :immediate-finish t
                                         :file-name "%<%Y-%m-%d-%A>"
                                         :head "#+TITLE: %<%A, %B %d, %Y>")))
  :config
  (add-hook! 'org-roam-buffer-prepare-hook :append (org-global-cycle 2)))
(use-package! mathpix
  :commands (mathpix-screenshot)
  :config
  (setq mathpix-app-id            "owenpriceskelly_gmail_com_2bbd51"
        mathpix-app-key           "0b3d8ae26f3762b4d5b8"
        mathpix-screenshot-method "screencapture -i %s"))
