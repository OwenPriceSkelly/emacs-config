;;; $DOOMDIR/+extra/org.el -*- lexical-binding: t; -*-
;;; Configure org and org-roam for notes

(use-package! org
  :init
  (setq org-directory                   (if IS-MAC "~/.org/" "~/.org.d/"))
  (sp-local-pair '(org-mode) "$" "$") ; For inline latex stuff
  (add-hook! (org-mode)
             #'(+org-pretty-mode  variable-pitch-mode))

  ;; TODO make other capital letters just bold caps
  (setq org-entities-user
        ;; org |latex |mathp|html         |ascii|latin1|utf-8
        '(("Z"   "\\mathbb{Z}" t "&#x2124;"  "Z" "Z"  "ℤ")
          ("C"   "\\mathbb{C}" t "&#x2102;"  "C" "C"  "ℂ")
          ("H"   "\\mathbb{H}" t "&#x210D;"  "H" "H"  "ℍ")
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
        org-agenda-files (list org-directory)
        org-ellipsis " ▾ "
        org-superstar-headline-bullets-list '("☰" "☱" "☲" "☳" "☴" "☵" "☶" "☷")
        org-startup-folded 'content
        org-todo-keywords '((sequence "[ ](t)"     ; A subtask
                                      "[~](p)"     ; Subtask currently in-progress
                                      "[*](w)"    ; Subtask is being held up or paused
                                      "|"
                                      "[X](d)"    ; Subtask was completed ; ; ;
                                      "[-](k)")   ; Subtask was dropped
                            (sequence "TODO(T)"    ; A task that needs doing & is ready to do
                                      "PROG(P)"    ; Mark a task as in-progress
                                      "WAIT(W)"   ; Something is holding up this task or it is paused
                                      "|"
                                      "DONE(D)"   ; Task successfully completed
                                      "DROP(K)")) ; Task was cancelled or is no longer applicable
        org-todo-keyword-faces '(("[~]"   . +org-todo-active)
                                 ("[*]"   . +org-todo-onhold)
                                 ("PROG"  . +org-todo-active)
                                 ("WAIT"  . +org-todo-onhold))))

(use-package! org-roam

  :commands
  (org-roam org-roam-find-file
            org-roam-buffer
            org-roam-insert
            org-roam-graph-show
            org-roam-today
            org-roam-tomorrow
            org-roam-yesterday
            org-roam-date)
  :init
  (setq +my/default-roam-header (concat "#+TITLE: ${title}\n"
                                        "* Tags:\n"
                                        "- Tag:  \n"
                                        "- Tag:  \n"
                                        "- Tag:  \n"
                                        "* Description: \n"
                                        "* Related: \n")
        +my/math-roam-header (concat "#+TITLE: ${title}\n"
                                     "* Tags:\n"
                                     "- Tag: [[file:2020-04-02-math.org][::math]]\n"
                                     "- Tag:  \n"
                                     "- Tag:  \n"
                                     "* Description: ")
        +my/work-roam-header (concat "#+TITLE: ${title}\n"
                                     "* Tags:\n"
                                     "- Sprint: [[file:2020-03-30-sprint_47.org][sprint 47]]\n"
                                     "- Category:  \n"
                                     "- Project:  \n"
                                     "* Description: ")
        +my/roam-ref-header (concat "#+TITLE: ${title}\n"
                                    "#+ROAM_KEY: ${ref}\n"
                                    "* Tags:\n"
                                    "- Tag:  \n"
                                    "- Tag:  \n"
                                    "- Tag:  \n"
                                    "* Description: ")
        +my/org-roam-ref-templates (list (list "r" "ref" 'entry (list 'function #'org-roam-capture--get-point)
                                               "%?"
                                               :file-name "${slug}"
                                               :head +my/roam-ref-header
                                               :unnarrowed t))
        +my/org-roam-capture-templates (if IS-MAC (list (list "d" "default" 'entry (list 'function #'org-roam-capture--get-point)
                                                              "%?"
                                                              :file-name "%<%Y-%m-%d>-${slug}"
                                                              :head +my/default-roam-header
                                                              :unnarrowed t)
                                                        (list "m" "math" 'entry (list 'function #'org-roam-capture--get-point)
                                                              "%?"
                                                              :file-name "%<%Y-%m-%d>-${slug}"
                                                              :head +my/math-roam-header
                                                              :unnarrowed t))
                                         (list (list "w" "work" 'entry (list 'function #'org-roam-capture--get-point)
                                                     "%?"
                                                     :file-name "%<%Y-%m-%d>-${slug}"
                                                     :head +my/default-roam-header
                                                     :unnarrowed t)
                                               (list "w" "work" 'entry (list 'function #'org-roam-capture--get-point)
                                                     "%?"
                                                     :file-name "%<%Y-%m-%d>-${slug}"
                                                     :head +my/work-roam-header
                                                     :unnarrowed t))))
  (setq org-roam-directory              org-directory
        org-roam-verbose                t
        org-roam-db-location            (concat org-roam-directory "org-roam.db")
        org-roam-buffer-width           0.3
        org-roam-completion-system      'ivy
        org-roam-completion-fuzzy-match t
        org-roam-date-filename-format   "%Y-%m-%d-%A"   ; YYYY-mm-dd-Weekday
        org-roam-date-title-format      "%A, %B %d, %Y" ; Weekday, Month dd, YYYY
        org-roam-graph-max-title-length 40
        org-roam-buffer-position        'right
        org-roam-graph-viewer           (if IS-MAC "open" "firefox")
        org-roam-graph-exclude-matcher  (list "old/" "Sunday" "Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday" "journal")
        org-roam-capture-templates      +my/org-roam-capture-templates
        org-roam-capture-ref-templates  +my/org-roam-ref-templates
        org-capture-templates           org-roam-capture-templates))


(use-package! mathpix
  :commands (mathpix-screenshot)
  :config
  (setq mathpix-app-id            "owenpriceskelly_gmail_com_2bbd51"
        mathpix-app-key           "0b3d8ae26f3762b4d5b8"
        mathpix-screenshot-method "screencapture -i %s"))
