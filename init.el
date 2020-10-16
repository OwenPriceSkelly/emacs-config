;;; init.el -*- lexical-binding: t; -*-
(doom! :completion
       (company +childframe)            ; the ultimate code completion backend
       (ivy +fuzzy                      ; a search engine for love and life
            +prescient
            +icons
            +childframe)

       :ui
       deft                             ; notational velocity for Emacs
       zen
       doom                             ; what makes DOOM look the way it does
       doom-dashboard                   ; a nifty splash screen for Emacs
       ligatures ;; +extra    ; ligatures and symbols to make your code pretty again
       hl-todo   ; highlight TODO/FIXME/NOTE/DEPRECATED/HACK/REVIEW
       hydra
       modeline                      ; snazzy, Atom-inspired modeline, plus API
       nav-flash                     ; blink the current line after jumping
       ophints                       ; highlight the region an operation acts on
       (popup +all ; tame sudden yet inevitable temporary windows; catch all popups that start with an asterix
              +defaults)             ; default popup rules
       treemacs                      ; a project drawer, like neotree but cooler
       vc-gutter       ; vcs diff in the fringe
       workspaces             ; tab emulation, persistence & separate workspaces
       ;;vi-tilde-fringe      ; fringe tildes to mark beyond EOB
       ;;indent-guides        ; highlighted indent columns
       ;;minimap
       ;;unicode
       ;;window-select        ; visually switch windows

       :editor
       lispy
       (evil +commands
             +everywhere)               ; come to the dark side, we have cookies
       file-templates                   ; auto-snippets for empty files
       fold                             ; (nigh) universal code folding
       (format +onsave)                 ; automated prettiness
       multiple-cursors                 ; editing in many places at once
       rotate-text               ; cycle region at point between text candidates
       snippets                  ; my elves. They type so I don't have to
       word-wrap                 ; soft wrapping with language-aware indent ;
       ;;parinfer             ; turn lisp into python, sort of

       :emacs
       (dired +ranger
              +icons)             ; making dired pretty [functional]
       electric                   ; smarter, keyword-based electric-indent
       (ibuffer +icons)           ; interactive buffer management
       vc                         ; version-control and Emacs, sitting in a tree
       (undo +tree)

       :term
       eshell                 ; a consistent, cross-platform shell (WIP)
       ;; vterm
       ;; shell               ; a terminal REPL for Emacs
       term                             ; another terminals in Emacs


       :checkers
       (syntax +childframe)          ; tasing you for every semicolon you forget
       spell                         ; tasing you for misspelling mispelling
       ;;grammar              ; tasing grammar mistake every you make

       :tools
       debugger              ; FIXME stepping through code, to help you add bugs
       docker
       ein                ; tame Jupyter notebooks with emacs
       (eval +overlay)    ; run code, run (also, repls)
       (lookup +docsets) ; helps you navigate your code and documentation ...or in Dash docsets locally
       ;; (lsp +eglot)
       (lsp +peek)
       (magit +forge)                   ; a git porcelain for Emacs
       make                             ; run make tasks from Emacs
       pass                             ; password manager for nerds
       pdf                              ; pdf enhancements
       rgb             ; creating color strings
       terraform       ; infrastructure as code
       upload                         ; map local to remote projects via ssh/ftp
       ;;tmux                 ; an API for interacting with tmux
       ;;ansible
       ;;direnv
       ;;editorconfig         ; let someone else argue about tabs vs spaces
       ;;gist                 ; interacting with github gists
       ;;prodigy              ; FIXME managing external services & code builders

       :os
       (:if IS-MAC macos)               ; improve compatibility with macOS
       tty                              ; improve the terminal Emacs experience

       :lang
       (cc  +lsp)                       ; C/C++/Obj-C madness
       clojure                          ; java with a lisp
       (csharp +lsp)             ; unity, .NET, and mono shenanigans
       data                      ; config/data formats
       emacs-lisp                       ; drown in parentheses
       (latex +cdlatex
              +latexmk)
       markdown                         ; writing docs for people to ignore
       yaml
       nix                           ; I hereby declare "nix geht mehr!"
       (org +pretty
            +roam                    ; organize your plain life in plain text
            +dragndrop               ; drag & drop files/images into org buffers
            +pandoc                  ; export-with-pandoc support
            +noter
            ;; +hugo          ; use Emacs for hugo blogging
            ;;+present        ; using org-mode for presentations
            ;;+gnuplot
            ;;+jupyter        ; ipython/jupyter support for babel
            ;;+pomodoro       ; be fruitful with the tomato technique
            +journal)
       (python +lsp
               +pyright)                ; beautiful is better than ugly ;
       rest                             ; Emacs as a REST client
       scheme                           ; a fully conniving family of lisps
       (sh +fish)
       ;;agda                 ; types of types of types of types...
       ;;assembly             ; assembly for fun or debugging
       ;;common-lisp          ; if you've seen one lisp, you've seen them all
       ;;coq                  ; proofs-as-programs
       ;;crystal              ; ruby at the speed of c
       ;;elixir               ; erlang done right
       ;;elm                  ; care for a cup of TEA?
       ;;erlang               ; an elegant language for a more civilized age
       ;;ess                  ; emacs speaks statistics
       ;;faust                ; dsp, but you get to keep your soul
       ;;fsharp               ; ML stands for Microsoft's Language
       ;;fstar                ; (dependent) types and (monadic) effects and Z3
       ;;go                   ; the hipster dialect
       ;;(haskell +dante)     ; a language that's lazier than I am
       ;;hy                   ; readability of scheme w/ speed of python
       ;;(java +meghanada)    ; the poster child for carpal tunnel syndrome
       ;;javascript           ; all(hope(abandon(ye(who(enter(here))))))
       ;;julia                ; a better, faster MATLAB
       ;;kotlin               ; a better, slicker Java(Script)
       ;;lean
       ;;factor
       ;;ledger               ; an accounting system in Emacs
       ;;lua                  ; one-based indices? one-based indices
       ;;nim                  ; python + lisp at the speed of c
       ;;ocaml                ; an objective camel

       ;;perl                 ; write code no one else can comprehend
       ;;php                  ; perl's insecure younger brother
       ;;plantuml             ; diagrams for confusing people more
       ;;purescript           ; javascript, but functional
       ;;qt                   ; the 'cutest' gui framework ever
       ;;racket               ; a DSL for DSLs
       ;;rst                  ; ReST in peace
       ;;ruby                 ; 1.step {|i| p "Ruby is #{i.even? ? 'love' : 'life'}"}
       ;;rust                 ; Fe2O3.unwrap().unwrap().unwrap().unwrap()
       ;;scala                ; java, but good
       ;;solidity             ; do you need a blockchain? No.
       ;;swift                ; who asked for emoji variables?
       ;;terra                ; Earth and Moon in alignment for performance.
       ;;web                  ; the tubes


       :config
       (default +bindings
         +smartparens)
       literate

       :email
       ;;(mu4e +gmail)
       ;;notmuch
       ;;(wanderlust +gmail)
       :app
       ;;calendar
       ;;irc                  ; how neckbeards socialize
       ;;(rss +org)           ; emacs as an RSS reader
       ;;twitter              ; twitter client https://twitter.com/vnought
       )
