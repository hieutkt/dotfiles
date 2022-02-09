;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

(package! visual-fill-column)
(package! lsp-treemacs)
(package! org-super-agenda)
(package! keycast)
(package! benchmark-init)

(package! tree-sitter)
(package! tree-sitter-langs)

(package! ox-pandoc)
(package! org-pandoc-import
  :recipe (:host github
           :repo "tecosaur/org-pandoc-import"
           :files ("*.el" "filters" "preprocessors")))

(package! page-break-lines
  :recipe (:host github :repo "purcell/page-break-lines"))
(package! org-appear
  :recipe (:host github :repo "awth13/org-appear"))
(package! org-fragtog
  :recipe (:host github :repo "io12/org-fragtog"))
(package! citeproc-org
  :recipe (:host github :repo "andras-simonyi/citeproc-org"))
(package! org-transclusion
  :recipe (:host github :repo "nobiot/org-transclusion"))

(package! ess-stata-mode
  :recipe (:host github :repo "emacs-ess/ess-stata-mode"))

(package! clip2org
  :recipe (:host github :repo "thamer/clip2org"))


;;Org-roam
(unpin! org-roam)
(package! websocket)
(package! org-roam-ui :recipe (:host github :repo "org-roam/org-roam-ui" :files ("*.el" "out")))

;;Graphviz
(package! graphviz-dot-mode
  :recipe (:host github :repo "ppareit/graphviz-dot-mode"))

;; Git
(package! blamer :recipe (:host github :repo "artawower/blamer.el"))

;; RSS
(package! elfeed-score
  :recipe (:host github :repo "sp1ff/elfeed-score"))

(package! ob-julia
  :recipe (:host github :repo "nico202/ob-julia" :files ("*.el" "julia")))
