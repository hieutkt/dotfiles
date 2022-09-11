;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

(package! visual-fill-column)
(package! lsp-treemacs)
(package! org-super-agenda)
(package! benchmark-init)

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
(package! org-csl-activate
  :recipe (:host github :repo "andras-simonyi/org-cite-csl-activate"))

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

;; RSS
(package! elfeed-score
  :recipe (:host github :repo "sp1ff/elfeed-score"))

(package! ob-julia
  :recipe (:host github :repo "nico202/ob-julia" :files ("*.el" "julia")))

;; Anki
(package! anki-editor
  :recipe (:host github :repo "louietan/anki-editor"))

(package! org-modern
  :recipe (:host github :repo "minad/org-modern"))
(package! svg-tag-mode
  :recipe (:host github :repo "rougier/svg-tag-mode"))

(package! laas
  :recipe (:host github :repo "tecosaur/LaTeX-auto-activating-snippets"))
