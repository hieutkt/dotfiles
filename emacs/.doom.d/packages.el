;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

(package! auto-olivetti
  :recipe (:host sourcehut :repo "ashton314/auto-olivetti"))

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

(package! engrave-faces
  :recipe (:host github :repo "tecosaur/engrave-faces"))

;;Org-roam
(package! org-roam-ui
  :recipe (:host github :repo "org-roam/org-roam-ui"
           :branch "main" :files ("*.el" "out")))

;;Graphviz
(package! graphviz-dot-mode
  :recipe (:host github :repo "ppareit/graphviz-dot-mode"))

;; RSS
(package! elfeed-score
  :recipe (:host github :repo "sp1ff/elfeed-score"))

(package! ob-julia
  :recipe (:host github :repo "nico202/ob-julia" :files ("*.el" "julia")))

(package! org-modern
  :recipe (:host github :repo "minad/org-modern"))
(package! svg-tag-mode
  :recipe (:host github :repo "rougier/svg-tag-mode"))

(package! calctex :recipe (:host github :repo "johnbcoughlin/calctex"
                           :files ("*.el" "calctex/*.el" "calctex-contrib/*.el" "org-calctex/*.el" "vendor")))
;; (package! calc-transient
;;   :recipe (:host github :repo "karthink/calc-transient"))

(package! laas
  :recipe (:host github :repo "tecosaur/LaTeX-auto-activating-snippets"))

;; Org-mode from tecosaur's development branch
(package! org :recipe
  (:host nil :repo "https://git.tecosaur.net/mirrors/org-mode.git" :remote "mirror" :fork
         (:host nil :repo "https://git.tecosaur.net/tec/org-mode.git" :branch "dev" :remote "tecosaur")
         :files
         (:defaults "etc")
         :build t :pre-build
         (with-temp-file "org-version.el"
           (require 'lisp-mnt)
           (let
               ((version
                 (with-temp-buffer
                   (insert-file-contents "lisp/org.el")
                   (lm-header "version")))
                (git-version
                 (string-trim
                  (with-temp-buffer
                    (call-process "git" nil t nil "rev-parse" "--short" "HEAD")
                    (buffer-string)))))
             (insert
              (format "(defun org-release () \"The release version of Org.\" %S)\n" version)
              (format "(defun org-git-version () \"The truncate git commit hash of Org mode.\" %S)\n" git-version)
              "(provide 'org-version)\n"))))
  :pin nil)

(unpin! org) ; there be bugs

(package! org-contrib
  :recipe (:host nil :repo "https://git.sr.ht/~bzg/org-contrib"
           :files ("lisp/*.el"))
  :pin "6422b265f1150204f024e33d54f2dcfd8323005c")

(package! gptel
  :recipe (:host github :repo "karthink/gptel"))

;Corfu
(package! cape-yasnippet
  :recipe (:host github :repo "elken/cape-yasnippet"))
