;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; General
(package! auto-olivetti
  :recipe (:host sourcehut :repo "ashton314/auto-olivetti"))

;; LATEX
(package! laas
  :recipe (:host github :repo "tecosaur/LaTeX-auto-activating-snippets"))

;; Icons
(package! nerd-icons-ibuffer
  :recipe (:host github :repo "seagle0128/nerd-icons-ibuffer"))

;; ORG-MODE
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

(package! org-modern
  :recipe (:host github :repo "minad/org-modern"))

(package! svg-tag-mode
  :recipe (:host github :repo "rougier/svg-tag-mode"))

(package! org-appear
  :recipe (:host github :repo "awth13/org-appear"))

(package! org-super-agenda)

(package! org-csl-activate
  :recipe (:host github :repo "andras-simonyi/org-cite-csl-activate"))

(package! org-special-block-extras
  :recipe (:host github :repo "alhassy/org-special-block-extras"))

(package! engrave-faces
  :recipe (:host github :repo "tecosaur/engrave-faces"))

;; Julia
(unpin! julia-snail)

(package! ob-julia
  :recipe (:host github :repo "karthink/ob-julia" :files ("*.el" "julia")))

;; STATA
(package! ess-stata-mode
  :recipe (:host github :repo "emacs-ess/ess-stata-mode"))

;; RSS
(package! elfeed-score
  :recipe (:host github :repo "sp1ff/elfeed-score"))

;;Testing
(package! indent-bars
  :recipe (:host github :repo "jdtsmith/indent-bars"))
