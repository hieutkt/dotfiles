;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Hieu Phay"
      user-mail-address "hieunguyen31371@gmail.com"
      default-input-method 'vietnamese-telex
      +doom-dashboard-banner-dir doom-private-dir
      +doom-dashboard-banner-file "favicon-pixel.png"
      +doom-dashboard-banner-padding '(0 . 2)
      pixel-scroll-precision-mode t)

;; Start Doom fullscreen
(add-to-list 'default-frame-alist '(width . 92))
(add-to-list 'default-frame-alist '(height . 35))
;; (add-to-list 'default-frame-alist '(alpha 97 100))

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(if (and (string-match-p "Windows" (getenv "PATH")) (not IS-WINDOWS))
    (setq dropbox-directory "/mnt/c/Users/X380/Dropbox/")
  (setq dropbox-directory "~/Dropbox/"))

(setq org-directory (concat dropbox-directory "Notes/"))


;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)
(remove-hook! '(text-mode-hook) #'display-line-numbers-mode)

(setq frame-title-format
      '(""
        (:eval
         (if (s-contains-p org-roam-directory (or buffer-file-name ""))
             (replace-regexp-in-string
              ".*/[0-9]*-?" "☰ "
              (subst-char-in-string ?_ ?  buffer-file-name))
           "%b"))
        (:eval
         (let ((project-name (projectile-project-name)))
           (unless (string= "-" project-name)
             (format (if (buffer-modified-p)  " ◉ %s" "  ●  %s") project-name))))))

(setq doom-theme 'doom-gruvbox
      doom-themes-treemacs-enable-variable-pitch nil)

(use-package! doom-modeline
  :config
  (setq doom-modeline-persp-name t))

(use-package! visual-fill-column
  :hook (visual-line-mode . visual-fill-column-mode)
  :init
  (setq visual-fill-column-width 92
        visual-fill-column-center-text t
        visual-fill-column-fringes-outside-margins nil))

(when (doom-font-exists-p "Sarasa Mono CL")
    (setq doom-font                (font-spec :name "Sarasa Mono CL" :size 18)))
(when (doom-font-exists-p "Alegreya")
    (setq doom-variable-pitch-font (font-spec :name "Alegreya"       :size 20)))

(use-package! unicode-fonts
  :config
  ;; CJK characters
  (dolist (unicode-block '("CJK Unified Ideographs" "CJK Symbols and Punctuation" "CJK Radicals Supplement" "CJK Compatibility Ideographs"))
    (push "Sarasa Mono SC" (cadr (assoc unicode-block unicode-fonts-block-font-mapping))))
  (dolist (unicode-block '("Hangul Syllables" "Hangul Jamo Extended-A" "Hangul Jamo Extended-B"))
    (push "Sarasa Mono K" (cadr (assoc unicode-block unicode-fonts-block-font-mapping)))))

(use-package! mixed-pitch
  :hook (org-mode . mixed-pitch-mode)
  :config
  (pushnew! mixed-pitch-fixed-pitch-faces 'warning 'org-cite-key 'org-list-dt)
  (setq mixed-pitch-set-height t))

(use-package! evil-escape
  :config
  (setq evil-esc-delay 0.25))

(use-package! evil-vimish-fold
  :config
  (global-evil-vimish-fold-mode))

(use-package! evil-goggles
  :init
  (setq evil-goggles-enable-change t
        evil-goggles-enable-delete t
        evil-goggles-pulse         t
        evil-goggles-duration      0.25)
  :config
  (custom-set-faces!
    `((evil-goggles-yank-face evil-goggles-surround-face)
      :background ,(doom-blend (doom-color 'blue) (doom-color 'bg-alt) 0.5)
      :extend t)
    `(evil-goggles-paste-face
      :background ,(doom-blend (doom-color 'green) (doom-color 'bg-alt) 0.5)
      :extend t)
    `(evil-goggles-delete-face
      :background ,(doom-blend (doom-color 'red) (doom-color 'bg-alt) 0.5)
      :extend t)
    `(evil-goggles-change-face
      :background ,(doom-blend (doom-color 'orange) (doom-color 'bg-alt) 0.5)
      :extend t)
    `(evil-goggles-commentary-face
      :background ,(doom-blend (doom-color 'grey) (doom-color 'bg-alt) 0.5)
      :extend t)
    `((evil-goggles-indent-face evil-goggles-join-face evil-goggles-shift-face)
      :background ,(doom-blend (doom-color 'yellow) (doom-color 'bg-alt) 0.25)
      :extend t)
    ))

(use-package! orderless
  :config
  (add-to-list 'orderless-matching-styles 'char-fold-to-regexp))

(use-package! graphviz-dot-mode
  :config
  (setq graphviz-dot-indent-width 4))

(use-package! company-graphviz-dot)

(use-package! lsp-ui
  :config
  (setq lsp-ui-doc-delay 2
        lsp-ui-doc-max-width 80)
  (setq lsp-signature-function 'lsp-signature-posframe))

(use-package! yasnippet
  :config
  ;; It will test whether it can expand, if yes, change cursor color
  (defun hp/change-cursor-color-if-yasnippet-can-fire (&optional field)
    (interactive)
    (setq yas--condition-cache-timestamp (current-time))
    (let (templates-and-pos)
      (unless (and yas-expand-only-for-last-commands
                   (not (member last-command yas-expand-only-for-last-commands)))
        (setq templates-and-pos (if field
                                    (save-restriction
                                      (narrow-to-region (yas--field-start field)
                                                        (yas--field-end field))
                                      (yas--templates-for-key-at-point))
                                  (yas--templates-for-key-at-point))))
      (set-cursor-color (if (and templates-and-pos (first templates-and-pos)
                                 (eq evil-state 'insert))
                            (doom-color 'red)
                          (face-attribute 'default :foreground)))))
  :hook (post-command . hp/change-cursor-color-if-yasnippet-can-fire))

(use-package! citar
  :custom
  (citar-bibliography (list (concat org-directory "/References/zotero.bib")))
  (citar-notes-paths (list (concat org-directory "/Org-roam/literature")))
  (citar-library-paths (list (concat org-directory "/Org-roam/")))
  (citar-file-variable "file")
  (citar-symbols
   `((file ,(all-the-icons-faicon "file-pdf-o" :face 'all-the-icons-red :v-adjust -0.1) . " ")
     (note ,(all-the-icons-material "speaker_notes" :face 'all-the-icons-blue :v-adjust -0.3) . " ")
     (link ,(all-the-icons-material "link" :face 'all-the-icons-blue) . " ")))
  (citar-symbol-separator "  ")
  (citar-templates
   `((main . "${author editor:30}     ${date year issued:4}     ${title:48}")
     (suffix . "          ${=key= id:15}    ${=type=:12}    ${tags keywords:*}")
     (preview . "${author editor} (${year issued date}) ${title}, ${journal publisher container-title collection-title}.\n")
     (note .
           ,(string-join
             '("${author editor} (${year issued date}) ${title}"
               "#+filetags: literature"
               "#+startup: overview"
               "#+startup: hideblocks"
               "#+options: toc:2 num:t"
               "#+hugo_base_dir: ~/Dropbox/Blogs/hieutkt/"
               "#+hugo_section: ./notes"
               "#+hugo_custom_front_matter: :exclude true :math true"
               "#+hugo_custom_front_matter: :bibinfo '((doi .\"${doi}\") (isbn . \"${isbn}\") (url . \"${url}\") (year . \"${year}\") (month . \"${month}\") (date . \"${date}\") (author . \"${author}\") (journal . \"${journal}\"))"
               "#+hugo_series: \"Reading notes\""
               "#+hugo_tags:"
               ""
               "* What?"
               "* Why?"
               "* How?"
               "* And?"
               ) "\n"))))
  :config
  (setq org-cite-global-bibliography citar-bibliography)
  ;; Search contents of PDFs
  (after! (embark pdf-occur)
    (defun citar/search-pdf-contents (keys-entries &optional str)
      "Search pdfs."
      (interactive (list (citar-select-refs)))
      (let ((files (citar-file--files-for-multiple-entries
                    (citar--ensure-entries keys-entries)
                    citar-library-paths
                    '("pdf")))
            (search-str (or str (read-string "Search string: "))))
        (pdf-occur-search files search-str t)))
    ;; with this, you can exploit embark's multitarget actions, so that you can run `embark-act-all`
    (add-to-list 'embark-multitarget-actions #'citar/search-pdf-contents)))

(use-package! ansi-color
  :config
  (defun hp/display-ansi-colors ()
    (interactive)
    (ansi-color-apply-on-region (point-min) (point-max))))

(use-package! page-break-lines
  :config
  (add-hook 'prog-mode-hook (lambda () (page-break-lines-mode 1))))

(use-package! pdf-occur)

(use-package! epa
  :config
  (epa-file-enable))

(use-package! org
  :config
  (define-key! 'org-mode-map "C-c [" nil) ;org-agenda-file-to-front
  ;; ORG LATEX PREVIEW
  (setq org-startup-with-latex-preview t
        ;; Make latex preview with "C-c C-x C-l" slightly bigger
        org-format-latex-options
        (plist-put org-format-latex-options
                   :scale 1.1)
        ;; Cache the preview images elsewhere
        org-preview-latex-image-directory "~/.cache/ltximg/"
        org-highlight-latex-and-related nil
        org-image-actual-width (/ (display-pixel-width) 3))
  ;; Setup custom links
  (+org-init-custom-links-h)
  ;; Set some faces
  (custom-set-faces!
    '((org-quote)
      :extend t)
    `((org-document-title)
      :foreground ,(face-attribute 'org-document-title :foreground)
      :height 1.3 :extend t :weight bold)
    `((org-level-1)
      :foreground ,(face-attribute 'outline-1 :foreground)
      :height 1.1 :weight bold)
    `((org-level-2)
      :foreground ,(face-attribute 'outline-2 :foreground)
      :weight bold)
    )
  ;; Custom keyword
  (font-lock-add-keywords 'org-mode
                          '(("^\\(?:[  ]*\\)\\(?:[-+]\\|[ ]+\\*\\|\\(?:[0-9]+\\|[A-Za-z]\\)[.)]\\)?[ ]+"
                             . 'fixed-pitch)))
  (font-lock-add-keywords 'org-mode '(("(\\?)" . 'error)))
  ;; Replace two consecutive hyphens with the em-dash
  (defun hp/org-mode-load-prettify-symbols ()
    (interactive)
    (setq org-ellipsis "")
    (pushnew! prettify-symbols-alist
              '("--" . "—")
              '("(?)" . "") '("(?)." . "") '("(?)," . ""))
    (prettify-symbols-mode 1))
  (when (not IS-WINDOWS)
    (add-hook 'org-mode-hook 'hp/org-mode-load-prettify-symbols)))

(use-package! org-modern
  :hook (org-mode . org-modern-mode)
  :config
  (setq
   ;; Edit settings
   org-auto-align-tags nil
   org-tags-column 0
   org-catch-invisible-edits 'show-and-error
   org-special-ctrl-a/e t
   org-insert-heading-respect-content t
   ;; Appearance
   org-modern-todo nil
   org-modern-tag nil
   org-modern-timestamp t
   org-modern-statistics nil
   org-modern-progress nil
   org-modern-priority nil
   org-modern-horizontal-rule "──────────"
   org-modern-star ["⁖"]
   org-modern-keyword "‣"
   org-modern-list '((43 . "•")
                     (45 . "–")
                     (42 . "∘")))
  (set-face-attribute 'org-modern-label nil :family "Alegreya Sans")
  (set-face-attribute 'org-modern-tag nil
                      :background (doom-blend (doom-color 'blue) (doom-color 'bg) 0.1)
                      :foreground (doom-color 'grey))
  )

(use-package! svg-tag-mode
  :config
  (defconst date-re "[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}")
  (defconst time-re "[0-9]\\{2\\}:[0-9]\\{2\\}")
  (defconst day-re "[A-Za-z]\\{3\\}")
  (defconst day-time-re (format "\\(%s\\)? ?\\(%s\\)?" day-re time-re))

  (defun svg-progress-percent (value)
    (svg-image (svg-lib-concat
                (svg-lib-progress-bar
                 (/ (string-to-number value) 100.0) nil
                 :height 0.8 :background (doom-color 'fg)
                 :margin 0 :stroke 2 :radius 3 :padding 2 :width 11)
                (svg-lib-tag (concat value "%") nil
                             :height 0.8 :background (doom-color 'bg)
                             :stroke 0 :margin 0)) :ascent 'center))

  (defun svg-progress-count (value)
    (let* ((seq (mapcar #'string-to-number (split-string value "/")))
           (count (float (car seq)))
           (total (float (cadr seq))))
      (svg-image (svg-lib-concat
                  (svg-lib-progress-bar (/ count total) nil
                                        :background (doom-color 'bg) :height 0.8
                                        :margin 0 :stroke 2 :radius 3 :padding 2 :width 11)
                  (svg-lib-tag value nil
                               :background (doom-color 'bg)
                               :stroke 0 :margin 0 :height 0.8)) :ascent 'center)))

  (setq svg-tag-tags
        `(
          ;; Org tags
          (":\\([A-Za-z0-9_]+\\):" . ((lambda (tag) (svg-tag-make tag :face 'org-tag :height 0.8))))
          (":\\([A-Za-z0-9_]+[ \-]\\):" . ((lambda (tag) tag)))

          ;; Progress
          ("\\(\\[[0-9]\\{1,3\\}%\\]\\)" . ((lambda (tag)
                                              (svg-progress-percent (substring tag 1 -2)))))
          ("\\(\\[[0-9]+/[0-9]+\\]\\)" . ((lambda (tag)
                                            (svg-progress-count (substring tag 1 -1)))))
          ;; Task priority
          ("\\[#A\\]" . ((lambda (tag) (svg-tag-make tag :face 'error
                                             :height 0.8 :inverse t
                                             :beg 2 :end -1 :margin 0 :radius 10))))
          ("\\[#B\\]" . ((lambda (tag) (svg-tag-make tag :face 'warning
                                             :height 0.8 :inverse t
                                             :beg 2 :end -1 :margin 0 :radius 10))))
          ("\\[#C\\]" . ((lambda (tag) (svg-tag-make tag :face 'org-todo
                                             :height 0.8 :inverse t
                                             :beg 2 :end -1 :margin 0 :radius 10))))
          ;; Keywords
          ("TODO" . ((lambda (tag) (svg-tag-make tag :height 0.8 :inverse t
                                            :face 'org-todo :margin 0 :radius 5))))
          ("HOLD" . ((lambda (tag) (svg-tag-make tag :height 0.8
                                            :face 'org-todo :margin 0 :radius 5))))
          ("DONE\\|STOP" . ((lambda (tag) (svg-tag-make tag :height 0.8 :inverse t
                                              :face 'org-done :margin 0 :radius 5))))
          ("NEXT\\|WAIT" . ((lambda (tag) (svg-tag-make tag :height 0.8 :inverse t
                                              :face '+org-todo-active :margin 0 :radius 5))))
          ("REPEAT\\|EVENT\\|PROJ\\|IDEA" .
           ((lambda (tag) (svg-tag-make tag
                                   :height 0.8 :inverse t
                                   :face '+org-todo-project :margin 0 :radius 5))))
          ("REVIEW" . ((lambda (tag) (svg-tag-make tag
                                   :height 0.8 :inverse t
                                   :face '+org-todo-onhold :margin 0 :radius 5))))
          ))

  :hook (org-mode . svg-tag-mode)
  )

(use-package! org-appear
  :hook
  (org-mode . org-appear-mode)
  :config
  (setq org-hide-emphasis-markers t))

(use-package! org-fragtog
  :hook
  (org-mode . org-fragtog-mode))

(use-package! oc-csl-activate
  :config
  (setq org-cite-activate-processor 'csl-activate)
  (setq org-cite-csl-activate-use-document-style t)
  (setq org-cite-csl-activate-use-document-locale t)
  (add-hook 'org-mode-hook
            (lambda ()
              (cursor-sensor-mode 1)
              (org-cite-csl-activate-render-all))))

(use-package! org-tempo
  :after org
  :config
  (tempo-define-template
   "Hugo info" '("#+attr_shortcode: info\n#+begin_notice\n" p "\n#+end_notice">)
   "<info")
  (tempo-define-template
   "Hugo tip" '("#+attr_shortcode:tip\n#+begin_notice\n" p "\n#+end_notice">)
   "<tip")
  (tempo-define-template
   "Hugo warning" '("#+attr_shortcode: warning\n#+begin_notice\n" p "\n#+end_notice">)
   "<warning")
  (tempo-define-template
   "Hugo error" '("#+attr_shortcode: error\n#+begin_notice\n" p "\n#+end_notice">)
   "<error")
  (tempo-define-template
   "Hugo example" '("#+attr_shortcode: example\n#+begin_notice\n" p "\n#+end_notice">)
   "<example")
  (tempo-define-template
   "Hugo question" '("#+attr_shortcode: question\n#+begin_notice\n" p "\n#+end_notice">)
   "<question")
  )

(use-package! ox
  :config
  ;; Auto export acronyms as small caps
  ;; Copied from tecosaur
  (defun org-latex-substitute-verb-with-texttt (content)
    "Replace instances of \\verb with \\texttt{}."
    (replace-regexp-in-string
     "\\\\verb\\(.\\).+?\\1"
     (lambda (verb-string)
       (replace-regexp-in-string
        "\\\\" "\\\\\\\\" ; Why elisp, why?
        (org-latex--text-markup (substring verb-string 6 -1) 'code '(:latex-text-markup-alist ((code . protectedtexttt))))))
     content))

  (defun org-export-filter-text-acronym (text backend _info)
    "Wrap suspected acronyms in acronyms-specific formatting.
Treat sequences of 2+ capital letters (optionally succeeded by \"s\") as an acronym.
Ignore if preceeded by \";\" (for manual prevention) or \"\\\" (for LaTeX commands).

TODO abstract backend implementations."
    (let ((base-backend
           (cond
            ;; ((org-export-derived-backend-p backend 'latex) 'latex)
            ((org-export-derived-backend-p backend 'html) 'html)))
          (case-fold-search nil))
      (when base-backend
        (replace-regexp-in-string
         "[;\\\\]?\\b[A-Z][A-Z]+s?\\(?:[^A-Za-z]\\|\\b\\)"
         (lambda (all-caps-str)
           (cond ((equal (aref all-caps-str 0) ?\\) all-caps-str)                ; don't format LaTeX commands
                 ((equal (aref all-caps-str 0) ?\;) (substring all-caps-str 1))  ; just remove not-acronym indicator char ";"
                 (t (let* ((final-char (if (string-match-p "[^A-Za-z]" (substring all-caps-str -1 (length all-caps-str)))
                                           (substring all-caps-str -1 (length all-caps-str))
                                         nil)) ; needed to re-insert the [^A-Za-z] at the end
                           (trailing-s (equal (aref all-caps-str (- (length all-caps-str) (if final-char 2 1))) ?s))
                           (acr (if final-char
                                    (substring all-caps-str 0 (if trailing-s -2 -1))
                                  (substring all-caps-str 0 (+ (if trailing-s -1 (length all-caps-str)))))))
                      (pcase base-backend
                        ('latex (concat "\\acr{" (s-downcase acr) "}" (when trailing-s "\\acrs{}") final-char))
                        ('html (concat "<span class='smallcap'>" (s-downcase acr) "</span>" (when trailing-s "<small>s</small>") final-char)))))))
         text t t))))

  (add-to-list 'org-export-filter-plain-text-functions
               #'org-export-filter-text-acronym)

  ;; We won't use `org-export-filter-headline-functions' because it
  ;; passes (and formats) the entire section contents. That's no good.

  (defun org-html-format-headline-acronymised (todo todo-type priority text tags info)
    "Like `org-html-format-headline-default-function', but with acronym formatting."
    (org-html-format-headline-default-function
     todo todo-type priority (org-export-filter-text-acronym text 'html info) tags info))
  (setq org-html-format-headline-function #'org-html-format-headline-acronymised)

  ;; (defun org-latex-format-headline-acronymised (todo todo-type priority text tags info)
  ;;   "Like `org-latex-format-headline-default-function', but with acronym formatting."
  ;;   (org-latex-format-headline-default-function
  ;;    todo todo-type priority (org-latex-substitute-verb-with-texttt
  ;;                             (org-export-filter-text-acronym text 'latex info)) tags info))
  ;; (setq org-latex-format-headline-function #'org-latex-format-headline-acronymised)
  )

(use-package! ox-latex
  :config
  (setq org-latex-pdf-process
        '("latexmk -pdflatex='%latex -shell-escape -bibtex -interaction=nonstopmode' -pdf -output-directory=%o -f %f"))

  (add-to-list
   'org-preview-latex-process-alist
   '(dvipng :programs
            ("latex" "dvipng")
            :description "dvi > png" :message "you need to install the programs: latex and dvipng." :image-input-type "dvi" :image-output-type "png" :image-size-adjust
            (1.0 . 1.0)
            :latex-compiler
            ("latex -shell-escape -interaction nonstopmode -output-directory %o %f")
            :image-converter
            ("dvipng -D %D -T tight -bg Transparent -o %O %f")))

  ;; Default packages
  (setq org-latex-default-packages-alist
        '(("AUTO" "inputenc" t ("pdflatex"))
          ("T1" "fontenc" t ("pdflatex"))
          ("" "fontspec" t ("xelatex"))
          ("" "graphicx" t)
          ("" "grffile" t)
          ;; Array, tabularx, booktabs are for tables
          ("" "array" nil)
          ("" "tabularx" nil)
          ("" "booktabs" nil)
          ("" "multirow" nil)
          ("" "siunitx" nil)
          ("" "wrapfig" nil)
          ("" "rotating" nil)
          ("normalem" "ulem" t)
          ("" "amsmath" t)
          ("" "mathrsfs" t)
          ("" "textcomp" t)
          ("" "amssymb" t)
          ("" "capt-of" nil)
          ("dvipsnames" "xcolor" nil)
          ("colorlinks=true, linkcolor=Blue, citecolor=BrickRed, urlcolor=PineGreen" "hyperref" nil)
          ("" "indentfirst" nil)))

  ;; Add KOMA-scripts classes to org export
  (add-to-list 'org-latex-classes
               '("koma-letter" "\\documentclass{scrletter}"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

  (add-to-list 'org-latex-classes
               '("koma-article" "\\documentclass{scrartcl}"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

  (add-to-list 'org-latex-classes
               '("koma-report" "\\documentclass{scrreprt}"
                 ("\\part{%s}" . "\\part*{%s}")
                 ("\\chapter{%s}" . "\\chapter*{%s}")
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))

  (add-to-list 'org-latex-classes
               '("koma-book" "\\documentclass[11pt]{scrbook}"
                 ("\\part{%s}" . "\\part*{%s}")
                 ("\\chapter{%s}" . "\\chapter*{%s}")
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))))

(use-package! ox-hugo
  :config
  (setq org-hugo-use-code-for-kbd t
        org-hugo-paired-shortcodes "sidenote marginnote notice"
        org-hugo-base-dir (concat dropbox-directory "Blogs/hieutkt"))
  ;; No reference heading in ox-hugo export
  (plist-put org-hugo-citations-plist :bibliography-section-heading ""))

(use-package! org-agenda
  :config
  ;; Setting the TODO keywords
  (setq org-todo-keywords
        '((sequence
           "TODO(t)"                    ;What needs to be done
           "NEXT(n)"                    ;A project without NEXTs is stuck
           "|"
           "DONE(d)")
          (sequence
           "REPEAT(e)"                    ;Repeating tasks
           "|"
           "DONE")
          (sequence
           "HOLD(h)"                    ;Task is on hold because of me
           "PROJ(p)"                    ;Contains sub-tasks
           "WAIT(w)"                    ;Tasks delegated to others
           "REVIEW(r)"                  ;Daily notes that need reviews
           "IDEA(i)"                    ;Daily notes that need reviews
           "|"
           "STOP(c)"                    ;Stopped/cancelled
           "EVENT(m)"                   ;Meetings
           )))
  ;; Appearance
  (setq org-agenda-prefix-format       " %i %?-2 t%s"
        org-agenda-todo-keyword-format "%-6s"
        org-agenda-current-time-string "ᐊ┈┈┈┈┈┈┈ Now"
        org-agenda-time-grid '((today require-timed remove-match)
                               (0900 1200 1400 1700 2100)
                               "      "
                               "┈┈┈┈┈┈┈┈┈┈┈┈┈")
        org-agenda-scheduled-leaders '("" "")
        org-agenda-deadline-leaders '("Deadline: " "Deadline: ")
        )
  ;; Clocking
  (setq org-clock-persist 'history
        org-columns-default-format "%50ITEM(Task) %10CLOCKSUM %16TIMESTAMP_IA"
        org-agenda-start-with-log-mode t)
  (org-clock-persistence-insinuate))

(use-package! org-habit
  :config
  (setq org-habit-show-all-today t))

(use-package! org-timer
  :config
  (setq org-clock-sound (concat doom-private-dir "OOT_Secret.wav")))

(use-package! org-super-agenda
  :after org-agenda
  :config
  ;; Enable org-super-agenda
  (org-super-agenda-mode)
  (setq org-agenda-block-separator ?―)
  ;; Customise the agenda view
  (setq org-agenda-custom-commands
        '(("o" "Overview"
           ((agenda "")
            (todo "NEXT"
                  ((org-super-agenda-groups
                    '((:auto-map hp/agenda-auto-group-title-olp)))))
            (tags-todo "task"
                       ((org-agenda-overriding-header
                         "Every TASKS under the sun")
                        (org-super-agenda-groups
                         '((:discard (:todo "IDEA"))
                           (:discard (:todo "REVIEW"))
                           (:discard (:tag "writings"))
                           (:discard (:tag "blog"))
                           (:auto-map hp/agenda-auto-group-title-olp)))))
            (todo "REVIEW"
                  ((org-agenda-overriding-header "Study")
                   (org-super-agenda-groups
                    '((:auto-map hp/agenda-auto-group-title-olp)))))
            (tags-todo "writings|blog"
                  ((org-agenda-overriding-header "Writings")
                   (org-super-agenda-groups
                    '((:auto-map hp/agenda-auto-group-title-olp)))))
            (todo "IDEA"
                  ((org-agenda-overriding-header "Ideas")
                   (org-super-agenda-groups
                    '((:auto-map hp/agenda-auto-group-title-olp)))))
            ))))

  (defun hp/agenda-auto-group-title-olp (item)
    (-when-let* ((marker (or (get-text-property 0 'org-marker item)
                             (get-text-property 0 'org-hd-marker item)))
                 (buffer (->> marker marker-buffer ))
                 (title (cadar (org-collect-keywords '("title"))))
                 (filledtitle (if (> (length title) 70)
                                  (concat (substring title 0 70)  "...") title))
                 (tags (org-get-tags))
                 (olp (org-super-agenda--when-with-marker-buffer
                        (org-super-agenda--get-marker item)
                        (s-join " → " (org-get-outline-path)))))
      (concat (if (not (member "journal" tags))
                 (concat "「" filledtitle "」" ) "    ") olp)))

  ;; Make evil keymaps works on org-super-agenda headers
  (after! evil-org-agenda
    (setq org-super-agenda-header-map (copy-keymap evil-org-agenda-mode-map)))
  ;; Change header face to make it standout more
  (custom-set-faces!
    `(org-super-agenda-header
      :inherit 'variable-pitch
      :weight bold :foreground ,(doom-color 'cyan))
    `(org-agenda-structure
      :inherit 'variable-pitch
      :weight bold :foreground ,(doom-color 'blue))))

(use-package! org-capture
  :config
  ;;CAPTURE TEMPLATES
  ;;Create IDs on certain capture
  (defun hp/org-capture-maybe-create-id ()
    (when (org-capture-get :create-id)
      (org-id-get-create)))
  (add-hook 'org-capture-mode-hook #'hp/org-capture-maybe-create-id)
  ;;Auxiliary functions
  (defun hp/capture-ox-hugo-post (lang)
    (setq hp/ox-hugo-post--title (read-from-minibuffer "Post Title: ")
          hp/ox-hugo-post--fname (org-hugo-slug hp/ox-hugo-post--title)
          hp/ox-hugo-post--fdate (format-time-string "%Y-%m-%d"))
    (expand-file-name (format "%s_%s.%s.org" hp/ox-hugo-post--fdate hp/ox-hugo-post--fname lang)
                      (concat dropbox-directory "/Notes/Org-roam/writings/")))
  ;; Capture templates
  (setq org-capture-templates
        `(("i" "Inbox" entry (file ,(concat org-directory "/Agenda/inbox.org"))
           "* TODO %?\n  %i\n")
          ("m" "Meeting" entry (file ,(concat org-directory "/Agenda/inbox.org"))
           "* MEETING with %? :meeting:\n%t" :clock-in t :clock-resume t)
          ;; Capture template for new blog posts
          ("b" "New blog post")
          ("be" "English" plain (file (lambda () (hp/capture-ox-hugo-post "en")))
           ,(string-join
             '("#+title: %(eval hp/ox-hugo-post--title)"
               "#+subtitle:"
               "#+author: %n"
               "#+filetags: blog"
               "#+date: %(eval hp/ox-hugo-post--fdate)"
               "#+export_file_name: %(concat hp/ox-hugo-post--fname \".en.md\")"
               "#+hugo_base_dir: ~/Dropbox/Blogs/hieutkt/"
               "#+hugo_section: ./posts/"
               "#+hugo_tags: %?"
               "#+hugo_url:"
               "#+hugo_slug:"
               "#+hugo_custom_front_matter:"
               "#+hugo_draft: false"
               "#+startup: content"
               "#+options: toc:2 num:t\n")
             "\n")
           :create-id t
           :immediate-finish t
           :jump-to-captured t)
          ("bv" "Vietnamese" plain (file (lambda () (hp/capture-ox-hugo-post "vi")))
           ,(string-join
             '("#+title: %(eval hp/ox-hugo-post--title)"
               "#+subtitle:"
               "#+author: %n"
               "#+filetags: blog"
               "#+date: %(eval hp/ox-hugo-post--fdate)"
               "#+export_file_name: %(concat hp/ox-hugo-post--fname \".vi.md\")"
               "#+hugo_base_dir: ~/Dropbox/Blogs/hieutkt/"
               "#+hugo_section: ./posts/"
               "#+hugo_tags: %?"
               "#+hugo_url:"
               "#+hugo_slug:"
               "#+hugo_custom_front_matter:"
               "#+hugo_draft: false"
               "#+startup: content"
               "#+options: toc:2 num:t\n")
             "\n")
           :create-id t
           :immediate-finish t
           :jump-to-captured t))))

(use-package! ob-julia
  :commands org-babel-execute:julia)

(use-package! org-gcal
  :commands org-gcal-fetch
  :config
  (load-file (concat dropbox-directory "/Auths/org-gcal-settings.el.gpg")))

(use-package! org-download
  :config
  (add-hook 'dired-mode-hook 'org-download-enable)
  ;; Change how inline images are displayed
  (setq org-download-display-inline-images nil))

(use-package! org-transclusion
  :after org
  :config
  (setq org-transclusion-include-first-section t)
  (add-to-list 'org-transclusion-exclude-elements 'keyword)
  (map! :map global-map "<f9>" #'org-transclusion-mode))

(use-package! clip2org)

(use-package! anki-editor
  :hook (org-mode . anki-editor-mode)
  :init
  (setq anki-editor-use-math-jax t
        anki-editor-org-tags-as-anki-tags nil)
  :config
  (map! :localleader
        :map org-mode-map
        (:prefix ("A" . "Anki")
         :desc "Push notes at point" "p" 'anki-editor-push-notes
         :desc "Retry failure notes" "r" 'anki-editor-retry-failure-notes
         :desc "Insert note" "n" 'anki-editor-insert-note
         (:prefix ("c" . "Cloze")
          :desc "Cloze DWIM" "d" 'anki-editor-cloze-dwim
          :desc "Cloze region" "r" 'anki-editor-cloze-region
          ))))

(use-package! org-roam
  :after org
  :init
  (setq org-roam-directory (concat org-directory "/Org-roam/")
        org-roam-mode-section-functions
        (list #'org-roam-backlinks-section
              #'org-roam-reflinks-section
              #'org-roam-unlinked-references-section)
        hp/org-roam-function-tags '("compilation" "argument" "journal" "concept" "tool" "data" "bio" "literature" "event" "website"))
  (add-to-list 'magit-section-initial-visibility-alist
               '(org-roam-unlinked-references-section . hide))
  :config
  ;; Org-roam interface
  (cl-defmethod org-roam-node-hierarchy ((node org-roam-node))
    "Return the node's TITLE, as well as it's HIERACHY."
    (let* ((title (org-roam-node-title node))
           (olp (mapcar (lambda (s) (if (> (length s) 10) (concat (substring s 0 10)  "...") s)) (org-roam-node-olp node)))
           (level (org-roam-node-level node))
           (filetitle (org-roam-get-keyword "TITLE" (org-roam-node-file node)))
           (filetitle-or-name (if filetitle filetitle (file-name-nondirectory (org-roam-node-file node))))
           (shortentitle (if (> (length filetitle-or-name) 20) (concat (substring filetitle-or-name 0 20)  "...") filetitle-or-name))
           (separator (concat " " (all-the-icons-material "chevron_right") " ")))
      (cond
       ((= level 1) (concat (propertize (format "=level:%d=" level) 'display (all-the-icons-material "insert_drive_file" :face 'all-the-icons-dyellow))
                            (propertize shortentitle 'face 'org-roam-olp) separator title))
       ((= level 2) (concat (propertize (format "=level:%d=" level) 'display (all-the-icons-material "insert_drive_file" :face 'all-the-icons-dsilver))
                            (propertize (concat shortentitle separator (string-join olp separator)) 'face 'org-roam-olp) separator title))
       ((> level 2) (concat (propertize (format "=level:%d=" level) 'display (all-the-icons-material "insert_drive_file" :face 'org-roam-olp))
                            (propertize (concat shortentitle separator (string-join olp separator)) 'face 'org-roam-olp) separator title))
       (t (concat (propertize (format "=level:%d=" level) 'display (all-the-icons-material "insert_drive_file" :face 'all-the-icons-yellow))
                  (if filetitle title (propertize filetitle-or-name 'face 'all-the-icons-dyellow)))))))

  (cl-defmethod org-roam-node-functiontag ((node org-roam-node))
    "Return the FUNCTION TAG for each node. These tags are intended to be unique to each file, and represent the note's function.
        journal data literature"
    (let* ((tags (seq-filter (lambda (tag) (not (string= tag "ATTACH"))) (org-roam-node-tags node))))
      (concat
       ;; Argument or compilation
       (cond
        ((member "argument" tags)
         (propertize "=f:argument=" 'display (all-the-icons-material "forum" :face 'all-the-icons-dred)))
        ((member "compilation" tags)
         (propertize "=f:compilation=" 'display (all-the-icons-material "collections" :face 'all-the-icons-dyellow)))
        (t (propertize "=f:empty=" 'display (all-the-icons-material "remove" :face 'org-hide))))
       ;; concept, bio, data or event
       (cond
        ((member "concept" tags)
         (propertize "=f:concept=" 'display (all-the-icons-material "blur_on" :face 'all-the-icons-dblue)))
        ((member "tool" tags)
         (propertize "=f:tool=" 'display (all-the-icons-material "build" :face 'all-the-icons-dblue)))
        ((member "bio" tags)
         (propertize "=f:bio=" 'display (all-the-icons-material "people" :face 'all-the-icons-dblue)))
        ((member "event" tags)
         (propertize "=f:event=" 'display (all-the-icons-material "event" :face 'all-the-icons-dblue)))
        ((member "data" tags)
         (propertize "=f:data=" 'display (all-the-icons-material "data_usage" :face 'all-the-icons-dblue)))
        (t (propertize "=f:nothing=" 'display (all-the-icons-material "format_shapes" :face 'org-hide))))
       ;; literature
       (cond
        ((member "literature" tags)
         (propertize "=f:literature=" 'display (all-the-icons-material "book" :face 'all-the-icons-dcyan)))
        ((member "website" tags)
         (propertize "=f:website=" 'display (all-the-icons-material "move_to_inbox" :face 'all-the-icons-dsilver)))
        (t (propertize "=f:nothing=" 'display (all-the-icons-material "book" :face 'org-hide))))
       ;; journal
       )))

  (cl-defmethod org-roam-node-othertags ((node org-roam-node))
    "Return the OTHER TAGS of each notes."
    (let* ((tags (seq-filter (lambda (tag) (not (string= tag "ATTACH"))) (org-roam-node-tags node)))
           (specialtags hp/org-roam-function-tags)
           (othertags (seq-difference tags specialtags 'string=)))
      (concat
       (if othertags
           (propertize "=has:tags=" 'display (all-the-icons-faicon "tags" :face 'all-the-icons-dgreen :v-adjust 0.02))) " "
       (propertize (string-join othertags ", ") 'face 'all-the-icons-dgreen))))

  (cl-defmethod org-roam-node-backlinkscount ((node org-roam-node))
    (let* ((count (caar (org-roam-db-query
                         [:select (funcall count source)
                          :from links
                          :where (= dest $s1)
                          :and (= type "id")]
                         (org-roam-node-id node)))))
      (if (> count 0)
          (concat (propertize "=has:backlinks=" 'display (all-the-icons-material "link" :face 'all-the-icons-blue)) (format "%d" count))
        (concat (propertize "=not-backlinks=" 'display (all-the-icons-material "link" :face 'org-hide))  " "))))

  (cl-defmethod org-roam-node-directories ((node org-roam-node))
    (if-let ((dirs (file-name-directory (file-relative-name (org-roam-node-file node) org-roam-directory))))
        (concat
         (if (string= "journal/" dirs)
             (all-the-icons-material "edit" :face 'all-the-icons-dsilver)
           (all-the-icons-material "folder" :face 'all-the-icons-dsilver))
         (propertize (string-join (f-split dirs) "/") 'face 'all-the-icons-dsilver) " ")
      ""))

  (setq org-roam-node-display-template
        (concat  "${backlinkscount:16} ${functiontag} ${directories}${hierarchy} ${othertags}"))
  ;; HACK A patch for org-html exports to properly handles org-roam links
  ;;   (defun org-html--reference (datum info &optional named-only)
  ;;   "Return an appropriate reference for DATUM.
  ;; DATUM is an element or a `target' type object.  INFO is the
  ;; current export state, as a plist.
  ;; When NAMED-ONLY is non-nil and DATUM has no NAME keyword, return
  ;; nil.  This doesn't apply to headlines, inline tasks, radio
  ;; targets and targets."
  ;;   (let* ((type (org-element-type datum))
  ;; 	 (user-label
  ;; 	  (org-element-property
  ;; 	   (pcase type
  ;; 	     ((or `headline `inlinetask) :CUSTOM_ID)
  ;; 	     ((or `radio-target `target) :value)
  ;; 	     (_ :name))
  ;; 	   datum))
  ;;          (user-label (or user-label
  ;;                          (when-let ((path (org-element-property :ID datum)))
  ;;                            (concat "ID-" path)))))
  ;;     (cond
  ;;      ((and user-label
  ;; 	   (or (plist-get info :html-prefer-user-labels)
  ;; 	       ;; Used CUSTOM_ID property unconditionally.
  ;; 	       (memq type '(headline inlinetask))))
  ;;       user-label)
  ;;      ((and named-only
  ;; 	   (not (memq type '(headline inlinetask radio-target target)))
  ;; 	   (not user-label))
  ;;       nil)
  ;;      (t
  ;;       (org-export-get-reference datum info)))))
  )

(use-package! org-roam-db
  :config
  (setq org-roam-db-location "~/.emacs.d/org-roam.db"))

(use-package! org-roam-capture
  :config
  (setq org-roam-capture-templates
        `(("d" "default" plain "%?"
           :target
           (file+head "${slug}_%<%Y-%m-%d--%H-%M-%S>.org"
                      ,(string-join
                        '("#+title: ${title}"
                          "#+created: %U"
                          "#+filetags: %(completing-read \"Function tags: \" hp/org-roam-function-tags)"
                          "#+startup: overview hideblocks"
                          "") "\n"))
           :unnarrowed t))))

(use-package! org-roam-protocol
  :after (org-roam org-roam-dailies)
  :config
  (add-to-list
   'org-roam-capture-ref-templates
   `(;; Browser bookletmark template:
     ;; javascript:location.href =
     ;; 'org-protocol://roam-ref?template=w&ref='
     ;; + encodeURIComponent(location.href)
     ;; + '&title='
     ;; + encodeURIComponent(document.getElementsByTagName("h1")[0].innerText)
     ;; + '&hostname='
     ;; + encodeURIComponent(location.hostname)
     ("w" "webref" entry "* ${title} ([[${ref}][${hostname}]])\n%?"
      :target
      (file+head
       ,(concat org-roam-dailies-directory "%<%Y-%m>.org")
       ,(string-join
         '(":properties:"
           ":roam_refs: %^{Key}"
           ":end:"
           "#+title: %<%Y-%m>"
           "#+filetags: journal"
           "#+startup: overview"
           "#+created: %U"
           "") "\n"))
      :unnarrowed t))))

(use-package! org-roam-dailies
  :config
  (setq org-roam-dailies-directory "journal/"
        org-roam-dailies-capture-templates
        '(("d" "daily" entry "* %?"
           :target
           (file+head "%<%Y-%m-%d>.org"
                      "#+title: %<%Y-%m-%d %a>\n#+filetags: journal\n#+startup: overview hideblocks\n#+created: %U\n\n")
           :immediate-finish t)))
  (map! :leader
        :prefix "n"
        (:prefix ("j" . "journal")
         :desc "Arbitrary date" "d" #'org-roam-dailies-goto-date
         :desc "Today"          "j" #'org-roam-dailies-goto-today
         :desc "Tomorrow"       "m" #'org-roam-dailies-goto-tomorrow
         :desc "Yesterday"      "y" #'org-roam-dailies-goto-yesterday)))

(use-package! websocket
  :after org-roam)

(use-package! org-roam-ui
  :after org-roam
  :commands (org-roam-ui-mode))

(after! (org-roam)
  (defadvice! yeet/org-roam-in-own-workspace-a (&rest _)
  "Open all roam buffers in there own workspace."
  :before #'org-roam-node-find
  :before #'org-roam-node-random
  :before #'org-roam-buffer-display-dedicated
  :before #'org-roam-buffer-toggle
  :before #'org-roam-dailies-goto-today
  (when (featurep! :ui workspaces)
    (+workspace-switch "Org-roam" t))))

(after! (org-agenda org-roam)
  (defun vulpea-task-p ()
    "Return non-nil if current buffer has any todo entry.

TODO entries marked as done are ignored, meaning the this
function returns nil if current buffer contains only completed
tasks."
    (seq-find                                 ; (3)
     (lambda (type)
       (eq type 'todo))
     (org-element-map                         ; (2)
         (org-element-parse-buffer 'headline) ; (1)
         'headline
       (lambda (h)
         (org-element-property :todo-type h)))))

  (defun vulpea-task-update-tag ()
    "Update task tag in the current buffer."
    (when (and (not (active-minibuffer-window))
               (vulpea-buffer-p))
      (save-excursion
        (goto-char (point-min))
        (let* ((tags (vulpea-buffer-tags-get))
               (original-tags tags))
          (if (vulpea-task-p)
              (setq tags (cons "task" tags))
            (setq tags (remove "task" tags)))

          ;; cleanup duplicates
          (setq tags (seq-uniq tags))

          ;; update tags if changed
          (when (or (seq-difference tags original-tags)
                    (seq-difference original-tags tags))
            (apply #'vulpea-buffer-tags-set tags))))))

  (defun vulpea-buffer-p ()
    "Return non-nil if the currently visited buffer is a note."
    (and buffer-file-name
         (string-prefix-p
          (expand-file-name (file-name-as-directory org-roam-directory))
          (file-name-directory buffer-file-name))))

  (defun vulpea-task-files ()
    "Return a list of note files containing 'task' tag." ;
    (seq-uniq
     (seq-map
      #'car
      (org-roam-db-query
       [:select [nodes:file]
        :from tags
        :left-join nodes
        :on (= tags:node-id nodes:id)
        :where (like tag (quote "%\"task\"%"))]))))

  (defun vulpea-agenda-files-update (&rest _)
    "Update the value of `org-agenda-files'."
    (setq org-agenda-files (vulpea-task-files)))

  (add-hook 'find-file-hook #'vulpea-task-update-tag)
  (add-hook 'before-save-hook #'vulpea-task-update-tag)

  (advice-add 'org-agenda :before #'vulpea-agenda-files-update)
  (advice-add 'org-todo-list :before #'vulpea-agenda-files-update)

  ;; functions borrowed from `vulpea' library
  ;; https://github.com/d12frosted/vulpea/blob/6a735c34f1f64e1f70da77989e9ce8da7864e5ff/vulpea-buffer.el

  (defun vulpea-buffer-tags-get ()
    "Return filetags value in current buffer."
    (vulpea-buffer-prop-get-list "filetags" "[ :]"))

  (defun vulpea-buffer-tags-set (&rest tags)
    "Set TAGS in current buffer.

If filetags value is already set, replace it."
    (if tags
        (vulpea-buffer-prop-set
         "filetags" (concat ":" (string-join tags ":") ":"))
      (vulpea-buffer-prop-remove "filetags")))

  (defun vulpea-buffer-tags-add (tag)
    "Add a TAG to filetags in current buffer."
    (let* ((tags (vulpea-buffer-tags-get))
           (tags (append tags (list tag))))
      (apply #'vulpea-buffer-tags-set tags)))

  (defun vulpea-buffer-tags-remove (tag)
    "Remove a TAG from filetags in current buffer."
    (let* ((tags (vulpea-buffer-tags-get))
           (tags (delete tag tags)))
      (apply #'vulpea-buffer-tags-set tags)))

  (defun vulpea-buffer-prop-set (name value)
    "Set a file property called NAME to VALUE in buffer file.
If the property is already set, replace its value."
    (setq name (downcase name))
    (org-with-point-at 1
      (let ((case-fold-search t))
        (if (re-search-forward (concat "^#\\+" name ":\\(.*\\)")
                               (point-max) t)
            (replace-match (concat "#+" name ": " value) 'fixedcase)
          (while (and (not (eobp))
                      (looking-at "^[#:]"))
            (if (save-excursion (end-of-line) (eobp))
                (progn
                  (end-of-line)
                  (insert "\n"))
              (forward-line)
              (beginning-of-line)))
          (insert "#+" name ": " value "\n")))))

  (defun vulpea-buffer-prop-set-list (name values &optional separators)
    "Set a file property called NAME to VALUES in current buffer.
VALUES are quoted and combined into single string using
`combine-and-quote-strings'.
If SEPARATORS is non-nil, it should be a regular expression
matching text that separates, but is not part of, the substrings.
If nil it defaults to `split-string-default-separators', normally
\"[ \f\t\n\r\v]+\", and OMIT-NULLS is forced to t.
If the property is already set, replace its value."
    (vulpea-buffer-prop-set
     name (combine-and-quote-strings values separators)))

  (defun vulpea-buffer-prop-get (name)
    "Get a buffer property called NAME as a string."
    (org-with-point-at 1
      (when (re-search-forward (concat "^#\\+" name ": \\(.*\\)")
                               (point-max) t)
        (buffer-substring-no-properties
         (match-beginning 1)
         (match-end 1)))))

  (defun vulpea-buffer-prop-get-list (name &optional separators)
    "Get a buffer property NAME as a list using SEPARATORS.
If SEPARATORS is non-nil, it should be a regular expression
matching text that separates, but is not part of, the substrings.
If nil it defaults to `split-string-default-separators', normally
\"[ \f\t\n\r\v]+\", and OMIT-NULLS is forced to t."
    (let ((value (vulpea-buffer-prop-get name)))
      (when (and value (not (string-empty-p value)))
        (split-string-and-unquote value separators))))

  (defun vulpea-buffer-prop-remove (name)
    "Remove a buffer property called NAME."
    (org-with-point-at 1
      (when (re-search-forward (concat "\\(^#\\+" name ":.*\n?\\)")
                               (point-max) t)
        (replace-match ""))))
  )

(use-package! ess
  :config
  (set-popup-rules!
    '(("^\\*R:*\\*$" :side right :size 0.5 :ttl nil))))

(use-package! ess-stata-mode
  :after ess
  :config
  (setq inferior-STA-start-args ""
        inferior-STA-program (executable-find "stata")
        inferior-STA-program-name (executable-find "stata"))
  (add-to-list 'org-src-lang-modes '("jupyter-stata" . stata)))

(use-package! python
  :config
  (set-popup-rules!
    '(("^\\*Python:*\\*$" :side right :size 0.5 :ttl nil))))

(use-package! julia-repl
  :config
  ;; Use vterm instead of the defautl term
  (when (featurep! :term vterm)
    (julia-repl-set-terminal-backend 'vterm)
    (map! :map vterm-mode-map :i "C-c C-z" nil))
  (setq lsp-julia-package-dir nil)
  ;; Make popup position similar to `ess'
  (set-popup-rules!
    '(("^\\*julia.*\\*$" :side right :size 0.5 :ttl nil))))

;; lsp-mode seems to serve an invalid response to the Julia server.
;; The pseudo-fix is rather simple at least.
(after! julia-mode
  (add-hook! 'julia-mode-hook
    (setq-local lsp-enable-folding t
                lsp-folding-range-limit 100)))

(use-package! elfeed
  :commands (elfeed)
  :custom
  (rmh-elfeed-org-files (list (concat org-directory "/Feeds/elfeed.org")))
  (elfeed-db-directory (concat org-directory "/Feeds/elfeed.db/"))
  (elfeed-goodies/wide-threshold 0.2)
  :bind ("<f10>" . #'elfeed)
  :config
  ;; (defun hp/elfeed-entry-line-draw (entry)
  ;;   (insert (format "%s" (elfeed-meta--plist entry))))
  (defun hp/elfeed-entry-line-draw (entry)
    "Print ENTRY to the buffer."
    (let* ((date (elfeed-search-format-date (elfeed-entry-date entry)))
           (title (or (elfeed-meta entry :title) (elfeed-entry-title entry) ""))
           (title-faces (elfeed-search--faces (elfeed-entry-tags entry)))
           (feed (elfeed-entry-feed entry))
           (feed-title
            (when feed
              (or (elfeed-meta feed :title) (elfeed-feed-title feed))))
           (tags (mapcar #'symbol-name (elfeed-entry-tags entry)))
           (tags-str (concat "[" (mapconcat 'identity tags ",") "]"))
           (title-width (- (window-width) elfeed-goodies/feed-source-column-width
                           elfeed-goodies/tag-column-width 4))
           (title-column (elfeed-format-column
                          title (elfeed-clamp
                                 elfeed-search-title-min-width
                                 title-width
                                 title-width)
                          :left))
           (tag-column (elfeed-format-column
                        tags-str (elfeed-clamp (length tags-str)
                                               elfeed-goodies/tag-column-width
                                               elfeed-goodies/tag-column-width)
                        :left))
           (feed-column (elfeed-format-column
                         feed-title (elfeed-clamp elfeed-goodies/feed-source-column-width
                                                  elfeed-goodies/feed-source-column-width
                                                  elfeed-goodies/feed-source-column-width)
                         :left))
           (entry-score (elfeed-format-column (number-to-string (elfeed-score-scoring-get-score-from-entry entry)) 6 :left))
           ;; (entry-authors (concatenate-authors
           ;;                 (elfeed-meta entry :authors)))
           ;; (authors-column (elfeed-format-column entry-authors elfeed-goodies/tag-column-width :left))
           )
      (if (>= (window-width) (* (frame-width) elfeed-goodies/wide-threshold))
          (progn
            (insert (propertize entry-score 'face 'elfeed-search-feed-face) " ")
            (insert (propertize date 'face 'elfeed-search-date-face) " ")
            (insert (propertize feed-column 'face 'elfeed-search-feed-face) " ")
            (insert (propertize tag-column 'face 'elfeed-search-tag-face) " ")
            ;; (insert (propertize authors-column 'face 'elfeed-search-tag-face) " ")
            (insert (propertize title 'face title-faces 'kbd-help title))
            )
        (insert (propertize title 'face title-faces 'kbd-help title)))))

  (defun concatenate-authors (authors-list)
    "Given AUTHORS-LIST, list of plists; return string of all authors concatenated."
    (if (> (length authors-list) 1)
        (format "%s et al." (plist-get (nth 0 authors-list) :name))
      (plist-get (nth 0 authors-list) :name)))

  (defun search-header/draw-wide (separator-left separator-right search-filter stats db-time)
    (let* ((update (format-time-string "%Y-%m-%d %H:%M:%S %z" db-time))
           (lhs (list
                 (powerline-raw (-pad-string-to "Score" (- 5 5)) 'powerline-active1 'l)
                 (funcall separator-left 'powerline-active1 'powerline-active2)
                 (powerline-raw (-pad-string-to "Date" (- 9 4)) 'powerline-active2 'l)
                 (funcall separator-left 'powerline-active2 'powerline-active1)
                 (powerline-raw (-pad-string-to "Feed" (- elfeed-goodies/feed-source-column-width 4)) 'powerline-active1 'l)
                 (funcall separator-left 'powerline-active1 'powerline-active2)
                 (powerline-raw (-pad-string-to "Tags" (- elfeed-goodies/tag-column-width 6)) 'powerline-active2 'l)
                 (funcall separator-left 'powerline-active2 'mode-line)
                 (powerline-raw "Subject" 'mode-line 'l)))
           (rhs (search-header/rhs separator-left separator-right search-filter stats update)))
      (concat (powerline-render lhs)
              (powerline-fill 'mode-line (powerline-width rhs))
              (powerline-render rhs))))

  ;; Tag entry as read when open
  (defadvice! hp/mark-read (&rest _)
    :before 'elfeed-search-show-entry
    :before 'elfeed-search-browse-url
    (let* ((offset (- (line-number-at-pos) elfeed-search--offset))
           (current-entry (nth offset elfeed-search-entries)))
      (elfeed-tag-1 current-entry 'read)))

  ;; Faces for diferent kinds of feeds
  (defface hp/elfeed-blog
    `((t :foreground ,(doom-color 'blue)))
    "Marks a Elfeed blog.")
  (push '(blog hp/elfeed-blog)
        elfeed-search-face-alist)
  (push '(read elfeed-search-title-face)
        elfeed-search-face-alist)

  ;; Variables
  (setq elfeed-search-print-entry-function 'hp/elfeed-entry-line-draw
        elfeed-search-filter "@8-weeks-ago -bury "))

(use-package! elfeed-score
  :after elfeed
  :custom
  (elfeed-score-score-file (concat org-directory "/Feeds/elfeed.score"))
  :config
  (map! :map elfeed-search-mode-map
        :n "=" elfeed-score-map)
  (elfeed-score-enable))
