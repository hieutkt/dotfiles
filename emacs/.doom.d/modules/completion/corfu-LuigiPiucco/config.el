;;; completion/corfu/config.el -*- lexical-binding: t; -*-

(defvar +corfu-buffer-scanning-size-limit (* 1 1024 1024) ; 1 MB
  "Size limit for a buffer to be scanned by `cape-line' or `cape-dabbrev'.

As an exception, `cape-line' will also scan buffers with the same
major mode regardless of size.")

;;
;;; Packages
(use-package! corfu
  :hook ((doom-first-buffer . global-corfu-mode))
  :config
  (setq corfu-auto t
        corfu-auto-delay 0.1
        corfu-auto-prefix 2
        global-corfu-modes '((not
                              erc-mode
                              circe-mode
                              help-mode
                              gud-mode
                              vterm-mode)
                             t)
        corfu-cycle t
        corfu-separator (when (modulep! +orderless) ?\s)
        corfu-preselect (if (modulep! +tng) 'prompt 'valid)
        corfu-count 16
        corfu-max-width 120
        corfu-preview-current 'insert
        corfu-on-exact-match nil
        corfu-quit-at-boundary (if (modulep! +orderless) 'separator t)
        corfu-quit-no-match (if (modulep! +orderless) 'separator t)
        ;; In the case of +tng, TAB should be smart regarding completion;
        ;; However, it should otherwise behave like normal, whatever normal was.
        tab-always-indent (if (modulep! +tng) 'complete tab-always-indent))
  (add-to-list 'completion-category-overrides `(lsp-capf (styles ,@completion-styles)))

  (map! :map corfu-mode-map
        :eig "C-SPC" #'completion-at-point
        :mnor "C-SPC" (cmd! (call-interactively #'evil-insert-state)
                            (call-interactively #'completion-at-point))
        :v "C-SPC" (cmd! (call-interactively #'evil-change)
                         (call-interactively #'completion-at-point)))

  (defun +corfu-enable-in-minibuffer ()
    "Enable Corfu in the minibuffer if `completion-at-point' is bound."
    (when (where-is-internal #'completion-at-point (list (current-local-map)))
      (setq-local corfu-echo-delay nil)
      (corfu-mode +1)))
  (add-hook 'minibuffer-setup-hook #'+corfu-enable-in-minibuffer)
  ;; If you want to update the visual hints after completing minibuffer commands
  ;; with Corfu and exiting, you have to do it manually.
  (defadvice! +corfu--insert-before-exit-minibuffer-a ()
    :before #'exit-minibuffer
    (when (or (and (frame-live-p corfu--frame)
                   (frame-visible-p corfu--frame))
              (and (featurep 'corfu-terminal)
                   (popon-live-p corfu-terminal--popon)))
      (when (member isearch-lazy-highlight-timer timer-idle-list)
        (apply (timer--function isearch-lazy-highlight-timer)
               (timer--args isearch-lazy-highlight-timer)))
      (when (member (bound-and-true-p anzu--update-timer) timer-idle-list)
        (apply (timer--function anzu--update-timer)
               (timer--args anzu--update-timer)))
      (when (member (bound-and-true-p evil--ex-search-update-timer)
                    timer-idle-list)
        (apply (timer--function evil--ex-search-update-timer)
               (timer--args evil--ex-search-update-timer)))))

  (after! evil
    (add-hook 'evil-insert-state-exit-hook #'corfu-quit))

  (when (modulep! +icons)
    (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

  (map! :map corfu-map
        [return] #'corfu-insert
        "RET" #'corfu-insert)
  (when (modulep! +orderless)
    (map! :map corfu-map
          "<remap> <completion-at-point>" #'+corfu-smart-sep-toggle-escape))
  (when (modulep! +tng)
    (map! :map corfu-map
          [tab] #'corfu-next
          [backtab] #'corfu-previous
          "TAB" #'corfu-next
          "S-TAB" #'corfu-previous)
    (let ((cmds-del (cmds! (and (modulep! +tng)
                                (> corfu--index -1)
                                (eq corfu-preview-current 'insert))
                           #'corfu-reset)))
      (map! :map corfu-map
            [backspace] cmds-del
            "DEL" cmds-del)))

  (when (modulep! +orderless)
    (after! orderless
      (setq orderless-component-separator #'orderless-escapable-split-on-space)))

  (add-hook! 'evil-insert-state-exit-hook
    (defun +corfu-quit-on-evil-insert-state-exit-h ()
      ;; This predicate a workaround for unexpected calls to `corfu-quit' in
      ;; :company-doc-buffer buffers. This was specifically happening when using
      ;; `yasnippet-capf' and `company-yasnippet'.
      (when (eq (current-buffer) (window-buffer (selected-window)))
        (corfu-quit))))

  (after! vertico
    (map! :map corfu-map
          "M-m" #'+corfu-move-to-minibuffer
          (:when (modulep! :editor evil)
            "M-J" #'+corfu-move-to-minibuffer))))

(use-package! cape
  :defer t
  :init
  (add-hook! prog-mode
    (defun +corfu-add-cape-file-h ()
      (add-hook 'completion-at-point-functions #'cape-file -10 t)))
  (add-hook! (org-mode markdown-mode)
    (defun +corfu-add-cape-elisp-block-h ()
      (add-hook 'completion-at-point-functions #'cape-elisp-block 0 t)))
  ;; Enable Dabbrev completion basically everywhere as a fallback.
  (when (modulep! +dabbrev)
    ;; Set up `cape-dabbrev' options.
    (defun +dabbrev-friend-buffer-p (other-buffer)
      (< (buffer-size other-buffer) +corfu-buffer-scanning-size-limit))
    (after! dabbrev
      (setq cape-dabbrev-check-other-buffers t
            dabbrev-friend-buffer-function #'+dabbrev-friend-buffer-p
            dabbrev-ignored-buffer-regexps
            '("^ "
              "\\(TAGS\\|tags\\|ETAGS\\|etags\\|GTAGS\\|GRTAGS\\|GPATH\\)\\(<[0-9]+>\\)?")
            dabbrev-upcase-means-case-search t)
      (add-to-list 'dabbrev-ignored-buffer-modes 'pdf-view-mode)

      (add-hook! (prog-mode text-mode conf-mode comint-mode minibuffer-setup
                            eshell-mode)
        (defun +corfu-add-cape-dabbrev-h ()
          (add-hook 'completion-at-point-functions #'cape-dabbrev 20 t)))))
  (when (modulep! +line)
    ;; Set up `cape-line' options.
    (defun +cape-line-buffers ()
      (cl-loop for buf in (buffer-list)
               if (or (eq major-mode (buffer-local-value 'major-mode buf))
                      (< (buffer-size buf) +corfu-buffer-scanning-size-limit))
               collect buf))
    (setq cape-line-buffer-function #'+cape-line-buffers)
    (add-hook! (text-mode comint-mode minibuffer-setup)
      (defun +corfu-add-cape-line-h ()
        (add-hook 'completion-at-point-functions #'cape-line 20 t))))
  ;; Complete emojis :).
  (when (and (modulep! +emoji) (> emacs-major-version 28))
    (add-hook! (prog-mode conf-mode)
      (defun +corfu-add-cape-emoji-h ()
        (add-hook 'completion-at-point-functions
                  (cape-capf-inside-faces
                   (cape-capf-prefix-length #'cape-emoji 1)
                   ;; Only call inside comments and docstrings.
                   'tree-sitter-hl-face:doc 'font-lock-doc-face
                   'font-lock-comment-face 'tree-sitter-hl-face:comment)
                  10 t)))
    (add-hook! text-mode
      (defun +corfu-add-cape-emoji-text-h ()
        (add-hook 'completion-at-point-functions
                  (cape-capf-prefix-length #'cape-emoji 1) 10 t))))
  ;; Enable dictionary-based autocompletion.
  (when (modulep! +dict)
    (add-hook! (prog-mode conf-mode)
      (defun +corfu-add-cape-dict-h ()
        (add-hook 'completion-at-point-functions
                  (cape-capf-inside-faces
                   ;; Only call inside comments and docstrings.
                   #'cape-dict 'tree-sitter-hl-face:doc 'font-lock-doc-face
                   'font-lock-comment-face 'tree-sitter-hl-face:comment)
                  40 t)))
    (add-hook! text-mode
      (defun +corfu-add-cape-dict-text-h ()
        (add-hook 'completion-at-point-functions #'cape-dict 40 t))))

  ;; Make these capfs composable.
  (advice-add #'comint-completion-at-point :around #'cape-wrap-nonexclusive)
  (advice-add #'eglot-completion-at-point :around #'cape-wrap-nonexclusive)
  (advice-add #'lsp-completion-at-point :around #'cape-wrap-nonexclusive)
  (advice-add #'pcomplete-completions-at-point :around #'cape-wrap-nonexclusive)
  ;; From the `cape' readme. Without this, Eshell autocompletion is broken on
  ;; Emacs28.
  (when (< emacs-major-version 29)
    (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent)
    (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-purify))
  (advice-add #'lsp-completion-at-point :around #'cape-wrap-noninterruptible))

(use-package! yasnippet-capf
  :when (modulep! :editor snippets)
  :config
  (add-hook 'yas-minor-mode-hook
            (defun +corfu-add-yasnippet-capf-h ()
              (add-hook 'completion-at-point-functions #'yasnippet-capf 30 t))))

(use-package! corfu-terminal
  :when (not (display-graphic-p))
  :hook ((corfu-mode . corfu-terminal-mode)))

;;
;;; Extensions

(use-package! corfu-history
  :hook ((corfu-mode . corfu-history-mode))
  :config
  (after! savehist (add-to-list 'savehist-additional-variables 'corfu-history)))


(use-package! corfu-popupinfo
  :hook ((corfu-mode . corfu-popupinfo-mode))
  :config
  (setq corfu-popupinfo-delay '(0.5 . 1.0))
  (map! :map corfu-map
        "C-<up>" #'corfu-popupinfo-scroll-down
        "C-<down>" #'corfu-popupinfo-scroll-up
        "C-S-p" #'corfu-popupinfo-scroll-down
        "C-S-n" #'corfu-popupinfo-scroll-up
        "C-h" #'corfu-popupinfo-toggle)
  (map! :when (modulep! :editor evil)
        :map corfu-popupinfo-map
        ;; Reversed because popupinfo assumes opposite of what feels intuitive
        ;; with evil.
        "C-S-k" #'corfu-popupinfo-scroll-down
        "C-S-j" #'corfu-popupinfo-scroll-up))
