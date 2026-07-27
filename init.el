;; Better scroll
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control) . nil)))
(setq mouse-wheel-progressive-speed nil)

;; Keep Custom's machine-written settings out of init.el (a second
;; custom-set-variables block appended by Custom would break both).
;; The file itself is loaded at the very end of init so GUI-saved
;; customizations win over the defaults set below.
(setq custom-file (locate-user-emacs-file "custom.el"))

(add-to-list 'initial-frame-alist '(fullscreen . maximized))
(setq magit-diff-use-overlays nil)

(put 'set-goal-column 'disabled nil)
(put 'narrow-to-region 'disabled nil)


;; ==================================================
;; Pequeno ganho de responsividade (LSP/TS)
;; ==================================================
(setq read-process-output-max (* 4 1024 1024)) ; 4MB
;; High threshold during startup, then drop to a sane interactive value so
;; individual GC pauses stay short.
(setq gc-cons-threshold (* 128 1024 1024))
(add-hook 'emacs-startup-hook
          (lambda () (setq gc-cons-threshold (* 32 1024 1024))))

;; ==================================================
;; Adding straight
;; ==================================================

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(use-package straight
  :custom
  (straight-use-package-by-default t))

;; ==================================================
;; renaming files and buffers
;; ==================================================


;; source: http://steve.yegge.googlepages.com/my-dot-emacs-file
;; with improvements from GPT-5
(defun rename-file-and-buffer (new-name)
  "Renomeia o arquivo visitado e o buffer para NEW-NAME no mesmo diretório."
  (interactive "sNew name: ")
  (let* ((filename (buffer-file-name)))
    (unless filename
      (user-error "Buffer não está visitando um arquivo"))
    (let* ((dir (file-name-directory filename))
           (new-path (expand-file-name new-name dir)))
      (when (get-buffer new-name)
        (user-error "Já existe um buffer chamado %s" new-name))
      (when (file-exists-p new-path)
        (user-error "Já existe um arquivo chamado %s" new-path))
      (rename-file filename new-path 1)
      (set-visited-file-name new-path t t)
      (rename-buffer (file-name-nondirectory new-path)))))

;; ==================================================
;; Convert camel case to underscore
;; ==================================================

(defun camel-to-snake (beg end)
  "Convert camel case to underscore case in region BEG..END."
  (interactive "r")
  (save-excursion
    (goto-char beg)
    (let ((end-marker (copy-marker end)))
      ;; Only insert "_" between a lower/digit and an upper char, so a
      ;; leading capital doesn't produce a leading underscore.
      (while (re-search-forward "\\([a-z0-9]\\)\\([A-Z]\\)" end-marker t)
        (replace-match "\\1_\\2" t nil))
      (downcase-region beg end-marker)
      (set-marker end-marker nil))))


;; ==================================================
;; LSP MODE
;; ==================================================

(use-package lsp-mode
  :commands lsp-deferred
  :hook ((python-mode      . lsp-deferred)
         (go-mode          . lsp-deferred)
         (typescript-ts-mode . lsp-deferred)
         (tsx-ts-mode        . lsp-deferred))
  :custom
  (lsp-idle-delay 0.20)  ;; default 0.5 – snappier hovers
  (lsp-completion-provider :capf))

;; ==================================================
;; Python with LSP
;; ==================================================

(use-package lsp-pyright
  :after lsp-mode
  :custom
  ;; Tell Pyright where Poetry stores its virtualenvs; auto-detection
  ;; will then Just Work for each project.
  (lsp-pyright-venv-path
   (expand-file-name "~/.cache/pypoetry/virtualenvs")))

;; No manual “poetry env info -p” hook needed; Pyright now finds the env.

;; ==========================================================
;; Exec path from shell (https://github.com/purcell/exec-path-from-shell)
;; ==========================================================

(use-package exec-path-from-shell
  :init
  ;; Drop "-l" (login shell): sourcing zprofile/zshrc/plugins adds hundreds of
  ;; ms to startup. PATH/NVM_DIR live in ~/.zshenv (static, no nvm.sh/pyenv
  ;; init), which non-login zsh does read.
  (setq exec-path-from-shell-arguments nil)
  (setq exec-path-from-shell-variables
		'("PATH" "OPENAI_API_KEY" "NVM_DIR" "GPG_TTY" "SSH_AUTH_SOCK" "LANG" "LC_ALL"))
  (setq exec-path-from-shell-shell-name "zsh")
  (exec-path-from-shell-initialize))


;; To check which shell is being used
;; (shell-command-to-string "echo $SHELL")

;; ==================================================
;; Hooks
;; ==================================================

;; Trim trailing whitespace only in code buffers — markdown uses two trailing
;; spaces as a hard line break, and shared repos get noisy diffs otherwise.
(add-hook 'prog-mode-hook
          (lambda ()
            (add-hook 'before-save-hook #'delete-trailing-whitespace nil t)))

;; ==================================================
;; General config
;; ==================================================

;; Show time at mode-line
(display-time-mode 1)

;; Removes toolbar and scrollbar
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Enable downcase shortcut (C-x C-l) and uppercase (C-x C-u)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; y/n instead of yes/no
(setopt use-short-answers t)

;; Display continuous lines
(setq-default truncate-lines t)

;; trucate even even when screen is split into multiple windows
(setq-default truncate-partial-width-windows nil)

;; Delete selection when we type
(delete-selection-mode 1)

;; Disable anoying alarm bell for errors (arrrgg..! =S)
(setq ring-bell-function 'ignore)

;;===========================================================
;; Git gutter
;;============================================================

(use-package git-gutter
  :config
  (setq git-gutter:update-interval 0.3 git-gutter:disabled-modes '(org-mode))  ; Update intervals for changes

  ;; Customize the signs in the gutter
  ;; (setq git-gutter:modified-sign "  ")  ; Two spaces for modified lines (change the signs as you prefer)
  ;; (setq git-gutter:added-sign "++")     ; '++' for added lines
  ;; (setq git-gutter:deleted-sign "--")   ; '--' for deleted lines

  ;; Choose how to display the signs in the gutter
  ;; (set-face-background 'git-gutter:modified "purple") ; background color for modified lines
  ;; (set-face-foreground 'git-gutter:added "green")     ; text color for added lines
  ;; (set-face-foreground 'git-gutter:deleted "red")     ; text color for deleted lines
  )

(global-git-gutter-mode +1)

;;===========================================================
;; Undo Fu - https://github.com/emacsmirror/undo-fu
;;============================================================

(use-package undo-fu
  :bind ([remap undo] . undo-fu-only-undo))
(use-package vundo :bind ("C-x u" . vundo))

;;============================================================
;; Multiple Cursors
;;============================================================

(use-package multiple-cursors
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-w" . mc/mark-all-like-this))
  )

;;============================================================
;; Appearance
;;============================================================

(global-hl-line-mode -1)
(blink-cursor-mode -1)

;; Line numbers in code and prose buffers only — keep dired/magit/vterm fast.
(setq display-line-numbers-width 2)
(dolist (h '(prog-mode-hook text-mode-hook conf-mode-hook))
  (add-hook h #'display-line-numbers-mode))

(use-package drag-stuff
  :bind (("M-p" . drag-stuff-up)
         ("M-n" . drag-stuff-down)))

(use-package expand-region
  :bind (("C-M-SPC" . er/expand-region)
         ("C-+" . er/contract-region))
  )

;; Themes:

;; (use-package solarized-theme :ensure t :init (load-theme 'solarized-dark :no-confirm))
;; (use-package monokai-theme :ensure t :init (load-theme 'monokai :no-confirm))
(use-package material-theme
  :init (load-theme 'material :no-confirm))

;; ==========================================================
;; IVY (replaces ace-jump)
;; ==========================================================

(use-package avy
  :bind (("C-:" . avy-goto-char)))

(use-package ivy
  :diminish ivy-mode
  :init (ivy-mode 1)
  :custom
  (ivy-use-virtual-buffers t)
  (ivy-count-format "(%d/%d) ")
  (ivy-wrap t)
  (ivy-re-builders-alist
   '((swiper . ivy--regex-plus)
     ;; Fuzzy would turn each char into ".*" in the regex sent to ripgrep:
     ;; noisy matches and slow searches in big repos.
     (counsel-rg . ivy--regex-plus)
     (counsel-find-file . ivy--regex-fuzzy)
     (counsel-projectile-find-file . ivy--regex-fuzzy)
     (t . ivy--regex-fuzzy)))
  :bind (("C-x b" . ivy-switch-buffer)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("RET" . ivy-done)
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)))

;; counsel-mode already remaps find-file → counsel-find-file; no extra bind needed.

(use-package counsel :after ivy :config (counsel-mode 1))
(use-package swiper  :after ivy :bind (("C-s" . swiper)))
(use-package flx)

;; ==========================================================
;; YAML-MODE
;; ==========================================================

(use-package yaml-mode
  :mode
  ("\\.ya?ml\\'" . yaml-mode)
  ("Aioros" . yaml-mode))

;;===========================================================
;; Projectile
;;============================================================

(use-package projectile
  :diminish projectile-mode
  :init (projectile-mode +1)
  :custom
  (projectile-enable-caching nil)
  (projectile-switch-project-action #'projectile-dired)
  (projectile-require-project-root t)
  (projectile-completion-system 'ivy)
  (projectile-globally-ignored-files '("package-lock.json" "poetry.lock"))
  :bind-keymap ("C-c p" . projectile-command-map))

(use-package counsel-projectile
  :config (counsel-projectile-mode))

;;===========================================================
;; Setup counsel-rg to start at project root and bind to projectile map
;;============================================================

(defun my/counsel-rg-at-project-root ()
  (interactive)
  (let ((default-directory (projectile-project-root)))
    (counsel-rg)))

(with-eval-after-load 'counsel-projectile
  (define-key projectile-command-map (kbd "s g") #'my/counsel-rg-at-project-root))

;; ==================================================
;; Paren pairing — built-in electric-pair-mode is lighter than smartparens.
;; ==================================================

(electric-pair-mode 1)
(show-paren-mode 1)

;;===========================================================
;; Magit
;;============================================================

(use-package magit
  :bind ("C-x g" . magit-status))

;; ===========================================================
;; Header line
;; ============================================================

(defmacro with-face (str &rest properties)
  `(propertize ,str 'face (list ,@properties)))

(defun sl/make-header ()
  ""
  (let* ((sl/full-header (abbreviate-file-name buffer-file-name))
		 (sl/header (file-name-directory sl/full-header))
		 (sl/drop-str "[...]"))
    (if (> (length sl/full-header)
		   (window-body-width))
		(if (> (length sl/header)
			   (window-body-width))
			(progn
			  (concat (with-face sl/drop-str
								 :background "blue"
								 :weight 'bold
								 )
					  (with-face (substring sl/header
											(+ (- (length sl/header)
												  (window-body-width))
											   (length sl/drop-str))
											(length sl/header))
								 ;; :background "red"
								 :weight 'bold
								 )))
		  (concat (with-face sl/header
							 ;; :background "red"
							 :foreground "#8fb28f"
							 :weight 'bold
							 )))
	  (concat (with-face sl/header
						 ;; :background "green"
						 ;; :foreground "black"
						 :weight 'bold
						 :foreground "#8fb28f"
						 )
			  (with-face (file-name-nondirectory buffer-file-name)
						 :weight 'bold
						 ;; :background "red"
						 )))))

;; Set header-line-format once via the default value. The :eval form is
;; re-run by redisplay automatically, so we don't need a hook to recompute it.
(setq-default header-line-format
              '("" (:eval (if (buffer-file-name)
                              (sl/make-header)
                            "%b"))))

;;============================================================
;; Duplicate Line
;;============================================================

(defun duplicate-current-line (&optional n)
  "Duplica a linha atual N vezes (padrão 1)."
  (interactive "p")
  (let* ((n (or n 1))
         (bol (line-beginning-position))
         (eol (line-end-position))
         (line (buffer-substring-no-properties bol eol)))
    (save-excursion
      (goto-char eol)
      (open-line n)
      (dotimes (_ n)
        (forward-line 1)
        (insert line)))))


(global-set-key (kbd "C-S-d") #'duplicate-current-line)

;;===========================================================
;; General shortcuts
;;============================================================

;; "Save as" a buffer
(global-set-key (kbd "C-x C-a") 'rename-file-and-buffer)

;; ==========================================================
;; highlight-indentation
;; ==========================================================

(use-package highlight-indent-guides
  :hook (prog-mode . highlight-indent-guides-mode)
  :config
  ;; 'bitmap is the fastest method in graphical Emacs; 'character is sluggish
  ;; on large files.
  (setq highlight-indent-guides-method
        (if (display-graphic-p) 'bitmap 'column)))

;; ==========================================================
;; Which key
;; ==========================================================

;; Built into Emacs 30 — no need for straight to clone it.
(use-package which-key
  :straight (:type built-in)
  :diminish which-key-mode
  :init
  (which-key-mode)
  :config
  (setq which-key-idle-delay 0.3))

;; ==========================================================
;; Company (replaces auto complete)
;; ==========================================================

(use-package company
  :diminish company-mode
  :bind (:map company-active-map
			  ("<tab>" . company-complete-selection))
  :hook (after-init . global-company-mode)
  :config
  (setq company-idle-delay 0.1
        company-minimum-prefix-length 1
        company-show-quick-access t))

(with-eval-after-load 'company
  (define-key company-active-map (kbd "C-<return>") nil)
  (add-hook 'minibuffer-setup-hook (lambda () (company-mode -1)))
  (dolist (m '(term-mode-hook vterm-mode-hook shell-mode-hook eshell-mode-hook))
    (add-hook m (lambda () (company-mode -1)))))

(use-package company-box
  :hook (company-mode . company-box-mode))

;; ==========================================================
;; Protobuffer
;; ==========================================================

(use-package protobuf-mode
  :mode "\\.proto\\'")

;; ==========================================================
;; Editor config
;; ==========================================================

(use-package editorconfig
  :config
  (editorconfig-mode 1)
  ;; Org mode requires tab-width 8; prevent editorconfig from overriding it
  (add-to-list 'editorconfig-exclude-modes 'org-mode))

;; ==========================================================
;; M-RET = open a new line below and indent (replaces textmate-next-line,
;; which depended on the unmaintained textmate.el package).
;; ==========================================================

(global-set-key (kbd "M-RET")
                (lambda ()
                  (interactive)
                  (end-of-line)
                  (newline-and-indent)))

;; ==========================================================
;; Yasippet
;; ==========================================================

(use-package yasnippet
  :config (yas-global-mode 1)
  :bind (("C-c C-h" . yas-expand)))

;; ==========================================================
;; Org Bullets
;; ==========================================================

(use-package org-bullets
  :hook (org-mode . org-bullets-mode))

;; ==========================================================
;; JS/TS mode
;; ==========================================================

;; Onde ficam os .dylib/.so
(setq treesit-extra-load-path
      (seq-filter #'file-directory-p
                  (list (expand-file-name "tree-sitter" "~/.emacs.d/"))))

(setq treesit-language-source-alist
      '((tsx        "https://github.com/tree-sitter/tree-sitter-typescript" nil "tsx/src")
        (typescript "https://github.com/tree-sitter/tree-sitter-typescript" nil "typescript/src")
        (bash       "https://github.com/tree-sitter/tree-sitter-bash")
        (javascript "https://github.com/tree-sitter/tree-sitter-javascript")
        (json       "https://github.com/tree-sitter/tree-sitter-json")))


;; Then run M-x treesit-install-language-grammar and pick
;;  - typescript
;;  - tsx
;;  - bash
;;  - javascript
;;  - json

;; Optional helper that installs grammars on first run
(use-package treesit-auto                       ; MELPA
  :init
  ;; (setq treesit-auto-install 'prompt)           ; or t to skip prompt
  (setq treesit-auto-install t)
  ;; Only manage the languages we intentionally use *-ts-mode for. Without
  ;; this, global-treesit-auto-mode also remaps python-mode/go-mode/yaml-mode
  ;; etc., bypassing the lsp/poetry/gofmt hooks configured on those modes.
  (setq treesit-auto-langs '(typescript tsx javascript bash json))
  :config
  (global-treesit-auto-mode))

(dolist (pair '((typescript-mode . typescript-ts-mode)
                (js-mode         . js-ts-mode)
                (js2-mode        . js-ts-mode)
                (sh-mode         . bash-ts-mode)))
  (add-to-list 'major-mode-remap-alist pair))

(add-to-list 'auto-mode-alist '("\\.ts\\'"  . typescript-ts-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'"  . js-ts-mode))
(add-to-list 'auto-mode-alist '("\\.mjs\\'" . js-ts-mode))
(add-to-list 'auto-mode-alist '("\\.cjs\\'" . js-ts-mode))
(add-to-list 'auto-mode-alist '("\\.json\\'" . json-ts-mode))
(add-to-list 'auto-mode-alist '("\\.sh\\'"   . bash-ts-mode))

;; ==========================================================
;; Other JS things...
;; ==========================================================

(use-package nodejs-repl)

;; use flycheck-verify-setup command to check if eslint is being used
(use-package flycheck
  :init (global-flycheck-mode))

;; This package automatically adds node_modules/.bin to my exec-path in Emacs,
;; ensuring that flycheck uses the local eslint executable from your project.
(use-package add-node-modules-path
  :hook ((js-mode          . add-node-modules-path)
         (web-mode         . add-node-modules-path)
         (typescript-ts-mode . add-node-modules-path)
         (tsx-ts-mode        . add-node-modules-path)))

(with-eval-after-load 'flycheck
  ;; Disable jshint and jscs checkers as we will use eslint
  (dolist (checker '(javascript-jshint javascript-jscs))
    (add-to-list 'flycheck-disabled-checkers checker)))

;; npm i -g typescript-language-server typescript  (once per machine)

;; Format with prettier on save
(use-package apheleia :straight t
  :config
  (setf (alist-get 'tsx-ts-mode apheleia-mode-alist) 'prettier)
  (setf (alist-get 'typescript-ts-mode apheleia-mode-alist) 'prettier)
  (apheleia-global-mode +1))

(unless (executable-find "prettier")
  (message "⚠ prettier not found – no on-save formatting"))

;; ==========================================================
;; GO Things...
;; ==========================================================

;; lsp-deferred is already hooked to go-mode in the lsp-mode block.
(use-package go-mode
  :hook (go-mode . (lambda ()
                     (setq-local tab-width 4)
                     (add-hook 'before-save-hook #'gofmt-before-save nil t))))


;; =========================================================
;; Dockerfile mode
;; =========================================================

(use-package dockerfile-mode)

;; =========================================================
;; Pipenv
;; =========================================================

;; Uncomment only for legacy Pipenv projects (keep disabled by default)
;; (use-package pipenv
;;   :hook (python-mode . pipenv-mode))


;; =========================================================
;; Poetry
;; =========================================================

;; Poetry tracking (optional but handy):
(use-package poetry
  :hook (python-mode . poetry-tracking-mode))

;; ==================================================
;; Using arrows to move over buffers (built-in feature)
;;   https://www.emacswiki.org/emacs/WindMove
;; ==================================================

(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

;; ==================================================
;; Add treemacs
;; https://github.com/Alexander-Miller/treemacs
;; ==================================================

(defun treemacs-toggle ()
  (interactive)
  (if (string= (buffer-name) "*treemacs*")
      (keyboard-escape-quit)
    (treemacs-add-and-display-current-project-exclusively)))

(use-package treemacs
  :bind (("M-0"     . treemacs-toggle)   ;; sobrescreve digit-argument
         ("C-x t t" . treemacs)                 ;; toggle
         ("C-x t 1" . treemacs-delete-other-windows)
         ("C-x t B" . treemacs-bookmark))
  :custom
  (treemacs-is-never-other-window t)
  (treemacs-width 55))


(use-package treemacs-projectile
  :after (treemacs projectile))

;; ==================================================
;; Markdown mode
;; https://github.com/defunkt/markdown-mode
;; ==================================================

(use-package markdown-mode
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)))

;; ==================================================
;; terraform mode
;; ==================================================

(use-package terraform-mode
  :mode (("\\.tf\\'" . terraform-mode)
		 )
  )

;; ==================================================
;; copilot
;; ==================================================

(use-package copilot
  :straight (:host github :repo "copilot-emacs/copilot.el" :files ("*.el"))
  :hook (prog-mode . copilot-mode)
  :bind (:map copilot-completion-map
			  ("C-<return>" . copilot-accept-completion))
  :custom
  ;; Default 100k is too small — silences "*temp* size exceeds copilot-max-char"
  ;; warnings and lets copilot work in larger files.
  (copilot-max-char 500000)
  :config
  ;; disable company inline previews to avoid overlap
  (with-eval-after-load 'company
    (setq company-frontends (delq 'company-preview-if-just-one-frontend company-frontends)))
  )

;; (define-key copilot-completion-map (kbd "C-<return>") 'copilot-accept-completion)

;; ==================================================
;; web-mode
;; ==================================================

(use-package web-mode
  :mode (("\\.jsx\\'" . web-mode))
  :config
  (setq web-mode-enable-auto-indentation nil) ;; Disable auto indentation
  (setq web-mode-enable-auto-quoting nil))  ; Disable automatic insertion of quotes

;; ==================================================
;; org-ai
;; ==================================================

;; Deferred: calling org-ai-global-mode at startup would drag org-ai (and all
;; of Org, one of the heaviest packages) into the initial load. Everything now
;; loads on the first org-mode buffer; the C-c M-a global bindings appear then.
(use-package org-ai
  :hook (org-mode . org-ai-mode)
  :config
  (org-ai-global-mode) ; installs global keybindings on C-c M-a
  ;; Read OPENAI_API_KEY after exec-path-from-shell has propagated env vars.
  (setq org-ai-openai-api-token (getenv "OPENAI_API_KEY"))
  (setq org-ai-default-chat-model "gpt-4o-mini")
  (org-ai-install-yasnippets))

;; ==================================================
;; Doom modeline
;; ==================================================

(use-package nerd-icons :if (display-graphic-p))

;; https://github.com/seagle0128/doom-modeline

(use-package doom-modeline
  :init
  (doom-modeline-mode 1)
  :custom
  (doom-modeline-icon t)
  (doom-modeline-height 25)
  (doom-modeline-lsp t)
  (doom-modeline-buffer-file-name-style 'truncate-except-project)
  :config
  ;; Customize settings here
  (setq doom-modeline-minor-modes nil)          ;; Hide minor modes
  )

;; Load Custom's settings last so they override anything set above.
(when (file-exists-p custom-file)
  (load custom-file nil 'nomessage))
