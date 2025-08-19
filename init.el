;; Better scroll
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control) . nil)))
(setq mouse-wheel-progressive-speed nil)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(desktop-save-mode t)
 '(initial-frame-alist '((fullscreen . maximized)))
 '(magit-commit-arguments '("--gpg-sign=A41BF0ECF08B6764"))
 '(magit-diff-use-overlays nil)
 '(package-selected-packages nil)
 '(python-shell-exec-path nil))

(put 'set-goal-column 'disabled nil)
(put 'narrow-to-region 'disabled nil)


;; ==================================================
;; Pequeno ganho de responsividade (LSP/TS)
;; ==================================================
(setq read-process-output-max (* 4 1024 1024)) ; 4MB
(setq gc-cons-threshold (* 128 1024 1024))     ; 128MB

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

(defun camel-to-snake () (interactive)
       "Convert camel case to underscore case"
       (progn
		 (replace-regexp "\\([A-Z]\\)" "_\\1" nil (region-beginning) (region-end))
		 (downcase-region (region-beginning) (region-end))))


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
  (lsp-prefer-flymake nil)
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
  (setq exec-path-from-shell-arguments '("-l"))
  (setq exec-path-from-shell-variables
		'("PATH" "OPENAI_API_KEY" "NVM_DIR" "GPG_TTY" "SSH_AUTH_SOCK" "LANG" "LC_ALL"))
  (setq exec-path-from-shell-shell-name "zsh")
  (exec-path-from-shell-initialize))


;; To check which shell is being used
;; (shell-command-to-string "echo $SHELL")

;; ==================================================
;; Recent files
;; ==================================================

;; (require 'recentf)
;; (recentf-mode 1)
;; (setq recentf-max-menu-items 25)
;; (global-set-key "\C-x\ \C-g" 'recentf-open-files)

;; ==================================================
;; Hooks
;; ==================================================

;; After save hook example

;; (defun make-html()
;;  (shell-command ". /home/alisson.perez/.virtualenvs/djomd/bin/activate; cd /home/alisson.perez/Devel/django-reporting/docs/; make html")
;;  )
;; (add-hook 'after-save-hook 'make-html)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; ==================================================
;; General config
;; ==================================================

;; Show time at mode-line
(display-time-mode 1)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Removes toolbar and scrollbar
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Enable downcase shortcut (C-x C-l) and uppercase (C-x C-u)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; y/n instead of yes/no
(defalias 'yes-or-no-p 'y-or-n-p)

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

(use-package undo-fu :init (global-set-key [remap undo] #'undo-fu-only-undo))
(use-package vundo :after undo-fu :bind ("C-x u" . vundo))

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

;; Line numbers
(global-display-line-numbers-mode 1)
(setq display-line-numbers-width 2)

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

(use-package avy)

;; Disable ido-mode if it's enabled
(ido-mode -1)

;; replace C-c p s g (original counsel-projectile-grep, by counsel-rg)
(global-set-key (kbd "C-c p s g") 'counsel-rg)

(use-package ivy
  :diminish ivy-mode
  :init (ivy-mode 1)
  :custom
  (ivy-use-virtual-buffers t)
  (ivy-count-format "(%d/%d) ")
  (ivy-wrap t)
  (ivy-re-builders-alist
   '((swiper . ivy--regex-plus)
     (counsel-find-file . ivy--regex-fuzzy)
     (counsel-projectile-find-file . ivy--regex-fuzzy)
     (t . ivy--regex-fuzzy)))
  :bind (("C-:" . avy-goto-char)
         ("C-x b" . ivy-switch-buffer)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("RET" . ivy-done)
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)))


;; Um único bind para find-file já basta:
(global-set-key (kbd "C-x C-f") #'counsel-find-file)

(use-package counsel :after ivy :config (counsel-mode 1))
(use-package swiper  :after ivy :bind (("C-s" . swiper)))
(use-package flx)

;; ==========================================================
;; YAML-MODE
;; ==========================================================

(use-package yaml-mode
  :init
  :mode
  ("\\.ya?ml$" . yaml-mode)
  ("Aioros" . yaml-mode)
  )

;;===========================================================
;; Projectile
;;============================================================

(use-package projectile
  :diminish projectile-mode
  :init (projectile-mode +1)
  :custom
  (projectile-enable-caching t)
  (projectile-switch-project-action #'projectile-dired)
  (projectile-require-project-root t)
  (projectile-completion-system 'ivy)
  (projectile-globally-ignored-files '("package-lock.json" "poetry.lock"))
  :bind-keymap ("C-c p" . projectile-command-map))

(use-package counsel-projectile
  :config (counsel-projectile-mode))

;; ==================================================
;; Smartparens
;; ==================================================

(use-package smartparens
  :diminish smartparens-mode
  :config
  (progn
    (require 'smartparens-config)
    (smartparens-global-mode 1)))

;;===========================================================
;; Magit
;;============================================================

(use-package magit
  :bind ("C-x g" . magit-status))

;;===========================================================

;; (setq org-cycle-emulate-tab 'whitestart)

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

(defun sl/display-header ()
  (setq header-line-format
		'("" ;; invocation-name
		  (:eval (if (buffer-file-name)
					 (sl/make-header)
				   "%b")))))

(add-hook 'buffer-list-update-hook
		  'sl/display-header)

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
;; Zoom package
;; ==========================================================

;; Zoom docs: https://github.com/cyrus-and/zoom

(defun size-callback ()
  (cond ((> (frame-pixel-width) 1280) '(120 . 0))
        (t                            '(0.5 . 0.5))))

(use-package zoom
  :custom
  (zoom-mode t)
  (zoom-size 'size-callback))

;; ==========================================================
;; highlight-indentation
;; ==========================================================

(use-package highlight-indent-guides
  :hook (prog-mode . highlight-indent-guides-mode)
  :config
  (setq highlight-indent-guides-method 'character))

;; ==========================================================
;; Which key
;; ==========================================================

(use-package which-key
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
  (setq company-idle-delay 0
        company-minimum-prefix-length 1
        company-show-numbers t))

(with-eval-after-load 'company
  (define-key company-active-map (kbd "C-<return>") nil)
  (add-hook 'minibuffer-setup-hook (lambda () (company-mode -1)))
  (dolist (m '(term-mode vterm-mode shell-mode eshell-mode))
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
  )

;; ==========================================================
;; Textmate minnor mode (https://melpa.org/#/textmate)
;; ==========================================================

(use-package textmate
  :init
  (textmate-mode)
  :config
  (global-set-key (kbd "M-RET") 'textmate-next-line))

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
;; JS things...
;; ==========================================================

;; Onde os .so/.dylib de grammars ficam
(setq treesit-extra-load-path
      (list (expand-file-name "tree-sitter" user-emacs-directory)))


(setq treesit-language-source-alist
      '((typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
        (tsx        "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
        (bash       "https://github.com/tree-sitter/tree-sitter-bash" "master" "src")
		))

;; Then run M-x treesit-install-language-grammar and pick
;;  - typescript
;;  - tsx
;;  - bash

;; Optional helper that installs grammars on first run
(use-package treesit-auto                       ; MELPA
  :init
  ;; (setq treesit-auto-install 'prompt)           ; or t to skip prompt
  (setq treesit-auto-install t)
  :config
  (global-treesit-auto-mode)
  )

(dolist (pair '((typescript-mode . typescript-ts-mode)
                (js-mode         . js-ts-mode)
                (js2-mode        . js-ts-mode)))
  (add-to-list 'major-mode-remap-alist pair))

;; Bash moderno
(add-to-list 'major-mode-remap-alist '(sh-mode . bash-ts-mode))

(use-package nodejs-repl)

;; use flycheck-verify-setup command to check if eslint is being used
(use-package flycheck
  :init (global-flycheck-mode))

;; This package automatically adds node_modules/.bin to my exec-path in Emacs,
;; ensuring that flycheck uses the local eslint executable from your project.
(use-package add-node-modules-path
  :hook ((js-mode . add-node-modules-path)
         (web-mode . add-node-modules-path)))

(add-hook 'typescript-ts-mode-hook #'add-node-modules-path)
(add-hook 'tsx-ts-mode-hook        #'add-node-modules-path)

(with-eval-after-load 'flycheck
  ;; Disable jshint and jscs checkers as we will use eslint
  (setq-default flycheck-disabled-checkers
                (append flycheck-disabled-checkers
                        '(javascript-jshint javascript-jscs))))

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

(use-package go-mode
  :hook ((go-mode . lsp-deferred)
         (before-save . gofmt-before-save))
  :custom (tab-width 4))


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
;; Macros
;; ==================================================

(fset 'single_quotes
	  (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([134217765 34 return 39 return 33] 0 "%d")) arg)))

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
  :ensure t
  :hook (prog-mode . copilot-mode)
  :bind (:map copilot-completion-map
			  ("C-<return>" . copilot-accept-completion))
  :config
  ;; disable company inline previews to avoid overlap
  (with-eval-after-load 'company
    (delq 'company-preview-if-just-one-frontend company-frontends))
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

(setq org-ai-openai-api-token (getenv "OPENAI_API_KEY"))

(use-package org-ai
  :commands (org-ai-mode
             org-ai-global-mode)
  :init
  (add-hook 'org-mode-hook #'org-ai-mode) ; enable org-ai in org-mode
  (org-ai-global-mode) ; installs global keybindings on C-c M-a
  :config
  (setq org-ai-default-chat-model "gpt-4o-mini") ; if you are on the gpt-4 beta:
  (org-ai-install-yasnippets)) ; if you are using yasnippet and want `ai` snippets

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
