;; Load path to load third party libs
(add-to-list 'load-path (expand-file-name "~/.emacs.d/packs"))

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
 '(python-shell-exec-path nil)
 '(pyvenv-exec-shell "/bin/zsh")
 '(pyvenv-tracking-ask-before-change t)
 '(zoom-mode t nil (zoom))
 '(zoom-size 'size-callback))

(put 'set-goal-column 'disabled nil)
(put 'narrow-to-region 'disabled nil)

;; (setq default-input-method "portuguese-prefix")

;; ==================================================
;; renaming files and buffers
;; ==================================================

;; source: http://steve.yegge.googlepages.com/my-dot-emacs-file
(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (progn
          (rename-file name new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil))))))

;; ==================================================
;; Convert camel case to underscore
;; ==================================================

(defun camel-to-snake () (interactive)
       "Convert camel case to underscore case"
       (progn
	 (replace-regexp "\\([A-Z]\\)" "_\\1" nil (region-beginning) (region-end))
	 (downcase-region (region-beginning) (region-end))))

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

;; ==================================================
;; Python
;; ==================================================

;; Add lsp-mode and lsp-pyright:
;; Use lsp-describe-session (M-x lsp-describe-session) to check the current session
(use-package lsp-mode
  :straight t
  :hook (python-mode . lsp)
  :commands lsp
  :config
  (setq lsp-prefer-flymake nil))  ; Use flycheck instead of flymake

(use-package lsp-pyright
  :straight t
  :after lsp-mode
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp))))

(defun use-current-project-poetry-venv ()
  (interactive)
  (let ((venv (shell-command-to-string "poetry env info -p")))
    (setq lsp-pyright-venv-path (string-trim venv))))

(add-hook 'python-mode-hook #'use-current-project-poetry-venv)

;; ==========================================================
;; Exec path from shell (https://github.com/purcell/exec-path-from-shell)
;; ==========================================================

(use-package exec-path-from-shell
  :straight t
  :init
  ;; (setq exec-path-from-shell-check-startup-files nil)
  ;; (setq exec-path-from-shell-arguments '("-l"))
  (setq exec-path-from-shell-arguments '("-l" "-i")) ;; Use this if needed
  (setq exec-path-from-shell-variables '("PATH" "OPENAI_API_KEY"))
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

;; Disable scroll all mode (scroll all opened buffers at same time), it's causing a screen blink
(scroll-all-mode -1)

;; Enable downcase shortcut (C-x C-l) and uppercase (C-x C-u)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; y/n instead of yes/no
(defalias 'yes-or-no-p 'y-or-n-p)

;; Display continuous lines
(setq-default truncate-lines 1)

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
  :straight t

  :config
  (setq git-gutter:update-interval 2)  ; Update intervals for changes
  (setq display-line-numbers-width 2)

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
;; Undo tree (https://www.emacswiki.org/emacs/UndoTree)
;;============================================================

(use-package undo-tree
  :straight t
  )

;;============================================================
;; Multiple Cursors
;;============================================================

(use-package multiple-cursors
  :straight t
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

(use-package drag-stuff
  :straight t
  :bind (("M-p" . drag-stuff-up)
         ("M-n" . drag-stuff-down)))

(use-package expand-region
  :straight t
  :bind (("C-M-SPC" . er/expand-region)
         ("C-+" . er/contract-region))
  )

;; Themes:

;; (use-package solarized-theme :ensure t :init (load-theme 'solarized-dark :no-confirm))
;; (use-package monokai-theme :ensure t :init (load-theme 'monokai :no-confirm))
(use-package material-theme
  :straight t
  :init (load-theme 'material :no-confirm))

;; ==========================================================
;; AVY (replaces ace-jump)
;; ==========================================================

(use-package counsel
  :straight t
  :after ivy
  :config (counsel-mode 1))

;; Disable ido-mode if it's enabled
(ido-mode -1)

;; Use counsel-find-file for C-x C-f
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
;; replace C-c p s g (original counsel-projectile-grep, by counsel-rg)
(global-set-key (kbd "C-c p s g") 'counsel-rg)

(use-package ivy
  :straight t
  :diminish (ivy-mode)
  :bind (("C-:" . avy-goto-char)
         ("C-x b" . ivy-switch-buffer)
         ("C-x C-f" . counsel-find-file)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("RET" . ivy-alt-done)  ;; Use 'ivy-alt-done' to enter directories
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line))
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t
        ivy-count-format "(%d/%d) "
        ivy-wrap t
        ;; Enable fuzzy matching for all commands except swiper
        ivy-re-builders-alist
        '((swiper . ivy--regex-plus)   ;; Regular regex matching for swiper
	  (counsel-M-x . ivy--regex-fuzzy) ;; Fuzzy matching for M-x
          (t . ivy--regex-plus))))    ;; Fuzzy matching for everything else

(use-package flx
  :straight t)

(use-package swiper
  :straight t
  :bind (("C-s" . swiper)))

;; ==========================================================
;; YAML-MODE
;; ==========================================================

(use-package yaml-mode
  :straight t
  :init
  :mode ("\\.yml$" . yaml-mode))

;;===========================================================
;; Projectile
;;============================================================

;; Need to be "setted" before load projectile
(setq projectile-keymap-prefix (kbd "C-c p"))

(use-package projectile
  :straight t
  :init
  (projectile-global-mode t)
  :config
  (setq projectile-enable-caching t
        projectile-use-git-grep t
        projectile-switch-project-action 'projectile-dired
        projectile-require-project-root nil
        projectile-mode-line '(:eval (format " P[%s]" (projectile-project-name)))
        ;; projectile-completion-system 'grizzl
        ;; projectile-completion-system 'helm
        projectile-require-project-root t
        ;; projectile-indexing-method 'alien
        projectile-completion-system 'ivy
	;; Optionally, exclude specific files globally
	projectile-globally-ignored-files '("package-lock.json" "poetry.lock")
        ))

(use-package counsel-projectile
  :straight t
  :config (counsel-projectile-mode))

;; ==================================================
;; Smartparens
;; ==================================================

(use-package smartparens
  :straight t
  :diminish smartparens-mode
  :config
  (progn
    (require 'smartparens-config)
    (smartparens-global-mode 1)))

;;===========================================================
;; Magit
;;============================================================

(use-package magit
  :straight t
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
  "duplicate current line, make more than 1 copy given a numeric argument"
  (interactive "p")
  (save-excursion
    (let ((nb (or n 1))
    	  (current-line (thing-at-point 'line)))
      ;; when on last line, insert a newline first
      (when (or (= 1 (forward-line 1)) (eq (point) (point-max)))
    	(insert "\n"))

      ;; now insert as many time as requested
      (while (> n 0)
    	(insert current-line)
    	(decf n)))))

(global-set-key (kbd "C-S-d") 'duplicate-current-line)

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
  :straight t
  :init
  (custom-set-variables '(zoom-mode t))
  (custom-set-variables
   '(zoom-size '(0.618 . 0.618)))
  (custom-set-variables
   '(zoom-size 'size-callback))
  )

;; ==========================================================
;; highlight-indentation
;; ==========================================================

(use-package highlight-indent-guides
  :straight t
  :hook (prog-mode . highlight-indent-guides-mode)
  :config
  (setq highlight-indent-guides-method 'character))

;; ==========================================================
;; Which key
;; ==========================================================

(use-package which-key
  :straight t
  :diminish which-key-mode
  :init
  (which-key-mode)
  :config
  (setq which-key-idle-delay 0.3))

;; ==========================================================
;; Company (replaces auto complete)
;; ==========================================================

(use-package company
  :straight t
  :diminish company-mode
  :bind (:map company-active-map
              ("<tab>" . company-complete-selection))
  :hook (after-init . global-company-mode)
  :config
  (setq company-idle-delay 0
        company-minimum-prefix-length 1
        company-show-numbers t))

(with-eval-after-load 'company
  (define-key company-active-map (kbd "C-<return>") nil))

(use-package company-box
  :straight t
  :hook (company-mode . company-box-mode))

;; ==========================================================
;; Protobuffer
;; ==========================================================

(use-package protobuf-mode
  :straight t
  )

;; ==========================================================
;; Editor config
;; ==========================================================

(use-package editorconfig
  :straight t
  :config
  (editorconfig-mode 1)
  )

;; ==========================================================
;; Textmate minnor mode (https://melpa.org/#/textmate)
;; ==========================================================

(use-package textmate
  :straight t
  :init
  (textmate-mode)
  :config
  (global-set-key (kbd "M-RET") 'textmate-next-line))

;; ==========================================================
;; Yasippet
;; ==========================================================

(use-package yasnippet
  :straight t
  :config
    (yas-reload-all)
    (yas-global-mode 1)
  :init
    (add-hook 'prog-mode-hook #'yas-minor-mode)
    :bind (("C-c C-h" . yas-expand)))

;; ==========================================================
;; Org Bullets
;; ==========================================================

(use-package org-bullets
  :straight t
  )

;; ==========================================================
;; JS things...
;; ==========================================================

(use-package nodejs-repl
  :straight t
  )

(use-package js2-mode
  :straight t
  :mode "\\.js\\'"
  :interpreter "node"
  :config
  (setq js2-basic-offset 2
        js-indent-level 2))

(use-package flycheck
  :straight t
  :init (global-flycheck-mode))

;; ==========================================================
;; Typescript
;; ==========================================================

(use-package typescript-mode
  :straight t
  :config
  (add-to-list 'auto-mode-alist '("\\.tsx$" . typescript-mode)))

;; ==========================================================
;; GO Things...
;; ==========================================================

(use-package go-mode
  :straight t
  :hook (before-save . gofmt-before-save)
  :config
  (setq tab-width 4
        indent-tabs-mode 1))

(use-package lsp-mode
  :straight t
  :hook (go-mode . lsp)
  :commands lsp)

;; =========================================================
;; Dockerfile mode
;; =========================================================

(use-package dockerfile-mode
  :straight t
  )

;; =========================================================
;; Pipenv
;; =========================================================

(use-package pipenv
  :hook (python-mode . pipenv-mode)
  :straight t
  :init
  (setq
   pipenv-projectile-after-switch-function
   'pipenv-projectile-after-switch-extended))

;; =========================================================
;; Poetry
;; =========================================================

;; Use poetry:
;; (use-package poetry
;;   :straight t
;;   :hook (python-mode . poetry-tracking-mode))

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

;; treemacs func to toggle treemacs-select-window and quit (q key) if treemacs is current buffer
(defun treemacs-toggle ()
  (interactive)
  (if (string= (buffer-name) "*treemacs*")
      (keyboard-escape-quit)
    (treemacs-select-window)))

(use-package treemacs-projectile
  :straight t
  ;; :after (treemacs projectile)
  :bind
  ("M-0" . treemacs-display-current-project-exclusively)

  :config
  (setq treemacs-is-never-other-window t)
  ;; increase width
  (setq treemacs-width 55))

;; ==================================================
;; Markdown mode
;; https://github.com/defunkt/markdown-mode
;; ==================================================

(use-package markdown-mode
  :straight t
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)))

;; ==================================================
;; terraform mode
;; ==================================================

(use-package terraform-mode
  :straight t
  :mode (("\\.tf\\'" . terraform-mode)
	 )
  )

;; ==================================================
;; copilot
;; ==================================================

(use-package copilot
  :straight (:host github :repo "copilot-emacs/copilot.el" :files ("dist" "*.el"))
  :init
  (add-hook 'prog-mode-hook 'copilot-mode))

(with-eval-after-load 'company
  ;; disable inline previews
  (delq 'company-preview-if-just-one-frontend company-frontends))

(define-key copilot-completion-map (kbd "C-<return>") 'copilot-accept-completion)

;; ==================================================
;; web-mode
;; ==================================================

(use-package web-mode
  :straight t
  :mode (("\\.tsx\\'" . web-mode)
         ("\\.jsx\\'" . web-mode))
  :config
  (setq web-mode-enable-auto-indentation nil) ;; Disable auto indentation
  (setq web-mode-enable-auto-quoting nil))  ; Disable automatic insertion of quotes

;; ==================================================
;; tide
;; ==================================================

;; https://github.com/ananthakumaran/tide
(use-package tide
  :straight t
  :after (typescript-mode company flycheck)
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)
         (before-save . tide-format-before-save)) ;; Indentation on save, if you want to disable it, just comment this line
  :config
  (setq tide-format-options '(:indentSize 2 :tabSize 2)))

;; ==================================================
;; org-ai
;; ==================================================

(setq org-ai-openai-api-token (getenv "OPENAI_API_KEY"))

(use-package org-ai
  :straight t
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

;; (use-package nerd-icons
;;   :straight t)

(use-package all-the-icons
  ;; :straight (:host github
  ;;            :repo "domtronn/all-the-icons.el"
  ;;            :commit "52d1f2d")  ;; Replace <commit-hash> with the hash for version 3.4.0
  :straight t
  :commands (nerd-icons-octicon
             nerd-icons-faicon
             nerd-icons-flicon
             nerd-icons-wicon
             nerd-icons-mdicon
             nerd-icons-codicon
             nerd-icons-devicon
             nerd-icons-ipsicon
             nerd-icons-pomicon
             nerd-icons-powerline)
  :if (display-graphic-p))  ;; Load only if Emacs is in GUI mode

;; https://github.com/seagle0128/doom-modeline

(use-package doom-modeline
  :straight t
  :init
  (doom-modeline-mode 1)
  :config
  ;; Customize settings here
  (setq doom-modeline-height 25)                ;; Set the height of the modeline
  (setq doom-modeline-minor-modes nil)          ;; Hide minor modes
  (setq doom-modeline-icon t)				    ;; Show icons
  (setq doom-modeline-icon-type 'all-the-icons) ;; Use all-the-icons as the icon set
  (setq doom-modeline-lsp t)                    ;; Show LSP info in the modeline
  (setq doom-modeline-buffer-file-name-style 'truncate-except-project)  ;; Truncate path
  )
