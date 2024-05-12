;; Load path to load third party libs
(add-to-list 'load-path (expand-file-name "~/.emacs.d/packs"))

(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

; Set up melpa package repository
; (require 'package)
(setq package-enable-at-startup nil)

(setq python-shell-interpreter "python")
(setq python-interpreter "python")

;; (add-to-list 'package-archives
;;              '("gnu" . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
;; (add-to-list 'package-archives
;; 	     '("elpy" . "https://jorgenschaefer.github.io/packages/"))

;; (package-initialize)

;; (let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
;;                     (not (gnutls-available-p))))
;;        (proto (if no-ssl "http" "https")))
;;   (when no-ssl
;;     (warn "\
;; Your version of Emacs does not support SSL connections,
;; which is unsafe because it allows man-in-the-middle attacks.
;; There are two things you can do about this warning:
;; 1. Install an Emacs version that does support SSL and be safe.
;; 2. Remove this warning from your init file so you won't see it again."))
;;   ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
;;   ;; (add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
;;   (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
;;   (add-to-list 'package-archives '("elpy" . "https://jorgenschaefer.github.io/packages/"))
;;   (when (< emacs-major-version 24)
;;     ;; For important compatibility libraries like cl-lib
;;     (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))

;; (require 'package)
;; (setq package-enable-at-startup nil)

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
 '(package-selected-packages
   '(applescript-mode kotlin-mode json-mode mode-mode terraform-mode terraform-doc govet typescript-mode zoom undo-tree monokai-theme format-sql rjsx-mode company-tabnine vue-mode go-mode php-mode csharp-mode magit-gh-pulls dockerfile-mode groovy-mode flycheck ac-js2 js2-mode nodejs-repl exec-path-from-shell org-bullets apib-mode textmate editorconfig protobuf-mode auto-complete golden-ratio magit elpy material-theme flx-ido ido-completing-read+ ido-vertical-mode smartparens projectile yaml-mode ace-jump-mode expand-region drag-stuff multiple-cursors use-package))
 '(python-shell-exec-path nil)
 '(pyvenv-exec-shell "/bin/zsh")
 '(pyvenv-tracking-ask-before-change t)
 '(zoom-mode t nil (zoom))
 '(zoom-size 'size-callback))

(put 'set-goal-column 'disabled nil)
(put 'narrow-to-region 'disabled nil)

;; (setq default-input-method "portuguese-prefix")

;; ==================================================
;; Env vars
;; ==================================================

(setenv "WORKON_HOME" (concat (getenv "HOME") "/.local/share/virtualenvs/"))
(setenv "OPENAI_API_KEY" (getenv "OPENAI_API_KEY"))

;; ==================================================
;; Renaming files and buffers
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
  :ensure t

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

  ;; Optional: Enable diff-hl-mode only in certain modes
  (add-hook 'prog-mode-hook 'git-gutter-mode)
  (add-hook 'text-mode-hook 'git-gutter-mode)
  )

(global-git-gutter-mode +1)

;;===========================================================
;; Undo tree (https://www.emacswiki.org/emacs/UndoTree)
;;============================================================

(use-package undo-tree
  :ensure t)

;;============================================================
;; Multiple Cursors
;;============================================================

(use-package multiple-cursors
  :ensure t
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
  :ensure t
  :bind (("M-p" . drag-stuff-up)
         ("M-n" . drag-stuff-down)))

(use-package expand-region
  :ensure t
  :bind (("C-M-SPC" . er/expand-region)
         ("C-+" . er/contract-region))
  )

(use-package ace-jump-mode
  :ensure t
  :bind (("C-c SPC" . ace-jump-mode)
         ("C-c C-SPC" . ace-jump-mode-pop-mark))
  :config
  (setq ace-jump-mode-case-fold t)
  )

;; ==========================================================
;; YAML-MODE
;; ==========================================================

(use-package yaml-mode
  :ensure t
  :init
  :mode ("\\.yml$" . yaml-mode))

;;===========================================================
;; Projectile
;;============================================================

;; Need to be "setted" before load projectile
(setq projectile-keymap-prefix (kbd "C-c p"))

(use-package projectile
  :ensure t
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
        ;; projectile-completion-system 'ivy
        ))

;; ==================================================
;; Smartparens
;; ==================================================

(use-package smartparens
  :ensure t
  :diminish smartparens-mode
  :config
  (progn
    (require 'smartparens-config)
    (smartparens-global-mode 1)))

;; ==================================================
;; Ido
;; ==================================================

(use-package ido
  :ensure t
  :init
  (ido-mode t)
  :config
  (setq ido-enable-prefix nil
        ido-enable-flex-matching t
        ido-create-new-buffer 'always
        ido-use-filename-at-point 'guess
        ido-max-prospects 10
        ido-default-file-method 'selected-window
        ido-file-extensions-order '(".py")
        ido-auto-merge-work-directories-length -1)
  (add-to-list 'ido-ignore-files '(".DS_Store" ".pyc"))
  (add-to-list 'ido-ignore-directories '("__pycache__", ".git"))

  (use-package ido-vertical-mode
    :ensure t
    :init
    (setq ido-vertical-decorations (list "\n➜ " "" "\n" "\n..." "[" "]" " [No match]" " [Matched]" " [Not readable]" " [Too big]" " [Confirm]" "\n" ""))
    (ido-vertical-mode 1))

  (use-package ido-completing-read+
    :ensure t
    :init
    (ido-ubiquitous-mode +1))

  (use-package flx-ido
    :ensure t
    :init
    (flx-ido-mode +1)))

;; ==================================================
;; Themes
;; ==================================================

;; (use-package solarized-theme :ensure t :init (load-theme 'solarized-dark :no-confirm))
;; (use-package monokai-theme :ensure t :init (load-theme 'monokai :no-confirm))
(use-package material-theme :ensure t :init (load-theme 'material :no-confirm))

;; ==================================================
;; Elpy
;; ==================================================

(use-package elpy
  :ensure t
  :init
  (elpy-enable))

;;===========================================================
;; Magit
;;============================================================

(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status))

;; ==========================================================
;; Evaluate lisp expressions and replace with result
;; ==========================================================

(defun replace-last-sexp ()
    (interactive)
    (let ((value (eval (preceding-sexp))))
      (kill-sexp -1)
      (insert (format "%S" value))))

(global-set-key (kbd "C-c C-f") 'replace-last-sexp)

;;===========================================================

(setq org-cycle-emulate-tab 'whitestart)

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
;; Golden Ratio / Zoom package
;; ==========================================================

;; Zoom docs: https://github.com/cyrus-and/zoom

(defun size-callback ()
  (cond ((> (frame-pixel-width) 1280) '(120 . 0))
        (t                            '(0.5 . 0.5))))

(use-package zoom
  :ensure t
  :init
  (custom-set-variables '(zoom-mode t))
  (custom-set-variables
   '(zoom-size '(0.618 . 0.618)))
  (custom-set-variables
   '(zoom-size 'size-callback))
  )

;; Disable to try Zoom package
;; (use-package golden-ratio
;;   :ensure t
;;   :diminish golden-ratio-mode
;;   :init
;;   (golden-ratio-mode 1)
;;   (setq golden-ratio-adjust-factor .9
;; 	golden-ratio-wide-adjust-factor .9))

;; ==========================================================
;; highlight-indentation
;; ==========================================================

(use-package highlight-indentation
  :ensure t
  :init
  (add-hook 'enh-ruby-mode-hook
	    (lambda () (highlight-indentation-current-column-mode)))
  (add-hook 'enh-ruby-mode-hook
	    (lambda () (highlight-indentation-mode)))
  :config
  (set-face-background 'highlight-indentation-face "#692C2C")
  (set-face-background 'highlight-indentation-current-column-face "#850B0B"))


;; ==========================================================
;; Auto complete
;; ==========================================================

(use-package auto-complete
  :ensure t
  :config
  (require 'auto-complete-config)
  (add-to-list 'ac-dictionary-directories
	       "~/.emacs.d/elpa/auto-complete-20160827.649/dict")
  (ac-config-default)
  (setq ac-ignore-case nil)
  (add-to-list 'ac-modes 'enh-ruby-mode)
  (add-to-list 'ac-modes 'web-mode))

;; ==========================================================
;; Protobuffer
;; ==========================================================

(use-package protobuf-mode
  :ensure t)

;; ==========================================================
;; Editor config
;; ==========================================================

(use-package editorconfig
  :ensure t
  :init
  (add-hook 'prog-mode-hook (editorconfig-mode 1))
  (add-hook 'text-mode-hook (editorconfig-mode 1))
  )

;; ==========================================================
;; Textmate minnor mode (https://melpa.org/#/textmate)
;; ==========================================================

(use-package textmate
  :ensure t
  :init
  (textmate-mode)
  :config
  (global-set-key (kbd "M-RET") 'textmate-next-line))

;; ==========================================================
;; Yasippet
;; ==========================================================

(use-package yasnippet
  :ensure t
  :config
    (yas-reload-all)
    (yas-global-mode 1)
  :init
    (add-hook 'prog-mode-hook #'yas-minor-mode)
    :bind (("C-c C-h" . yas-expand)))

;; ==========================================================
;; API Blue print
;; ==========================================================

(use-package apib-mode
  :ensure t)

;; ==========================================================
;; Org Bullets
;; ==========================================================

(use-package org-bullets
  :ensure t)

;; ==========================================================
;; Exec path from shell (https://github.com/purcell/exec-path-from-shell)
;; ==========================================================

(use-package exec-path-from-shell
  :ensure t
  :init
  (exec-path-from-shell-initialize))

;; ==========================================================
;; JS things...
;; ==========================================================

(use-package nodejs-repl
  :ensure t)

(use-package js2-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
  (setq js2-mode-show-parse-errors nil)
  (setq js2-mode-show-strict-warnings nil))

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

;; ==========================================================
;; Typescript
;; ==========================================================

(use-package typescript-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.tsx$" . typescript-mode)))

;; ==========================================================
;; GO Things...
;; ==========================================================

;; Before safe hook to format
(add-hook 'go-mode-hook
          (lambda ()
            (add-hook 'before-save-hook 'gofmt-before-save)
            (setq tab-width 4)
            (setq indent-tabs-mode 1)))

(use-package go-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.go$" . go-mode)))

;; =========================================================
;; Groovy mode (using to edit files like Jenkinsfile)
;; =========================================================

(use-package groovy-mode
  :ensure t)

;; =========================================================
;; Dockerfile mode
;; =========================================================

(use-package dockerfile-mode
  :ensure t)

;; =========================================================
;; Pipenv
;; =========================================================

(use-package pipenv
  :hook (python-mode . pipenv-mode)
  :init
  (setq
   pipenv-projectile-after-switch-function
   'pipenv-projectile-after-switch-extended))

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
;; Add dired-subtree
;; https://github.com/Fuco1/dired-hacks
;; ==================================================

(use-package dired-subtree
  :config
  (define-key dired-mode-map "i" 'dired-subtree-insert)
  (define-key dired-mode-map ";" 'dired-subtree-remove))

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

;; disable markdown-mode shortcut C-c C-c (to be used with treemacs)
(with-eval-after-load 'markdown-mode
  (define-key markdown-mode-map (kbd "C-c C-c") nil))

(use-package treemacs-projectile
  :ensure t

  :bind
  ;; ("M-0" . treemacs-select-window)
  ("M-0" . treemacs-toggle)
  ("C-c C-c" . treemacs-display-current-project-exclusively)

  :config
  (setq treemacs-is-never-other-window t)
  ;; increase width
  (setq treemacs-width 55))


(use-package treemacs-projectile
  :after (treemacs projectile)
  :ensure t)

;; ==================================================
;; Markdown mode
;; https://github.com/defunkt/markdown-mode
;; ==================================================

(use-package markdown-mode
  :ensure t
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)))

;; ==================================================
;; terraform mode
;; ==================================================

(use-package terraform-mode
  :ensure t
  :mode (("\\.tf\\'" . terraform-mode)
	 )
  )

;; ==================================================
;; copilot
;; ==================================================

(use-package copilot
  :straight (:host github :repo "copilot-emacs/copilot.el" :files ("dist" "*.el"))
  :ensure t
  :init
  (add-hook 'prog-mode-hook 'copilot-mode))

(with-eval-after-load 'company
  ;; disable inline previews
  (delq 'company-preview-if-just-one-frontend company-frontends))

(define-key copilot-completion-map (kbd "C-<return>") 'copilot-accept-completion)

;; ==================================================
;; rxjs-mode
;; ==================================================

(use-package rjsx-mode
  :ensure t
  :mode (("\\.js\\'" . rjsx-mode)
	 ("\\.jsx\\'" . rjsx-mode))
  )
