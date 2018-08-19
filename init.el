(require 'package) ;; You might already have this line

;; Better scroll
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control) . nil)))
(setq mouse-wheel-progressive-speed nil)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(desktop-save-mode t)
 '(initial-frame-alist (quote ((fullscreen . maximized))))
 '(magit-diff-use-overlays nil)
 '(package-selected-packages
   (quote
    (ido-completing-read+ js2-mode magit-gh-pulls typescript-mode auto-complete groovy-mode go-mode helm-spotify flycheck ac-js2 tern-auto-complete tern js3-mode nodejs-repl exec-path-from-shell tern-mode org-bullets neotree apib-mode git-gutter-fringe solarized-theme monokai-theme yaml-mode use-package textmate smartparens ruby-tools rubocop rspec-mode robe rbenv protobuf-mode projectile multiple-cursors material-theme magit ido-vertical-mode ido-ubiquitous grizzl gradle-mode golden-ratio flx-ido expand-region enh-ruby-mode elpy editorconfig drag-stuff dash-at-point ace-jump-mode))))

;; ==================================================
;; Package archives
;; ==================================================

(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("elpy" . "https://jorgenschaefer.github.io/packages/"))

(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))

(package-initialize) ;; You might already have this line

;; ==================================================
;; use-package
;; ==================================================

(if (not (package-installed-p 'use-package))
    (progn
      (package-refresh-contents)
      (package-install 'use-package)))
(require 'use-package)

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
;; Recent files
;; ==================================================

(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(global-set-key "\C-x\ \C-g" 'recentf-open-files)

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
(setq-default truncate-lines 1)

;; trucate even even when screen is split into multiple windows
(setq-default truncate-partial-width-windows nil)

;; Delete selection when we type
(delete-selection-mode 1)

;; Disable anoying alarm bell for errors (arrrgg..! =S)
(setq ring-bell-function 'ignore)

;;===========================================================
;; Git gutter fringe
;;============================================================

(use-package git-gutter-fringe
  :ensure t
  :init
  (global-git-gutter-mode t)
  )

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
(global-linum-mode t)

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
  (use-package grizzl :ensure t)
  (setq projectile-enable-caching t
        projectile-use-git-grep t
        projectile-switch-project-action 'projectile-dired
        projectile-require-project-root nil
        projectile-mode-line '(:eval (format " P[%s]" (projectile-project-name)))
        projectile-completion-system 'grizzl
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
;; ORG-MODE
;; ==========================================================


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
;; Golden Ratio
;; ==========================================================

(use-package golden-ratio
  :ensure t
  :diminish golden-ratio-mode
  :init
  (golden-ratio-mode 1)
  (setq golden-ratio-adjust-factor .9
	golden-ratio-wide-adjust-factor .9))

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
;; Ruby things...
;; ==========================================================

;; (setenv "PATH"
;;       (concat
;;        (getenv "HOME") "/.rbenv/shims:"
;;        (getenv "HOME") "/.rbenv/bin:"
;;        (getenv "PATH")))

;; (setq exec-path
;;       (cons (concat (getenv "HOME") "/.rbenv/shims")
;; 	    (cons (concat (getenv "HOME") "/.rbenv/bin") exec-path)))

;; (use-package rubocop
;;   :ensure t
;;   :config
;;   (add-hook 'enh-ruby-mode-hook (rubocop-mode 1)))

;; (use-package ruby-tools
;;   :ensure t)

;; (use-package rspec-mode
;;   :ensure t
;;   :config
;;   (progn
;;     (setq rspec-use-rake-flag nil)))
;;     ;; (defadvice rspec-compile (around rspec-compile-around activate)
;;     ;;   "Use BASH shell for running the specs because of ZSH issues."
;;     ;;   (let ((shell-file-name "/bin/bash"))
;;     ;; 	ad-do-it))
;;     ;; ))

;; (use-package enh-ruby-mode
;;   :ensure t
;;   :config
;;   (setq enh-ruby-deep-indent-paren nil)
;;   :init
;;   (add-to-list 'auto-mode-alist
;; 	       '("\\(?:\\.rb\\|ru\\|rake\\|thor\\|jbuilder\\|gemspec\\|podspec\\|/\\(?:Gem\\|Rake\\|Cap\\|Thor\\|Vagrant\\|Guard\\|Pod\\)file\\)\\'" . enh-ruby-mode)))

;; (use-package robe
;;   :ensure t
;;   :init
;;   (add-hook 'enh-ruby-mode-hook 'robe-mode)
;;   (add-hook 'robe-mode-hook 'ac-robe-setup))

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
;; Dash at point
;; ==========================================================

;; (use-package dash-at-point
;;   :ensure t
;;   :config
;;   (global-set-key "\C-cd" 'dash-at-point))

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
  :init
    (add-hook 'prog-mode-hook #'yas-minor-mode)
  :bind (("C-c C-h" . yas-expand)))
    ;;(yas-global-mode 1))

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
  (when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize))
  )

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

(use-package ac-js2
  :ensure t
  :init
  (add-hook 'js2-mode-hook 'ac-js2-mode)
  (add-hook 'js2-mode-hook 'skewer-mode))

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

;; ==========================================================
;; GO Things...
;; ==========================================================

;; Before safe hook to format
(add-hook 'go-mode-hook
          (lambda ()
            (add-hook 'before-save-hook 'gofmt-before-save)
            (setq tab-width 4)
            (setq indent-tabs-mode 1)))

;; =========================================================
;; Magit GH Pull Request (open pull request on git directly with emacs)
;; =========================================================

(use-package magit-gh-pulls
  :ensure t
  :init
  (add-hook 'magit-mode-hook 'turn-on-magit-gh-pulls))
