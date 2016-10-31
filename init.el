(require 'package) ;; You might already have this line

;; Start in fullscreen mode
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["black" "#d55e00" "#009e73" "#f8ec59" "#0072b2" "#cc79a7" "#56b4e9" "white"])
 '(compilation-message-face (quote default))
 '(custom-enabled-themes (quote (manoj-dark)))
 '(custom-safe-themes t)
 '(desktop-save-mode t)
 '(elpy-test-django-runner-command
   (quote
    ("python" "manage.py" "test" "--settings=e4l.settings.test" "--noinput")))
 '(elpy-test-runner (quote elpy-test-django-runner))
 '(fci-rule-color "#49483E")
 '(highlight-changes-colors ("#FD5FF0" "#AE81FF"))
 '(highlight-tail-colors
   (("#49483E" . 0)
    ("#67930F" . 20)
    ("#349B8D" . 30)
    ("#21889B" . 50)
    ("#968B26" . 60)
    ("#A45E0A" . 70)
    ("#A41F99" . 85)
    ("#49483E" . 100)))
 '(initial-frame-alist (quote ((fullscreen . maximized))))
 '(magit-diff-use-overlays nil)
 '(package-selected-packages
   (quote
    (ruby-tools rbenv rspec-mode yaml-mode use-package smartparens projectile multiple-cursors monokai-theme material-theme magit ido-vertical-mode ido-ubiquitous grizzl golden-ratio git-gutter flx-ido expand-region elpy editorconfig drag-stuff ace-jump-mode)))
 '(pyvenv-workon "e4l")
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#F92672")
     (40 . "#CF4F1F")
     (60 . "#C26C0F")
     (80 . "#E6DB74")
     (100 . "#AB8C00")
     (120 . "#A18F00")
     (140 . "#989200")
     (160 . "#8E9500")
     (180 . "#A6E22E")
     (200 . "#729A1E")
     (220 . "#609C3C")
     (240 . "#4E9D5B")
     (260 . "#3C9F79")
     (280 . "#A1EFE4")
     (300 . "#299BA6")
     (320 . "#2896B5")
     (340 . "#2790C3")
     (360 . "#66D9EF"))))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   (unspecified "#272822" "#49483E" "#A20C41" "#F92672" "#67930F" "#A6E22E" "#968B26" "#E6DB74" "#21889B" "#66D9EF" "#A41F99" "#FD5FF0" "#349B8D" "#A1EFE4" "#F8F8F2" "#F8F8F0")))

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

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(tool-bar-mode -1)
(scroll-bar-mode -1)

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

;; Enable downcase shortcut (C-x C-l) and uppercase (C-x C-u)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; y/n instead of yes/no
(defalias 'yes-or-no-p 'y-or-n-p)

;; Display continuous lines
(setq-default truncate-lines nil)

;; trucate even even when screen is split into multiple windows
(setq-default truncate-partial-width-windows nil)

;;===========================================================
;; Git gutter
;;============================================================

(use-package git-gutter
  :ensure t
  :bind (("C-c v =" . git-gutter:popup-hunk) ;; show hunk diff
         ("C-c v p" . git-gutter:previous-hunk)
         ("C-c v n" . git-gutter:next-hunk)
         ("C-c v s" . git-gutter:stage-hunk)
         ("C-c v r" . git-gutter:revert-hunk))
  :diminish git-gutter-mode
  :init
  (global-git-gutter-mode t)
  :config
  (custom-set-variables
   '(git-gutter:window-width 1)
   '(git-gutter:modified-sign "●") ;; two space
   '(git-gutter:added-sign "●")    ;; multiple character is OK
   '(git-gutter:deleted-sign "●"))
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
(global-linum-mode -1)
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
;; Others
;; ==========================================================

(use-package yaml-mode
  :ensure t
  :init
  :mode ("\\.yml$" . yaml-mode))

;;===========================================================
;; Projectile
;;============================================================

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

  (use-package ido-ubiquitous
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

;; Using with magit
;; (global-set-key (kbd "C-x g") 'magit-status)

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

(setenv "PATH"
      (concat
       (getenv "HOME") "/.rbenv/shims:"
       (getenv "HOME") "/.rbenv/bin:"
       (getenv "PATH")))

(setq exec-path
      (cons (concat (getenv "HOME") "/.rbenv/shims")
	    (cons (concat (getenv "HOME") "/.rbenv/bin") exec-path)))

(use-package rubocop
  :ensure t
  :config
  (add-hook 'enh-ruby-mode-hook (rubocop-mode 1)))

(use-package ruby-tools
  :ensure t)

(use-package rspec-mode
  :ensure t
  :config
  (progn
    (setq rspec-use-rake-flag nil)
    (defadvice rspec-compile (around rspec-compile-around activate)
      "Use BASH shell for running the specs because of ZSH issues."
      (let ((shell-file-name "/bin/bash"))
    	ad-do-it))
    ))

(use-package enh-ruby-mode
  :ensure t
  :init
  (add-to-list 'auto-mode-alist
	       '("\\(?:\\.rb\\|ru\\|rake\\|thor\\|jbuilder\\|gemspec\\|podspec\\|/\\(?:Gem\\|Rake\\|Cap\\|Thor\\|Vagrant\\|Guard\\|Pod\\)file\\)\\'" . enh-ruby-mode)))

(use-package robe
  :ensure t
  :init
  (add-hook 'enh-ruby-mode-hook 'robe-mode)
  (add-hook 'robe-mode-hook 'ac-robe-setup))

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

(use-package dash-at-point
  :ensure t
  :config
  (global-set-key "\C-cd" 'dash-at-point))

;; ==========================================================
;; Textmate minnor mode (https://melpa.org/#/textmate)
;; ==========================================================

(use-package textmate
  :ensure t
  :init
  (textmate-mode)
  :config
  (global-set-key (kbd "M-RET") 'textmate-next-line))
