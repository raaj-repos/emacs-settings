;; You will mostly likely need to adjust this font size for your system
(defvar runemacs/default-font-size 140)

(setq inhibit-startup-message t)

(scroll-bar-mode -1)		; Disbale visible scrollbar
(tool-bar-mode -1)              ; Disable the toolbar
(tooltip-mode -1)               ; Disable tooltips
(set-fringe-mode 10)		; Give some breathing room

(setq gc-cons-threshold (* 50 1000 1000))

(defun efs/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                     (time-subtract after-init-time before-init-time)))
           gcs-done))

(add-hook 'emacs-startup-hook #'efs/display-startup-time)

;; Set up the visible bell
(setq visible-bell t)


;; Font configuration
(set-face-attribute 'default nil :font "Fira Code Retina" :height runemacs/default-font-size)

;; Set the fixed pitch faces
(set-face-attribute 'fixed-pitch nil :font "Fira Code Retina" :height 140)

;; Set the variable pitch face
(set-face-attribute 'variable-pitch nil :font "Cantarell" :height 140 :weight 'regular)

;;(load-theme 'tango-dark)
(load-theme 'misterioso)

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)


;; Initialize package sources
(require 'package)

(require 'bind-key)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")
			 ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

 ;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))


(require 'use-package)
(setq use-package-always-ensure t)


(column-number-mode)
(global-display-line-numbers-mode t)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
		org-roam-mode-hook
                term-mode-hook
                shell-mode-hook
                treemacs-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))


(use-package command-log-mode)

(use-package swiper)
(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
	 :map ivy-minibuffer-map
	 ("TAB" . ivy-alt-done)
	 ("C-l" . ivy-alt-done)
	 ("C-j" . ivy-next-line)
	 ("C-k" . ivy-previous-line) 
	 :map ivy-switch-buffer-map
	 ("C-k" . ivy-previous-line)
	 ("C-l" . ivy-done)
	 ("C-d" . ivy-switch-buffer-kill)
	 :map ivy-reverse-i-search-map
	 ("C-k" . ivy-previous-line)
	 ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

(use-package all-the-icons)

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 25)
	   (doom-modeline-major-mode-icon t)
	   ))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package rainbow-mode
  :hook (css-mode sass-mode scss-mode ssass-mode vue-mode))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))

(use-package counsel
  :bind (("M-x" . counsel-M-x)
	 ("C-x b" . counsel-ibuffer)
	 ("C-x C-f" . counsel-find-file)
	 :map minibuffer-local-map
	 ("C-r" . 'counsel-minibuffer-history))
  :config
  (counsel-mode 1))


(use-package ivy-rich
  :after ivy
  :init
  (ivy-rich-mode 1))

(use-package helpful
  :commands (helpful-callable helpful-variable helpful-command helpful-key)
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package evil
  :ensure t
  :init
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1))

(use-package general
  :config
  (general-create-definer my-leader-key-def
    :keymaps '(normal insert visual emacs)    
    :prefix "SPC"    
    :non-normal-prefix "C-SPC"
    "" nil
    )
  (my-leader-key-def
   "t" '(:ignore t :which-key "toggles")
   "tt" '(counsel-load-theme :which-key "choose theme")))

(use-package hydra
  :defer t)

(defhydra hydra-zoom (global-map "<f2>")
  "zoom"
  ("g" text-scale-increase "in")
  ("l" text-scale-decrease "out")
  ("f" nil "finished" :exit t))

(defhydra hydra-magit (:hint nil)
  "
   ^Magit^
   ^-----
   _s_: Status
  "
  ("s" magit-status)
  ("b" magit-branch)
  ("c" magit-commit)
  ("p" magit-push)
  ("f" magit-pull)
  ("g" magit-fetch))

(global-set-key (kbd "C-c m") 'hydra-magit/body)

(use-package multiple-cursors
  :commands (mc/mark-all-in-region mc/mark-all-in-region-regexp)
  :bind (("C-|" . mc/mark-all-in-region)
   ("C-M-|" . mc/mark-all-in-region-regexp))
  :config
  (global-unset-key (kbd "C-<down-mouse-1>"))
  (global-set-key (kbd "C-<mouse-1>") 'mc/add-cursor-on-click))


(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  ;; NOTE: Set this to the folder where you keep your Git repos!
  (when (file-directory-p "~/Workspace/Sides")
    (setq projectile-project-search-path '("~/Projects/Code")))
  (setq projectile-switch-project-action #'projectile-dired))

;;TODO fix this
;;(projectile-register-project-type 'npm '("package.json")
;;				  :project-file "package.json"
;;				  :compile "npm install"
;;				  :test "npm test"
;;				  :run "npm start"
;;				  :test-suffix ".spec")

(use-package counsel-projectile
  :after projectile
  :config (counsel-projectile-mode))

(use-package magit
  :commands magit-status
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

;(use-package forge
;  :after magit)

(defun efs/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1))

;; Org Mode Configuration
(defun efs/org-font-setup ()
  ;; Replace list hyphen with dot
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

  ;; Set faces for heading levels
  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font "Cantarell" :weight 'regular :height (cdr face)))

  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil :foreground "light slate blue"  :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch))

(use-package org
  :hook (org-mode . efs/org-mode-setup)
  :config
  (setq org-ellipsis " ▾")

  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)
  
  (setq org-agenda-files
	'("~/Chatham/_NOTES/Tasks.org"
	  "~/Chatham/_NOTES/Birthdays.org"))
	
   (efs/org-font-setup))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(defun efs/org-mode-visual-fill ()
      (setq visual-fill-colum-width 100
	    visual-fill-column-center-text t)
      (visual-fill-column-mode 1))



(use-package visual-fill-column
;; (use-package lsp-mode
;;   :commands (lsp lsp-deferred)
;;   :hook (typescript-mode css-mode scss-mode sass-mode ng2-mode
;; 	 (lsp-mode .(lambda()
;; 		     (let ((lsp-keymap-prefix "C-c l"))
;; 		       (lsp-enable-which-key-integration)))))
;;   :init
;;   (setq lsp-keep-workspace-alive nil)
;;   (setq read-process-output-max (* 1024 1024)) ;; 1mb
;;   (setq gc-cons-threshold 100000000)
;;   (setq lsp-log-io nil) ;; if set to true can cause performance hit
;;   (setq lsp-modeline-diagnostics-enable t) ;; show error statistics in modeline
;;   :config
;;   (define-key lsp-mode-map (kbd "C-c l") lsp-command-map))

   :hook (org-mode . efs/org-mode-visual-fill))

(use-package org-roam
  :ensure t
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory "~/Chatham/_RoamNotes")
  (org-roam-completion-everywhere t)
  (org-roam-capture-templates
   '(("d" "default" plain "%?"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+date: %U\n")
      :unnarrowed t)
   ("b" "book notes" plain
    (file "~/Chatham/_RoamNotes/Templates/BookNoteTemplate.org")
    :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
    :unnarrowed t)))
  :bind (("C-c n l" . org-roam-buffer-toggle)
	 ("C-c n f" . org-roam-node-find)
	 ("C-c n i" . org-roam-node-insert)
	 :map org-mode-map
	 ("C-M-i"   . completion-at-point))
  :config 
  (org-roam-setup))

(add-hook 'org-mode-hook
	  (lambda ()
	    (setq-local fill-column 220)))

(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :mode ("\\.md\\'" . markdown-mode)
  :mode ("\\.markdown\\'" . markdown-mode)
  :init (setq markdown-command "multimarkdown"))


(use-package lsp-mode
  :hook (typescript-mode . lsp)
  :config
  (setq lsp-clients-angular-language-server-command
	'("ngserver" "--stdio" "--tsProbeLocations" "node_modules;." "--ngProbeLocations" "node_modules;.")))


(use-package typescript-mode
  :mode "\\.ts\\'"
  :hook (typescript-mode . lsp-deferred)
  :config
  (setq typescript-indent-level 2))

(use-package company
  :after lsp-mode
  :hook (lsp-mode . company-mode)
  :bind (:map company-active-map
	      ("<tab>" . company-complete-selection))
  (:map lsp-mode-map
	("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

(use-package company-box
  :hook (company-mode . company-box-mode))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-position 'bottom))

(use-package lsp-treemacs
  :after lsp)

(use-package lsp-ivy
  :after lsp)

(use-package yasnippet
  :config
  (setq yas-snippet-dir '("~/.emacs.d/snippets"))
  (yas-global-mode 1))


(use-package yasnippet-snippets
  :after yasnippet)

(use-package evil-nerd-commenter
  :bind ("M-/" . evilnc-comment-or-uncomment-lines))

;; Languages
(use-package scss-mode
  :mode "\\.scss\\'"
  :hook (scss-mode . lsp-deferred))
(use-package sass-mode
  :mode "\\.sass\\'"
  :hook (sass-mode . lsp-deferred))
(use-package ssass-mode
  :mode "\\.ssass\\'"
  :hook (ssass-mode . lsp-deferred))
(use-package vue-mode
  :mode "\\.vue\\'"
  :hook (vue-mode . lsp-deferred))
(use-package js2-mode
  :mode "\\.js\\'"
  :hook (js2-mode . lsp-deferred))
(use-package js2-refactor)
(use-package xref-js2)
(use-package web-mode
  :mode ("\\.tsx" "\\.jsx")
  :hook (web-mode . lsp-deferred))
(use-package html-mode
  :ensure sgml-mode
  :mode "\\.html\\'"
  :hook (html-mode . lsp-deferred))
(use-package prettier
  :ensure t)
(use-package ng2-mode
  :mode ("\\.component.html\\'" "\\.component.ts\\'")
  :hook (ng2-mode . lsp-deferred))
(use-package django-mode
  :ensure django-mode
  :mode "\\.djhtml$\\'"
  :hook (django-mode . lsp-deferred))

;;(add-to-list 'projectile-globally-ignored-directories "node_modules")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(global-display-line-numbers-mode t)
 '(package-selected-packages
   '(grip-mode pandoc-mode ripgrep prettier web-beautify xref-js2 js2-refactor js2-mode multiple-cursors ng2-mode web-mode vue-mode ssass-mode sass-mode scss-mode rainbow-mode yasnippet-snippets yasnippet lsp-mode visual-fill-column magit counsel-projectile projectile hydra evil-collection evil general helpful counsel ivy-rich which-key rainbow-delimiters doom-modeline all-the-icons swiper use-package ivy command-log-mode django-mode))
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
