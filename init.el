;;; init.el --- Emacs configuration

;;; ~~ Bootstrapping straight.el ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defvar bootstrap-version)
(let ((bootstrap-file (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
;;; ~~ Use use-package with straight.el by default ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)
(eval-when-compile
  (require 'use-package))
;;; ~~~ Separate custom settings into custom.el ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file 'noerror 'nomessage))

;;; ~~~ Basic UI and behavior ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(setq inhibit-startup-message t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(column-number-mode t)
(display-time-mode t)
(setq-default indent-tabs-mode nil)
(global-set-key (kbd "C-c <left>")  'windmove-left)
(global-set-key (kbd "C-c <down>")  'windmove-down)
(global-set-key (kbd "C-c <up>")    'windmove-up)
(global-set-key (kbd "C-c <right>") 'windmove-right)
;;; ~~~ Ivy configuration ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(use-package ivy
  :diminish ivy-mode
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t
        ivy-wrap t
        ivy-truncate-lines nil)
  (when (require 'ivy-hydra nil t)
    (setq ivy-read-action-function #'ivy-hydra-read-action))
  (define-key ivy-minibuffer-map (kbd "<escape>") 'minibuffer-keyboard-quit))

;;; ~~~ Theme setup ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(use-package doom-themes
  :config
  (setq doom-themes-enable-italic t
        doom-themes-enable-bold t)
  (load-theme 'doom-dracula t)
  (doom-themes-neotree-config)
  (doom-themes-org-config))

;;; ~~~ Fonts and icons ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(use-package all-the-icons :defer t)
(use-package cnfonts
  :after all-the-icons
  :hook (cnfonts-set-font-finish . my/cnfonts-fix-icons)
  :config
  (defun my/cnfonts-fix-icons (_)
    (dolist (fam '("all-the-icons" "file-icons" "Material Icons" "github-octicons" "FontAwesome" "Weather Icons"))
      (set-fontset-font t 'unicode (font-spec :family fam) nil 'append)))
  (cnfonts-enable))

;;; ~~~ Modeline ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :init
  (setq doom-modeline-buffer-file-name-style 'truncate-with-project
        doom-modeline-icon t
        doom-modeline-major-mode-icon nil
        doom-modeline-minor-modes nil
        doom-modeline-buffer-encoding t)
  :config
  (doom-modeline-def-modeline
   'main
   '(bar window-number matches buffer-info remote-host buffer-position parrot selection-info)
   '(misc-info persp-name debug minor-modes input-method major-mode process vcs checker)))

;;; Discord Rich Presence
(use-package elcord
  :config
  (elcord-mode -1))

;;; SLIME for Common Lisp
(use-package slime
  :if (file-exists-p "~/.roswell/helper.el")
  :init (load "~/.roswell/helper.el")
  :custom (inferior-lisp-program "ros -Q run")
  :config
  (setq slime-lisp-implementations
        '((sbcl ("qlot" "exec" "sbcl" "--dynamic-space-size" "16384"))
          (roswell ("qlot" "exec" "ros" "-Q" "run" "dynamic-space-size=64384"))
          (roswell-no-qlot ("ros" "-Q" "run" "dynamic-space-size=16384"))))
  (setq slime-default-lisp 'roswell)
  (slime-setup '(slime-repl slime-fancy slime-indentation slime-fuzzy slime-banner)))

;;; Org Pomodoro
(use-package org-pomodoro
  :after org-agenda
  :custom
  (org-pomodoro-ask-upon-killing t)
  (org-pomodoro-format "%s")
  (org-pomodoro-short-break-format "%s")
  (org-pomodoro-long-break-format "%s")
  :custom-face
  (org-link ((t (:foreground "#ebe087" :underline t))))
  (org-list-dt ((t (:foreground "#bd93f9"))))
  (org-special-keyword ((t (:foreground "#6272a4"))))
  (org-todo ((t (:background "#272934" :foreground "#51fa7b" :weight bold))))
  (org-document-title ((t (:foreground "#f1fa8c" :weight bold))))
  (org-done ((t (:background "#373844" :foreground "#216933" :strike-through nil :weight bold))))
  (org-footnote ((t (:foreground "#76e0f3"))))
  :hook
  (org-pomodoro-started . (lambda () (notifications-notify :title "org-pomodoro" :body "Let's focus for 25 minutes!" :app-icon "~/.emacs.d/img/001-food-and-restaurant.png")))
  (org-pomodoro-finished . (lambda () (notifications-notify :title "org-pomodoro" :body "Well done! Take a break." :app-icon "~/.emacs.d/img/004-beer.png")))
  :config
  (setq org-todo-keyword-faces
        '(("WAIT" . (:foreground "#6272a4" :weight bold))
          ("NEXT" . (:foreground "#f1fa8c" :weight bold))
          ("CARRY/O" . (:foreground "#6272a4" :background "#373844" :weight bold))))
  :bind (:map org-agenda-mode-map
              ("p" . org-pomodoro)))

;;; Org bullets
(use-package org-bullets
  :hook (org-mode . org-bullets-mode))

;;; Which Key
(use-package which-key
  :diminish which-key-mode
  :hook (after-init . which-key-mode))

;;; amx
(use-package amx :defer t)

;;; NeoTree
(use-package neotree
  :after projectile
  :commands (neotree-show neotree-hide neotree-dir neotree-find)
  :bind ("<f9>" . neotree-projectile-toggle)
  :init
  (defun neotree-projectile-toggle ()
    (interactive)
    (let ((project-dir (ignore-errors (projectile-project-root)))
          (file-name (buffer-file-name)))
      (if (neo-global--window-exists-p)
          (neotree-hide)
        (progn
          (neotree-show)
          (when project-dir (neotree-dir project-dir))
          (when file-name (neotree-find file-name))))))
  :config
  (setq neo-theme 'nerd2
        neo-hidden-regexp-list '("^\." "\.pyc$" "\.fasl$" "~$" "^#.*#$" "\.elc$")
        neo-show-hidden-files nil))

;;; Show paren
(use-package paren
  :ensure nil
  :hook (after-init . show-paren-mode)
  :custom
  (show-paren-style 'mixed)
  (show-paren-when-point-inside-paren t)
  (show-paren-when-point-in-periphery t)
  :custom-face
  (show-paren-match ((t (:background "#44475a" :foreground "#f1fa8c")))))

;;; Beacon
(use-package beacon
  :config (beacon-mode 1)
  :custom (beacon-color "yellow"))

;;; Dashboard
(use-package dashboard
  :ensure t
  :init
  (setq dashboard-banner-logo-title "Emacs"
        dashboard-startup-banner 'logo
        dashboard-center-content t
        dashboard-show-shortcuts nil
        initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-navigator-buttons
        `(((,(all-the-icons-octicon "mark-github" :height 1.1 :v-adjust 0.0) "Homepage" "Browse homepage" (lambda (&rest _) (browse-url "homepage")))
           ("★" "Star" "Show stars" (lambda (&rest _) (show-stars)) warning)
           ("?" "" "?/h" #'show-help nil "<" ">"))
          ((,(all-the-icons-faicon "linkedin" :height 1.1 :v-adjust 0.0) "LinkedIn" "" (lambda (&rest _) (browse-url "homepage")))
           ("⚑" nil "Show flags" (lambda (&rest _) (message "flag")) error)))))

;;; Python development
(use-package elpy
  :init (advice-add 'python-mode :before 'elpy-enable)
  :custom
  (elpy-rpc-python-command "python3")
  (python-shell-interpreter "python")
  :config
  (remove-hook 'elpy-modules 'elpy-module-highlight-indentation)
  (remove-hook 'elpy-modules 'elpy-module-flymake))

(use-package jedi
  :after elpy
  :hook (python-mode . jedi:setup)
  :custom
  (jedi:setup-keys t)
  (jedi:complete-on-dot t))

;;; Markdown mode
(use-package markdown-mode
  :mode "\\.md\\'"
  :init
  (setq markdown-command '("pandoc" "--from=markdown" "--to=html5")
        markdown-fontify-code-blocks-natively t
        markdown-header-scaling nil
        markdown-indent-on-enter 'indent-and-new-item)
  :bind (:map markdown-mode-map ("<S-tab>" . markdown-shifttab)))

;;; GitHub Copilot
(use-package copilot
  :straight (copilot :type git :host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
  :hook (prog-mode . copilot-mode)
  :config
  (setq copilot-node-executable "node")
  (defun my-tab ()
    (interactive)
    (or (copilot-accept-completion)
        (company-indent-or-complete-common nil)))
  (global-set-key (kbd "C-TAB") 'my-tab)
  (global-set-key (kbd "C-<tab>") 'my-tab)
  (define-key copilot-completion-map (kbd "<tab>") 'my-tab)
  (define-key copilot-completion-map (kbd "TAB") 'my-tab)
  (add-to-list 'copilot-indentation-alist '(lisp-mode . 4)))
