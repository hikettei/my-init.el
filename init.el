;; This file provides a dev env dedicated to C++/Common Lisp(SLIME), and Python.
;; ~~ Packaging ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(require 'package)
;; TODO: use-package使ってるけど，elpaca+setupに書き換えたい
(require 'use-package)
;; Preparing MELPA
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ;;("melpa" . "http://melpa.org/packages/")
        ("org" . "http://orgmode.org/elpa/")
	("melpa-stable" . "https://stable.melpa.org/packages/")))
(package-initialize)
;; ~~ Elcord ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(use-package elcord
  :ensure t)
(elcord-mode)
;; ~~ SLIME Setup ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(use-package slime
  :if (file-exists-p "~/.roswell/helper.el")
  :ensure slime-company
  :init (load "~/.roswell/helper.el")
  :custom (inferior-lisp-program "ros -Q run")
  :config (slime-setup '(slime-fancy))) ;;slime-company

(slime-setup '(slime-repl slime-fancy slime-banner)) 
(slime-setup '(slime-fancy slime-indentation))
(slime-setup '(slime-fuzzy))
;; ~~ Key Bindings For Ivy ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(use-package ivy
  :ensure t)
;; Dispatch M-o as a ivh-hydra-read-action
(when (require 'ivy-hydra nil t)
  (setq ivy-read-action-function #'ivy-hydra-read-action))

;; `ivy-switch-buffer (C-x b)` includes recent-files and bookmarks.
(setq ivy-use-virtual-buffers t)

;; ミニバッファでコマンド発行を認める
(when (setq enable-recursive-minibuffers t)
  (minibuffer-depth-indicate-mode 1)) ;; 何回層入ったかプロンプトに表示．

;; ESC連打でミニバッファを閉じる
(define-key ivy-minibuffer-map (kbd "<escape>") 'minibuffer-keyboard-quit)

;; プロンプトの表示が長い時に折り返す（選択候補も折り返される）
(setq ivy-truncate-lines nil)

;; リスト先頭で `C-p' するとき，リストの最後に移動する
(setq ivy-wrap t)

;; アクティベート
(ivy-mode 1)
;; ~~ Themes ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(use-package doom-themes
    :custom
    (doom-themes-enable-italic t)
    (doom-themes-enable-bold t)
    :custom-face
    (doom-modeline-bar ((t (:background "#6272a4"))))
    :config
    (load-theme 'doom-dracula t)
    (doom-themes-neotree-config)
    (doom-themes-org-config))
;; ~~ Fonts ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(use-package cnfonts
  :ensure t
  :after all-the-icons
  :hook (cnfonts-set-font-finish
         . (lambda (fontsizes-list)
             (set-fontset-font t 'unicode (font-spec :family "all-the-icons") nil 'append)
             (set-fontset-font t 'unicode (font-spec :family "file-icons") nil 'append)
             (set-fontset-font t 'unicode (font-spec :family "Material Icons") nil 'append)
             (set-fontset-font t 'unicode (font-spec :family "github-octicons") nil 'append)
             (set-fontset-font t 'unicode (font-spec :family "FontAwesome") nil 'append)
             (set-fontset-font t 'unicode (font-spec :family "Weather Icons") nil 'append)))
  :config
  (cnfonts-enable))

(use-package all-the-icons
  :ensure t
  :custom
  (all-the-icons-scale-factor 1.0))

;; ~~ Nyan Mode ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(nyan-mode 1)
(setq doom-modeline-workspace-name t)

(use-package doom-modeline
  :commands (doom-modeline-def-modeline)
  :custom
  (doom-modeline-buffer-file-name-style 'truncate-with-project)
  (doom-modeline-icon t)
  (doom-modeline-major-mode-icon nil)
  (doom-modeline-minor-modes nil)
  :hook
  (after-init . doom-modeline-mode)
  :config
  (line-number-mode 0)
  (column-number-mode 0)
  (doom-modeline-def-modeline
    'main
    '(bar window-number matches buffer-info remote-host buffer-position parrot selection-info)
    '(misc-info persp-name debug minor-modes input-method major-mode process vcs checker)))

(setq doom-modeline-buffer-encoding t)
	
(defun ladicle/task-clocked-time ()
        "Return a string with the clocked time and effort, if any"
        (interactive)
        (let* ((clocked-time (org-clock-get-clocked-time))
               (h (truncate clocked-time 60))
               (m (mod clocked-time 60))
               (work-done-str (format "%d:%02d" h m)))
          (if org-clock-effort
              (let* ((effort-in-minutes
                  (org-duration-to-minutes org-clock-effort))
                 (effort-h (truncate effort-in-minutes 60))
                 (effort-m (truncate (mod effort-in-minutes 60)))
                 (effort-str (format "%d:%02d" effort-h effort-m)))
            (format "%s/%s" work-done-str effort-str))
            (format "%s" work-done-str))))

(use-package org-pomodoro
    :after org-agenda
    :custom
    (org-pomodoro-ask-upon-killing t)
    (org-pomodoro-format "%s")
    (org-pomodoro-short-break-format "%s")
    (org-pomodoro-long-break-format  "%s")
    :custom-face
    (org-link ((t (:foreground "#ebe087" :underline t))))
    (org-list-dt ((t (:foreground "#bd93f9"))))
    (org-special-keyword ((t (:foreground "#6272a4"))))
    (org-todo ((t (:background "#272934" :foreground "#51fa7b" :weight bold))))
    (org-document-title ((t (:foreground "#f1fa8c" :weight bold))))
    (org-done ((t (:background "#373844" :foreground "#216933" :strike-through nil :weight bold))))
    (org-footnote ((t (:foreground "#76e0f3"))))
    :hook
    (org-pomodoro-started . (lambda () (notifications-notify
                                               :title "org-pomodoro"
                           :body "Let's focus for 25 minutes!"
                           :app-icon "~/.emacs.d/img/001-food-and-restaurant.png")))
    (org-pomodoro-finished . (lambda () (notifications-notify
                                               :title "org-pomodoro"
                           :body "Well done! Take a break."
                           :app-icon "~/.emacs.d/img/004-beer.png")))
    :config
    (setq org-todo-keyword-faces
        '(("WAIT" . (:foreground "#6272a4":weight bold))
          ("NEXT"   . (:foreground "#f1fa8c" :weight bold))
          ("CARRY/O" . (:foreground "#6272a4" :background "#373844" :weight bold))))
    :bind (:map org-agenda-mode-map
                ("p" . org-pomodoro)))

(use-package org-bullets
      :custom (org-bullets-bullet-list '("" "" "" "" "" "" "" "" "" ""))
      :hook (org-mode . org-bullets-mode))


(use-package which-key
    :diminish which-key-mode
    :hook (after-init . which-key-mode))

(use-package amx)

;; ~~ NeoTree ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(use-package neotree
    :after
    projectile
    :commands
    (neotree-show neotree-hide neotree-dir neotree-find)
    :custom
    (neo-theme 'nerd2)
    :bind
    ("<f9>" . neotree-projectile-toggle)
    :preface
    (defun neotree-projectile-toggle ()
      (interactive)
      (let ((project-dir
         (ignore-errors
         ;;; Pick one: projectile or find-file-in-project
           (projectile-project-root)
           ))
        (file-name (buffer-file-name))
        (neo-smart-open t))
    (if (and (fboundp 'neo-global--window-exists-p)
         (neo-global--window-exists-p))
        (neotree-hide)
      (progn
        (neotree-show)
        (if project-dir
        (neotree-dir project-dir))
        (if file-name
	    (neotree-find file-name)))))))

  (use-package paren
    :ensure nil
    :hook
    (after-init . show-paren-mode)
    :custom-face
    (show-paren-match ((nil (:background "#44475a" :foreground "#f1fa8c"))))
    :custom
    (show-paren-style 'mixed)
    (show-paren-when-point-inside-paren t)
    (show-paren-when-point-in-periphery t))


 ;; ~~ Never Lose My Cursor ~~~~~~~~~~~~~~
 (use-package beacon
    :custom
    (beacon-color "yellow")
    :config
    (beacon-mode 1))

(eval-after-load "neotree"
  '(progn
     (setq neo-hidden-regexp-list '("^\\." "\\.pyc$" "\\.fasl$" "~$" "^#.*#$" "\\.elc$"))
     (setq neo-show-hidden-files nil)))

;; Set the neo-window-width to the current width of the
;; neotree window, to trick neotree into resetting the
;; width back to the actual window width.
;; Fixes: https://github.com/jaypei/emacs-neotree/issues/262
(eval-after-load "neotree"
  '(add-to-list 'window-size-change-functions
                (lambda (frame)
                  (let ((neo-window (neo-global--get-window)))
                    (unless (null neo-window)
                      (setq neo-window-width (window-width neo-window)))))))

(customize-set-variable 'scroll-bar-mode nil)
(setq scroll-bar-mode nil)

;; control + q
(global-set-key "\C-q" 'neotree-toggle)

					;; control + q neotree
;; control + m minimap

(cua-mode t) 
(setq cua-enable-cua-keys nil) 

(tool-bar-mode -1)
(display-time-mode t)
(column-number-mode t)
;;(global-linum-mode t)

;; ~~ Linum ~~~~~~
(if (version<= "26.0.50" emacs-version)
    (progn
      (global-display-line-numbers-mode)
      ;;(set-face-attribute 'line-number nil
      ;;                    :foreground "DarkOliveGreen"
      ;;                    :background "#131521")
      ;;(set-face-attribute 'line-number-current-line nil
      ;;                    :foreground "gold")

      ))

(setq inhibit-startup-message t) 
(setq initial-scratch-message "")

;;(mac-auto-ascii-mode 1)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(markdown-command '("pandoc" "--from=markdown" "--to=html5"))
 '(markdown-fontify-code-blocks-natively t)
 '(markdown-header-scaling nil)
 '(markdown-indent-on-enter 'indent-and-new-item)
 '(package-selected-packages
   '(use-package-hydra blackout hydra leaf-keywords el-get pandoc-mode pandoc markdown-mode markdown-preview-mode jedi haskell-mode readline-complete py-isort nerd-icons page-break-lines dashboard ivy rust-mode slime-theme slime-repl-ansi-color highlight color-identifiers-mode clang-format+ swift3-mode swift-mode elpy better-defaults elcord all-the-icons-gnus nyan-mode cnfonts minimap beacon lsp-mode ccls ivy-rich counsel amx which-key hide-mode-line doom-modeline doom-themes ztree use-package neotree all-the-icons-ivy all-the-icons-dired)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :extend nil :stipple nil :background "#282a36" :foreground "#dcdcdc" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight regular :height 120 :width normal :foundry "nil" :family "Monaco"))))
 '(doom-modeline-bar ((t (:background "#6272a4"))) t)
 '(markdown-bold-face ((t (:foreground "#f0f8ff" :inherit bold))))
 '(markdown-header-face ((t (:foreground "#87cefa" :inherit bold))))
 '(markdown-link-face ((t (:foreground "#c0c0c0"))))
 '(markdown-list-face ((t (:foreground "#ff7f50"))))
 '(show-paren-match ((nil (:background "#44475a" :foreground "#f1fa8c")))))
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; ~~~~ SLIME SetUp ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;(setq inferior-lisp-program "ros -Q run")
(setf slime-lisp-implementations `((sbcl ("qlot" "exec" "sbcl" "--dynamic-space-size" "16384"))
				   (roswell ("qlot" "exec" "ros" "-Q" "run" "dynamic-space-size=64384"))
				   (roswell-no-qlot ("ros" "-Q" "run"  "dynamic-space-size=16384"))))
(setf slime-default-lisp 'roswell)
;;(setf slime-default-lisp 'roswell-no-qlot)
;; ~~ Dash Board ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Don't forget to type:
;;  M-x package-refresh-contents
;;  M-x package-install dashboard
;;                      page-break-lines
;;                      projectile
;;                      nerd-icons
(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook))

(setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))

;; Set the title
(setq dashboard-banner-logo-title "Emacs")
;; Set the banner
(setq dashboard-startup-banner 'logo)

;; Content is not centered by default. To center, set
(setq dashboard-center-content t)

;; To disable shortcut "jump" indicators for each section, set
(setq dashboard-show-shortcuts nil)

;; Format: "(icon title help action face prefix suffix)"
(setq dashboard-navigator-buttons
      `(;; line1
        ((,(all-the-icons-octicon "mark-github" :height 1.1 :v-adjust 0.0)
         "Homepage"
         "Browse homepage"
         (lambda (&rest _) (browse-url "homepage")))
        ("★" "Star" "Show stars" (lambda (&rest _) (show-stars)) warning)
        ("?" "" "?/h" #'show-help nil "<" ">"))
         ;; line 2
        ((,(all-the-icons-faicon "linkedin" :height 1.1 :v-adjust 0.0)
          "Linkedin"
          ""
          (lambda (&rest _) (browse-url "homepage")))
         ("⚑" nil "Show flags" (lambda (&rest _) (message "flag")) error))))

(setq dashboard-footer-icon (all-the-icons-octicon "dashboard"
                                                   :height 1.1
                                                   :v-adjust -0.05
                                                   :face 'font-lock-keyword-face))

;; ~~ Python Dev ~~~~~~~~~~~~~~~
;; - REPL-Driven Style
;; - C-c and tests the whole file
(require 'leaf)
;;(leaf poetry
;;  :ensure t
;;  :hook ((elpy-mode-hook . poetry-tracking-mode)))
;; M-x package install flycheck
(leaf elpy
  :ensure t
  :init
  (advice-add 'python-mode :before 'elpy-enable)
  :config
  (setq elpy-rpc-python-command "python")
  (setq python-shell-interpreter "python")
  (remove-hook 'elpy-modules 'elpy-module-highlight-indentation) ;; インデントハイライトの無効化
  (remove-hook 'elpy-modules 'elpy-module-flymake) ;; flymakeの無効化
  :custom
  (elpy-rpc-python-command . "python3") ;; https://mako-note.com/ja/elpy-rpc-python-version/の問題を回避するための設定
  ;;(flycheck-python-flake8-executable . "flake8");
;  :bind (elpy-mode-map
;         ("C-c C-r f" . elpy-format-code))
;;  :hook ((elpy-mode-hook . flycheck-mode))
  )

(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:setup-keys t)
(setq jedi:complete-on-dot t)

;;;)

(add-to-list 'auto-mode-alist '("\\.md\\'" . gfm-mode))
(with-eval-after-load 'markdown-mode
  (custom-set-variables
   '(markdown-command '("pandoc" "--from=markdown" "--to=html5"))
   '(markdown-fontify-code-blocks-natively t)
   '(markdown-header-scaling nil)
   '(markdown-indent-on-enter 'indent-and-new-item))
  (define-key markdown-mode-map (kbd "<S-tab>") #'markdown-shifttab))

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))


(straight-use-package
 '(copilot :type git :host github :repo "zerolfx/copilot.el" :files ("dist" "*.el")))

(setq copilot-node-executable "node")
;; プログラムモードの場合、copilot-modeを実行
(add-hook 'prog-mode-hook 'copilot-mode)
;; copilot用にキーバインドを設定
(defun my-tab ()
  (interactive)
  (or (copilot-accept-completion)
      (company-indent-or-complete-common nil)))
(global-set-key (kbd "C-TAB") #'my-tab)
(global-set-key (kbd "C-<tab>") #'my-tab)

(require 'copilot)
(define-key copilot-completion-map (kbd "<tab>") 'my-tab)
(define-key copilot-completion-map (kbd "TAB") 'my-tab)

(with-eval-after-load 'company
  (define-key company-active-map (kbd "C-TAB") #'my-tab)
  (define-key company-active-map (kbd "C-<tab>") #'my-tab)
  (define-key company-mode-map (kbd "C-TAB") #'my-tab)
  (define-key company-mode-map (kbd "C-<tab>") #'my-tab))

(add-to-list
 'copilot-indentation-alist
 '(lisp-mode 4))

(setq-default indent-tabs-mode nil)

(global-set-key (kbd "C-c <left>")  'windmove-left)
(global-set-key (kbd "C-c <down>")  'windmove-down)
(global-set-key (kbd "C-c <up>")    'windmove-up)
(global-set-key (kbd "C-c <right>") 'windmove-right)

(use-package centaur-tabs
  :ensure t
  :demand
  :config
  (centaur-tabs-mode t)
  :bind
  ("C-<prior>" . centaur-tabs-backward)
  ("C-<next>"  . centaur-tabs-forward))
(centaur-tabs-headline-match)
(setq centaur-tabs-set-icons t)
(setq centaur-tabs-icon-type 'all-the-icons)
(setq centaur-tabs-set-bar 'under)
;; Note: If you're not using Spacmeacs, in order for the underline to display
;; correctly you must add the following line:
(setq x-underline-at-descent-line t)
(setq centaur-tabs-close-button "X")
(setq centaur-tabs-set-modified-marker t)
(setq centaur-tabs-modified-marker "*")
;; ~~~~~~ 起動時点での画面 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; 1. Neotreeをデフォルトで表示
;; 2. multi-termを右下に表示したい

(setq neo-window-width 40)
(add-hook 'emacs-startup-hook #'neotree-show)

(load-file "~/.emacs.d/multi-term.el")

(add-hook 'emacs-startup-hook
          (lambda ()
            (let ((height (/ (window-body-height) 4)))
              (split-window-vertically (* 3 height)))
            (other-window 1)
            (multi-term)
            (other-window -1)))
