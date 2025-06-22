;; This file provides a dev env dedicated to C++/Common Lisp(SLIME), and Python.

(defun include (name) (load-file name))
(include "0package-manager.el") ;; Setup Elpaca and use-package
(include "1multi-term.el")      ;; Multi-term setup
(include "2theme.el")           ;; Doom-Theme, Fonts, Configuration

;; ~~ Packaging ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;; ~~ Elcord ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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

;; ~~ Fonts ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;; ~~ Nyan Mode ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


 ;; who need this?
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
;; kesu?
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


;; sita ha kesite ok?
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
;; sita ha nani?
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
  ("C-c <prior>" . centaur-tabs-backward)
  ("C-c <next>"  . centaur-tabs-forward)
  ;("C-c <left>" . centaur-tabs-move-current-tab-to-left)
  ;("C-c <right>" . centaur-tabs-move-current-tab-to-right)
  )
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
            (let ((height (/ (window-body-height) 5)))
              (split-window-vertically (* 4 height)))
            (other-window 1)
            (multi-term)
            (other-window -1)))
;; vundo
(use-package vundo
  :ensure t
  :bind
  ("C-x u" . vundo))
(setq vundo-window-max-height 10)
(setq vundo-glyph-alist vundo-unicode-symbols)
;; (set-face-attribute 'vundo-default nil :family "Symbola")
