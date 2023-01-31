
(setq inferior-lisp-program "/usr/local/bin/sbcl")
(add-to-list 'load-path (expand-file-name "~/.emacs.d/slime"))
(require 'slime)
(require 'package)
(require 'use-package)

(slime-setup '(slime-repl slime-fancy slime-banner)) 
(slime-setup '(slime-fancy slime-indentation))

(when (require 'ivy nil t)

  ;; M-o を ivy-hydra-read-action に割り当てる．
  (when (require 'ivy-hydra nil t)
    (setq ivy-read-action-function #'ivy-hydra-read-action))

  ;; `ivy-switch-buffer' (C-x b) のリストに recent files と bookmark を含める．
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
  (ivy-mode 1))

(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa" . "http://melpa.org/packages/")
        ("org" . "http://orgmode.org/elpa/")))

(package-initialize)

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
  :custom
  (all-the-icons-scale-factor 1.0))

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


 (use-package beacon
    :custom
    (beacon-color "yellow")
    :config
    (beacon-mode 1))

					; control + q
(global-set-key "\C-q" 'neotree-toggle)

					; control + q neotree
; control + m minimap

(cua-mode t) 
(setq cua-enable-cua-keys nil) 

(tool-bar-mode -1)
(display-time-mode t)
(column-number-mode t)
(global-linum-mode t)
(setq inhibit-startup-message t) 
(setq initial-scratch-message "")
(mac-auto-ascii-mode 1)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(slime-company company nyan-mode cnfonts minimap beacon lsp-mode ccls ivy-rich counsel amx which-key hide-mode-line doom-modeline doom-themes ztree use-package neotree all-the-icons-ivy all-the-icons-dired)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(doom-modeline-bar ((t (:background "#6272a4"))))
 '(show-paren-match ((nil (:background "#44475a" :foreground "#f1fa8c")))))

;; Heap領域を増やす
(setq slime-lisp-implementations '(("sbcl" ("sbcl" "--dynamic-space-size" "4096")) ("clisp" ("clisp")) ("ecl" ("ecl")) ("cmucl" ("cmucl"))))
