(use-package transient :ensure t)
(use-package eat :ensure t)

(use-package claude-code :ensure t
  :vc (:url "https://github.com/stevemolitor/claude-code.el" :rev :newest)
  :config
  (setq claude-code-term-name "xterm-256color")
  ;; For example, to enable verbose output
  (setq claude-code-program-switches '("--verbose"))
  ;; Add hooks to run after Claude is started
  ;; (add-hook 'claude-code-start-hook 'my-claude-setup-function)
  ;; Adjust initialization delay (default is 0.1 seconds)
  ;; This helps prevent terminal layout issues if the buffer is displayed before Claude is fully ready
  (setq claude-code-startup-delay 0.2)
  ;; Configure the buffer size threshold for confirmation prompt (default is 100000 characters)
  ;; If a buffer is larger than this threshold, claude-code-send-region will ask for confirmation
  ;; before sending the entire buffer to Claude
  (setq claude-code-large-buffer-threshold 100000)
  ;; Disable truncation of Claude output buffer (default is nil)
  ;; When set to t, claude-code.el can output display content without truncation
  ;; This is useful when working with large Claude buffers
  (setq claude-code-never-truncate-claude-buffer t)
  (claude-code-mode)
  (add-to-list 'display-buffer-alist
                 `("^\\*claude"
                   (display-buffer-in-side-window)
                   (side . right)
                   (window-width . ,width)
                   (window-parameters . ((no-delete-other-windows . t)))))
  :bind-keymap ("C-c C-a" . claude-code-command-map))
