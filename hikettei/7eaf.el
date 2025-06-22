(eval-and-compile
  (defun run-cmd (cmd)
    (message (format "Running: %s" cmd))
    (shell-command cmd)))

(eval-and-compile
  (defun ensure-emacs-application-framework ()
    (let ((eaf-dir (expand-file-name "~/.emacs.d/site-lisp/emacs-application-framework/")))
      (if (file-directory-p eaf-dir)
          eaf-dir
        (progn
          (message "EAF is not installed yet, cloning and building automatically ...")
          (run-cmd "git clone --depth=1 -b master https://github.com/emacs-eaf/emacs-application-framework.git ~/.emacs.d/site-lisp/emacs-application-framework/")
          (run-cmd "python ~/.emacs.d/site-lisp/emacs-application-framework/install-eaf.py")
          eaf-dir)))))

(use-package eaf
  :load-path (lambda () (list (ensure-emacs-application-framework)))
  :custom
  ;; See https://github.com/emacs-eaf/emacs-application-framework/wiki/Customization
  (eaf-browser-continue-where-left-off t)
  (eaf-browser-enable-adblocker t)
  (browse-url-browser-function 'eaf-open-browser)
  :config
  (defalias 'browse-web #'eaf-open-browser)
  (eaf-bind-key scroll_up "C-n" eaf-pdf-viewer-keybinding)
  (eaf-bind-key scroll_down "C-p" eaf-pdf-viewer-keybinding)
  (eaf-bind-key take_photo "p" eaf-camera-keybinding)
  (eaf-bind-key nil "M-q" eaf-browser-keybinding))
