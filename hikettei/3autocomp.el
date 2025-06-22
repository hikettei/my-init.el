(use-package ivy
  :ensure t
  :init
  (setq ivy-use-virtual-buffers    t
        enable-recursive-minibuffers t
        ivy-truncate-lines          nil
        ivy-wrap                    t)
  :config
  (use-package ivy-hydra
    :ensure t
    :after ivy
    :config
    (setq ivy-read-action-function #'ivy-hydra-read-action))
  (minibuffer-depth-indicate-mode 1)
  (ivy-mode 1))
