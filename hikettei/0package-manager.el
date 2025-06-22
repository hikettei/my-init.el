;; [TODO] Use setup.el + Elpaca
(require 'package)
(require 'use-package)
;; Preparing MELPA
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ;;("melpa" . "http://melpa.org/packages/")
        ("org" . "http://orgmode.org/elpa/")
	("melpa-stable" . "https://stable.melpa.org/packages/")))
(package-initialize)
