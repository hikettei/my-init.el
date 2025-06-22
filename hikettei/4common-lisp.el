(use-package slime
  :if (file-exists-p "~/.roswell/helper.el")
  :ensure slime-company
  :init (load "~/.roswell/helper.el")
  :custom (inferior-lisp-program "ros -Q run")
  :config (slime-setup '(slime-fancy slime-repl slime-banner slime-indentation slime-fuzzy))) ;;slime-company

;; [TODO] DashboardにCommon Lisp Modeの設定を追加したい
(setf slime-lisp-implementations `((sbcl ("qlot" "exec" "sbcl" "--dynamic-space-size" "16384"))
				   (roswell ("qlot" "exec" "ros" "-Q" "run" "dynamic-space-size=64384"))
				   (roswell-no-qlot ("ros" "-Q" "run"  "dynamic-space-size=16384"))))
(setf slime-default-lisp 'roswell)
;;(setf slime-default-lisp 'roswell-no-qlot)
