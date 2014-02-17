;;; init --- init emacs
;;; Commentary:
;;; this is my cofig file
;;; Code:

;;setup el-get
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")
(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))
(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes")
(el-get 'sync)

;显示列号,行号
(setq column-number-mode t)
(require 'linum+)
(global-linum-mode t)

;设置sentence-end识别中文标点
(setq sentence-end "\\([。！？]\\|……\\|[.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*")
(setq sentence-end-double-space nil)

;鼠标指针自动弹开
(mouse-avoidance-mode 'animate)

;设置个人信息
(setq user-full-name "Zheqi Lu")
(setq user-mail-address "albuscrow@gmail.com")

;;pos-tip
(require 'pos-tip)

;;注释相关
(defun my-comment-dwim-line(&optional arg)
    "Replacement for the comment-dwim command. If no region is selected and current line is not blank and we are not at the end of the line, then comment current line. Replaces default behaviour of comment-dwim, when it inserts comment at the end of the line."
  (interactive "*P")
  (comment-normalize-vars)
  (if (and (not (region-active-p)) (not (looking-at "[ \t]*$")))
      (comment-or-uncomment-region (line-beginning-position)
                                   (line-end-position))
    (comment-dwim arg)))

(global-set-key "\M-;" 'my-comment-dwim-line)

;;拷贝代码格式化
(dolist (command '(yank yank-app))
  (eval
   `(defadvice ,command (after indent-region activate)
      (and (not current-prefix-arg)
           (member major-mode
                   '(emacs-lisp-mode
                     lisp-mode
                     clojure-mode
                     scheme-mode
                     haskell-mode
                     ruby-mode
                     rspec-mode
                     python-mode
                     c-mode
                     c++-mode
                     objc-mode
                     LaTeX-mode
                     js-mode
                     plain-tex-mode))
           (let ((mark-even-if-inactive transient-mark-mode))
             (indent-region (region-beginning) (region-end) nil))))))

;;setup window number
(require 'window-number)
(window-number-mode 1)
(window-number-meta-mode 1)

;;dired
(require 'dired+)
(require 'dired-single)
(defun my-dired-init()
  (define-key dired-mode-map [return] 'dired-single-buffer)
  (define-key dired-mode-map [mouse-1] 'dired-single-buffer-mouse)
  (define-key dired-mode-map "^"
    (function (lambda nil (interactive) (dired-single-buffer "..")))))
(if (boundp 'dired-mode-map)
    (my-dired-init)
  (add-hook 'dired-load-hook 'my-dired-init))
(setq dired-single-magic-buffer-name "dired")
(global-set-key [(f5)] 'dired-single-magic-buffer)
(global-set-key [(control f5)] (function
                                (lambda nil (interactive)
                                  (dired-single-magic-buffer default-directory))))
(global-set-key [(shift f5)] (function
                              (lambda nil (interactive)
                                (message "Current directory is: %s" default-directory))))
(global-set-key [(meta f5)] 'dired-single-toggle-buffer-name)

(require 'dired-isearch)
(define-key dired-mode-map (kbd "C-s") 'dired-isearch-forward)
(define-key dired-mode-map (kbd "C-r") 'dired-isearch-backward)
(define-key dired-mode-map (kbd "M-C-s") 'dired-isearch-forward-regexp)
(define-key dired-mode-map (kbd "M-C-r") 'dired-isearch-backward-regexp)

(add-hook 'dired-load-hook
          (lambda () (require 'dired-sort-menu+)))

(require 'dired-view)
(define-key dired-mode-map (kbd ";") 'dired-view-minor-mode-toggle)
(define-key dired-mode-map (kbd ":") 'dired-view-minor-mode-dired-toggle)

;;隐藏隐藏文件
(dired-omit-mode 1)

(require 'dired-details+)

(require 'yasnippet)
(yas/global-mode 1)

(require 'auto-complete-config)
(setq ac-dictionary-directories "~/.emacs.d/elpa/auto-complete-20140208.653/dict")
(ac-config-default)

(require 'auto-complete-clang-async)
(defun ac-cc-mode-setup ()
  (setq ac-clang-complete-executable "~/.emacs.d/elpa/auto-complete-clang-async-20130526.814/clang-complete")
  (setq ac-sources '(ac-source-clang-async))
  (ac-clang-launch-completion-process)
  )
(defun my-ac-config ()
  (add-hook 'c-mode-common-hook 'ac-cc-mode-setup)
  (add-hook 'auto-complete-mode-hook 'ac-common-setup)
  (global-auto-complete-mode t))
(my-ac-config)

(provide 'init-local)
;;; init-local.el ends here
