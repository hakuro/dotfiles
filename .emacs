;; -*- MODE: emacs-lisp; coding: utf-8-emacs-unix; indent-tabs-mode: nil -*-


(package-initialize)

(require 'cl)
(setq evil-want-C-u-scroll t)

(setq user-full-name "Naohisa Takahashi")

;; user emacs directory setting
(unless (boundp 'user-emacs-directory)
      (defvar user-emacs-directory (expand-file-name "~/.emacs.d/")))

;; set color theme
(load-theme 'manoj-dark t)

;; set line number
(global-linum-mode 1)
(setq linum-format "%3d ")

(show-paren-mode t)

;; do not display startup message
(setq inhibit-startup-message t)

;; do not display scratch message
(setq initial-scratch-message "")

;; always add new line, end of buffer
(setq require-final-newline t)

;; backup directory setting
(setq backup-directory-alist '(("" . "~/.emacs.d/backup")))

;; auto save directory setting
(add-to-list 'auto-save-file-name-transforms '("~/\\([^/]*/\\)*\\([^/]*\\)$" "~/.emacs.d/autosave/\\2" t))
(setq auto-save-list-file-prefix (expand-file-name "~/.emacs.d/autosave/.saves-"))

;; clipboard os sharing
(defun copy-from-osx ()
  (shell-command-to-string "pbpaste"))

(defun paste-to-osx (text &optional push)
  (let ((process-connection-type nil))
    (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
      (process-send-string proc text)
      (process-send-eof proc))))

(setq interprogram-cut-function 'paste-to-osx)
(setq interprogram-paste-function 'copy-from-osx)

;; add load path recursive
(defun add-to-load-path (&rest paths)
  (let (path)
    (dolist (path paths paths)
      (let ((default-directory
              (expand-file-name (concat user-emacs-directory path))))
        (add-to-list 'load-path default-directory)
        (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
          (normal-top-level-add-subdirs-to-load-path))))))
(unless (file-directory-p "~/.emacs.d/el-get")
  (make-directory "~/.emacs.d/el-get"))
(unless (file-directory-p "~/.emacs.d/init.d")
  (make-directory "~/.emacs.d/init.d"))
(add-to-load-path "el-get" "init.d")

;; el-get
(unless (require 'el-get nil 'noerror)
    (with-current-buffer
     (url-retrieve
      "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el"
      (lambda (s)
       (goto-char (point-max))
       (eval-print-last-sexp)))))

(setq el-get-user-package-directory "~/.emacs.d/init.d/")

;; el-get packages
(defvar my:packages
  '(el-get
    evil evil-surround evil-numbers evil-leader evil-nerd-commenter
    helm helm-cider
    clojure-mode paredit cider rainbow-delimiters company-mode))

(defvar my:opt-packages '(powerline))
(if window-system
  (el-get 'sync my:packages my:opt-packages)
  (el-get 'sync my:packages))

;; backspace
(define-key key-translation-map [?\C-h] [?\C-?])
(global-set-key (kbd "C-?") 'help-for-help)

;; evil
(evil-mode 1)
(define-key evil-insert-state-map (kbd "C-a") 'move-beginning-of-line)
(define-key evil-insert-state-map (kbd "C-e") 'move-end-of-line)
(define-key evil-insert-state-map (kbd "C-h") 'delete-backward-char)

;; evil-numbers
(define-key evil-normal-state-map (kbd "+") 'evil-numbers/inc-at-pt)
(define-key evil-normal-state-map (kbd "-") 'evil-numbers/dec-at-pt)

;; evil-leader
(global-evil-leader-mode)
(evil-leader/set-leader ",")
(evil-leader/set-key
  "f" 'helm-find-files
  "b" 'helm-buffers-list
  "x" 'helm-M-x
  "c" 'evilnc-comment-or-uncomment-lines
  "j" 'cider-jack-in
  "v" 'cider-eval-last-sexp
  "d" 'helm-cider-cheatsheet
  "s" 'eshell
  "e" 'dired
  )

;; company-mode
(global-company-mode)

;; cider
(helm-cider-mode 1)
(add-hook 'cider-mode-hook 'company-mode)
(add-hook 'cider-mode-hook 'eldoc-mode)
(add-hook 'cider-repl-mode-hook 'company-mode)
(add-hook 'cider-repl-mode-hook 'eldoc-mode)

;; rainbow-delimiters
(add-hook 'lisp-interaction-mode-hook 'rainbow-delimiters-mode)
(add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)
(add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)
(add-hook 'cider-mode-hook 'rainbow-delimiters-mode)

(custom-set-variables
 '(package-selected-packages (quote (queue))))
(custom-set-faces)
