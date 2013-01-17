(tool-bar-mode -1)
(menu-bar-mode -1)
(show-paren-mode t)
(setq display-time-day-and-date t)
(setq display-time-24hr-format t)
(display-time)

;; Set find-file to be case insensitive
(setq read-file-name-completion-ignore-case t)

(setq default-frame-alist
      '((height . 60) (width . 150)))
(add-to-list 'default-frame-alist
	     '(font . "Inconsolata-11"))

(setq backup-by-copyting t
      backup-directory-alist
      '(("." . "~/.saves"))
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

(global-set-key [f11] 'my-fullscreen)
;; 全屏
(defun my-fullscreen ()
  (interactive)
  (x-send-client-message
   nil 0 nil "_NET_WM_STATE" 32
   '(2 "_NET_WM_STATE_FULLSCREEN" 0))
)
(add-to-list 'load-path "~/.emacs.d")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
;;(load-theme 'base16-railscasts t)

(require 'tmtheme)
(setq tmtheme-directory "~/.emacs.d/tmthemes")
(tmtheme-scan)
(tmtheme-Railscasts)

(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(require 'auto-complete-config)
(ac-config-default)

;; (setq-default cursor-type 'hbar)
;; (blink-cursor-mode -1)
;; (require 'cursor-chg)
;; (change-cursor-mode t)
;; (toggle-cursor-type-when-idle t)

(require 'xcscope)

(global-set-key [f1] 'shell)
(global-set-key (kbd "C-,") 'beginning-of-buffer)
(global-set-key (kbd "C-.") 'end-of-buffer)
(global-set-key [f5] 'revert-buffer)

(require 'markdown-mode)
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-hook 'markdown-mode-hook
          (function
           (lambda ()
             (setq tab-width 4
		   indent-tabs-mode nil)
	     )))
(add-hook 'markdown-mode-hook
          (function
           (lambda ()
	     (local-set-key (kbd "<tab>") 'markdown-insert-pre)
	     )))
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(require 'org-install)
(setq org-todo-keywords
       '((sequence "TODO(t)" "WAIT(w@/!)" "|" "DONE(d!)" "CANCELED(c@)")))
(defun org-summary-todo (n-done n-not-done)
       "Switch entry to DONE when all subentries are done, to TODO otherwise."
       (let (org-log-done org-log-states)   ; turn off logging
         (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

(add-hook 'org-after-todo-statistics-hook 'org-summary-todo)

(global-linum-mode 1)
;; seperate line numbers from text
(setq linum-format
      (lambda (line)
	(propertize (format
		     (let ((w (length (number-to-string
				       (count-lines (point-min) (point-max)))))) 
		       (concat " %" (number-to-string w) "d ")) line) 'face 'linum)))
(column-number-mode 1)

;; Linux Kernel Coding Style
;; (defun c-lineup-arglist-tabs-only (ignored)
;;   "Line up argument lists by tabs, not spaces"
;;   (let* ((anchor (c-langelem-pos c-syntactic-element))
;; 	 (column (c-langelem-2nd-pos c-syntactic-element))
;; 	 (offset (- (1+ column) anchor))
;; 	 (steps (floor offset c-basic-offset)))
;;     (* (max steps 1)
;;        c-basic-offset)))

;; (add-hook 'c-mode-common-hook
;;           (lambda ()
;;             ;; Add kernel style
;;             (c-add-style
;;              "linux-tabs-only"
;;              '("linux" (c-offsets-alist
;;                         (arglist-cont-nonempty
;;                          c-lineup-gcc-asm-reg
;;                          c-lineup-arglist-tabs-only))))))

;; (add-hook 'c-mode-hook
;;           (lambda ()
;;             (let ((filename (buffer-file-name)))
;;               ;; Enable kernel mode for the appropriate files
;;               (when (and filename
;;                          (string-match (expand-file-name "~/asgard/summerpj")
;;                                        filename))
;;                 (setq indent-tabs-mode t)
;;                 (c-set-style "linux-tabs-only")))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(linum ((t (:inherit (shadow default) :foreground "medium slate blue")))))
