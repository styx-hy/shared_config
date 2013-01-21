;; General settings
(tool-bar-mode -1)
(menu-bar-mode -1)
(show-paren-mode t)
(setq display-time-day-and-date t)
(setq display-time-24hr-format t)
(display-time)

(setq read-file-name-completion-ignore-case t) ; Set find-file to be case insensitive

(setq default-frame-alist
      '((height . 60) (width . 180)))
(add-to-list 'default-frame-alist
	     '(font . "Inconsolata-11"))
(add-to-list 'load-path "~/.emacs.d")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(setq backup-by-copyting t
      backup-directory-alist
      '(("." . "~/.saves"))
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

;;(load-theme 'base16-railscasts t)

;; ido-mode
(require 'ido)
(ido-mode t)
(setq ido-decorations (quote ("\n-> " "" "\n   " "\n   ..." "[" "]" " [No match]" " [Matched]" " [Not readable]" " [Too big]" " [Confirm]"))) ; Display ido results vertically, rather than horizontally
(defun ido-disable-line-truncation () (set (make-local-variable 'truncate-lines) nil))
(add-hook 'ido-minibuffer-setup-hook 'ido-disable-line-truncation)

;; speedbar
(when window-system
  (require 'sr-speedbar))
(setq speedbar-show-unknown-files t)
(setq sr-speedbar-right-side nil)
;; (custom-set-variables '(sr-speedbar-right-side nil) '(sr-speedbar-skip-other-window-p t) '(sr-speedbar-max-width 30) '(sr-speedbar-width-x 30))
(setq sr-speedbar-width 16)
(setq sr-speedbar-width-x 16)
(setq speedbar-frame-parameters (quote
				 ((minibuffer)
				  (width . 20)
				  (border-width . 0)
				  (menu-bar-lines . 0)
				  (tool-bar-lines . 0)
				  (unsplittable . t)
				  (left-fringe . 0))))
(sr-speedbar-open);

;; tmtheme
(require 'tmtheme)
(setq tmtheme-directory "~/.emacs.d/tmthemes")
(tmtheme-scan)
(tmtheme-Railscasts)

;; auto-complete
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(require 'auto-complete-config)
(ac-config-default)

;; cursor-mode
;; (setq-default cursor-type 'hbar)
;; (blink-cursor-mode -1)
;; (require 'cursor-chg)
;; (change-cursor-mode t)
;; (toggle-cursor-type-when-idle t)

;; xcscope
(require 'xcscope)

;; eshell
(setq eshell-cmpl-ignore-case t)
(add-hook 'eshell-mode-hook
	  (lambda ()
	    (define-key eshell-mode-map (kbd "C-g") 'delete-window)))


;; Key bindings
;; (global-set-key [f1] 'term) ;; instead of shell
(global-set-key [f1] 'eshell)
(global-set-key (kbd "C-,") 'beginning-of-buffer)
(global-set-key (kbd "C-.") 'end-of-buffer)
(global-set-key [f5] 'revert-buffer)
(global-set-key [f8] 'sr-speedbar-toggle)
(global-set-key [f11] 'my-fullscreen)
(defun my-fullscreen ()
  (interactive)
  (x-send-client-message
   nil 0 nil "_NET_WM_STATE" 32
   '(2 "_NET_WM_STATE_FULLSCREEN" 0))
)

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

;; linum-mode
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
 '(cursor ((t (:background "gray"))))
 '(linum ((t (:inherit (shadow default) :foreground "medium slate blue")))))
