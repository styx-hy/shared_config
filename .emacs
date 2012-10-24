(server-start)
(tool-bar-mode nil)
(show-paren-mode t)
(setq display-time-day-and-date t)
(setq display-time-24hr-format t)
(display-time)

;;(menu-bar-mode nil)

(setq default-frame-alist
      '((height . 45) (width . 100)))
(set-default-font "Inconsolata 11")

(setq backup-by-copyting t
      backup-directory-alist
      '(("." . "~/.saves"))
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

(global-set-key [(control f12)] 'cscope-find-global-definition)
(global-set-key [f11] 'my-fullscreen)
;; 全屏
(defun my-fullscreen ()
  (interactive)
  (x-send-client-message
   nil 0 nil "_NET_WM_STATE" 32
   '(2 "_NET_WM_STATE_FULLSCREEN" 0))
)



(add-to-list 'load-path "~/.elisp")
(require 'color-theme)
(eval-after-load "color-theme"
  '(progn
     (color-theme-initialize)
     (color-theme-xemacs)))

(require 'cursor-chg)
(change-cursor-mode t)
(toggle-cursor-type-when-idle t)

(require 'xcscope)
;; (load-file "/usr/share/emacs/23.4/lisp/cedet/common/cedet.el")

(add-to-list 'load-path "/usr/share/emacs/23.4/lisp/cedet/semantic")
(add-to-list 'load-path "/usr/share/emacs/23.4/lisp/cedet/edc")
(add-to-list 'load-path "/usr/share/emacs/23.4/lisp/cedet/srecode")
(global-ede-mode 1)                      ; Enable the Project management system
;; (semantic-load-enable-code-helpers)      ; Enable prototype help and smart completion 
;; (global-srecode-minor-mode 1)            ; Enable template insertion menu


(add-to-list 'load-path "~/.elisp/ecb-2.40")

(global-set-key [f1] 'shell)
(global-set-key (kbd "C-,") 'beginning-of-buffer)
(global-set-key (kbd "C-.") 'end-of-buffer)
(global-set-key [f5] 'revert-buffer)
;; (load "~/.emacs.d/nxhtml/autostart.el")
;; (add-to-list 'load-path "~/.emacs.d")
;; (add-to-list 'load-path "~/.emacs.d/nxhtml")
;; (add-to-list 'load-path "D:/emacs/site-lisp/emacs-rails")
;; (add-to-list 'load-path "~/.emacs.d/elpa/*")
;; (let ((base "~/.emacs.d/elpa"))
;;   (add-to-list 'load-path base)
;;   (dolist (f (directory-files base))
;;     (let ((name (concat base "/" f)))
;;       (when (and (file-directory-p name)
;;                  (not (equal f ".."))
;;                  (not (equal f ".")))
;;         (add-to-list 'load-path name)))))


(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(require 'org-install)
(setq org-todo-keywords
       '((sequence "TODO(t)" "WAIT(w@/!)" "|" "DONE(d!)" "CANCELED(c@)")))
(defun org-summary-todo (n-done n-not-done)
       "Switch entry to DONE when all subentries are done, to TODO otherwise."
       (let (org-log-done org-log-states)   ; turn off logging
         (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

(add-hook 'org-after-todo-statistics-hook 'org-summary-todo)

(require 'linum)
(global-linum-mode 1)

(setq indent-tabs-mode nil)
(setq c-hungry-delete-key t)
;; (add-hook 'ruby-mode-hook
;; 	  (lambda () (local-set-key (kbd "RET") 'reindent-then-newline-and-indent)))

(defun my-java-mode-common-hook()
  (setq tab-width 4
	indent-tabs-mode t))
(add-hook 'java-mode-hook 'my-java-mode-common-hook)

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(ede-project-directories (quote ("/home/styx/asgard/summerpj")))
 '(inhibit-startup-screen t)
 '(nxhtml-autoload-web t))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(jde-java-font-lock-code-face ((t (:foreground "white"))) t))


;;; This was installed by package-install.el.
;;; This provides support for the package system and
;;; interfacing with ELPA, the package archive.
;;; Move this code earlier if you want to reference
;;; packages in your .emacs.
;; (when
;;     (load
;;      (expand-file-name "~/.emacs.d/elpa/package.el"))
;;   (package-initialize))
;;
;; Setup for ediff.
;;
(require 'ediff)

(defvar ediff-after-quit-hooks nil
  "* Hooks to run after ediff or emerge is quit.")

(defadvice ediff-quit (after edit-after-quit-hooks activate)
  (run-hooks 'ediff-after-quit-hooks))

(setq git-mergetool-emacsclient-ediff-active nil)

(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-split-window-function 'split-window-horizontally)

(defun local-ediff-before-setup-hook ()
  (setq local-ediff-saved-frame-configuration (current-frame-configuration))
  (setq local-ediff-saved-window-configuration (current-window-configuration))
  ;; (local-ediff-frame-maximize)
  (if git-mergetool-emacsclient-ediff-active
      (raise-frame)))

(defun local-ediff-quit-hook ()
  (set-frame-configuration local-ediff-saved-frame-configuration)
  (set-window-configuration local-ediff-saved-window-configuration))

(defun local-ediff-suspend-hook ()
  (set-frame-configuration local-ediff-saved-frame-configuration)
  (set-window-configuration local-ediff-saved-window-configuration))

(add-hook 'ediff-before-setup-hook 'local-ediff-before-setup-hook)
(add-hook 'ediff-quit-hook 'local-ediff-quit-hook 'append)
(add-hook 'ediff-suspend-hook 'local-ediff-suspend-hook 'append)

;; Useful for ediff merge from emacsclient.
(defun git-mergetool-emacsclient-ediff (local remote base merged)
  (setq git-mergetool-emacsclient-ediff-active t)
  (if (file-readable-p base)
      (ediff-merge-files-with-ancestor local remote base nil merged)
    (ediff-merge-files local remote nil merged))
  (recursive-edit))

(defun git-mergetool-emacsclient-ediff-after-quit-hook ()
  (exit-recursive-edit))

(add-hook 'ediff-after-quit-hooks 'git-mergetool-emacsclient-ediff-after-quit-hook 'append)

;; Linux Kernel Coding Style
(defun c-lineup-arglist-tabs-only (ignored)
  "Line up argument lists by tabs, not spaces"
  (let* ((anchor (c-langelem-pos c-syntactic-element))
	 (column (c-langelem-2nd-pos c-syntactic-element))
	 (offset (- (1+ column) anchor))
	 (steps (floor offset c-basic-offset)))
    (* (max steps 1)
       c-basic-offset)))

(add-hook 'c-mode-common-hook
          (lambda ()
            ;; Add kernel style
            (c-add-style
             "linux-tabs-only"
             '("linux" (c-offsets-alist
                        (arglist-cont-nonempty
                         c-lineup-gcc-asm-reg
                         c-lineup-arglist-tabs-only))))))

(add-hook 'c-mode-hook
          (lambda ()
            (let ((filename (buffer-file-name)))
              ;; Enable kernel mode for the appropriate files
              (when (and filename
                         (string-match (expand-file-name "~/asgard/summerpj")
                                       filename))
                (setq indent-tabs-mode t)
                (c-set-style "linux-tabs-only")))))
