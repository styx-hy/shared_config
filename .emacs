;;; * General settings
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)
(when window-system
  (scroll-bar-mode -1)
  (tool-bar-mode -1))
(display-time-mode t)
(when (not window-system)
  (menu-bar-mode -1))
(setq display-time-day-and-date t)
(setq display-time-24hr-format t)
(show-paren-mode t)
(blink-cursor-mode -1)
(add-to-list 'load-path "~/.emacs.d")

;; emacs -nw:  emacs in terminal
;; map RET to [return] in terminal mode, fix `ret` in cscope jumping
(let ((map (if (boundp 'input-decode-map)
	       input-decode-map function-key-map)))
  (define-key map (kbd "RET") [return]))

;;; * Miscellaneous

;; metrics
(setq initial-frame-alist
      `((height . 60)
	(width . 150)))

;; scrolling
(setq scroll-margin 10)
(setq scroll-step 1)

;; Set find-file to be case insensitive
(setq read-file-name-completion-ignore-case t)

;; font
(add-to-list 'default-frame-alist
	     '(font . "Monaco-12"))

;; backup
(setq backup-by-copyting t
      backup-directory-alist
      '(("." . "~/.saves"))
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

;; whole line or region
(defun my-kill-ring-save (beg end flash)
  (interactive (if (use-region-p)
		   (list (region-beginning) (region-end) nil)
		 (list (line-beginning-position)
		       (line-beginning-position 2) 'flash)))
  (kill-ring-save beg end)
  (when flash
    (save-excursion
      (if (equal (current-column) 0)
	  (goto-char end)
	(goto-char beg))
      (sit-for blink-matching-delay))))
(global-set-key [remap kill-ring-save] 'my-kill-ring-save)

(put 'kill-region 'interactive-form      
     '(interactive
       (if (use-region-p)
           (list (region-beginning) (region-end))
         (list (line-beginning-position) (line-beginning-position 2)))))

;; open large files
(defun my-find-file-check-make-large-file-read-only-hook ()
  "If a file is over a given size, make the buffer read only."
  (when (> (buffer-size) (* 1024 1024))
    (setq buffer-read-only t)
    (buffer-disable-undo)
    (fundamental-mode)))

(add-hook 'find-file-hooks 'my-find-file-check-make-large-file-read-only-hook)

;;; * Packages

(package-initialize)

;;; ** ansi-term
(add-hook 'term-exec-hook
          (function
           (lambda ()
             (set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix))))

;;; ** perl

;; append perlbrew path to environment variable
(setenv "PATH"
	(concat "~/.pb/perls/perl-5.16-thread/bin:~/zion/go/bin:~/zion/letsgo/bin:/usr/local/bin:" 
		(getenv "PATH")))
(setenv "PATH" (concat "~/zion/racket/racket/bin:" (getenv "PATH")))
(setq exec-path (split-string (getenv "PATH") ":"))

;;; ** ELPA

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
			 ("marmalade" . "http://marmalade-repo.org/packages/")
			 ("melpa" . "http://melpa.milkbox.net/packages/")
			 ("org" . "http://orgmode.org/elpa/")))

;;; ** Git

(add-to-list 'load-path "/usr/local/share/git-core/contrib/emacs")
(require 'git)
(global-set-key (kbd "C-c i") 'magit-status) ; 'i' for info (of git repo)

;; ;; bookmark+
;; (add-to-list 'load-path "~/.emacs.d/elpa/bookmark+-20130317.1522")
;; (require 'bookmark+)


;;; ** gnus Gmail

;; (require 'gnus)
;; (setq gnus-select-method '(nnimap "gmail"
;; 				  (nnimap-address "imap.gmail.com")   ; it could also be imap.googlemail.com if that's your server.
;; 				  (nnimap-server-port 993)
;; 				  (nnimap-stream ssl)))

;;; ** gocode

;; (eval-after-load "auto-complete-config"
;;   '(progn
;;      (require 'go-autocomplete)
;;      ))
;; (require 'auto-complete-config)

;; (eval-after-load "company-mode"
;;   '(progn
;;      ))
;; (setq company-tooltip-limit 20)                      ; bigger popup window
;; (setq company-minimum-prefix-length 0)               ; autocomplete right after '.'
;; (setq company-idle-delay .3)                         ; shorter delay before autocompletion popup
;; (setq company-echo-delay 0)                          ; removes annoying blinking
;; (setq company-begin-commands '(self-insert-command)) ; start autocompletion only after typing
;; (add-hook 'after-init-hook (lambda ()
;; 			     (progn
;; 			       (require 'company-clang)
;; 			       (add-hook 'go-mode-hook (lambda ()
;; 							 (progn
;; 							   (require 'company) ; load company mode
;; 							   (require 'company-go) ; load company mode go backend
;; 							   (set (make-local-variable 'company-backends) '(company-go))
;; 							   (company-mode))))
;; 			       (add-hook 'c-mode-hook (lambda ()
;; 							(company-mode)
;; 							)))))
;; (setq geiser-company--completions t)

;; (custom-set-faces
;;  '(company-preview
;;    ((t (:foreground "darkgray" :underline t))))
;;  '(company-preview-common
;;    ((t (:inherit company-preview))))
;;  '(company-tooltip
;;    ((t (:background "lightgray" :foreground "black"))))
;;  '(company-tooltip-selection
;;    ((t (:background "steelblue" :foreground "white"))))
;;  '(company-tooltip-common
;;    ((((type x)) (:inherit company-tooltip :weight bold))
;;     (t (:inherit company-tooltip))))
;;  '(company-tooltip-common-selection
;;    ((((type x)) (:inherit company-tooltip-selection :weight bold))
;;     (t (:inherit company-tooltip-selection)))))

;;; ** go-mode

;; change default indent tab width from 8 to 4
(add-hook 'go-mode-hook (lambda ()
			  (setq tab-width 4)))

;;; ** powerline

;; (add-to-list 'load-path "~/.emacs.d/powerline")
;; (add-hook 'after-init-hook (lambda ()
;; 			     (progn
;; 			       (require 'powerline)
;; 			       (when window-system
;; 				 (set-face-attribute 'mode-line nil
;; 				 		     ;; :background "OliveDrab3"
;; 				 		     :foreground "black")
;; 				 (powerline-default-theme))
;; 				)))

;; hl-line-mode
;; (require 'hl-line)
;; (when window-system
;;   (global-hl-line-mode t))

;;; ** ido

(require 'ido)
(ido-mode t)
(setq ido-decorations (quote ("\n-> " "" "\n   " "\n   ..." "[" "]" " [No match]" " [Matched]" " [Not readable]" " [Too big]" " [Confirm]"))) ; Display ido results vertically, rather than horizontally
(defun ido-disable-line-truncation () (set (make-local-variable 'truncate-lines) nil))
(add-hook 'ido-minibuffer-setup-hook 'ido-disable-line-truncation)
(global-set-key (kbd "M-i") 'ido-goto-symbol)
(global-set-key (kbd "C-x C-b") 'ido-switch-buffer)
(global-set-key (kbd "C-x b") 'list-buffers)
;; (global-set-key (kbd "C-x C-b") 'list-buffers)
;; (global-set-key (kbd "C-x b") 'ido-switch-buffer)
(defun ido-goto-symbol (&optional symbol-list)
  "Refresh imenu and jump to a place in the buffer using Ido."
  (interactive)
  (unless (featurep 'imenu)
	  (require 'imenu nil t))
  (cond
   ((not symbol-list)
    (let ((ido-mode ido-mode)
	  (ido-enable-flex-matching
	   (if (boundp 'ido-enable-flex-matching)
	       ido-enable-flex-matching t))
	  name-and-pos symbol-names position)
      (unless ido-mode
	      (ido-mode 1)
	      (setq ido-enable-flex-matching t))
      (while (progn
	      (imenu--cleanup)
	      (setq imenu--index-alist nil)
	      (ido-goto-symbol (imenu--make-index-alist))
	      (setq selected-symbol
		    (ido-completing-read "Symbol? " symbol-names))
	      (string= (car imenu--rescan-item) selected-symbol)))
      (unless (and (boundp 'mark-active) mark-active)
	      (push-mark nil t nil))
      (setq position (cdr (assoc selected-symbol name-and-pos)))
      (cond
       ((overlayp position)
	(goto-char (overlay-start position)))
       (t
	(goto-char position)))))
   ((listp symbol-list)
    (dolist (symbol symbol-list)
	    (let (name position)
	      (cond
	       ((and (listp symbol) (imenu--subalist-p symbol))
		(ido-goto-symbol symbol))
	       ((listp symbol)
		(setq name (car symbol))
		(setq position (cdr symbol)))
	       ((stringp symbol)
		(setq name symbol)
		(setq position
		      (get-text-property 1 'org-imenu-marker symbol))))
	      (unless (or (null position) (null name)
			  (string= (car imenu--rescan-item) name))
		      (add-to-list 'symbol-names name)
		      (add-to-list 'name-and-pos (cons name position))))))))

(defvar smart-use-extended-syntax nil
  "If t the smart symbol functionality will consider extended
syntax in finding matches, if such matches exist.")

(defvar smart-last-symbol-name ""
  "Contains the current symbol name.

This is only refreshed when `last-command' does not contain
either `smart-symbol-go-forward' or `smart-symbol-go-backward'")

(make-local-variable 'smart-use-extended-syntax)

(defvar smart-symbol-old-pt nil
  "Contains the location of the old point")

(defun smart-symbol-goto (name direction)
  "Jumps to the next NAME in DIRECTION in the current buffer.

DIRECTION must be either `forward' or `backward'; no other option
is valid."

  ;; if `last-command' did not contain
  ;; `smart-symbol-go-forward/backward' then we assume it's a
  ;; brand-new command and we re-set the search term.
  (unless (memq last-command '(smart-symbol-go-forward
                               smart-symbol-go-backward))
    (setq smart-last-symbol-name name))
  (setq smart-symbol-old-pt (point))
  (message (format "%s scan for symbol \"%s\""
                   (capitalize (symbol-name direction))
                   smart-last-symbol-name))
  (unless (catch 'done
            (while (funcall (cond
                             ((eq direction 'forward) ; forward
                              'search-forward)
                             ((eq direction 'backward) ; backward
                              'search-backward)
                             (t (error "Invalid direction"))) ; all others
                            smart-last-symbol-name nil t)
              (unless (memq (syntax-ppss-context
                             (syntax-ppss (point))) '(string comment))
                (throw 'done t))))
    (goto-char smart-symbol-old-pt)))

(defun smart-symbol-go-forward ()
  "Jumps forward to the next symbol at point"
  (interactive)
  (smart-symbol-goto (smart-symbol-at-pt 'end) 'forward))

(defun smart-symbol-go-backward ()
  "Jumps backward to the previous symbol at point"
  (interactive)
  (smart-symbol-goto (smart-symbol-at-pt 'beginning) 'backward))

(defun smart-symbol-at-pt (&optional dir)
  "Returns the symbol at point and moves point to DIR (either `beginning' or `end') of the symbol.

If `smart-use-extended-syntax' is t then that symbol is returned
instead."
  (with-syntax-table (make-syntax-table)
    (if smart-use-extended-syntax
        (modify-syntax-entry ?. "w"))
    (modify-syntax-entry ?_ "w")
    (modify-syntax-entry ?- "w")
    ;; grab the word and return it
    (let ((word (thing-at-point 'word))
          (bounds (bounds-of-thing-at-point 'word)))
      (if word
          (progn
            (cond
             ((eq dir 'beginning) (goto-char (car bounds)))
             ((eq dir 'end) (goto-char (cdr bounds)))
             (t (error "Invalid direction")))
            word)
        (error "No symbol found")))))

(global-set-key (kbd "M-n") 'smart-symbol-go-forward)
(global-set-key (kbd "M-p") 'smart-symbol-go-backward)

;;; ** speedbar

;; (when window-system
;;   (require 'sr-speedbar))
;; (defun my-semantic-hook ()
;;   (imenu-add-to-menubar "TAGS"))
;; (add-hook 'semantic-init-hooks 'my-semantic-hook)
;; (setq speedbar-show-unknown-files t)
;; (setq sr-speedbar-right-side nil)
;; ;; (custom-set-variables '(sr-speedbar-right-side nil) '(sr-speedbar-skip-other-window-p t) '(sr-speedbar-max-width 30) '(sr-speedbar-width-x 30))
;; (setq sr-speedbar-width 16)
;; (setq sr-speedbar-width-x 16)
;; (setq sr-speedbar-skip-other-window-p t)
;; (setq speedbar-frame-parameters (quote
;; 				 ((minibuffer)
;; 				  (width . 20)
;; 				  (border-width . 0)
;; 				  (menu-bar-lines . 0)
;; 				  (tool-bar-lines . 0)
;; 				  (unsplittable . t)
;; 				  (left-fringe . 0))))
;; (sr-speedbar-open);


;;; ** tramp
(setq tramp-ssh-controlmaster-options "")
(setq tramp-shell-prompt-pattern "\\(?:^\\|
\\)[^]#$%>›\n]*#?[]#$%>›] *\\(\\[[0-9;]*[a-zA-Z] *\\)*")

;;; ** yasnippet

(add-to-list 'load-path "~/.emacs.d/yasnippet")
(require 'yasnippet)

(define-key yas-minor-mode-map (kbd "C-c ; u") 'yas/expand)
(defadvice yas/insert-snippet (around use-completing-prompt activate)
  "Use `yas/completing-prompt' for `yas/prompt-functions' but only here..."
  (let ((yas/prompt-functions '(yas/completing-prompt)))
    ad-do-it))
(add-hook 'c-mode-hook
	  '(lambda ()
	     (yas-minor-mode)))
(add-hook 'org-mode-hook
	  '(lambda ()
	     (yas-minor-mode)))
;; (yas-global-mode 1)
(defun yas-ido-expand ()
  "Lets you select (and expand) a yasnippet key"
  (interactive)
    (let ((original-point (point)))
      (while (and
              (not (= (point) (point-min) ))
              (not
               (string-match "[[:space:]\n]" (char-to-string (char-before)))))
        (backward-word 1))
    (let* ((init-word (point))
           (word (buffer-substring init-word original-point))
           (list (yas-active-keys)))
      (goto-char original-point)
      (let ((key (remove-if-not
                  (lambda (s) (string-match (concat "^" word) s)) list)))
        (if (= (length key) 1)
            (setq key (pop key))
          (setq key (ido-completing-read "key: " list nil nil word)))
        (delete-char (- init-word original-point))
        (insert key)
        (yas-expand)))))
(define-key yas-minor-mode-map (kbd "<C-tab>") 'yas-ido-expand)
(yas-reload-all)

(add-to-list 'yas-snippet-dirs "~/.emacs.d/yas-go")

;; (font-lock-add-keywords 'c-mode '(("\\(\\w+\\)\\s-*\(" . font-lock-function-name-face)))
;; (font-lock-add-keywords 'c-mode '(("if" . font-lock-keyword-face)))
;; (font-lock-add-keywords 'c-mode '(("for" . font-lock-keyword-face)))
;; (font-lock-add-keywords 'c-mode '(("while" . font-lock-keyword-face)))
(global-font-lock-mode t)
;; (setq font-lock-defaults t)

;; (when (and (fboundp 'semantic-mode)
;;            (not (locate-library "semantic-ctxt"))) ; can't found offical cedet
;;       (setq semantic-default-submodes '(global-semantic-idle-scheduler-mode
;; 					global-semanticdb-minor-mode
;; 					global-semantic-idle-summary-mode
;; 					global-semantic-mru-bookmark-mode)))
;; (semantic-mode 1)
;; (require 'semantic/ctxt)
;; (add-to-list 'load-path "~/.emacs.d/elpa/highlight-21.0")
;; (require 'zjl-hl)
;; ;
;;(zjl-hl-enable-global-all-modes)

;; (zjl-hl-enable-global 'c-mode);; (zjl-hl-disable-global 'c-mode)

;; (zjl-hl-enable-global 'emacs-lisp-mode);; (zjl-hl-disable-global 'emacs-lisp-mode)


;;; ** auto-complete

(add-hook 'after-init-hook (lambda ()
			     (require 'auto-complete-config)
			     (add-to-list 'ac-dictionary-directories "~/.emacs.d/auto-complete/dict")
			     (ac-config-default)
			     (setq-default ac-sources '(
							ac-source-yasnippet
							ac-source-abbrev
							ac-source-dictionary
							ac-source-words-in-same-mode-buffers
							))
))

(add-hook 'after-init-hook (lambda ()
			     (progn
			       (defun ac-cc-mode-setup ()
				 (setq ac-clang-complete-executable "~/.emacs.d/emacs-clang-complete-async/clang-complete")
				 (setq ac-sources '(ac-source-clang-async))
				 (ac-clang-launch-completion-process)
				 )
			       (defun ac-go-mode-setup ()
				 (require 'go-autocomplete)
				 (require 'auto-complete-config)
				 (add-hook 'before-save-hook #'gofmt-before-save))

			       (defun my-ac-config ()
				 (add-hook 'c-mode-common-hook 'ac-cc-mode-setup)
				 (add-hook 'auto-complete-mode-hook 'ac-common-setup)
				 (add-hook 'go-mode-hook 'ac-go-mode-setup)
				 )

			       (my-ac-config))))

;;; cursor
;; (setq-default cursor-type 'hbar)
;; (require 'cursor-chg)
;; (change-cursor-mode t)
;; (toggle-cursor-type-when-idle t)

;;; ** xcscope/ascope

;; (add-to-list 'load-path "~/.emacs.d/xcscope")
;; (require 'xcscope)
(require 'ascope)
(add-hook 'after-init-hook
	  (lambda ()
	    (progn
	      (defun ascope-c-mode-setup ()
		(progn
		  (define-key c-mode-base-map (kbd "C-c s a") 'ascope-init)
		  (define-key c-mode-base-map (kbd "C-c s g") 'ascope-find-global-definition)
		  (define-key c-mode-base-map (kbd "C-c s c") 'ascope-find-functions-calling-this-function)
		  (define-key c-mode-base-map (kbd "C-c s s") 'ascope-find-this-symbol)
		  (define-key c-mode-base-map (kbd "C-c s u") 'ascope-pop-mark)))
	      (defun ascope-asm-mode-setup ()
		(progn
		  (define-key asm-mode-map (kbd "C-c s a") 'ascope-init)
		  (define-key asm-mode-map (kbd "C-c s g") 'ascope-find-global-definition)
		  (define-key asm-mode-map (kbd "C-c s c") 'ascope-find-functions-calling-this-function)
		  (define-key asm-mode-map (kbd "C-c s s") 'ascope-find-this-symbol)
		  (define-key asm-mode-map (kbd "C-c s u") 'ascope-pop-mark)))
	      (defun my-ascope-config ()
		(add-hook 'c-mode-common-hook 'ascope-c-mode-setup)
		(add-hook 'asm-mode-hook 'ascope-asm-mode-setup))
	      (my-ascope-config))))

;; eshell
(setq eshell-cmpl-ignore-case t)
(add-hook 'eshell-mode-hook
	  (lambda ()
	    (define-key eshell-mode-map (kbd "C-g") 'delete-window)))


;; Key bindings
;; (setq mac-command-modifier 'meta)
;; (setq mac-option-modifier nil)
;; (global-set-key [f1] 'term) ;; instead of shell
(global-set-key [f1] 'eshell)
(global-set-key (kbd "C-,") 'beginning-of-buffer)
(global-set-key (kbd "C-.") 'end-of-buffer)
(global-set-key [f5] 'revert-buffer)
(global-set-key [f8] 'sr-speedbar-toggle)
(global-set-key [f11] 'ns-toggle-fullscreen)
(defun my-fullscreen ()
  (interactive)
  (x-send-client-message
   nil 0 nil "_NET_WM_STATE" 32
   '(2 "_NET_WM_STATE_FULLSCREEN" 0))
)

(defun push-mark-no-activate ()
  "Pushes `point' to `mark-ring' and does not activate the region
Equivalent to \\[set-mark-command] when \\[transient-mark-mode] is disabled"
  (interactive)
  (push-mark (point) t nil)
  (message "Pushed mark to ring"))
(global-set-key (kbd "C-`") 'push-mark-no-activate)

(defun jump-to-mark ()
  "Jumps to the local mark, respecting the `mark-ring' order.
This is the same as using \\[set-mark-command] with the prefix argument."
  (interactive)
  (set-mark-command 1))
(global-set-key (kbd "M-`") 'jump-to-mark)


;;; ** markdown-mode

(add-to-list 'load-path "~/.emacs.d/markdown-mode")
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

(require 'load-theme-buffer-local)

;;; ** org

(setq org-todo-keywords
       '((sequence "TODO(t)" "WAIT(w@/!)" "|" "DONE(d!)" "CANCELED(c@)")))
(defun org-summary-todo (n-done n-not-done)
       "Switch entry to DONE when all subentries are done, to TODO otherwise."
       (let (org-log-done org-log-states)   ; turn off logging
         (org-todo (if (= n-not-done 0) "DONE" "TODO"))))
(add-hook 'org-after-todo-statistics-hook 'org-summary-todo)

;; organize structure of .emacs configuraton
(add-hook 'emacs-lisp-mode-hook 'turn-on-orgstruct)
(setq orgstruct-heading-prefix-regexp "^;;; *")
(setq org-startup-truncated nil)

(setq org-src-fontify-natively t)

;; Someone says it is not necessary to do the `require' and also shouldn't
(require 'org-latex)

(defun my-auto-tex-cmd ()
  "When exporting from .org with latex, automatically run latex,
     pdflatex, or xelatex as appropriate, using latexmk."
  (let ((texcmd)))
  ;; default command: oldstyle latex via dvi
  (setq texcmd "latexmk -dvi -pdfps -quiet %f")
  ;; pdflatex -> .pdf
  (if (string-match "LATEX_CMD: pdflatex" (buffer-string))
      (setq texcmd "latexmk -pdf -quiet %f"))
  ;; xelatex -> .pdf
  (if (string-match "LATEX_CMD: xelatex" (buffer-string))
      (setq texcmd "latexmk -pdflatex='xelatex -8bit --shell-escape' -pdf -quiet %f"))
  ;; LaTeX compilation command
  (setq org-latex-to-pdf-process (list texcmd)))
(add-hook 'org-latex-after-initial-vars-hook 'my-auto-tex-cmd)

(defun my-auto-tex-parameters ()
  "Automatically select the tex packages to include."
  ;; default packages for ordinary latex or pdflatex export
  (setq org-latex-default-packages-alist
	'(("AUTO" "inputenc" t)
	  ("T1"   "fontenc"   t)
	  (""     "fixltx2e"  nil)
	  (""     "wrapfig"   nil)
	  (""     "soul"      t)
	  (""     "textcomp"  t)
	  (""     "marvosym"  t)
	  (""     "wasysym"   t)
	  (""     "latexsym"  t)
	  (""     "amssymb"   t)
	  (""     "hyperref"  nil)))
  
  ;; Packages to include when xelatex is used
  ;; (see https://github.com/kjhealy/latex-custom-kjh for the 
  ;; non-standard ones.)
  (if (string-match "LATEX_CMD: xelatex" (buffer-string))
      (setq org-latex-default-packages-alist
	    '(("" "fontspec" t)
	      ("" "xunicode" t)
	      ("" "url" t)
	      ("" "rotating" t)
	      ("" "memoir-article-styles" t)
	      ("american" "babel" t)
	      ("babel" "csquotes" t)
	      ("" "listings" nil)
	      ("" "listings-sweave-xelatex" nil)
	      ("svgnames" "xcolor" t)
	      ("" "soul" t)
	      ("xetex, colorlinks=true, urlcolor=FireBrick, plainpages=false, pdfpagelabels, bookmarksnumbered" "hyperref" nil)
	      )))
  
  (if (string-match "LATEX_CMD: xelatex" (buffer-string))
      (setq org-latex-classes
	    (cons '("article"
		    "\\documentclass[11pt,article,oneside]{memoir}
  \\input{vc}
  \\usepackage[style=authoryear-comp-ajs, abbreviate=true]{biblatex}
  \\bibliography{socbib}"
		    ("\\section{%s}" . "\\section*{%s}")
		    ("\\subsection{%s}" . "\\subsection*{%s}")
		    ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
		    ("\\paragraph{%s}" . "\\paragraph*{%s}")
		    ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
		  org-latex-classes))))  

(add-hook 'org-latex-after-initial-vars-hook 'my-auto-tex-parameters)

(setq org-latex-listings 'minted)
(add-to-list 'org-latex-packages-alist '("" "minted"))
;; (add-to-list 'org-latex-packages-alist '("" "color"))


;;; ** org-jekyll
(require 'org-publish)

(setq org-publish-yh "~/zion/yanghong.info/")
(setq org-publish-yh-blog "~/zion/yanghong.info/")

(setq org-jekyll-lang-subdirs '(("en" . "publish-blog/blog/")))

(add-to-list 'org-publish-project-alist
             `("yh-org"
               :base-directory "~/Dropbox/notes/"
               :recursive t
               :base-extension "org"
               :publishing-directory ,org-publish-yh
               ;; :exclude "^blog\\|^bitacora\\|yanghong.info"
               :site-root "http://yanghong.info"
               :jekyll-sanitize-permalinks t
               :publishing-function org-html-publish-to-html
               :section-numbers nil
               :headline-levels 4
               :table-of-contents t
               :auto-index nil
               :auto-preamble nil
               :body-only nil
               :auto-postamble nil))

;; (add-to-list 'org-publish-project-alist
;;              '("yh" :components ("yh-org"
;;                                  ;; "jr-img")))
;; 				 )))


;;; ** linum-mode
(global-linum-mode 1)
;; seperate line numbers from text
(setq linum-format
      (lambda (line)
	(propertize (format
		     (let ((w (length (number-to-string
				       (count-lines (point-min) (point-max))))))
		       (concat " %" (number-to-string w) "d ")) line) 'face 'linum)))
(column-number-mode 1)
(add-hook 'linum-mode-hook (lambda ()
			     (progn
			       (require 'auto-complete-config)
			       (ac-linum-workaround))))

;;; ** slime & scheme

;; (setq inferior-lisp-program "/opt/sbcl/bin/sbcl") ; your Lisp system
;; (require 'slime)
;; (slime-setup)

;; gambit - emacs interface
;; (autoload 'gambit-inferior-mode "gambit" "Hook Gambit mode into cmuscheme.")
;; (autoload 'gambit-mode "gambit" "Hook Gambit mode into scheme.")
;; (add-hook 'inferior-scheme-mode-hook (function gambit-inferior-mode))
;; (add-hook 'scheme-mode-hook (function gambit-mode))
(setq scheme-program-name "/usr/local/bin/scheme")
(require 'xscheme)
(setq geiser-active-implementations '(racket))

;; (require 'gambit)

;;; ** zencoding

(add-to-list 'load-path "~/.emacs.d/zencoding")
(require 'zencoding-mode)
(add-hook 'sgml-mode-hook 'zencoding-mode)

;;; ** c-mode and c-common-mode

(require 'google-c-style)
(add-hook 'c-mode-common-hook 'google-set-c-style)
(defun my-c-style-hook ()
  (let ((filename (buffer-file-name)))
    ;; Enable kernel mode for the appropriate files
    (when (and filename
	       (string-match "drreplay"
			     (expand-file-name filename)))
      (setq comment-start "//"
	    comment-end   "")
      (defun google-c-lineup-expression-plus-8 (langelem)
	(save-excursion
	  (back-to-indentation)
	  ;; Go to beginning of *previous* line:
	  (c-backward-syntactic-ws)
	  (back-to-indentation)
	  ;; We are making a reasonable assumption that if there is a control
	  ;; structure to indent past, it has to be at the beginning of the line.
	  (if (looking-at "\\(\\(if\\|for\\|while\\)\\s *(\\)")
	      (goto-char (match-end 1)))
	  (vector (+ 8 (current-column)))))
      (c-set-offset 'arglist-intro
		    'google-c-lineup-expression-plus-8)
      (c-set-offset 'arglist-cont-nonempty
		    '(c-lineup-gcc-asm-reg (c-lineup-argcont 8))))

    (when (and filename
	       (string-match "cse-lab"
			     (expand-file-name filename)))
      (defun google-c-lineup-expression-plus-8 (langelem)
	(save-excursion
	  (back-to-indentation)
	  ;; Go to beginning of *previous* line:
	  (c-backward-syntactic-ws)
	  (back-to-indentation)
	  ;; We are making a reasonable assumption that if there is a control
	  ;; structure to indent past, it has to be at the beginning of the line.
	  (if (looking-at "\\(\\(if\\|for\\|while\\)\\s *(\\)")
	      (goto-char (match-end 1)))
	  (vector (+ 8 (current-column)))))
      (setq c-basic-offset 2)
      (c-set-offset 'arglist-intro
		    'google-c-lineup-expression-plus-8)
      (c-set-offset 'arglist-cont-nonempty
		    '(c-lineup-gcc-asm-reg (c-lineup-argcont 8))))))
(add-hook 'c-mode-hook 'my-c-style-hook)
(add-hook 'c++-mode-hook 'my-c-style-hook)


;; highlight fixme
;; (add-hook 'c-mode-common-hook
;;                (lambda ()
;;                 (font-lock-add-keywords nil
;;                  '(("\\<\\(FIXME\\|TODO\\|BUG\\):" 1 font-lock-warning-face t)))))

;; (add-hook 'c-mode-common-hook 'google-make-newline-indent)

;; (add-hook 'c-mode-hook
;; 	  '(lambda()
;; 	     (c-set-style "k&r")
;; 	     (setq c-basic-offset 4)
;; 	     (setq indent-tabs-mode nil)))

;; Linux Kernel Coding Style
;; (defun c-lineup-arglist-tabs-only (ignored)
;;   "Line up argument lists by tabs, not spaces"
;;   (let* ((anchor (c-langelem-pos c-syntactic-element))
;; 	 (column (c-langelem-2nd-pos c-syntactic-element))
;; 	 (offset (- (1+ column) anchor))
;; 	 (steps (floor offset c-basic-offset)))
;;     (* (max steps 1)
;;        c-basic-offset)))

;;; ** cperl-mode

(defalias 'perl-mode 'cperl-mode)


;;; ** evernote-mode

;;; ** daemon settings

(when window-system
  ;; (setq server-host "styx-mbp")
  ;; (setq server-use-tcp t)
  (unless
      (and (boundp 'server-process)
	   (memq (process-status server-process) '(connect listen open run)))
    (server-start)))

(add-hook 'server-switch-hook
        (lambda ()
          (when (current-local-map)
            (use-local-map (copy-keymap (current-local-map))))
            (when server-buffer-clients
              (local-set-key (kbd "C-x j") 'server-edit))))

;; test existence and state of server
;; (and (boundp 'server-process)
;;      (memq (process-status server-process) '(connect listen open run)))

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

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("ea0c5df0f067d2e3c0f048c1f8795af7b873f5014837feb0a7c8317f34417b04" default)))
 '(ido-max-window-height 15)
 '(split-height-threshold 120))


;;; * Themes
;; (if window-system
;;     (load-theme 'monokai t)
;;   (load-theme 'cyberpunk t))

(add-to-list 'custom-theme-load-path "~/.emacs.d/zenburn-emacs")
(add-to-list 'custom-theme-load-path "~/.emacs.d/yamonokai-theme")
;; (load-theme 'zenburn t)

(add-to-list 'custom-theme-load-path "~/.emacs.d/molokai-theme")
;; (setq molokai-theme-kit t)
;; (load-theme 'molokai t)
;; (setq linum-format "%4d ")
;; (toggle-indicate-empty-lines nil)
;; (setq-default mode-line-format
;; 	      (list
;; 	       ;; the buffer name; the file name as a tool tip
;; 	       '(:eval (propertize "%b " 'face 'font-lock-keyword-face 'help-echo (buffer-file-name)))

;; 	       ;; line and column
;; 	       "(" (propertize "%4l" 'face 'font-lock-type-face) "," (propertize "%4c" 'face 'font-lock-type-face) ") "
;; 	       '(:eval (propertize "%4l" 'face 'font-lock-type-face) (-3 "%4l"))

;; 	       ;; relative position, size of file
;; 	       "[" (propertize "%p" 'face 'font-lock-constant-face) "/" (propertize "%I" 'face 'font-lock-constant-face) "] "

;; 	       ;; the current major mode for the buffer.
;; 	       "[" '(:eval (propertize "%m" 'face 'font-lock-string-face 'help-echo buffer-file-coding-system)) "] "


;; 	       "[" ;; insert vs overwrite mode, input-method in a tooltip
;; 	       '(:eval (propertize (if overwrite-mode "Ovr" "Ins")
;; 				   'face 'font-lock-preprocessor-face
;; 				   'help-echo (concat "Buffer is in "
;; 						      (if overwrite-mode "overwrite" "insert") " mode")))

;; 	       ;; was this buffer modified since the last save?
;; 	       '(:eval (when (buffer-modified-p)
;; 			 (concat ","  (propertize "Mod"
;; 						  'face 'font-lock-warning-face
;; 						  'help-echo "Buffer has been modified"))))

;; 	       ;; is this buffer read-only?
;; 	       '(:eval (when buffer-read-only
;; 			 (concat ","  (propertize "RO"
;; 						  'face 'font-lock-type-face
;; 						  'help-echo "Buffer is read-only"))))
;; 	       "] "

;; 	       ;; add the time, with the date and the emacs uptime in the tooltip
;; 	       '(:eval (propertize (format-time-string "%H:%M")
;; 				   'help-echo
;; 				   (concat (format-time-string "%c; ")
;; 					   (emacs-uptime "Uptime:%hh"))))
;; 	       " --"
;; 	       ;; i don't want to see minor-modes; but if you want, uncomment this:
;; 	       ;; minor-mode-alist  ;; list of minor modes
;; 	       "%-" ;; fill with '-'
;; 	       ))
;; (when window-system
;;   (load-theme 'yamonokai t))
(load-theme 'monokai t)
(set-face-attribute 'mode-line nil :box nil)
(set-face-attribute 'mode-line-inactive nil :box nil)
