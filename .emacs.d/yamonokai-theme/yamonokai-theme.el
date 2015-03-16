;;; yamonokai-theme.el --- Yet another monokai theme for Emacs 24

;; Copyright (C) 2013 Yang Hong
;; Author: Huang Bin <hy.styx@gmail.com>
;; URL: https://github.com/styx-hy/yamonokai-theme
;; Version: 0.1

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This is another molokai dark theme for Emacs 24.
;; Equiped with my favorites.

;;; Requirements:
;;
;; Emacs 24

;;; Code:

(deftheme yamonokai "The YaMonokai color theme")

;;; Color Palette

(defvar yamonokai-colors-alist
  '(("yamonokai-fg"       . "#F8F8F2")
    ("yamonokai-fg-1"     . "#656555")
    ("yamonokai-bg-1"     . "#2B2B2B")
    ("yamonokai-bg-05"    . "#383838")
    ("yamonokai-bg"       . "#272822")	;background black
    ;; ("yamonokai-bg+1"     . "#4F4F4F")
    ("yamonokai-bg+1"     . "#3E4036")
    ("yamonokai-bg+2"     . "#5F5F5F")
    ("yamonokai-bg+3"     . "#6F6F6F")
    ("yamonokai-red+1"    . "#F92672")
    ("yamonokai-red"      . "#F92672")
    ("yamonokai-red-1"    . "#BC8383")
    ;; ("yamonokai-red-2"    . "#AC7373")
    ("yamonokai-red-2"    . "#F92672")
    ("yamonokai-red-3"    . "#9C6363")
    ("yamonokai-red-4"    . "#8C5353")
    ("yamonokai-orange"   . "#DFAF8F")
    ("yamonokai-yellow"   . "#E6DB74")  ;yellow for string
    ("yamonokai-yellow+1" . "#FDDB0A")  ;yellow for brighter
    ("yamonokai-yellow-1" . "#E0CF9F")
    ("yamonokai-yellow-2" . "#D0BF8F")
    ("yamonokai-green-1"  . "#5F7F5F")
    ("yamonokai-green"    . "#A6E22E")
    ("yamonokai-green+1"  . "#8FB28F")
    ("yamonokai-green+2"  . "#9FC59F")
    ("yamonokai-green+3"  . "#AFD8AF")
    ("yamonokai-green+4"  . "#BFEBBF")
    ("yamonokai-grey"     . "#75715E")	;color of comment
    ("yamonokai-cyan"     . "#93E0E3")
    ("yamonokai-blue+1"   . "#94BFF3")
    ("yamonokai-blue"     . "#66D9EF")
    ("yamonokai-blue-1"   . "#7CB8BB")
    ("yamonokai-blue-2"   . "#66D9EF")
    ;; ("yamonokai-blue-2"   . "#6CA0A3")
    ("yamonokai-blue-3"   . "#66D9EF")
    ("yamonokai-blue-4"   . "#66D9EF")
    ;; ("yamonokai-blue-4"   . "#4C7073")
    ("yamonokai-blue-5"   . "#66D9EF")
    ;; ("yamonokai-blue-5"   . "#366060")
    ("yamonokai-magenta"  . "#DC8CC3")
    ("yamonokai-purple"   . "#AE81FF"))
  "List of Yamonokai colors.
Each element has the form (NAME . HEX).

`+N' suffixes indicate a color is lighter.
`-N' suffixes indicate a color is darker.")

(defmacro yamonokai-with-color-variables (&rest body)
  "`let' bind all colors defined in `yamonokai-colors-alist' around BODY.
Also bind `class' to ((class color) (min-colors 89))."
  (declare (indent 0))
  `(let ((class '((class color) (min-colors 89)))
         ,@(mapcar (lambda (cons)
                     (list (intern (car cons)) (cdr cons)))
                   yamonokai-colors-alist))
     ,@body))

;;; Theme Faces
(yamonokai-with-color-variables
  (custom-theme-set-faces
   'yamonokai
;;;; Built-in
;;;;; basic coloring
   '(button ((t (:underline t))))
   `(link ((t (:foreground ,yamonokai-yellow :underline t :weight bold))))
   `(link-visited ((t (:foreground ,yamonokai-yellow-2 :underline t :weight normal))))
   `(default ((t (:foreground ,yamonokai-fg :background ,yamonokai-bg))))
   `(cursor ((t (:foreground ,yamonokai-fg :background "white"))))
   `(escape-glyph ((t (:foreground ,yamonokai-yellow :bold t))))
   `(fringe ((t (:foreground ,yamonokai-fg :background ,yamonokai-bg))))
   `(header-line ((t (:foreground ,yamonokai-yellow
                                  :background ,yamonokai-bg-1
                                  :box (:line-width -1 :style released-button)))))
   `(highlight ((t (:background ,yamonokai-bg-05))))
   `(success ((t (:foreground ,yamonokai-green :weight bold))))
   `(warning ((t (:foreground ,yamonokai-orange :weight bold))))
;;;;; compilation
   `(compilation-column-face ((t (:foreground ,yamonokai-yellow))))
   `(compilation-enter-directory-face ((t (:foreground ,yamonokai-green))))
   `(compilation-error-face ((t (:foreground ,yamonokai-red-1 :weight bold :underline t))))
   `(compilation-face ((t (:foreground ,yamonokai-fg))))
   `(compilation-info-face ((t (:foreground ,yamonokai-blue))))
   `(compilation-info ((t (:foreground ,yamonokai-green+4 :underline t))))
   `(compilation-leave-directory-face ((t (:foreground ,yamonokai-green))))
   `(compilation-line-face ((t (:foreground ,yamonokai-yellow))))
   `(compilation-line-number ((t (:foreground ,yamonokai-purple))))
   `(compilation-message-face ((t (:foreground ,yamonokai-blue))))
   `(compilation-warning-face ((t (:foreground ,yamonokai-orange :weight bold :underline t))))
   `(compilation-mode-line-exit ((t (:foreground ,yamonokai-green+2 :weight bold))))
   `(compilation-mode-line-fail ((t (:foreground ,yamonokai-red :weight bold))))
   `(compilation-mode-line-run ((t (:foreground ,yamonokai-yellow :weight bold))))
;;;;; grep
   `(grep-context-face ((t (:foreground ,yamonokai-fg))))
   `(grep-error-face ((t (:foreground ,yamonokai-red-1 :weight bold :underline t))))
   `(grep-hit-face ((t (:foreground ,yamonokai-blue))))
   `(grep-match-face ((t (:foreground ,yamonokai-orange :weight bold))))
   `(match ((t (:background ,yamonokai-bg-1 :foreground ,yamonokai-orange :weight bold))))
;;;;; isearch
   `(isearch ((t (:foreground ,yamonokai-yellow-2 :weight bold :background ,yamonokai-bg-1))))
   `(isearch-fail ((t (:foreground ,yamonokai-fg :background ,yamonokai-red-4))))
   `(lazy-highlight ((t (:foreground ,yamonokai-yellow-2 :weight bold :background ,yamonokai-bg-05))))

   `(menu ((t (:foreground ,yamonokai-fg :background ,yamonokai-bg))))
   `(minibuffer-prompt ((t (:foreground ,yamonokai-yellow))))
;;;;; mode-line
   `(mode-line
     ((,class (:foreground ,yamonokai-green
                           :background ,yamonokai-bg+1
                           ;; :box (:line-width -1 :style pressed-button)
                           ))
      (t :inverse-video t)))
   `(mode-line-buffer-id ((t (:foreground ,yamonokai-yellow :weight bold))))
   `(mode-line-inactive
     ((t (:foreground ,yamonokai-green-1
                      :background ,yamonokai-bg-05
                      ;; :box (:line-width -1 :style released-button)
                      ))))
   `(region ((,class (:background ,yamonokai-bg+1))
             (t :inverse-video t)))
   `(secondary-selection ((t (:background ,yamonokai-bg+2))))
   `(trailing-whitespace ((t (:background ,yamonokai-red))))
   `(vertical-border ((t (:foreground ,yamonokai-fg))))
;;;;; font lock
   `(font-lock-builtin-face ((t (:foreground ,yamonokai-fg :weight bold))))
   `(font-lock-comment-face ((t (:foreground ,yamonokai-grey))))
   `(font-lock-comment-delimiter-face ((t (:foreground ,yamonokai-grey))))
   `(font-lock-constant-face ((t (:foreground ,yamonokai-purple))))
   ;; `(font-lock-doc-face ((t (:foreground ,yamonokai-grey))))
   `(font-lock-function-name-face ((t (:foreground ,yamonokai-green))))
   `(font-lock-keyword-face ((t (:foreground ,yamonokai-red :weight bold))))
   `(font-lock-negation-char-face ((t (:foreground ,yamonokai-yellow :weight bold))))
   `(font-lock-preprocessor-face ((t (:foreground ,yamonokai-red))))
   `(font-lock-regexp-grouping-construct ((t (:foreground ,yamonokai-yellow :weight bold))))
   `(font-lock-regexp-grouping-backslash ((t (:foreground ,yamonokai-green :weight bold))))
   `(font-lock-string-face ((t (:foreground ,yamonokai-yellow))))
   `(font-lock-type-face ((t (:foreground ,yamonokai-blue))))
   `(font-lock-variable-name-face ((t (:foreground ,yamonokai-fg))))
   `(font-lock-warning-face ((t (:foreground ,yamonokai-yellow-2 :weight bold))))

   `(c-annotation-face ((t (:inherit font-lock-constant-face))))
;;;;; newsticker
   `(newsticker-date-face ((t (:foreground ,yamonokai-fg))))
   `(newsticker-default-face ((t (:foreground ,yamonokai-fg))))
   `(newsticker-enclosure-face ((t (:foreground ,yamonokai-green+3))))
   `(newsticker-extra-face ((t (:foreground ,yamonokai-bg+2 :height 0.8))))
   `(newsticker-feed-face ((t (:foreground ,yamonokai-fg))))
   `(newsticker-immortal-item-face ((t (:foreground ,yamonokai-green))))
   `(newsticker-new-item-face ((t (:foreground ,yamonokai-blue))))
   `(newsticker-obsolete-item-face ((t (:foreground ,yamonokai-red))))
   `(newsticker-old-item-face ((t (:foreground ,yamonokai-bg+3))))
   `(newsticker-statistics-face ((t (:foreground ,yamonokai-fg))))
   `(newsticker-treeview-face ((t (:foreground ,yamonokai-fg))))
   `(newsticker-treeview-immortal-face ((t (:foreground ,yamonokai-green))))
   `(newsticker-treeview-listwindow-face ((t (:foreground ,yamonokai-fg))))
   `(newsticker-treeview-new-face ((t (:foreground ,yamonokai-blue :weight bold))))
   `(newsticker-treeview-obsolete-face ((t (:foreground ,yamonokai-red))))
   `(newsticker-treeview-old-face ((t (:foreground ,yamonokai-bg+3))))
   `(newsticker-treeview-selection-face ((t (:foreground ,yamonokai-yellow))))
;;;; Third-party
;;;;; ace-jump
   `(ace-jump-face-background
     ((t (:foreground ,yamonokai-fg-1 :background ,yamonokai-bg :inverse-video nil))))
   `(ace-jump-face-foreground
     ((t (:foreground ,yamonokai-green+2 :background ,yamonokai-bg :inverse-video nil))))
;;;;; full-ack
   `(ack-separator ((t (:foreground ,yamonokai-fg))))
   `(ack-file ((t (:foreground ,yamonokai-blue))))
   `(ack-line ((t (:foreground ,yamonokai-yellow))))
   `(ack-match ((t (:foreground ,yamonokai-orange :background ,yamonokai-bg-1 :weight bold))))
;;;;; auctex
   `(font-latex-bold ((t (:inherit bold))))
   `(font-latex-warning ((t (:inherit font-lock-warning))))
   `(font-latex-sedate ((t (:foreground ,yamonokai-yellow :weight bold ))))
   `(font-latex-title-4 ((t (:inherit variable-pitch :weight bold))))
;;;;; auto-complete
   `(ac-candidate-face ((t (:background ,yamonokai-bg+3 :foreground "black"))))
   `(ac-selection-face ((t (:background ,yamonokai-blue-4 :foreground ,yamonokai-fg))))
   `(popup-tip-face ((t (:background ,yamonokai-yellow-2 :foreground "black"))))
   `(popup-scroll-bar-foreground-face ((t (:background ,yamonokai-blue-5))))
   `(popup-scroll-bar-background-face ((t (:background ,yamonokai-bg-1))))
   `(popup-isearch-match ((t (:background ,yamonokai-bg :foreground ,yamonokai-fg))))
;;;;; android mode
   `(android-mode-debug-face ((t (:foreground ,yamonokai-green+1))))
   `(android-mode-error-face ((t (:foreground ,yamonokai-orange :weight bold))))
   `(android-mode-info-face ((t (:foreground ,yamonokai-fg))))
   `(android-mode-verbose-face ((t (:foreground ,yamonokai-green))))
   `(android-mode-warning-face ((t (:foreground ,yamonokai-yellow))))
;;;;; bm
   `(bm-face ((t (:background ,yamonokai-yellow-1 :foreground ,yamonokai-bg))))
   `(bm-fringe-face ((t (:background ,yamonokai-yellow-1 :foreground ,yamonokai-bg))))
   `(bm-fringe-persistent-face ((t (:background ,yamonokai-green-1 :foreground ,yamonokai-bg))))
   `(bm-persistent-face ((t (:background ,yamonokai-green-1 :foreground ,yamonokai-bg))))
;;;;; clojure-test-mode
   `(clojure-test-failure-face ((t (:foreground ,yamonokai-orange :weight bold :underline t))))
   `(clojure-test-error-face ((t (:foreground ,yamonokai-red :weight bold :underline t))))
   `(clojure-test-success-face ((t (:foreground ,yamonokai-green+1 :weight bold :underline t))))
;;;;; ctable
   `(ctbl:face-cell-select ((t (:background ,yamonokai-blue :foreground ,yamonokai-bg))))
   `(ctbl:face-continue-bar ((t (:background ,yamonokai-bg-05 :foreground ,yamonokai-bg))))
   `(ctbl:face-row-select ((t (:background ,yamonokai-cyan :foreground ,yamonokai-bg))))
;;;;; diff
   `(diff-added ((,class (:foreground ,yamonokai-green+4 :background nil))
                 (t (:foreground ,yamonokai-green-1 :background nil))))
   `(diff-changed ((t (:foreground ,yamonokai-yellow))))
   `(diff-removed ((,class (:foreground ,yamonokai-red :background nil))
                   (t (:foreground ,yamonokai-red-3 :background nil))))
   `(diff-refine-added ((t :inherit diff-added :weight bold)))
   `(diff-refine-change ((t :inherit diff-changed :weight bold)))
   `(diff-refine-removed ((t :inherit diff-removed :weight bold)))
   `(diff-header ((,class (:background ,yamonokai-bg+2))
                  (t (:background ,yamonokai-fg :foreground ,yamonokai-bg))))
   `(diff-file-header
     ((,class (:background ,yamonokai-bg+2 :foreground ,yamonokai-fg :bold t))
      (t (:background ,yamonokai-fg :foreground ,yamonokai-bg :bold t))))
;;;;; dired+
   `(diredp-display-msg ((t (:foreground ,yamonokai-blue))))
   `(diredp-compressed-file-suffix ((t (:foreground ,yamonokai-orange))))
   `(diredp-date-time ((t (:foreground ,yamonokai-magenta))))
   `(diredp-deletion ((t (:foreground ,yamonokai-yellow))))
   `(diredp-deletion-file-name ((t (:foreground ,yamonokai-red))))
   `(diredp-dir-heading ((t (:foreground ,yamonokai-blue :background ,yamonokai-bg-1))))
   `(diredp-dir-priv ((t (:foreground ,yamonokai-cyan))))
   `(diredp-exec-priv ((t (:foreground ,yamonokai-red))))
   `(diredp-executable-tag ((t (:foreground ,yamonokai-green+1))))
   `(diredp-file-name ((t (:foreground ,yamonokai-blue))))
   `(diredp-file-suffix ((t (:foreground ,yamonokai-green))))
   `(diredp-flag-mark ((t (:foreground ,yamonokai-yellow))))
   `(diredp-flag-mark-line ((t (:foreground ,yamonokai-orange))))
   `(diredp-ignored-file-name ((t (:foreground ,yamonokai-red))))
   `(diredp-link-priv ((t (:foreground ,yamonokai-yellow))))
   `(diredp-mode-line-flagged ((t (:foreground ,yamonokai-yellow))))
   `(diredp-mode-line-marked ((t (:foreground ,yamonokai-orange))))
   `(diredp-no-priv ((t (:foreground ,yamonokai-fg))))
   `(diredp-number ((t (:foreground ,yamonokai-green+1))))
   `(diredp-other-priv ((t (:foreground ,yamonokai-yellow-1))))
   `(diredp-rare-priv ((t (:foreground ,yamonokai-red-1))))
   `(diredp-read-priv ((t (:foreground ,yamonokai-green-1))))
   `(diredp-symlink ((t (:foreground ,yamonokai-yellow))))
   `(diredp-write-priv ((t (:foreground ,yamonokai-magenta))))
;;;;; ert
   `(ert-test-result-expected ((t (:foreground ,yamonokai-green+4 :background ,yamonokai-bg))))
   `(ert-test-result-unexpected ((t (:foreground ,yamonokai-red :background ,yamonokai-bg))))
;;;;; eshell
   `(eshell-prompt ((t (:foreground ,yamonokai-yellow :weight bold))))
   `(eshell-ls-archive ((t (:foreground ,yamonokai-red-1 :weight bold))))
   `(eshell-ls-backup ((t (:inherit font-lock-comment))))
   `(eshell-ls-clutter ((t (:inherit font-lock-comment))))
   `(eshell-ls-directory ((t (:foreground ,yamonokai-blue+1 :weight bold))))
   `(eshell-ls-executable ((t (:foreground ,yamonokai-red+1 :weight bold))))
   `(eshell-ls-unreadable ((t (:foreground ,yamonokai-fg))))
   `(eshell-ls-missing ((t (:inherit font-lock-warning))))
   `(eshell-ls-product ((t (:inherit font-lock-doc))))
   `(eshell-ls-special ((t (:foreground ,yamonokai-yellow :weight bold))))
   `(eshell-ls-symlink ((t (:foreground ,yamonokai-cyan :weight bold))))
;;;;; flx
   `(flx-highlight-face ((t (:foreground ,yamonokai-green+2 :weight bold))))
;;;;; flycheck
   `(flycheck-error
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,yamonokai-red) :inherit unspecified))
      (t (:foreground ,yamonokai-red-1 :weight bold :underline t))))
   `(flycheck-warning
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,yamonokai-orange) :inherit unspecified))
      (t (:foreground ,yamonokai-orange :weight bold :underline t))))
   `(flycheck-fringe-error ((t (:foreground ,yamonokai-red-1 :weight bold))))
   `(flycheck-fringe-warning ((t (:foreground ,yamonokai-orange :weight bold))))
;;;;; flymake
   `(flymake-errline
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,yamonokai-red)
                   :inherit unspecified :foreground unspecified :background unspecified))
      (t (:foreground ,yamonokai-red-1 :weight bold :underline t))))
   `(flymake-warnline
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,yamonokai-orange)
                   :inherit unspecified :foreground unspecified :background unspecified))
      (t (:foreground ,yamonokai-orange :weight bold :underline t))))
   `(flymake-infoline
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,yamonokai-green)
                   :inherit unspecified :foreground unspecified :background unspecified))
      (t (:foreground ,yamonokai-green-1 :weight bold :underline t))))
;;;;; flyspell
   `(flyspell-duplicate
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,yamonokai-orange) :inherit unspecified))
      (t (:foreground ,yamonokai-orange :weight bold :underline t))))
   `(flyspell-incorrect
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,yamonokai-red) :inherit unspecified))
      (t (:foreground ,yamonokai-red-1 :weight bold :underline t))))
;;;;; erc
   `(erc-action-face ((t (:inherit erc-default-face))))
   `(erc-bold-face ((t (:weight bold))))
   `(erc-current-nick-face ((t (:foreground ,yamonokai-blue :weight bold))))
   `(erc-dangerous-host-face ((t (:inherit font-lock-warning))))
   `(erc-default-face ((t (:foreground ,yamonokai-fg))))
   `(erc-direct-msg-face ((t (:inherit erc-default))))
   `(erc-error-face ((t (:inherit font-lock-warning))))
   `(erc-fool-face ((t (:inherit erc-default))))
   `(erc-highlight-face ((t (:inherit hover-highlight))))
   `(erc-input-face ((t (:foreground ,yamonokai-yellow))))
   `(erc-keyword-face ((t (:foreground ,yamonokai-blue :weight bold))))
   `(erc-nick-default-face ((t (:foreground ,yamonokai-yellow :weight bold))))
   `(erc-my-nick-face ((t (:foreground ,yamonokai-red :weight bold))))
   `(erc-nick-msg-face ((t (:inherit erc-default))))
   `(erc-notice-face ((t (:foreground ,yamonokai-green))))
   `(erc-pal-face ((t (:foreground ,yamonokai-orange :weight bold))))
   `(erc-prompt-face ((t (:foreground ,yamonokai-orange :background ,yamonokai-bg :weight bold))))
   `(erc-timestamp-face ((t (:foreground ,yamonokai-green+1))))
   `(erc-underline-face ((t (:underline t))))
;;;;; git-gutter
   `(git-gutter:added ((t (:foreground ,yamonokai-green :weight bold :inverse-video t))))
   `(git-gutter:deleted ((t (:foreground ,yamonokai-red :weight bold :inverse-video t))))
   `(git-gutter:modified ((t (:foreground ,yamonokai-magenta :weight bold :inverse-video t))))
   `(git-gutter:unchanged ((t (:foreground ,yamonokai-fg :weight bold :inverse-video t))))
;;;;; git-gutter-fr
   `(git-gutter-fr:added ((t (:foreground ,yamonokai-green  :weight bold))))
   `(git-gutter-fr:deleted ((t (:foreground ,yamonokai-red :weight bold))))
   `(git-gutter-fr:modified ((t (:foreground ,yamonokai-magenta :weight bold))))
;;;;; gnus
   `(gnus-group-mail-1 ((t (:bold t :inherit gnus-group-mail-1-empty))))
   `(gnus-group-mail-1-empty ((t (:inherit gnus-group-news-1-empty))))
   `(gnus-group-mail-2 ((t (:bold t :inherit gnus-group-mail-2-empty))))
   `(gnus-group-mail-2-empty ((t (:inherit gnus-group-news-2-empty))))
   `(gnus-group-mail-3 ((t (:bold t :inherit gnus-group-mail-3-empty))))
   `(gnus-group-mail-3-empty ((t (:inherit gnus-group-news-3-empty))))
   `(gnus-group-mail-4 ((t (:bold t :inherit gnus-group-mail-4-empty))))
   `(gnus-group-mail-4-empty ((t (:inherit gnus-group-news-4-empty))))
   `(gnus-group-mail-5 ((t (:bold t :inherit gnus-group-mail-5-empty))))
   `(gnus-group-mail-5-empty ((t (:inherit gnus-group-news-5-empty))))
   `(gnus-group-mail-6 ((t (:bold t :inherit gnus-group-mail-6-empty))))
   `(gnus-group-mail-6-empty ((t (:inherit gnus-group-news-6-empty))))
   `(gnus-group-mail-low ((t (:bold t :inherit gnus-group-mail-low-empty))))
   `(gnus-group-mail-low-empty ((t (:inherit gnus-group-news-low-empty))))
   `(gnus-group-news-1 ((t (:bold t :inherit gnus-group-news-1-empty))))
   `(gnus-group-news-2 ((t (:bold t :inherit gnus-group-news-2-empty))))
   `(gnus-group-news-3 ((t (:bold t :inherit gnus-group-news-3-empty))))
   `(gnus-group-news-4 ((t (:bold t :inherit gnus-group-news-4-empty))))
   `(gnus-group-news-5 ((t (:bold t :inherit gnus-group-news-5-empty))))
   `(gnus-group-news-6 ((t (:bold t :inherit gnus-group-news-6-empty))))
   `(gnus-group-news-low ((t (:bold t :inherit gnus-group-news-low-empty))))
   `(gnus-header-content ((t (:inherit message-header-other))))
   `(gnus-header-from ((t (:inherit message-header-from))))
   `(gnus-header-name ((t (:inherit message-header-name))))
   `(gnus-header-newsgroups ((t (:inherit message-header-other))))
   `(gnus-header-subject ((t (:inherit message-header-subject))))
   `(gnus-summary-cancelled ((t (:foreground ,yamonokai-orange))))
   `(gnus-summary-high-ancient ((t (:foreground ,yamonokai-blue))))
   `(gnus-summary-high-read ((t (:foreground ,yamonokai-green :weight bold))))
   `(gnus-summary-high-ticked ((t (:foreground ,yamonokai-orange :weight bold))))
   `(gnus-summary-high-unread ((t (:foreground ,yamonokai-fg :weight bold))))
   `(gnus-summary-low-ancient ((t (:foreground ,yamonokai-blue))))
   `(gnus-summary-low-read ((t (:foreground ,yamonokai-green))))
   `(gnus-summary-low-ticked ((t (:foreground ,yamonokai-orange :weight bold))))
   `(gnus-summary-low-unread ((t (:foreground ,yamonokai-fg))))
   `(gnus-summary-normal-ancient ((t (:foreground ,yamonokai-blue))))
   `(gnus-summary-normal-read ((t (:foreground ,yamonokai-green))))
   `(gnus-summary-normal-ticked ((t (:foreground ,yamonokai-orange :weight bold))))
   `(gnus-summary-normal-unread ((t (:foreground ,yamonokai-fg))))
   `(gnus-summary-selected ((t (:foreground ,yamonokai-yellow :weight bold))))
   `(gnus-cite-1 ((t (:foreground ,yamonokai-blue))))
   `(gnus-cite-10 ((t (:foreground ,yamonokai-yellow-1))))
   `(gnus-cite-11 ((t (:foreground ,yamonokai-yellow))))
   `(gnus-cite-2 ((t (:foreground ,yamonokai-blue-1))))
   `(gnus-cite-3 ((t (:foreground ,yamonokai-blue-2))))
   `(gnus-cite-4 ((t (:foreground ,yamonokai-green+2))))
   `(gnus-cite-5 ((t (:foreground ,yamonokai-green+1))))
   `(gnus-cite-6 ((t (:foreground ,yamonokai-green))))
   `(gnus-cite-7 ((t (:foreground ,yamonokai-red))))
   `(gnus-cite-8 ((t (:foreground ,yamonokai-red-1))))
   `(gnus-cite-9 ((t (:foreground ,yamonokai-red-2))))
   `(gnus-group-news-1-empty ((t (:foreground ,yamonokai-yellow))))
   `(gnus-group-news-2-empty ((t (:foreground ,yamonokai-green+3))))
   `(gnus-group-news-3-empty ((t (:foreground ,yamonokai-green+1))))
   `(gnus-group-news-4-empty ((t (:foreground ,yamonokai-blue-2))))
   `(gnus-group-news-5-empty ((t (:foreground ,yamonokai-blue-3))))
   `(gnus-group-news-6-empty ((t (:foreground ,yamonokai-bg+2))))
   `(gnus-group-news-low-empty ((t (:foreground ,yamonokai-bg+2))))
   `(gnus-signature ((t (:foreground ,yamonokai-yellow))))
   `(gnus-x ((t (:background ,yamonokai-fg :foreground ,yamonokai-bg))))
;;;;; guide-key
   `(guide-key/highlight-command-face ((t (:foreground ,yamonokai-blue))))
   `(guide-key/key-face ((t (:foreground ,yamonokai-green))))
   `(guide-key/prefix-command-face ((t (:foreground ,yamonokai-green+1))))
;;;;; helm
   `(helm-header
     ((t (:foreground ,yamonokai-green
                      :background ,yamonokai-bg
                      :underline nil
                      :box nil))))
   `(helm-source-header
     ((t (:foreground ,yamonokai-yellow
                      :background ,yamonokai-bg-1
                      :underline nil
                      :weight bold
                      :box (:line-width -1 :style released-button)))))
   `(helm-selection ((t (:background ,yamonokai-bg+1 :underline nil))))
   `(helm-selection-line ((t (:background ,yamonokai-bg+1))))
   `(helm-visible-mark ((t (:foreground ,yamonokai-bg :background ,yamonokai-yellow-2))))
   `(helm-candidate-number ((t (:foreground ,yamonokai-green+4 :background ,yamonokai-bg-1))))
   `(helm-ff-directory ((t (:foreground ,yamonokai-magenta))))
;;;;; hl-line-mode
   `(hl-line-face ((,class (:background ,yamonokai-bg-05))
                   (t :weight bold)))
   `(hl-line ((,class (:background ,yamonokai-bg-05)) ; old emacsen
              (t :weight bold)))
;;;;; hl-sexp
   `(hl-sexp-face ((,class (:background ,yamonokai-bg+1))
                   (t :weight bold)))
;;;;; ido-mode
   `(ido-first-match ((t (:foreground ,yamonokai-yellow :weight bold))))
   `(ido-only-match ((t (:foreground ,yamonokai-orange :weight bold))))
   `(ido-subdir ((t (:foreground ,yamonokai-yellow))))
;;;;; js2-mode
   `(js2-warning ((t (:underline ,yamonokai-orange))))
   `(js2-error ((t (:foreground ,yamonokai-red :weight bold))))
   `(js2-jsdoc-tag ((t (:foreground ,yamonokai-green-1))))
   `(js2-jsdoc-type ((t (:foreground ,yamonokai-green+2))))
   `(js2-jsdoc-value ((t (:foreground ,yamonokai-green+3))))
   `(js2-function-param ((t (:foreground, yamonokai-green+3))))
   `(js2-external-variable ((t (:foreground ,yamonokai-orange))))
;;;;; jabber-mode
   `(jabber-roster-user-away ((t (:foreground ,yamonokai-green+2))))
   `(jabber-roster-user-online ((t (:foreground ,yamonokai-blue-1))))
   `(jabber-roster-user-dnd ((t (:foreground ,yamonokai-red+1))))
   `(jabber-rare-time-face ((t (:foreground ,yamonokai-green+1))))
   `(jabber-chat-prompt-local ((t (:foreground ,yamonokai-blue-1))))
   `(jabber-chat-prompt-foreign ((t (:foreground ,yamonokai-red+1))))
   `(jabber-activity-face((t (:foreground ,yamonokai-red+1))))
   `(jabber-activity-personal-face ((t (:foreground ,yamonokai-blue+1))))
   `(jabber-title-small ((t (:height 1.1 :weight bold))))
   `(jabber-title-medium ((t (:height 1.2 :weight bold))))
   `(jabber-title-large ((t (:height 1.3 :weight bold))))
;;;;; linum-mode
   `(linum ((t (:foreground ,yamonokai-purple :background ,yamonokai-bg))))
   ;; `(linum ((t (:foreground ,molokai-grey-2 :background ,molokai-grey+5))))
;;;;; macrostep
   `(macrostep-gensym-1
     ((t (:foreground ,yamonokai-green+2 :background ,yamonokai-bg-1))))
   `(macrostep-gensym-2
     ((t (:foreground ,yamonokai-red+1 :background ,yamonokai-bg-1))))
   `(macrostep-gensym-3
     ((t (:foreground ,yamonokai-blue+1 :background ,yamonokai-bg-1))))
   `(macrostep-gensym-4
     ((t (:foreground ,yamonokai-magenta :background ,yamonokai-bg-1))))
   `(macrostep-gensym-5
     ((t (:foreground ,yamonokai-yellow :background ,yamonokai-bg-1))))
   `(macrostep-expansion-highlight-face
     ((t (:inherit highlight))))
   `(macrostep-macro-face
     ((t (:underline t))))
;;;;; magit
   `(magit-section-title ((t (:foreground ,yamonokai-yellow :weight bold))))
   `(magit-branch ((t (:foreground ,yamonokai-orange :weight bold))))
   `(magit-item-highlight ((t (:background ,yamonokai-bg+1 :bold nil))))
;;;;; egg
   `(egg-text-base ((t (:foreground ,yamonokai-fg))))
   `(egg-help-header-1 ((t (:foreground ,yamonokai-yellow))))
   `(egg-help-header-2 ((t (:foreground ,yamonokai-green+3))))
   `(egg-branch ((t (:foreground ,yamonokai-yellow))))
   `(egg-branch-mono ((t (:foreground ,yamonokai-yellow))))
   `(egg-term ((t (:foreground ,yamonokai-yellow))))
   `(egg-diff-add ((t (:foreground ,yamonokai-green+4))))
   `(egg-diff-del ((t (:foreground ,yamonokai-red+1))))
   `(egg-diff-file-header ((t (:foreground ,yamonokai-yellow-2))))
   `(egg-section-title ((t (:foreground ,yamonokai-yellow))))
   `(egg-stash-mono ((t (:foreground ,yamonokai-green+4))))
;;;;; message-mode
   `(message-cited-text ((t (:inherit font-lock-comment))))
   `(message-header-name ((t (:foreground ,yamonokai-green+1))))
   `(message-header-other ((t (:foreground ,yamonokai-green))))
   `(message-header-to ((t (:foreground ,yamonokai-yellow :weight bold))))
   `(message-header-from ((t (:foreground ,yamonokai-yellow :weight bold))))
   `(message-header-cc ((t (:foreground ,yamonokai-yellow :weight bold))))
   `(message-header-newsgroups ((t (:foreground ,yamonokai-yellow :weight bold))))
   `(message-header-subject ((t (:foreground ,yamonokai-orange :weight bold))))
   `(message-header-xheader ((t (:foreground ,yamonokai-green))))
   `(message-mml ((t (:foreground ,yamonokai-yellow :weight bold))))
   `(message-separator ((t (:inherit font-lock-comment))))
;;;;; mew
   `(mew-face-header-subject ((t (:foreground ,yamonokai-orange))))
   `(mew-face-header-from ((t (:foreground ,yamonokai-yellow))))
   `(mew-face-header-date ((t (:foreground ,yamonokai-green))))
   `(mew-face-header-to ((t (:foreground ,yamonokai-red))))
   `(mew-face-header-key ((t (:foreground ,yamonokai-green))))
   `(mew-face-header-private ((t (:foreground ,yamonokai-green))))
   `(mew-face-header-important ((t (:foreground ,yamonokai-blue))))
   `(mew-face-header-marginal ((t (:foreground ,yamonokai-fg :weight bold))))
   `(mew-face-header-warning ((t (:foreground ,yamonokai-red))))
   `(mew-face-header-xmew ((t (:foreground ,yamonokai-green))))
   `(mew-face-header-xmew-bad ((t (:foreground ,yamonokai-red))))
   `(mew-face-body-url ((t (:foreground ,yamonokai-orange))))
   `(mew-face-body-comment ((t (:foreground ,yamonokai-fg :slant italic))))
   `(mew-face-body-cite1 ((t (:foreground ,yamonokai-green))))
   `(mew-face-body-cite2 ((t (:foreground ,yamonokai-blue))))
   `(mew-face-body-cite3 ((t (:foreground ,yamonokai-orange))))
   `(mew-face-body-cite4 ((t (:foreground ,yamonokai-yellow))))
   `(mew-face-body-cite5 ((t (:foreground ,yamonokai-red))))
   `(mew-face-mark-review ((t (:foreground ,yamonokai-blue))))
   `(mew-face-mark-escape ((t (:foreground ,yamonokai-green))))
   `(mew-face-mark-delete ((t (:foreground ,yamonokai-red))))
   `(mew-face-mark-unlink ((t (:foreground ,yamonokai-yellow))))
   `(mew-face-mark-refile ((t (:foreground ,yamonokai-green))))
   `(mew-face-mark-unread ((t (:foreground ,yamonokai-red-2))))
   `(mew-face-eof-message ((t (:foreground ,yamonokai-green))))
   `(mew-face-eof-part ((t (:foreground ,yamonokai-yellow))))
;;;;; mic-paren
   `(paren-face-match ((t (:foreground ,yamonokai-cyan :background ,yamonokai-bg :weight bold))))
   `(paren-face-mismatch ((t (:foreground ,yamonokai-bg :background ,yamonokai-magenta :weight bold))))
   `(paren-face-no-match ((t (:foreground ,yamonokai-bg :background ,yamonokai-red :weight bold))))
;;;;; mingus
   `(mingus-directory-face ((t (:foreground ,yamonokai-blue))))
   `(mingus-pausing-face ((t (:foreground ,yamonokai-magenta))))
   `(mingus-playing-face ((t (:foreground ,yamonokai-cyan))))
   `(mingus-playlist-face ((t (:foreground ,yamonokai-cyan ))))
   `(mingus-song-file-face ((t (:foreground ,yamonokai-yellow))))
   `(mingus-stopped-face ((t (:foreground ,yamonokai-red))))
;;;;; nav
   `(nav-face-heading ((t (:foreground ,yamonokai-yellow))))
   `(nav-face-button-num ((t (:foreground ,yamonokai-cyan))))
   `(nav-face-dir ((t (:foreground ,yamonokai-green))))
   `(nav-face-hdir ((t (:foreground ,yamonokai-red))))
   `(nav-face-file ((t (:foreground ,yamonokai-fg))))
   `(nav-face-hfile ((t (:foreground ,yamonokai-red-4))))
;;;;; mu4e
   `(mu4e-cited-1-face ((t (:foreground ,yamonokai-blue    :slant italic))))
   `(mu4e-cited-2-face ((t (:foreground ,yamonokai-green+2 :slant italic))))
   `(mu4e-cited-3-face ((t (:foreground ,yamonokai-blue-2  :slant italic))))
   `(mu4e-cited-4-face ((t (:foreground ,yamonokai-green   :slant italic))))
   `(mu4e-cited-5-face ((t (:foreground ,yamonokai-blue-4  :slant italic))))
   `(mu4e-cited-6-face ((t (:foreground ,yamonokai-green-1 :slant italic))))
   `(mu4e-cited-7-face ((t (:foreground ,yamonokai-blue    :slant italic))))
   `(mu4e-replied-face ((t (:foreground ,yamonokai-bg+3))))
   `(mu4e-trashed-face ((t (:foreground ,yamonokai-bg+3 :strike-through t))))
;;;;; mumamo
   `(mumamo-background-chunk-major ((t (:background nil))))
   `(mumamo-background-chunk-submode1 ((t (:background ,yamonokai-bg-1))))
   `(mumamo-background-chunk-submode2 ((t (:background ,yamonokai-bg+2))))
   `(mumamo-background-chunk-submode3 ((t (:background ,yamonokai-bg+3))))
   `(mumamo-background-chunk-submode4 ((t (:background ,yamonokai-bg+1))))
;;;;; org-mode
   `(org-agenda-date-today
     ((t (:foreground "white" :slant italic :weight bold))) t)
   `(org-agenda-structure
     ((t (:inherit font-lock-comment-face))))
   `(org-archived ((t (:foreground ,yamonokai-fg :weight bold))))
   `(org-checkbox ((t (:background ,yamonokai-bg+2 :foreground "white"
                                   :box (:line-width 1 :style released-button)))))
   `(org-date ((t (:foreground ,yamonokai-blue :underline t))))
   `(org-deadline-announce ((t (:foreground ,yamonokai-red-1))))
   `(org-done ((t (:bold t :weight bold :foreground ,yamonokai-green+3))))
   `(org-formula ((t (:foreground ,yamonokai-yellow-2))))
   `(org-headline-done ((t (:foreground ,yamonokai-green+3))))
   `(org-hide ((t (:foreground ,yamonokai-bg-1))))
   `(org-level-1 ((t (:foreground ,yamonokai-orange))))
   `(org-level-2 ((t (:foreground ,yamonokai-green+4))))
   `(org-level-3 ((t (:foreground ,yamonokai-blue-1))))
   `(org-level-4 ((t (:foreground ,yamonokai-yellow-2))))
   `(org-level-5 ((t (:foreground ,yamonokai-cyan))))
   `(org-level-6 ((t (:foreground ,yamonokai-green+2))))
   `(org-level-7 ((t (:foreground ,yamonokai-red-4))))
   `(org-level-8 ((t (:foreground ,yamonokai-blue-4))))
   `(org-link ((t (:foreground ,yamonokai-yellow-2 :underline t))))
   `(org-scheduled ((t (:foreground ,yamonokai-green+4))))
   `(org-scheduled-previously ((t (:foreground ,yamonokai-red-4))))
   `(org-scheduled-today ((t (:foreground ,yamonokai-blue+1))))
   `(org-sexp-date ((t (:foreground ,yamonokai-blue+1 :underline t))))
   `(org-special-keyword ((t (:foreground ,yamonokai-fg-1 :weight normal))))
   `(org-table ((t (:foreground ,yamonokai-green+2))))
   `(org-tag ((t (:bold t :weight bold))))
   `(org-time-grid ((t (:foreground ,yamonokai-orange))))
   `(org-todo ((t (:bold t :foreground ,yamonokai-red :weight bold))))
   `(org-upcoming-deadline ((t (:inherit font-lock-keyword-face))))
   `(org-warning ((t (:bold t :foreground ,yamonokai-red :weight bold :underline nil))))
   `(org-column ((t (:background ,yamonokai-bg-1))))
   `(org-column-title ((t (:background ,yamonokai-bg-1 :underline t :weight bold))))
;;;;; outline
   `(outline-1 ((t (:foreground ,yamonokai-orange))))
   `(outline-2 ((t (:foreground ,yamonokai-green+4))))
   `(outline-3 ((t (:foreground ,yamonokai-blue-1))))
   `(outline-4 ((t (:foreground ,yamonokai-yellow-2))))
   `(outline-5 ((t (:foreground ,yamonokai-cyan))))
   `(outline-6 ((t (:foreground ,yamonokai-green+2))))
   `(outline-7 ((t (:foreground ,yamonokai-red-4))))
   `(outline-8 ((t (:foreground ,yamonokai-blue-4))))
;;;;; rainbow-delimiters
   `(rainbow-delimiters-depth-1-face ((t (:foreground ,yamonokai-fg))))
   `(rainbow-delimiters-depth-2-face ((t (:foreground ,yamonokai-green+2))))
   `(rainbow-delimiters-depth-3-face ((t (:foreground ,yamonokai-yellow-2))))
   `(rainbow-delimiters-depth-4-face ((t (:foreground ,yamonokai-cyan))))
   `(rainbow-delimiters-depth-5-face ((t (:foreground ,yamonokai-green-1))))
   `(rainbow-delimiters-depth-6-face ((t (:foreground ,yamonokai-blue+1))))
   `(rainbow-delimiters-depth-7-face ((t (:foreground ,yamonokai-yellow-1))))
   `(rainbow-delimiters-depth-8-face ((t (:foreground ,yamonokai-green+1))))
   `(rainbow-delimiters-depth-9-face ((t (:foreground ,yamonokai-blue-2))))
   `(rainbow-delimiters-depth-10-face ((t (:foreground ,yamonokai-orange))))
   `(rainbow-delimiters-depth-11-face ((t (:foreground ,yamonokai-green))))
   `( rainbow-delimiters-depth-12-face ((t (:foreground ,yamonokai-blue-5))))
;;;;; rcirc
   `(rcirc-my-nick ((t (:foreground ,yamonokai-blue))))
   `(rcirc-other-nick ((t (:foreground ,yamonokai-orange))))
   `(rcirc-bright-nick ((t (:foreground ,yamonokai-blue+1))))
   `(rcirc-dim-nick ((t (:foreground ,yamonokai-blue-2))))
   `(rcirc-server ((t (:foreground ,yamonokai-green))))
   `(rcirc-server-prefix ((t (:foreground ,yamonokai-green+1))))
   `(rcirc-timestamp ((t (:foreground ,yamonokai-green+2))))
   `(rcirc-nick-in-message ((t (:foreground ,yamonokai-yellow))))
   `(rcirc-nick-in-message-full-line ((t (:bold t))))
   `(rcirc-prompt ((t (:foreground ,yamonokai-yellow :bold t))))
   `(rcirc-track-nick ((t (:inverse-video t))))
   `(rcirc-track-keyword ((t (:bold t))))
   `(rcirc-url ((t (:bold t))))
   `(rcirc-keyword ((t (:foreground ,yamonokai-yellow :bold t))))
;;;;; rpm-mode
   `(rpm-spec-dir-face ((t (:foreground ,yamonokai-green))))
   `(rpm-spec-doc-face ((t (:foreground ,yamonokai-green))))
   `(rpm-spec-ghost-face ((t (:foreground ,yamonokai-red))))
   `(rpm-spec-macro-face ((t (:foreground ,yamonokai-yellow))))
   `(rpm-spec-obsolete-tag-face ((t (:foreground ,yamonokai-red))))
   `(rpm-spec-package-face ((t (:foreground ,yamonokai-red))))
   `(rpm-spec-section-face ((t (:foreground ,yamonokai-yellow))))
   `(rpm-spec-tag-face ((t (:foreground ,yamonokai-blue))))
   `(rpm-spec-var-face ((t (:foreground ,yamonokai-red))))
;;;;; rst-mode
   `(rst-level-1-face ((t (:foreground ,yamonokai-orange))))
   `(rst-level-2-face ((t (:foreground ,yamonokai-green+1))))
   `(rst-level-3-face ((t (:foreground ,yamonokai-blue-1))))
   `(rst-level-4-face ((t (:foreground ,yamonokai-yellow-2))))
   `(rst-level-5-face ((t (:foreground ,yamonokai-cyan))))
   `(rst-level-6-face ((t (:foreground ,yamonokai-green-1))))
;;;;; show-paren
   `(show-paren-mismatch ((t (:foreground ,yamonokai-red-3 :background ,yamonokai-bg :weight bold))))
   `(show-paren-match ((t (:foreground ,yamonokai-blue-1 :background ,yamonokai-bg :weight bold))))
;;;;; sml-mode-line
   '(sml-modeline-end-face ((t :inherit default :width condensed)))
;;;;; SLIME
   `(slime-repl-inputed-output-face ((t (:foreground ,yamonokai-red))))
;;;;; tabbar
   `(tabbar-button ((t (:foreground ,yamonokai-fg
                                    :background ,yamonokai-bg))))
   `(tabbar-selected ((t (:foreground ,yamonokai-fg
                                      :background ,yamonokai-bg
                                      :box (:line-width -1 :style pressed-button)))))
   `(tabbar-unselected ((t (:foreground ,yamonokai-fg
                                        :background ,yamonokai-bg+1
                                        :box (:line-width -1 :style released-button)))))
;;;;; term
   `(term-color-black ((t (:foreground ,yamonokai-bg
                                       :background ,yamonokai-bg-1))))
   `(term-color-red ((t (:foreground ,yamonokai-red
                                       :background ,yamonokai-red-4))))
   `(term-color-green ((t (:foreground ,yamonokai-green
                                       :background ,yamonokai-green+2))))
   `(term-color-yellow ((t (:foreground ,yamonokai-orange
                                       :background ,yamonokai-yellow))))
   `(term-color-blue ((t (:foreground ,yamonokai-blue
                                      :background ,yamonokai-blue-4))))
   `(term-color-magenta ((t (:foreground ,yamonokai-magenta
                                         :background ,yamonokai-red))))
   `(term-color-cyan ((t (:foreground ,yamonokai-cyan
                                       :background ,yamonokai-blue))))
   `(term-color-white ((t (:foreground ,yamonokai-fg
                                       :background ,yamonokai-fg-1))))
   '(term-default-fg-color ((t (:inherit term-color-white))))
   '(term-default-bg-color ((t (:inherit term-color-black))))
;;;;; volatile-highlights
   `(vhl/default-face ((t (:background ,yamonokai-bg-05))))
;;;;; emacs-w3m
   `(w3m-anchor ((t (:foreground ,yamonokai-yellow :underline t
                                 :weight bold))))
   `(w3m-arrived-anchor ((t (:foreground ,yamonokai-yellow-2
                                         :underline t :weight normal))))
   `(w3m-form ((t (:foreground ,yamonokai-red-1 :underline t))))
   `(w3m-header-line-location-title ((t (:foreground ,yamonokai-yellow
                                                     :underline t :weight bold))))
   '(w3m-history-current-url ((t (:inherit match))))
   `(w3m-lnum ((t (:foreground ,yamonokai-green+2 :background ,yamonokai-bg))))
   `(w3m-lnum-match ((t (:background ,yamonokai-bg-1
                                     :foreground ,yamonokai-orange
                                     :weight bold))))
   `(w3m-lnum-minibuffer-prompt ((t (:foreground ,yamonokai-yellow))))
;;;;; whitespace-mode
   `(whitespace-space ((t (:background ,yamonokai-bg+1 :foreground ,yamonokai-bg+1))))
   `(whitespace-hspace ((t (:background ,yamonokai-bg+1 :foreground ,yamonokai-bg+1))))
   `(whitespace-tab ((t (:background ,yamonokai-red-1))))
   `(whitespace-newline ((t (:foreground ,yamonokai-bg+1))))
   `(whitespace-trailing ((t (:background ,yamonokai-red))))
   `(whitespace-line ((t (:background ,yamonokai-bg :foreground ,yamonokai-magenta))))
   `(whitespace-space-before-tab ((t (:background ,yamonokai-orange :foreground ,yamonokai-orange))))
   `(whitespace-indentation ((t (:background ,yamonokai-yellow :foreground ,yamonokai-red))))
   `(whitespace-empty ((t (:background ,yamonokai-yellow))))
   `(whitespace-space-after-tab ((t (:background ,yamonokai-yellow :foreground ,yamonokai-red))))
;;;;; wanderlust
   `(wl-highlight-folder-few-face ((t (:foreground ,yamonokai-red-2))))
   `(wl-highlight-folder-many-face ((t (:foreground ,yamonokai-red-1))))
   `(wl-highlight-folder-path-face ((t (:foreground ,yamonokai-orange))))
   `(wl-highlight-folder-unread-face ((t (:foreground ,yamonokai-blue))))
   `(wl-highlight-folder-zero-face ((t (:foreground ,yamonokai-fg))))
   `(wl-highlight-folder-unknown-face ((t (:foreground ,yamonokai-blue))))
   `(wl-highlight-message-citation-header ((t (:foreground ,yamonokai-red-1))))
   `(wl-highlight-message-cited-text-1 ((t (:foreground ,yamonokai-red))))
   `(wl-highlight-message-cited-text-2 ((t (:foreground ,yamonokai-green+2))))
   `(wl-highlight-message-cited-text-3 ((t (:foreground ,yamonokai-blue))))
   `(wl-highlight-message-cited-text-4 ((t (:foreground ,yamonokai-blue+1))))
   `(wl-highlight-message-header-contents-face ((t (:foreground ,yamonokai-green))))
   `(wl-highlight-message-headers-face ((t (:foreground ,yamonokai-red+1))))
   `(wl-highlight-message-important-header-contents ((t (:foreground ,yamonokai-green+2))))
   `(wl-highlight-message-header-contents ((t (:foreground ,yamonokai-green+1))))
   `(wl-highlight-message-important-header-contents2 ((t (:foreground ,yamonokai-green+2))))
   `(wl-highlight-message-signature ((t (:foreground ,yamonokai-green))))
   `(wl-highlight-message-unimportant-header-contents ((t (:foreground ,yamonokai-fg))))
   `(wl-highlight-summary-answered-face ((t (:foreground ,yamonokai-blue))))
   `(wl-highlight-summary-disposed-face ((t (:foreground ,yamonokai-fg
                                                         :slant italic))))
   `(wl-highlight-summary-new-face ((t (:foreground ,yamonokai-blue))))
   `(wl-highlight-summary-normal-face ((t (:foreground ,yamonokai-fg))))
   `(wl-highlight-summary-thread-top-face ((t (:foreground ,yamonokai-yellow))))
   `(wl-highlight-thread-indent-face ((t (:foreground ,yamonokai-magenta))))
   `(wl-highlight-summary-refiled-face ((t (:foreground ,yamonokai-fg))))
   `(wl-highlight-summary-displaying-face ((t (:underline t :weight bold))))
;;;;; which-func-mode
   `(which-func ((t (:foreground ,yamonokai-green+4))))
;;;;; yascroll
   `(yascroll:thumb-text-area ((t (:background ,yamonokai-bg-1))))
   `(yascroll:thumb-fringe ((t (:background ,yamonokai-bg-1 :foreground ,yamonokai-bg-1))))
   ))

;;; Theme Variables
(yamonokai-with-color-variables
  (custom-theme-set-variables
   'yamonokai
;;;;; ansi-color
   `(ansi-color-names-vector [,yamonokai-bg ,yamonokai-red ,yamonokai-green ,yamonokai-yellow
                                          ,yamonokai-blue ,yamonokai-magenta ,yamonokai-cyan ,yamonokai-fg])
;;;;; fill-column-indicator
   `(fci-rule-color ,yamonokai-bg-05)
;;;;; vc-annotate
   `(vc-annotate-color-map
     '(( 20. . ,yamonokai-red-1)
       ( 40. . ,yamonokai-red)
       ( 60. . ,yamonokai-orange)
       ( 80. . ,yamonokai-yellow-2)
       (100. . ,yamonokai-yellow-1)
       (120. . ,yamonokai-yellow)
       (140. . ,yamonokai-green-1)
       (160. . ,yamonokai-green)
       (180. . ,yamonokai-green+1)
       (200. . ,yamonokai-green+2)
       (220. . ,yamonokai-green+3)
       (240. . ,yamonokai-green+4)
       (260. . ,yamonokai-cyan)
       (280. . ,yamonokai-blue-2)
       (300. . ,yamonokai-blue-1)
       (320. . ,yamonokai-blue)
       (340. . ,yamonokai-blue+1)
       (360. . ,yamonokai-magenta)))
   `(vc-annotate-very-old-color ,yamonokai-magenta)
   `(vc-annotate-background ,yamonokai-bg-1)
   ))

;;; Rainbow Support

(declare-function rainbow-mode 'rainbow-mode)
(declare-function rainbow-colorize-by-assoc 'rainbow-mode)

(defvar yamonokai-add-font-lock-keywords nil
  "Whether to add font-lock keywords for yamonokai color names.
In buffers visiting library `yamonokai-theme.el' the yamonokai
specific keywords are always added.  In all other Emacs-Lisp
buffers this variable controls whether this should be done.
This requires library `rainbow-mode'.")

(defvar yamonokai-colors-font-lock-keywords nil)

;; (defadvice rainbow-turn-on (after zenburn activate)
;;   "Maybe also add font-lock keywords for zenburn colors."
;;   (when (and (derived-mode-p 'emacs-lisp-mode)
;;              (or zenburn-add-font-lock-keywords
;;                  (equal (file-name-nondirectory (buffer-file-name))
;;                         "zenburn-theme.el")))
;;     (unless zenburn-colors-font-lock-keywords
;;       (setq zenburn-colors-font-lock-keywords
;;             `((,(regexp-opt (mapcar 'car zenburn-colors-alist) 'words)
;;                (0 (rainbow-colorize-by-assoc zenburn-colors-alist))))))
;;     (font-lock-add-keywords nil zenburn-colors-font-lock-keywords)))

;; (defadvice rainbow-turn-off (after zenburn activate)
;;   "Also remove font-lock keywords for zenburn colors."
;;   (font-lock-remove-keywords nil zenburn-colors-font-lock-keywords))

;;; Footer

;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))

(provide-theme 'yamonokai)

;;;###autoload
(add-to-list 'safe-local-eval-forms
             '(when (require 'rainbow-mode nil t) (rainbow-mode 1)))

;; Local Variables:
;; no-byte-compile: t
;; indent-tabs-mode: nil
;; eval: (when (require 'rainbow-mode nil t) (rainbow-mode 1))
;; End:
;;; zenburn-theme.el ends here
