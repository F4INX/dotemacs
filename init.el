; ----------------------------------------------------------------------
; .emacs config file
; -*- coding: utf-8 -*-
;
; Quick use command: TBD
;
; Copyright (c) 2008-2025 Hadrien Theveneau.
; All rights reserved.
;
; Redistribution and use in source and binary forms, with or without
; modification, are permitted provided that the following conditions are
; met:
;
;     Redistributions of source code must retain the above copyright
;     notice, this list of conditions and the following disclaimer.
;
;     Redistributions in binary form must reproduce the above copyright
;     notice, this list of conditions and the following disclaimer in
;     the documentation and/or other materials provided with the
;     distribution.
;
;     Neither the name of the copyright holder nor the names of its
;     contributors may be used to endorse or promote products derived
;     from this software without specific prior written permission.
;
; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
; “AS IS” AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
; FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
; COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
; INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
; BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
; CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
; LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
; ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
; POSSIBILITY OF SUCH DAMAGE.
; ----------------------------------------------------------------------

; ----------------------------------------------------------------------
; Useful links
; https://realpython.com/emacs-the-best-python-editor/
; https://ianyepan.github.io/posts/emacs-ide/
; https://medspx.fr/blog/Debian/emacs_2020/
; https://emacs.stackexchange.com/questions/582/how-to-change-size-of-split-screen-emacs-windows
; https://doc.ubuntu-fr.org/emacs
; https://gist.github.com/Ladicle/119c57fc97439c1b103f7847aa03be52
; https://www.reddit.com/r/emacs/comments/scjjs4/how_tf_do_you_enable_line_numbering/
; https://wikemacs.org/wiki/Smex
; http://xahlee.info/emacs/emacs/emacs_hippie_expand_setup.html
; ----------------------------------------------------------------------

; ----------------------------------------------------------------------
; Melpa packages
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/")
	     '("nongnu" . "https://elpa.nongnu.org/nongnu/"))
(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))
; ----------------------------------------------------------------------

;; ----------------------------------------------------------------------
;; Recent files
(recentf-mode 1)
(setq recentf-max-menu-items 10)
(global-set-key (kbd "C-c r") 'recentf-open-files)
;; ----------------------------------------------------------------------

; ----------------------------------------------------------------------
; Which key
(unless (package-installed-p 'which-key)
  (package-install 'which-key))
(use-package which-key
  :diminish which-key-mode
  :init
  (setq which-key-sort-uppercase-first nil
		max-mini-window-height 15)
  ; Use of dedicated window instead of minibuffer
  (which-key-setup-side-window-bottom)
  ; Activated everywhere, everywhen
  (which-key-mode t))
; ----------------------------------------------------------------------

;; ----------------------------------------------------------------------
;; Magit
(use-package magit
  :ensure t)
;; ----------------------------------------------------------------------

; ----------------------------------------------------------------------
; Treemacs
(use-package treemacs
  :ensure t
  :defer t
  :after (treemacs-all-the-icons)
  :hook (treemacs-mode . no_code_mode)
  :bind (("M-²" . treemacs-select-window)
		 ("M-œ" . treemacs-select-window))
  :config
  (setq treemacs-width 20
		treemacs-indentation '(4 px)
		treemacs-is-never-other-window t
		treemacs-width-is-initially-locked nil
		treemacs-space-between-root-nodes nil
		treemacs-collapse-dirs 4
		treemacs-text-scale -1)
	;;(treemacs-indent-guide-mode)
	(treemacs-resize-icons 14)
	(treemacs-follow-mode t)
	(treemacs-tag-follow-mode t)
	(treemacs-filewatch-mode t)
	(treemacs-fringe-indicator-mode 'always)
	(treemacs-hide-gitignored-files-mode nil)
	(treemacs-load-theme "all-the-icons")
  )
; ----------------------------------------------------------------------

; ----------------------------------------------------------------------
; Company
(use-package company
  :ensure t
  :diminish company-mode
  :config
  (global-company-mode)
  )
; ----------------------------------------------------------------------

;; ----------------------------------------------------------------------
;; Interface and display
;;
;; Show line and column number
(column-number-mode 1)
(line-number-mode 1)
;;
;; Show line numbers globally
(global-display-line-numbers-mode)
;;
;; Show date and time in status bar (french format)
;; (setq display-time-24hr-format t)
;; (setq display-time-day-and-date t)
(setq display-time-format "%a %e %b %H:%M")
(display-time)
;; Visible instead of audible bell
;; cf. David Cobac - http://clx.anet.fr/spip/article.php3?id_article=181
(setq visible-bell t)
;;
;; Short yes or no questions
(setopt use-short-answers t)
;;
;; Improved automatic completion of text (M-/ and C-M-/)
(with-eval-after-load "dabbrev"
  (global-set-key [remap dabbrev-expand] 'hippie-expand)
  (global-set-key (kbd "C-c /") 'dabbrev-expand))
;;
;; Overline keywords like FIXME, WARNING, ATTENTION, CRITICAL, and TODO
;; cf. http://members.iinet.net.au/~bethandmark/elisp/highlight-fixmes-mode.el
;; cf. http://www.gnu.org/software/emacs/manual/html_node/emacs/Font-Lock.html
;; cf. http://www.emacswiki.org/emacs/AddKeywords
(let ((words '("FIXME" "WARNING" "ATTENTION" "CRITICAL" "TODO"))
      (modes '(c-mode c++-mode emacs-lisp-mode vhdl-mode text-mode
	       latex-mode python-mode julia-mode)))
  (dolist (word words)
    (dolist (mode modes)
      (let ((str (format "\\<\\(%s\\)" word)))
	(font-lock-add-keywords
	      mode
	      (list (list str 1 font-lock-warning-face t)))))))
;; ----------------------------------------------------------------------

;; ----------------------------------------------------------------------
;; Misc
;;					;
;; Put all backup files in a single folder
(setq backup-directory-alist
     '(("." . "~/.emacs.d/backup-files/")))
;; Do backup files
(setq make-backup-files t)
;; ----------------------------------------------------------------------

; ----------------------------------------------------------------------
; Projectile
(unless (package-installed-p 'projectile)
  (package-install 'projectile))
(use-package projectile
  :init
  (projectile-mode +1)
  ; Recommended keymap prefix on macOS
  ; (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  ; Recommended keymap prefix on Windows/Linux
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
)
; ----------------------------------------------------------------------

;; ----------------------------------------------------------------------
;; Julia mode
(use-package julia-mode
  :ensure t)
(use-package eat
  :ensure t)
(use-package julia-repl
  :ensure t
  :config
  (julia-repl-set-terminal-backend 'eat))
;; ----------------------------------------------------------------------

;; ----------------------------------------------------------------------
;; Terminal settings
(use-package term
  :bind (:map term-raw-map ("C-c y" . term-paste)))
;; ----------------------------------------------------------------------

;; ----------------------------------------------------------------------
;; Printing
;; Raw printing functions
(setq ps-lpr-command "C:/Program Files/gs/gs10.05.1/bin/gswin64c.exe")
(setq ps-lpr-switches '("-q" "-dNOPAUSE" "-dBATCH" "-sDEVICE=mswinpr2"))
(setq ps-printer-name t)
;; printing.el
(use-package printing
  :config
  (when (equal system-type 'windows-nt)
    (setq pr-gv-command "C:/Program Files/SumatraPDF/SumatraPDF.exe")
    (setq pr-gs-command "C:/Program Files/gs/gs10.05.1/bin/gswin64c.exe")
    (setq pr-gs-switches '("-q" "-dNOPAUSE" "-dBATCH"))
    (setq pr-faces-p t)
    (setq pr-print-using-ghostscript t))
  (pr-update-menus t))
;; ----------------------------------------------------------------------

;; ----------------------------------------------------------------------
;; AI assistant
;;
(use-package gptel
  :ensure t
  :config
  (setq gptel-model 'claude-3.5-sonnet  ; Max supported in free plan
      gptel-backend (gptel-make-gh-copilot "Copilot")))
;; ----------------------------------------------------------------------

; ----------------------------------------------------------------------
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(all-the-icons-dired all-the-icons-ivy company eat gptel julia-mode
			 julia-repl magit projectile smex treemacs)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
; ----------------------------------------------------------------------
