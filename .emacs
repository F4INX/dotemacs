;; ----------------------------------------------------------------------
;; .emacs config file
;; -*- coding: utf-8 -*-

;; Quick use: wget "https://raw.githubusercontent.com/thadrien/dotemacs/master/.emacs" -O ~/.emacs

;; Copyright (C) 2008-2019 Hadrien Theveneau
;; Portions copyright (C) 2008 Dominique Quatravaux

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.
;; ----------------------------------------------------------------------

;; ----------------------------------------------------------------------
;; Changelog:
;; * 2019-04-24
;;   * [MINOR] Cleanup for git/github
;;   * [MINOR] Translation in English
;;   * [MINOR] Dates to iso format
;; * 2016-03-28
;;   * [MINOR] Replace tabs by spaces for indenting
;; * 2016-03-27
;;   * [MINOR] Add highlighting keyword TODO
;; * 2015-10-31
;;   * [MAJOR] Remove useless line break and add needed line break
;; * 2015-09-09
;;   * [MINOR] Add highliting FIXME in python-mode
;;   * [MINOR] Add keyword CRITICAL to highlighted words
;;   * [MAJOR] [PENDING] Add some Python settings
;; * 2010-12-15
;;   * Add modes text-mode and latex-mode to highlighting feature of keywords like
;;     FIXME and others
;; * 2010-05-03
;;   * Disable VHDL fill
;; * 2010-04-28
;;   * Disable electric mode VHDL
;;   * Add VHDL mode to highlighting keywords
;; * 2009-12-30
;;   * [MINOR] Add keywords WARNING and ATTENTION to highlighted words
;; * 2009-12-29
;;   * [MINOR] Correction path Ghostscript program
;;   * [MINOR] Add Aspell configuration
;; * 2009-12-05
;;   * [MAJOR] Add mode dosboat
;;   * [MAJOR] Add feature load-weak
;; * 2009-07-20
;;   * [MINOR] Remove mode slime
;; * 2009-06-26
;;   * Add highlighting to C++ mode
;; * 2009-06-22
;;   * [MINOR] Correction slight bug on OS checking
;; * 2009-03-06
;;   * [MINOR] Switching keyboard shortcuts to C-c, except M-g
;; * 2009-06-02
;;   * [MINOR] Rewriting of require-weak to my taste
;;   * [MINOR] Some cleaning up and changes of the shortcuts
;;   * [MINOR] Add mode recentf
;; * 2009-06-01
;;   * [MINEUR] Add highlighting of keywords like FIXME
;; * 2009-01-02
;;   * Change paths for haskell-mode
;; * 2008-12-22
;;   * Remove mixed mode for PHP, replaced by PHP abbrev mode
;; * 2008-12-12
;;   * Changes of keyboard shortcuts
;;   * Compilation of this file
;; * 2008-10-01
;;   * [MAJOR] Moving lisp files in site-lisp
;;   * [MAJOR] Byte-compiling of those files
;; * 2008-07-15
;;   * [MINOR] Correction of windows-nt checking
;;   * [MINOR] Add keyboard shortcut for search
;; * 2008-06-03
;;   * [MAJOR] Add Haskell mode
;;   * [MAJOR] Disable fonts
;;   * [MAJOR] Disable SLIME
;; * 2008-06-02
;;   * [MAJOR] Add printing
;;   * [MAJOR] Add keyboard shortcuts
;; * 2008-05-31
;;   * [MINOR] Colors for font-lock mode
;; * 2008-04-01
;;   * [MAJOR] Changes of coding for Windows
;; * 2008-04-03
;;   * [MINOR] Moving location in this files of Windows modes
;;   * [MINOR] Remove obsolete code standard-display-europeen
;; * 2008-04-24
;;   * [UPDATE] Update TUAREG extension for OCaml
; ---------------------------------------------------------------------


; ----------------------------------------------------------------------
; Fonction require-weak
; Permet à Emacs de continuer son exécution même si il ne trouve pas
; des fichiers lisp
; From Dominique Quatravaux .emacs

(defvar pointemacs-reserves
     (list)
"Ce .emacs est tolérant aux pannes, et essaye de continuer son exécution
en cas de paquetage requis absent. Dans ce cas, une explication (une
\"réserve\", comme dans \"réserves de recette\") sera ajoutée à cette liste."
)

(defun require-weak (symbol)
"Identique à `require', mais ne provoque pas une erreur si le
paquetage n'existe pas. Au contraire, insère `symbol' dans
`pointemacs-reserves'. Renvoie une valeur Booléenne
indiquant si le paquetage s'est bien chargé.
"
  (condition-case nil
      (progn
	(require symbol)
	t)
    (file-error
     (add-to-list 'pointemacs-reserves
		  (concat "Paquetage \"" (symbol-name symbol) "\" absent"))
     nil)))

(defun load-weak (symbol)
"Identique à `load', mais ne provoque pas une erreur si le
fichier n'existe pas. Au contraire, insère `symbol' dans
`pointemacs-reserves'. Renvoie une valeur Booléenne
indiquant si le fichier s'est bien chargé.
"
  (condition-case nil
      (progn
	(load symbol)
	t)
    (file-error
     (add-to-list 'pointemacs-reserves
		  (concat "Fichier \"" (symbol-name symbol) "\" absent"))
     nil)))
; ----------------------------------------------------------------------


; ----------------------------------------------------------------------
; Affichage
 
; Mettre un titre aux fenêtres
(setq frame-title-format '(buffer-file-name "Emacs: %b (%f)" "Emacs: %b"))

; Afficher les numéros de ligne et de colonne
(column-number-mode 1)
(line-number-mode 1)

; Afficher la date et l'heure dans la barre d'état (format français)
;(setq display-time-24hr-format t)
;(setq display-time-day-and-date t)
(setq display-time-format "%a %e %b %H:%M")
(display-time)

; Fonte d'affichage par défaut
; Pas réglée pour l'instant, je verrai plus tard
; ----------------------------------------------------------------------


; ----------------------------------------------------------------------
; Interface

; Cloche visible et non sonore
; cf. David Cobac - http://clx.anet.fr/spip/article.php3?id_article=181
(setq visible-bell t)

; Enlève les barres de menu
; cf. http://www.clubic.com/forum/os-alternatifs/configuration-de-emacs-id378342-page1.html
(menu-bar-mode -1)
(tool-bar-mode -1)

; Ouverture rapide des fichiers recemment ouverts
; cf. http://www.joegrossberg.com/archives/000182.html
(when (require-weak 'recentf)
  (recentf-mode 1)
  (setq recentf-max-menu-items 10))
; ----------------------------------------------------------------------

; ----------------------------------------------------------------------
; Raccourcis claviers

; Raccourcis clavier pratiques pour la compilation
; cf. http://tiny-tools.sourceforge.net/emacs-keys.html
; cf. http://www.haypocalc.com/wiki/Configuration_d'Emacs
(global-set-key (kbd "<f5>") 'compile)

; M-g pour aller à la x-ième ligne
(global-set-key (kbd "M-g") 'goto-line)

; Recherche
; cf. http://www.gnu.org/software/emacs/manual/html_node/emacs/Other-Repeating-Search.html
(global-set-key (kbd "C-c s") 'multi-occur-in-matching-buffers)

; Impression
(global-set-key (kbd "C-c p") 'ps-print-buffer-with-faces)

; Ouverture rapide des fichiers recemment ouverts
(global-set-key (kbd "C-c r") 'recentf-open-files)
; ----------------------------------------------------------------------



; ----------------------------------------------------------------------
; Edition

(setq c-basic-offset 2)              ; Mettre seulement 2 espaces pour l'indentation
(setq-default indent-tabs-mode nil)  ; Indentation avec des espaces et non des tabulations
(global-font-lock-mode t)            ; Activer la coloration syntaxique
(setq font-lock-maximum-size nil)    ; Mettre un maximum de couleurs

; Mode texte en auto-fill par défaut
; (créé une nouvelle ligne à chaque fois que vous taper du texte)
(add-hook 'text-mode-hook 'turn-on-auto-fill)

; Recherche automatique des fermetures et ouvertures des parenthèses
; Voir cette adresse pour quelquechose de plus fin :
; http://www.linux-france.org/article/appli/emacs/faq/emacs-faq-7.html
(load-library "paren")
(show-paren-mode 1)

; surlignage d'une région sélectionnée
(transient-mark-mode t)

; From Dominique Quatravaux .emacs
; Complétion automatique du texte (méta-/ et control-méta-/)
(eval-after-load "dabbrev"
   '(defalias 'dabbrev-expand 'hippie-expand)) ;; Trouve plus de complétions
(custom-set-variables '(dabbrev-upcase-means-case-search t))

; Surligner les mots clefs FIXME, WARNING, ATTENTION, CRITICAL et TODO
; cf. http://members.iinet.net.au/~bethandmark/elisp/highlight-fixmes-mode.el
; cf. http://www.gnu.org/software/emacs/manual/html_node/emacs/Font-Lock.html
; cf. http://www.emacswiki.org/emacs/AddKeywords

(let ((mots  '("FIXME" "WARNING" "ATTENTION" "CRITICAL" "TODO"))
      (modes '(c-mode c++-mode emacs-lisp-mode vhdl-mode text-mode latex-mode python-mode)))
  (dolist (mot mots)
    (dolist (mode modes)
      (let ((str (format "\\<\\(%s\\)" mot)))
	(font-lock-add-keywords
	      mode
	      (list (list str 1 font-lock-warning-face t)))))))
; ----------------------------------------------------------------------


; ----------------------------------------------------------------------
; Couleurs personalisées pour font-lock

; cf. http://www.pps.jussieu.fr/~balat/.emacs
; cf. http://www.delorie.com/gnu/docs/elisp-manual-21/elisp_633.html
;-----------------------------------------------------------------------


; ----------------------------------------------------------------------
; Accents
; Ils sont normalement supportés par votre distribution mais on ne sait
; jamais

(setq selection-coding-system 'compound-text-with-extensions)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment 'utf-8)

; Pour la console
; (if (not (eq window-system 'x))
;     (standard-display-european t))
; ----------------------------------------------------------------------


; ----------------------------------------------------------------------
; Divers

; Mettre tous les fichiers de sauvegarde dans un seul répertoire
(setq backup-directory-alist
     '(("." . "~/.emacs-backup-files/")))

; Faire des fichiers de backup
(setq make-backup-files t)

; Pour ne pas avoir à taper en entier la réponse yes/no
(fset 'yes-or-no-p 'y-or-n-p)

; Affichage des images et fichiers compressés
(setq auto-image-file-mode t)
(setq auto-compression-mode t)

; Molette de la souris
(defun up-slightly () (interactive) (scroll-up 5))
(defun down-slightly () (interactive) (scroll-down 5))
(global-set-key [mouse-4] 'down-slightly)
(global-set-key [mouse-5] 'up-slightly)
(defun up-one () (interactive) (scroll-up 1))
(defun down-one () (interactive) (scroll-down 1))
(global-set-key [S-mouse-4] 'down-one)
(global-set-key [S-mouse-5] 'up-one)
(defun up-a-lot () (interactive) (scroll-up))
(defun down-a-lot () (interactive) (scroll-down))
(global-set-key [C-mouse-4] 'down-a-lot)
(global-set-key [C-mouse-5] 'up-a-lot)
; ----------------------------------------------------------------------


;; ; ----------------------------------------------------------------------
;; ; Programmation PHP - HTML - CSS
;; ;
;; ; Chemin vers le fichier de mon repertoire personnel ou j'ai mis les
;; ; fichiers elisp concernés

;; ; Invocation du mode pour l'édition PHP
;; (require-weak 'php-mode)

;; ; Invocation du mode pour le CSS
;; (autoload 'css-mode "css-mode")
;; (add-to-list 'auto-mode-alist '("\\.css\\'" . css-mode))
;; (setq cssm-indent-function #'cssm-c-style-indenter)
;; (setq cssm-indent-level '2)

;; ; Coloration syntaxique en PHP
;; (add-hook 'php-mode-user-hook 'turn-on-font-lock)

;; ; Multi-mode: pour les mélanges PHP - HTML
;; (when (require-weak 'mmm-mode)
;;   (setq mmm-global-mode 'maybe)

;; ; Création d'un groupe mmm pour l'édition de code HTML aisée
;;   (mmm-add-group
;;    'fancy-html
;;    '(
;;      (html-php-tagged
;;       :submode php-mode
;;       :face mmm-code-submode-face
;;       :front "<[?]php"
;;       :back "[?]>")
;;      (html-php-echo
;;       :submode php-mode
;;       :face mmm-code-submode-face
;;       :front "<[?]"
;;       :back "[?]>")
;;      (html-css-attribute
;;       :submode css-mode
;;       :face mmm-declaration-submode-face
;;       :front "style=\""
;;       :back "\"")))

;; ; What files to invoke the new html-mode for?
;;   (add-to-list 'auto-mode-alist '("\\.inc\\'" . xml-mode))
;;   (add-to-list 'auto-mode-alist '("\\.phtml\\'" . xml-mode))
;;   (add-to-list 'auto-mode-alist '("\\.php[34]?\\'" . xml-mode))
;;   (add-to-list 'auto-mode-alist '("\\.[sj]?html?\\'" . xml-mode))
;;   (add-to-list 'auto-mode-alist '("\\.jsp\\'" . xml-mode))

;; ; What features should be turned on in this html-mode?
;;   (add-to-list 'mmm-mode-ext-classes-alist '(xml-mode nil html-js))
;;   (add-to-list 'mmm-mode-ext-classes-alist '(xml-mode nil embedded-css))
;;   (add-to-list 'mmm-mode-ext-classes-alist '(xml-mode nil fancy-html))
;; ) ; End of (when (require-weak 'mmm-mode)

;; ; Not exactly related to editing HTML: enable editing help with mouse-3 in all sgml files
;; (defun go-bind-markup-menu-to-mouse3 ()
;;   (define-key sgml-mode-map [(down-mouse-3)] 'sgml-tags-menu))

;; (add-hook 'sgml-mode-hook 'go-bind-markup-menu-to-mouse3)
;; ; ----------------------------------------------------------------------


; ----------------------------------------------------------------------
; Mode PHP pour l'edition de fichiers en PHP

(autoload 'php-mode "php-mode" "PHP editing mode." t)
(add-to-list 'auto-mode-alist '("\\.php[34]?\\'" . php-mode))
; ----------------------------------------------------------------------


; ----------------------------------------------------------------------
; LUA editing mode

(setq auto-mode-alist (cons '("\\.lua$" . lua-mode) auto-mode-alist))
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)
; ----------------------------------------------------------------------


; ----------------------------------------------------------------------
; PROLOG editing mode

(autoload 'run-prolog "prolog" "Start a Prolog sub-process." t)
(autoload 'prolog-mode "prolog" "Major mode for editing Prolog programs." t)
(autoload 'mercury-mode "prolog" "Major mode for editing Mercury programs." t)
(setq prolog-system 'swi)
(setq auto-mode-alist (append '(("\\.pl$"  . prolog-mode)
				("\\.pro$" . prolog-mode)
				("\\.m$"   . mercury-mode))
			      auto-mode-alist))
; ----------------------------------------------------------------------


; ----------------------------------------------------------------------
; imaxima (image support for Maxima) et imath (interactive math)
;
(autoload 'imaxima "imaxima" "Image support for Maxima." t)
(autoload 'imath-mode "imath" "Interactive Math minor mode." t)
; ----------------------------------------------------------------------


; ----------------------------------------------------------------------
; tuareg mode for OCaml

; (setq load-path (cons "~/../Mes Documents/Languages/Ocaml/tuareg-mode-1.45.6" load-path))

(setq auto-mode-alist (cons '("\\.ml\\w?" . tuareg-mode) auto-mode-alist))
(autoload 'tuareg-mode "tuareg" "Mode majeur pour éditer du code Caml" t)
(autoload 'camldebug "camldebug" "Exécuter le débogueur Caml" t)
; ----------------------------------------------------------------------


; ----------------------------------------------------------------------
; HASKELL MODE

(load-weak "haskell-site-file.el")

(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
;;(add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)
; ----------------------------------------------------------------------


; ----------------------------------------------------------------------
; DOSBAT MODE

(autoload 'dos-mode "dos" "Edit Dos scripts." t)
(add-to-list 'auto-mode-alist '("\\.bat$" . dos-mode))
; ----------------------------------------------------------------------


; ----------------------------------------------------------------------
; Réglages mode VHDL

(setq vhdl-electric-mode nil)
(setq vhdl-end-comment-column 999)
; ----------------------------------------------------------------------


; ----------------------------------------------------------------------
; Réglages du mode terminal
(when (require-weak 'term)
  (term-set-escape-char ?\C-C)
  )
; ----------------------------------------------------------------------


; ----------------------------------------------------------------------
; Programmes UNIX sous windows

(when (equal system-type 'windows-nt)
  ; Impression
  (setenv "GS_LIB" "C:/Program Files/gs/gs8.61/lib;C:/Program Files/gs/fonts")
  (setq ps-lpr-command "C:/Program Files/gs/gs8.61/bin/gswin32c.exe")
  (setq ps-lpr-switches '("-q" "-dNOPAUSE" "-dBATCH" "-sDEVICE=mswinpr2"))
  (setq ps-printer-name t)

  ; Aspell
  (setq ispell-program-name "C:/Program Files/Aspell/bin/aspell.exe"))
; ----------------------------------------------------------------------


; ----------------------------------------------------------------------
; Indique les problèmes éventuels de démarrage
; Voir début
(when pointemacs-reserves
  (setq inhibit-startup-message t)
  (insert "Chargement du .emacs reussi avec reserves, taper C-h v pointemacs-reserves")
  )
; ----------------------------------------------------------------------


; ----------------------------------------------------------------------
; Python settings
; (require 'ipython)
; (setq
;   python-shell-interpreter "ipython"
;   python-shell-interpreter-args ""
;   python-shell-prompt-regexp "In \\[[0-9]+\\]: "
;   python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
;   python-shell-completion-setup-code
;     "from IPython.core.completerlib import module_completion"
;   python-shell-completion-module-string-code
;     "';'.join(module_completion('''%s'''))\n"
;   python-shell-completion-string-code
;     "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")
;  (setq
;    python-shell-interpreter "C:\\Python27\\python.exe"
;    python-shell-interpreter-args
;    "-i C:\\Python27\\Scripts\\ipython-script.py")
; ----------------------------------------------------------------------


; ----------------------------------------------------------------------
; Fin de ce fichier .emacs :-)
; ----------------------------------------------------------------------

