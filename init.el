;; Goals and rules:
;; + Support Terminal, Mac OS, and Linux.
;; + Minimize number of shortcuts by merging similar functions, e.g., use helm-mini instead of helm-buffers-list, helm-locate, etc

;; Coding style:
;; + Names of user-defined function are prefixed by "my-"

;; (setq debug-on-error t)  ; uncomment this line for debug

;;; Speedup

;; reduced startup time from 2.7s to 2.0s https://www.reddit.com/r/emacs/comments/3kqt6e
(setq gc-cons-threshold-original gc-cons-threshold)
(setq gc-cons-threshold (* 1024 1024 100))

;; <2017-11-20 Mon> reduced Linux startup time from 3.5s to 3s. https://emacs.stackexchange.com/questions/34342 https://www.reddit.com/r/emacs/comments/3kqt6e
(setq file-name-handler-alist-original file-name-handler-alist)
(setq file-name-handler-alist nil)

(tool-bar-mode -1)
(scroll-bar-mode -1)
(tooltip-mode -1)
(setq inhibit-startup-screen t)  ; no splash screen, related http://fuhm.livejournal.com/5894.html
(setq initial-scratch-message nil)

(run-with-idle-timer
 1 nil
 (lambda ()
   (require 'recentf)
   (recentf-mode 1)  ; to run "Cleaning up the recentf list" during idle time
   (setq gc-cons-threshold gc-cons-threshold-original)  ; Reset `gc-cons-threshold' to its default value. http://emacs.stackexchange.com/a/16595/12987
   (setq file-name-handler-alist file-name-handler-alist-original)
   (makunbound 'file-name-handler-alist-original)
   (message "init-time = %s, last session = %s" (emacs-init-time) (shell-command-to-string "tail -n1 ~/Google\\ Drive/_doc/z.emacs_sessions.txt"))
   ))

(setq my-startup-filename "~/Google Drive/_doc/_work.txt")
(add-hook
 'emacs-startup-hook
 '(lambda ()
    ;; open my-startup-file only when no file specified on cmdline http://emacs.stackexchange.com/questions/20644/how-to-get-buffer-for-file-specified-on-command-line
    (unless (cadr command-line-args)
      (find-file my-startup-filename)
      )
    (if (eq system-type 'darwin)
        (use-package openwith
          :config
          ;; https://emacs.stackexchange.com/questions/3105/how-to-use-an-external-program-as-the-default-way-to-open-pdfs-from-emacs
          ;; (openwith-mode 1)
          (setq openwith-associations
                '(("\\.pdf\\'" "/Applications/Preview.app/Contents/MacOS/Preview" (file))
                  ("\\.mp3\\'" "/Applications/VLC.app/Contents/MacOS/VLC" (file))))
          (openwith-mode t)
          ))
    (setq-default default-input-method "chinese-cns-tsangchi")  ; run it after startup, coz Emacs will automatically set default-input-method to rfc1345 if locale is UTF-8 https://github.com/purcell/emacs.d/issues/320\
    ))
(add-hook 'focus-out-hook #'garbage-collect)  ; Collect garbage when Emacs is focused out to make it faster https://github.com/elnawe/.emacs.d/blob/master/configuration.org
(add-hook 'kill-emacs-hook (lambda () (append-to-file (concat "\n" system-name " " (format-time-string "%a, %Y/%m/%d, %l:%M %p")) nil "~/Google Drive/_doc/z.emacs_sessions.txt")))

;; temporarily increase memory threshold so that GC runs less often, which speeds up some operations https://github.com/hrs/sensible-defaults.el/blob/master/sensible-defaults.el
;; http://bling.github.io/blog/2016/01/18/why-are-you-changing-gc-cons-threshold/
(defun my-minibuffer-setup-hook ()
  (setq gc-cons-threshold 100000000)  ; The default is 800kb - Spacemacs uses 100mb http://emacs.stackexchange.com/questions/19705
  (yas-minor-mode 1)  ; make yasnippet also available in minibuffer / helm
  ;; select input string (if any). helm doesn't support it by design https://github.com/emacs-helm/helm/issues/491
  (setq this-command-keys-shift-translated t)
  (handle-shift-selection)
  (beginning-of-line)
  )
(defun my-minibuffer-exit-hook () (setq gc-cons-threshold gc-cons-threshold-original))
(add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-hook)
(add-hook 'minibuffer-exit-hook #'my-minibuffer-exit-hook)

;;; Display, utf-8
(setq initial-major-mode 'org-mode)  ; i seldom use the default lisp-interaction-mode (which rebinds C-j)
(add-to-list 'initial-frame-alist '(fullscreen . maximized))  ; better than manually setting width and height http://emacs.stackexchange.com/a/3008
(setq-default frame-title-format "%b (%f)") ; show file path in frame title http://stackoverflow.com/a/12009623 note: don't rely on it, as it is N/A in terminal
;; (menu-bar-mode -1)  ; TODO try out menu
(setq-default cursor-type '(bar . 3)) (blink-cursor-mode t)
(setq blink-cursor-blinks 0)  ; blinks forever
(delete-selection-mode t)  ; delete seleted text when typing
(setq redisplay-highlight-region-function  ; https://www.reddit.com/r/emacs/comments/345by9/having_the_background_face_for_selection_region/
      (lambda (start end window rol)
        (if (not (overlayp rol))
            (let ((nrol (make-overlay start end)))
              (funcall redisplay-unhighlight-region-function rol)
              (overlay-put nrol 'window window)
              (overlay-put nrol 'face 'region)
              ;; Normal priority so that a large region doesn't hide all the
              ;; overlays within it, but high secondary priority so that if it
              ;; ends/starts in the middle of a small overlay, that small overlay
              ;; won't hide the region's boundaries.
              (overlay-put nrol 'priority '(10000 . 10000))
              nrol)
          (unless (and (eq (overlay-buffer rol) (current-buffer))
                       (eq (overlay-start rol) start)
                       (eq (overlay-end rol) end))
            (move-overlay rol start end (current-buffer)))
          rol)))
(setq save-interprogram-paste-before-kill t)  ; If you have something on the system clipboard, and then kill something in Emacs, then by default whatever you had on the system clipboard is gone and there is no way to get it back. Setting the following option makes it so that when you kill something in Emacs, whatever was previously on the system clipboard is pushed into the kill ring.  https://github.com/raxod502/radian/blob/master/init.el
(setq-default show-trailing-whitespace t)  ; http://trey-jackson.blogspot.com/2008/03/emacs-tip-12-show-trailing-whitespace.html
(setq-default indent-tabs-mode nil)        ; use space instead of tab https://www.emacswiki.org/emacs/NoTabs as we have auto-indent in every mode anyway
(setq-default tab-width 2)
(setq sh-basic-offset 2 sh-indentation 2)  ; indent with 2 spaces for .sh files
(setq-default indicate-empty-lines t)
(setq sentence-end-double-space nil)  ; don't assume that sentences should have two spaces after periods
(defalias 'yes-or-no-p 'y-or-n-p)     ; answering just 'y' or 'n' will do

(when (window-system)
  ;; https://emacs-china.org/t/topic/440/27 http://baohaojun.github.io/perfect-emacs-chinese-font.html
  ;; 如果配置好了，下面20个汉字与40个英文字母应该等长. here are 20 hanzi and 40 english chars, see if they are the same width:
  ;; aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa|
  ;; 你你你你你你你你你你你你你你你你你你你你|
  ;; ,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,|
  ;; 。。。。。。。。。。。。。。。。。。。。|
  ;; 1111111111111111111111111111111111111111|
  ;; 東東東東東東東東東東東東東東東東東東東東|
  ;; ここここここここここここここここここここ|
  ;; ｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺｺ|
  ;; 까까까까까까까까까까까까까까까까까까까까|
  ;; ✔✔✔✔✔✔✔✔✔✔✔✔✔✔✔✔✔✔✔✔✔✔✔✔✔✔✔✔✔✔✔✔✔✔✔✔✔✔✔✔|
  (setq fonts
        ;; OSX en: Monaco and Monospace have larger size for '✔' (not good), DejaVu Sans Mono don't have size 10
        ;; OSX cn: 用默认黑体 STHeiti http://cnborn.net/blog/2014/10/emacs-chinese-font-on-osx/ http://stackoverflow.com/a/19547480/550243 otherwise, cannot properly display bold chinese char on mac
        (cond ((eq system-type 'darwin)     '("Menlo"     "STHeiti"))
              ((eq system-type 'gnu/linux)  '("Monospace" "WenQuanYi Zen Hei Mono")) ;"WenQuanYi Zen Hei"))  ; "Menlo" is N/A
              ((eq system-type 'windows-nt) '("Consolas"  "Microsoft Yahei"))))
  (set-face-attribute 'default nil :font (format "%s:pixelsize=%d" (car fonts) 10))  ; setting English Font
  (dolist (charset '(kana han symbol cjk-misc bopomofo))  ; Chinese Font: 根据字符编码去找合适的字体 http://zhuoqiang.me/torture-emacs.html
    (set-fontset-font (frame-parameter nil 'font) charset
                      (font-spec :family (car (cdr fonts)))))
  ;; Fix chinese font width and rescale for org-table
  (setq face-font-rescale-alist '(("STHeiti". 1.2) ("WenQuanYi Zen Hei Mono" . 1.2) ("Microsoft Yahei" . 1.2)))
  )

;; UTF-8 as default encoding
;; Do not add a bunch of other variables such as set-terminal-coding-system, unless you know emacs's encoding system very well. http://ergoemacs.org/emacs/emacs_make_modern.html
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)

;;; use-package
(require 'package)  ; https://www.emacswiki.org/emacs/ELPA https://github.com/durantschoon/.emacs.d/tree/boilerplate-sane-defaults_v1.0
;; (add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)  ; safer to get stable version first https://github.com/magnars/.emacs.d/pull/7
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)  ; stable Melpa doesn't have helm-cmd-t -> need non-stable Melpa repository, url copied from http://melpa.org/#/getting-started
(add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
;; http://elpa.gnu.org/packages/ is on the list by default after version 24

(add-to-list 'load-path "~/Google Drive/_doc/bin/")
(setq load-prefer-newer t)  ; load newer elisp
(package-initialize)
(unless (package-installed-p 'use-package)  ; install use-package if it isn't installed yet.
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile (require 'use-package))  ; load use-package
(setq use-package-always-ensure t)  ; install all packages; for non-package, override it to nil
;; (setq use-package-always-defer t) cause too many dependence bugs

;; https://github.com/jwiegley/use-package/blob/master/bind-key.el TODO clean up this file by better bind-key syntax
(require 'bind-key)
(require 'cl)  ; for using cl-letf, and remove-if http://ergoemacs.org/emacs/elisp_filter_list.html

;;; backup / auto-save / saveplace / lock files
(defvar user-temporary-file-directory "~/.emacs.d/backups/")  ; ref: http://www.martinaulbach.net/linux/software/21-my-emacs-configuration-file
(setq backup-directory-alist `(("." . ,user-temporary-file-directory)))  ; ref: http://pages.sachachua.com/.emacs.d/Sacha.html
(setq tramp-backup-directory-alist backup-directory-alist)
(setq backup-by-copying t)         ; prevents links from being made to point at the backup file rather than the original.  It's the safest but slowest bet. http://stackoverflow.com/questions/151945  and to stop emacs's backup changing the file's creation date of the original file http://ergoemacs.org/emacs/emacs_make_modern.html
(setq delete-old-versions 'never)  ; disk space is cheap. save lots.
(setq version-control t)           ; keep multiple numbered backup files, rather than a single unnumbered backup file.
(setq vc-make-backup-files t)      ; vc = version-control, can use ediff-backup to diff
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)))
(setq delete-by-moving-to-trash t)  ; don’t really delete the file
(use-package osx-trash
  :if (eq system-type 'darwin) :config (osx-trash-setup)) ; Provide OS X system trash support for GNU Emacs 24, makes delete-by-moving-to-trash do what you expect it to do.
(setq custom-file (make-temp-file ""))  ; disabled the custom file; don't call "(load custom-file)" coz i don't use custom file https://www.reddit.com/r/emacs/comments/4q4ixw
(setq auto-revert-interval 1)  ; Turn the delay on auto-reloading from 5 seconds down to 1 second.  setq before loading autorevert was faster https://emacs.stackexchange.com/questions/102/#comment48662_106
(global-auto-revert-mode 1)    ; http://stackoverflow.com/questions/1480572 but not prompt for revert if changed???
(setq create-lockfiles nil)  ; disable lockfile which is not useful (as i only open one emacs at a time), and cost overhead (to Google Drive) https://www.gnu.org/software/emacs/manual/html_node/emacs/Interlocking.html#Interlocking
(use-package saveplace
  :config
  (setq-default save-place t)
  (setq save-place-file "~/.emacs.d/saveplace")
  (when (>= emacs-major-version 25) (save-place-mode t))  ; http://emacs.stackexchange.com/questions/14670
  )
(setq history-delete-duplicates t)

(defun my-find-file-check-large-file-hook ()  ; https://stackoverflow.com/questions/18316665/how-to-improve-emacs-performance-when-view-large-file
  "If a file is over a given size, make the buffer read only."
  (when (> (buffer-size) (* 10 1024 1024))
    ;; (setq buffer-read-only t)
    ;; (buffer-disable-undo)
    (fundamental-mode)
    (setq bidi-display-reordering nil)  ; to navigate long lines faster https://emacs.stackexchange.com/questions/598/how-do-i-prevent-extremely-long-lines-making-emacs-slow
    ;; (highlight-thing-mode -1)  ; cannot be "nil"?
    (setq truncate-lines t)  ; i.e., turn off word-wrap, as large file may have super-long lines
    ))
(add-hook 'find-file-hook 'my-find-file-check-large-file-hook)

;; Remember password when connected to remote sites via Tramp http://stackoverflow.com/questions/840279/passwords-in-emacs-tramp-mode-editing
;; Emacs "tramp" service (ssh connection) constantly asks for the log in password without this
;; (setq password-cache-expiry nil)  ; need?

;; https://www.reddit.com/r/emacs/comments/3srwz6/idelike_go_back/
(use-package back-button
  :config
  (back-button-mode 1)
  (advice-add 'back-button-local-backward :before (lambda () (deactivate-mark)))
  (advice-add 'back-button-local-forward  :before (lambda () (deactivate-mark)))
  :bind
  ("C-h" . back-button-local-backward)
  ("C-l" . back-button-local-forward)
  )

(use-package auto-mark
  :load-path "~/bin/"
  :ensure nil  ; https://emacs.stackexchange.com/questions/36122
  :config
  (setq auto-mark-ignore-move-on-sameline nil)  ; coz line-number-at-pos is costly https://emacs.stackexchange.com/questions/3821
  (setq auto-mark-command-class-alist
        '((back-button-local-backward . ignore)
          (back-button-local-forward . ignore)))
  ;; redefine auto-mark-handle-command-class to:
  ;; 1. push to mark-ring directly instead of push-mark which may not update mark-ring, also good for functions that use (let ((deactivate-mark nil)) ...)
  ;; 2. only push when the position is different than the previous one, to avoid duplicate marks
  ;; 3. check auto-mark-previous-point to allow nil value, to avoid extra point-min mark (i.e., can comment out "auto-mark-previous-point (point-min)" in auto-mark.el)
  (defun auto-mark-handle-command-class (class)
    (if (and class
             (not (or (eq class 'ignore)
                      (eq class auto-mark-command-class))))
        (progn
          (when (or (null mark-ring) (not (= auto-mark-previous-point (marker-position (car mark-ring)))))
            (setq mark-ring (cons (copy-marker auto-mark-previous-point) mark-ring)))
          (setq auto-mark-command-class class))))
  (setq global-mark-ring-max 256  ; original values = 16, too small for edit marks
        mark-ring-max 256)
  (global-auto-mark-mode 1)
  )

;; https://github.com/lewang/backup-walker http://emacs.stackexchange.com/questions/29142/recover-backup-files
;; The typical workflow is:
;;   1) I'm in a buffer and realize I need to check some backups.
;;        M-x backup-walker-start
;;   2) I press <p> to go backwards in history until I see something
;;      interesting.  Then I press <enter> to bring it up.  OOPs this isn't
;;      it, I go back to the backup-walker window and find the right file.
;;   3) I get what I need from the backup, go back to backup-walker, and press
;;      <q> and kill all open backups.
;;   4) the end.
(require 'backup-walker nil 'noerror)  ; 'noerror to avoid "file error" https://stackoverflow.com/questions/2816019/

;; (use-package benchmark-init)  ; https://www.emacswiki.org/emacs/BenchmarkInit
(use-package esup :defer t)  ; for benchmarking https://github.com/jschaf/esup

(use-package dired
  :ensure nil  ; for non-package
  :config
  ;; https://gist.github.com/tototoshi/648425
  (defun dired-open-file ()
    "In dired, open the file named on this line."
    (interactive)
    (let* ((file (dired-get-filename)))
      (message "Opening %s..." file)
      (call-process "gnome-open" nil 0 nil file)
      (message "Opening %s done" file)))
  (setq dired-auto-revert-buffer t    ; Revert on re-visiting
        ;; Better dired flags: `-l' is mandatory, `-a' shows all files, `-h'
        ;; uses human-readable sizes, and `-F' appends file-type classifiers
        ;; to file names (for better highlighting)
        dired-listing-switches "-alhF"
        dired-ls-F-marks-symlinks t   ; -F marks links with @
        ;; Inhibit prompts for simple recursive operations
        dired-recursive-copies 'always
        ;; Auto-copy to other Dired split window
        dired-dwim-target t)
  (put 'dired-find-alternate-file 'disabled nil)
  (add-hook 'dired-mode-hook 'auto-revert-mode)  ; auto refresh dired when file changes

  :bind
  (:map dired-mode-map
        ("e" . wdired-change-to-wdired-mode)
        ("h" . dired-up-directory)
        ("l" . dired-find-alternate-file)  ; visit w/o a new buffer, using emacs
        ("j" . dired-next-line)
        ("k" . dired-previous-line)
        ("o" . dired-open-file)  ; using system app
        ))


;;; mouse
;; double-click on the quotation mark -> select all text in double quotes
;; double-click on the '_' -> select the whole variable
;; scroll down and keep cursor is not possible?
(setq mouse-wheel-scroll-amount '(5 ((shift) . 5) ((control) . nil)))  ; http://stackoverflow.com/a/445881 https://www.emacswiki.org/emacs/SmoothScrolling
(setq mouse-wheel-progressive-speed nil)  ; don't accelerate scrolling http://stackoverflow.com/a/445881 https://www.emacswiki.org/emacs/SmoothScrolling
(setq mouse-drag-copy-region nil)  ; highlight does not alter kill ring
(setq mouse-yank-at-point t)  ; when middle-clicking the mouse to yank from the clipboard, insert the text where point is, not where the mouse cursor is

(defadvice mouse-set-point (around testing activate)
  (with-syntax-table (copy-syntax-table (syntax-table))
    ;; modify syntax temporarily; global change -> syntax-subword doesn't work.
    (modify-syntax-entry ?_ "w")
    ad-do-it))
(defun my-mouse-start-rectangle (start-event)  ; http://emacs.stackexchange.com/a/7261
  (interactive "e")
  (deactivate-mark)
  (mouse-set-point start-event)
  (rectangle-mark-mode +1)
  (let ((drag-event))
    (track-mouse
      (while (progn
               (setq drag-event (read-event))
               (mouse-movement-p drag-event))
        (mouse-set-point drag-event)))))
(bind-key "M-<down-mouse-1>" #'my-mouse-start-rectangle)  ; = http://codemirror.net
(bind-key "C-S-<down-mouse-1>" #'my-mouse-start-rectangle)  ; S/C/M-mouse-1 are N/A in Ubuntu
;; rectangle-mark-mode (C-x SPC) is better than cua mode, as i cannot do C-c with cua-set-rectangle-mark

;; from https://writequit.org/eos/eos-appearance.html#orgheadline12
(setq scroll-conservatively 101  ; scroll only one line when move past the bottom of the screen. faq 5.45 http://www.gnu.org/s/emacs/emacs-faq.html#Modifying-pull_002ddown-menus
      scroll-preserve-screen-position 'always  ; so that i don't need to find the cursor after page up/down, not 'always which will keep the cursor position even for minor wheel scroll
      scroll-error-top-bottom t  ; moves point to the farthest possible position, same as modern editor https://www.gnu.org/software/emacs/manual/html_node/emacs/Scrolling.html
      auto-window-vscroll nil
      scroll-margin 3   ; vertical margin
      hscroll-margin 5  ; horizontal margin
      hscroll-step 1)


;;; basic keys

;; Use M-q as my main user map prefix key. It's easy to hit (as a Caps-as-Control disciple) http://emacs-fu.blogspot.com/2009/07/keyboard-shortcuts.html
;; Also good for left-hand-only-mode
(global-unset-key (kbd "M-q"))  ; change it to a prefix key http://stackoverflow.com/questions/13965966/unset-key-binding-in-emacs
(global-unset-key (kbd "M-v"))  ; change it to a prefix key, original M-v runs the command scroll-down-command
(global-unset-key (kbd "C-<prior>"))  ; scroll-left is useless
(global-unset-key (kbd "C-<next>"))   ; scroll-right is useless

;; single ESC to cancel prefix key
(defun my-prefix-cancel () (interactive) (message "Prefix canceled."))  ; cannot use keyboard-quit which will quit mc mode too.
(bind-key "M-q <escape>" #'my-prefix-cancel)
(bind-key "C-c <escape>" #'my-prefix-cancel)
(bind-key "<f1> <escape>" #'my-prefix-cancel)

;; rule: C-c = mode-dependent prefix
;; C-c C-c is a common action (e.g., to send "C-c" in shell), better to have a new easy shortcut, e.g., M-c M-c  http://emacs.stackexchange.com/questions/14322
(define-key key-translation-map (kbd "M-c") (kbd "C-c"))
(define-key key-translation-map (kbd "C-c") (kbd "M-c"))
(define-key key-translation-map (kbd "C-S-c") (kbd "M-C"))          ; for term-char-mode
(define-key key-translation-map (kbd "C-i") (kbd "<tab>"))          ; in TTY, tab key generates C-i instead of <tab>
(define-key key-translation-map (kbd "<return>") (kbd "RET"))       ; RET is more common than <return>
(define-key key-translation-map (kbd "<S-tab>") (kbd "<backtab>"))  ; unify key strings
;; todo use "M-S-c"?
(define-key key-translation-map (kbd "M-<kp-enter>") (kbd "M-RET")) ; for numpad enter
(define-key key-translation-map (kbd "M-<enter>")    (kbd "M-RET")) ; for numpad enter
;; "C-M-@" to M-SPC?  ; original C-M-SPC runs the command mark-sexp.  in case M-SPC is n/a (used by OS)
(define-key key-translation-map (kbd "M-ESC") (kbd "<escape>"))     ; so that i can keep pressing meta key

(bind-key "M-q C-q" #'quoted-insert)     ; original shortcut of quoted-insert = C-q
(bind-key "M-q C-h" #'help)              ; more informative version of help-map
(bind-key "M-q M-e" #'eval-last-sexp)    ; original kbd of eval-last-sexp = C-x C-e
(bind-key "M-q e"   #'eval-expression)   ; original kbd of eval-expression = M-:
(bind-key "M-q M-SPC" #'toggle-input-method)
(bind-key "<f10>" #'kmacro-start-macro-or-insert-counter)
(bind-key "<f12>" #'kmacro-end-or-call-macro)
(defun my-narrow-to-region-and-deselect (x y) (interactive "r") (narrow-to-region x y) (deactivate-mark))
(bind-key "M-v" #'my-narrow-to-region-and-deselect) (put 'narrow-to-region 'disabled nil)  ; good for searching within region
(global-set-key (kbd "M-q C-x") ctl-x-map)  ; cannot use (bind-key "M-q C-x" #'ctl-x-map) which return "Wrong type argument: commandp, ctl-x-map". ref: http://ergoemacs.org/emacs/emacs_dvorak_C-x.html

;; Bind C-+ and C-- to increase and decrease text size, respectively.
;; original keys: C-x C-+, C-x C--, and C-x C-0.
;; from https://github.com/hrs/sensible-defaults.el/blob/master/sensible-defaults.el
(define-key global-map (kbd "C-0") 'text-scale-adjust)  ; = (lambda () (interactive) (text-scale-set 0)), -> use M-num (or C-u num) instead of C-num for num arg
(define-key global-map (kbd "C-+") 'text-scale-increase)
(define-key global-map (kbd "C-=") 'text-scale-increase)
(define-key global-map (kbd "C-_") 'text-scale-decrease)
(define-key global-map (kbd "C--") 'text-scale-decrease)

;; remap frequently used file commands http://ergoemacs.org/emacs/emacs_kb_shortcuts_pain.html
(defun my-save-all-buffers () (interactive) (save-some-buffers t))
(bind-key "C-s" #'my-save-all-buffers)
(bind-key "C-M-s" #'write-file)  ; = save-as
(bind-key "C-q" #'save-buffers-kill-terminal)
(bind-key "C-w" #'kill-this-buffer)

;; original binding of other-window = C-x o, not convenient.
;; C-` is N/A in TTY (= C-@ = C-SPC), M-` is used by Ubuntu to flip through windows in the switcher. M-v is not easy enough.
(bind-key "M-SPC" #'other-window)
(bind-key "M-S-SPC" (lambda () (interactive) (other-window -1)))

(defun my-recenter-no-redraw ()
  "Like `recenter', but no redrawing."  ; avoid flashing in terminal http://stackoverflow.com/a/36896945/550243
  (let ((recenter-redisplay nil)) (recenter nil))  ; recenter without arg -> go to middle of the screen
  )

;;; emacs -nw, terminal mode, TTY
(when (not (window-system))
  (menu-bar-mode -1)
  ;; (xterm-mouse-mode +1)  ; make mouse clicks work in xterm.
  (define-key input-decode-map "\e[1;2A" [S-up])  ; http://emacs.stackexchange.com/questions/977 http://stackoverflow.com/questions/10871745
  (define-key input-decode-map "\e[1;2B" [S-down])
  (define-key key-translation-map (kbd "ESC <up>") (kbd "M-<up>"))
  (define-key key-translation-map (kbd "ESC <down>") (kbd "M-<down>"))
  (define-key key-translation-map (kbd "C-M-c") (kbd "C-M-c"))  ; without this, C-M-c will be translated to Esc M-c

  ;; to make emacs more like a modern editor https://emacs.stackexchange.com/questions/36639/how-to-use-m-o-on-emacs-in-terminal
  ;; adapted from https://gist.github.com/Pitometsu/f2f68d5e81862ceffb0d76d277376cf1
  ;; answer in http://stackoverflow.com/questions/26164029 doesn't work after a prefix key (e.g., "C-c Esc" won't call cancel-prefix)
  ;; alt: normal-escape uses read-event, unread-command-events, and a var with a pre-command-hook for reset https://www.emacswiki.org/emacs/CancelingInEmacs
  (defun my-tty-init-esc (frame)
    "Update `input-decode-map' in terminal with FRAME."
    (with-selected-frame frame
      (let ((term (frame-terminal frame)))
        (when (not (terminal-parameter term 'my-tty-esc-map))
          (let ((my-tty-esc-map (lookup-key input-decode-map [?\e])))
            (set-terminal-parameter term 'my-tty-esc-map my-tty-esc-map)
            (define-key input-decode-map [?\e]
              `(menu-item "" ,my-tty-esc-map :filter ,#'my-tty-esc))))
        (when (not (terminal-parameter term 'my-tty-M-O-map))
          (let ((my-tty-M-O-map (lookup-key input-decode-map "\eO")))  ; [?\eO] is not valid
            (set-terminal-parameter term 'my-tty-M-O-map my-tty-M-O-map)
            (define-key input-decode-map "\eO"
              `(menu-item "" ,my-tty-M-O-map :filter ,#'my-tty-M-O)))))))
  (defun my-tty-esc (map)
    (if (and
         (let ((keys (this-single-command-keys)))
           (and (> (length keys) 0)
                (= (aref keys (1- (length keys))) ?\e)))
         (sit-for my-tty-prefix-timeout))
        (prog1 [escape]
          (when defining-kbd-macro
            (end-kbd-macro)
            (setq last-kbd-macro (vconcat last-kbd-macro [escape]))
            (start-kbd-macro t t)))
      map))
  (defun my-tty-M-O (map)
    (if (and
         (let ((keys (this-single-command-keys)))
           (> (length keys) 0))
         (sit-for my-tty-prefix-timeout))
        (progn
          (setq this-command-keys-shift-translated t)
          (handle-shift-selection)
          (next-space-seperated-word)
          )
      map))
  (setq my-tty-prefix-timeout 0.01)
  (add-hook 'after-make-frame-functions #'my-tty-init-esc)
  (mapc #'my-tty-init-esc (frame-list))
  )

;;; frame and window
;; https://emacs.stackexchange.com/questions/30420
;; instead of (pos (frame-position frame)) (left (car pos)) (top (cdr pos)), use (left (frame-parameter frame 'left)) (top (frame-parameter frame 'top)) http://emacs.1067599.n8.nabble.com/bug-25943-21-5-Frame-Display-Difficulties-tp421693p423702.html
(defun my-offset-minibuf-height (frame-height) (- frame-height (* (frame-char-height) 2)))
(defun my-frame-resize-half-up ()  ; no op for Linux
  (interactive)
  (set-frame-parameter nil 'fullscreen nil)  ; so that toggle-frame-maximized knows that current state isn't maximized
  (let ((frame (selected-frame))
        (half-of-display-pixel-height (/ (display-pixel-height) 2)))
    (set-frame-height frame (my-offset-minibuf-height half-of-display-pixel-height) nil 'pixelwise)
    (set-frame-position frame (frame-parameter frame 'left) 0)))
(defun my-frame-resize-half-down ()  ; no op for Linux
  (interactive)
  (set-frame-parameter nil 'fullscreen nil)
  (let ((frame (selected-frame))
        (half-of-display-pixel-height (/ (display-pixel-height) 2)))
    (set-frame-height frame (my-offset-minibuf-height half-of-display-pixel-height) nil 'pixelwise)
    (set-frame-position frame (frame-parameter frame 'left) half-of-display-pixel-height)))
(defun my-frame-resize-half-left ()  ; for Linux, move frame to left monitor
  (interactive)
  (set-frame-parameter nil 'fullscreen nil)
  (let ((frame (selected-frame))
        (half-of-display-pixel-width (/ (display-pixel-width) 2)))
    (set-frame-width frame half-of-display-pixel-width nil 'pixelwise)
    (set-frame-position frame 0 (frame-parameter frame 'top))))
(defun my-frame-resize-half-right ()
  (interactive)
  (set-frame-parameter nil 'fullscreen nil)
  (let ((frame (selected-frame))  ; for Linux, move frame to right monitor
        (half-of-display-pixel-width (/ (display-pixel-width) 2)))
    (set-frame-width frame half-of-display-pixel-width nil 'pixelwise)
    (set-frame-position frame half-of-display-pixel-width (frame-parameter frame 'top))))
(global-unset-key (kbd "<f11>"))
(bind-key "<f11> <escape>" #'my-prefix-cancel)
(bind-key "<f11> <f11>"   #'toggle-frame-maximized)
(bind-key "<f11> <up>"    #'my-frame-resize-half-up)
(bind-key "<f11> <down>"  #'my-frame-resize-half-down)
(bind-key "<f11> <left>"  #'my-frame-resize-half-left)
(bind-key "<f11> <right>" #'my-frame-resize-half-right)

(setq winner-dont-bind-my-keys t)  ; don't use default binding (C-c <left>/<right>) coz C-c is used for mode-specific functions only.
(winner-mode)  ; https://github.com/emacs-mirror/emacs/blob/master/lisp/winner.el
(bind-key "M-<f11>" #'winner-undo)
(bind-key "M-S-<f11>" #'winner-redo)
(bind-key "M-C-<f11>" #'winner-redo)
(bind-key "<f11> =" #'delete-other-windows)
(bind-key "<f11> <backspace>" #'delete-window)
(bind-key "C-M-w" #'delete-window)
(bind-key "<f11> \\" #'split-window-horizontally)
(bind-key "<f11> -" #'split-window-vertically)

;; http://stackoverflow.com/a/28876359 more fine-grain than subword-mode
(use-package syntax-subword  ; and navigation / movement
  :init
  ;; delete instead of kill, ref: http://ergoemacs.org/emacs/emacs_kill-ring.html http://ergoemacs.org/emacs/emacs_delete_whole_line.html https://www.emacswiki.org/emacs/BackwardDeleteWord
  ;; do not use (setq kill-ring (cdr kill-ring)) to remove last item from kill-ring, as it screws up the OS clipboard http://stackoverflow.com/questions/637351
  (defun my-delete-subword (arg) (if (use-region-p) (delete-region (region-beginning) (region-end)) (delete-region (point) (progn (syntax-subword-forward arg) (point)))))
  (defun my-delete-prev-subword () (interactive) (my-delete-subword -1))
  (defun my-delete-next-subword () (interactive) (my-delete-subword 1))
  (bind-key "<C-backspace>" #'my-delete-prev-subword)
  (bind-key "<C-delete>" #'my-delete-next-subword)
  (bind-key "M-S" #'my-delete-prev-subword)
  (bind-key "M-D" #'my-delete-next-subword)
  (bind-key "M-d" #'delete-char)
  (bind-key "M-s" #'delete-backward-char)
  (bind-key "M-n" #'next-line)
  (bind-key "M-p" #'previous-line)
  (bind-key "M-f" #'forward-char)
  (bind-key "M-b" #'backward-char)
  (bind-key "M-q M-u" #'scroll-up-command)
  (bind-key "M-q M-i" #'scroll-down-command)

  (add-hook 'post-command-hook
            (lambda ()
              ;; use called-handle-shift-selection to make sure count-words-region is called only after interactive^ commands, i.e., C-n/p is not affected.
              (when (and (boundp 'called-handle-shift-selection) called-handle-shift-selection (region-active-p))
                (setq-local called-handle-shift-selection nil)
                (call-interactively 'count-words-region))))  ; call-interactively to print the result message.
  (advice-add 'handle-shift-selection :around
              (lambda (orig-fun &rest args)
                (setq-local was-active (region-active-p))  ; https://emacs.stackexchange.com/questions/30414/how-to-move-the-cursor-to-the-beginning-end-of-a-shift-selected-region-by-a-left
                (setq-local called-handle-shift-selection t)
                ;; don't update mark-ring, and suppress "Mark set" message
                (cl-letf (((symbol-function 'push-mark) (lambda ( &optional LOCATION NOMSG ACTIVATE) (set-marker (mark-marker) (point) (current-buffer)) (set-mark (mark t)))))
                  (apply orig-fun args))))

  (bind-key "M-l" (lambda (arg)  ; original M-l runs the command downcase-word
                    (interactive "^p")
                    (if (and was-active (not (region-active-p)))
                        (goto-char (region-end))
                      (syntax-subword-forward arg)
                      )))
  (bind-key "M-h" (lambda (arg)  ; original M-h runs the command mark-paragraph / org-mark-element; need * to override org-mode rebind.
                    (interactive "^p")
                    (if (and was-active (not (region-active-p)))
                        (goto-char (region-beginning))
                      (syntax-subword-backward arg)
                      )))

  ;; go to next/prev block-start. related: http://ergoemacs.org/emacs/emacs_move_by_paragraph.html http://whattheemacsd.com/setup-html-mode.el-01.html http://endlessparentheses.com/meta-binds-part-2-a-peeve-with-paragraphs.html
  ;; do not use "*" so that it can be redefined in helm-map
  (bind-key "M-j" (lambda ()  ; original M-j runs the command indent-new-comment-line
                    "Next block-start."
                    (interactive "^")
                    (unless (search-forward-regexp "\n[\t\n ]*\n" nil t) (goto-char (point-max)))
                    ))
  (bind-key "M-k" (lambda ()  ; original M-k runs the command kill-sentence
                    "Previous block-start."
                    (interactive "^")
                    (skip-chars-backward "\n\t ")
                    (if (search-backward-regexp "\n[\t ]*\n" nil t)  ; go to start of prev empty line
                        (progn (skip-chars-forward "\n\t ")  ; go to the block
                               (skip-chars-backward "\t "))  ; go to line-beginning-position, (move-beginning-of-line) doesn't work.
                      (goto-char (point-min))
                      )))

  ;; do not use viper-forward-Word which cannot eat multiple "\n"
  (defun my-next-space-seperated-word ()
    (interactive "^")
    (skip-chars-forward "^\n\t ") (skip-chars-forward "\n\t ")  ; == (search-forward-regexp "[^\t\n ][\t\n ]+" nil t)
    )
  (defun my-prev-space-seperated-word ()
    (interactive "^")
    (skip-chars-backward "\n\t ") (skip-chars-backward "^\n\t ")
    )
  (bind-key "M-o" #'my-next-space-seperated-word)
  (bind-key "M-y" #'my-prev-space-seperated-word)

  (bind-key "M-a" (lambda ()
                    "Move point to the first non-whitespace character on this line. If point was already at that position, move point to beginning of line."
                    (interactive "^")
                    (let ((oldpos (point))) (back-to-indentation) (and (= oldpos (point)) (beginning-of-line)))))
  (bind-key "M-e" #'end-of-line)

  (global-visual-line-mode 1)  ; enable Word Wrap, o/w may split a word into 2 parts
  (setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))  ; disabled by default https://www.gnu.org/software/emacs/manual/html_node/emacs/Visual-Line-Mode.html  alt: use whitespace-mode to display hard newlines https://vlevit.org/en/blog/tech/visual-line-wrap

  ;; On Mac, the Home and End keys scroll to the top or bottom of a document rather than the start or end of a line. http://www.popcornfarmer.com/2008/04/mac-home-and-end-keys/ http://stackoverflow.com/a/19614725/550243
  ;; Set them back to normal (as in Chrome, Window, and Ubuntu), which are useful to "unify" cursors when using multiple-cursors.
  (bind-key "<home>" #'beginning-of-visual-line)
  (bind-key "<end>"  #'end-of-visual-line)
  (bind-key "C-M-a"  #'beginning-of-visual-line)  ; original C-M-e runs the command beginning-of-defun
  (bind-key "C-M-e"  #'end-of-visual-line)        ; original C-M-e runs the command end-of-defun

  ;; Usages:
  ;; restore selection https://emacs.stackexchange.com/questions/12952
  ;; extend or shrink the other end of a selection http://ergoemacs.org/emacs/emacs_jump_to_previous_position.html
  (bind-key "M-?"
            (lambda ()
              (interactive)
              (exchange-point-and-mark)
              (convert-to-shift-selection)
              (my-recenter-no-redraw)))
  (bind-key "M-/"  ; useful for going back to last point / editing two points (e.g., parentheses) in a single buffer without splitting window https://emacs.stackexchange.com/questions/12952
            (lambda ()
              (interactive)
              (exchange-point-and-mark t)  ; t -> no selection
              (deactivate-mark)
              (my-recenter-no-redraw)))
  ;; don't use ":bind" here, as that will delay loading of this package
  )

(use-package which-key
  :defer 5
  :config
  (which-key-mode)
  (setq which-key-sort-order 'which-key-description-order)
  :bind
  ("C-?" . which-key-show-top-level)
  )

(use-package keyfreq
  :init
  (keyfreq-mode t)
  (keyfreq-autosave-mode t))

;; https://github.com/justbur/emacs-vdiff/blob/master/vdiff.el
;; ✔ shows deleted line as empty line
(use-package vdiff
  :config
  (setq
   vdiff-disable-folding t
   vdiff-auto-refine t
   vdiff-only-highlight-refinements nil  ; changed-line highlight is still useful, as refinements don't include symbol changes
   vdiff-subtraction-fill-char ? )  ; default = '-'
  (set-face-background 'vdiff-refine-changed (face-attribute 'diff-added :background))  ; default = yellow, change to light green to reduce num of colors
  (defun my-diff-cur-file-on-disk () (interactive) (cl-letf (((symbol-function 'y-or-n-p) (lambda (prompt) nil))) (vdiff-current-file)))  ; set y-or-n-p to nil -> do not compare with auto-save file, ref: https://github.com/jwiegley/emacs-release/blob/master/lisp/vc/ediff.el  flet is deprecated
  :bind
  ("M-q M-d" . my-diff-cur-file-on-disk)
  :bind
  (:map vdiff-mode-map
        ("M-u" . vdiff-next-hunk)
        ("M-i" . vdiff-previous-hunk)
        ("C-c C-c" . vdiff-receive-changes)
        ("C-c M-v" . vdiff-send-changes)
        ))

(advice-add 'mwheel-scroll :before (lambda (arg) (if (not (eq last-command 'mwheel-scroll)) (set-marker (mark-marker) (point)))))
(defun my-escape ()
  (interactive)
  (if (eq last-command 'mwheel-scroll)
      (progn
        (exchange-point-and-mark t)  ; go back to pre-scroll position
        (my-recenter-no-redraw)
        (beacon-blink))
    (if (eq last-command 'keyboard-quit)
        (progn
          (widen)
          (when (and (boundp 'vdiff--session) (not (null vdiff--session))) (vdiff-quit))
          (keyboard-escape-quit)  ; = original "ESC ESC ESC"
          (my-recenter-no-redraw)
          (beacon-blink))
      ;; Use unread-command-events instead of calling keyboard-quit directly, in case C-g is rebound, e.g., in company-mode / minibuffer-local-map http://superuser.com/a/945245/59765
      (setq unread-command-events (listify-key-sequence "\C-g"))
      )))

;; single esc = C-g = keyboard-quit, same as all other apps like closing a dialog box in Chrome (solving a problem mentioned in https://www.reddit.com/r/emacs/comments/4a0421 )
;; double esc = keyboard-escape-quit + widen = keyboard-quit + delete-other-windows + ...
(bind-key "<escape>" #'my-escape)
;; query-replace-map is keymap-parent of map-y-or-n-p (used by save-buffers-kill-terminal) http://superuser.com/questions/795763/how-to-make-emacs-quit-the-minibuffer-with-one-press-of-esc
(define-key query-replace-map (kbd "<escape>") 'keyboard-quit)  ; originally bind to exit-prefix

;; my org-mode also use this function -> do not put it inside any use-package.
(defun my-get-clipboard-contents-as-one-line ()  ; good for pasting title+url
  ;; related package for link-capture: org-mac-link http://heikkil.github.io/blog/2015/05/08/notes-from-browser-window/ but no Linux version
  (let ((text (simpleclip-get-contents)))  ; (substring-no-properties (car kill-ring)) cannot get content from system clipboard
    (replace-regexp-in-string " *$" "\n" (replace-regexp-in-string "\n" " " text))))

;; Use simpleclip to get content from system clipboard http://blog.binchen.org/posts/the-reliable-way-to-access-system-clipboard-from-emacs.html https://www.emacswiki.org/emacs/CopyAndPaste
(use-package simpleclip
  :config
  (defun my-paste-as-one-line () (interactive) (insert (my-get-clipboard-contents-as-one-line)))
  :bind
  ("M-q v" . my-paste-as-one-line)
  )

(define-globalized-minor-mode  ; https://stackoverflow.com/questions/16048231/how-to-enable-a-non-global-minor-mode-by-default-on-emacs-startup
  global-goto-address-mode
  goto-address-mode
  (lambda ()
    ;; todo use (eq major-mode 'helm-mode) ??
    (when (or (not (stringp mode-name))  ; "mode-name" is not a string for html-mode
              (not (string= "Hmm" mode-name)))  ; disable for helm-mini
      (goto-address-mode))))
(global-goto-address-mode)  ; clickable URLs

(defun google-search (keywords)
  (browse-url (concat "http://www.google.com/search?q=" (url-hexify-string keywords))))  ; url-hexify-string is more general than (replace-regexp-in-string " " "+" str)
(defun google-region-or-goto-address-at-point ()
  (interactive)
  (cond
   ((use-region-p)
    (google-search (buffer-substring-no-properties (region-beginning) (region-end))))
   ((thing-at-point 'url t)
    (goto-address-at-point))
   ((setq bounds (thing-at-point-bounds-of-url-at-point))  ; cannot use (thing-at-point-url-at-point) which requires delimiter ':' that doesn't exist in my customized thing-at-point-uri-schemes
    (browse-url (concat "http://" (buffer-substring-no-properties (car bounds) (cdr bounds)))))
   (t (google-search (thing-at-point 'symbol)))  ; symbol instead of word because it's more inclusive http://irreal.org/blog/?p=50
   ))
(bind-key "<f3>" #'google-region-or-goto-address-at-point)

;; https://github.com/emacsmirror/bing-dict  related: https://github.com/atykhonov/google-translate
(use-package bing-dict
  :config
  (setq bing-dict-show-thesaurus 'both)  ; show both synonyms and antonyms
  (defun dictionary-region-or-word-at-point ()
    (interactive)
    (if (use-region-p)
        (setq word (url-hexify-string (buffer-substring-no-properties (region-beginning) (region-end))))
      (setq word (url-hexify-string (thing-at-point 'word))))
    (if (eq system-type 'darwin) (shell-command (concat "open \"dict:///" word "\"")))  ; also open Dictionary.app which works offline http://larkery-blog-blog.tumblr.com/post/465585528/emacs-dictionaryapp
    (bing-dict-brief word))
  :bind
  ("<f2>" . dictionary-region-or-word-at-point)
  )

(defun my-new-chrome-tab ()
  (interactive)
  (if (window-system)
      ;; use about:blank to open a new tab https://productforums.google.com/forum/#!topic/chrome/PmCbBZ06gBA
      (if (eq system-type 'darwin)
          ;; (browse-url "https://www.google.com") doesn't have address bar focus
          ;; cannot use browse-url, which doesn't support non-url parameter.
          (shell-command (concat "/Applications/Google\\ Chrome.app/Contents/MacOS/Google\\ Chrome 'about:blank' && open -a Google\\ Chrome"))
        ;; 2>/dev/null to silent the message: "Failed to launch GPU process."
        ;; alt: https://emacs.stackexchange.com/questions/40829/
        ;;   cannot use (shell-command "xdotool search --class 'google-chrome' windowactivate") which always activate oldest window (wanted most recent window)
        (shell-command "google-chrome 'about:blank' 2>/dev/null"))
    (message "Not in window-system")))
(bind-key "C-t" #'my-new-chrome-tab)

(defun sudo-edit (&optional arg)  ; From http://emacsredux.com/blog/2013/04/21/edit-files-as-root/
  "Edit currently visited file as root. With a prefix ARG prompt for a file to visit. Will also prompt for a file to visit if current buffer is not visiting a file."
  (interactive "P")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:" (ido-read-file-name "Find file(as root): ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

;; http://emacsredux.com/blog/2013/03/27/copy-filename-to-the-clipboard/ https://github.com/bbatsov/prelude
(defun copy-file-path-to-clipboard ()  ; note: I replaced "name" by "path", as it copies the full path, not just the filename.
  "Copy the current buffer file path to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode) default-directory (buffer-file-name))))
    (when filename (kill-new filename) (message "Copied buffer file path '%s' to the clipboard." filename))))

;; related: smart-shift: has annoying keychord function, smart-shift-left/right not useful due to auto-format
;; related: http://stackoverflow.com/questions/2423834/move-line-region-up-and-down-in-emacs
;; (drag-stuff-global-mode 1)  ; doesn't work
(use-package drag-stuff  ; and indent
  :init
  (defun region-line-beg ()
    (if (region-active-p)
        (save-excursion (goto-char (region-beginning)) (line-beginning-position))
      (line-beginning-position)))
  (defun region-line-end ()
    (if (region-active-p)
        (save-excursion (goto-char (region-end)) (line-end-position))
      (line-end-position)))

  (bind-key "M-\\"  ; indent-region-or-buffer http://emacsredux.com/blog/2013/03/27/indent-region-or-buffer/
            (lambda ()
              "Indent a region if selected, otherwise the current line."
              (interactive)
              (let ((deactivate-mark nil))  ; keep region
                (indent-region (region-line-beg) (region-line-end))
                (message "Indented line or region.")
                )))
  (bind-key "M-|"
            (lambda ()
              "Indent whole buffer."
              (interactive)
              (let ((deactivate-mark nil))  ; keep region
                (indent-region (point-min) (point-max))
                (message "Indented whole buffer.")
                )))

  ;; http://stackoverflow.com/questions/11623189/how-to-bind-keys-to-indent-unindent-region-in-emacs
  (defun keyboard-indent (&optional arg)
    (interactive)
    (let ((deactivate-mark nil))  ; keep region
      (indent-rigidly (region-line-beg) (region-line-end) (* (or arg 1) tab-width))))
  (defun keyboard-unindent (&optional arg)
    (interactive)
    (keyboard-indent (* -1 (or arg 1))))
  (bind-key "M-}" 'keyboard-indent)  ; cannot use sublimetext shortcut M-[/] which is used by shift key in TTY
  (bind-key "M-{" 'keyboard-unindent)

  ;; fix comment-region (original binding: M-;), which doesn't work well for multi-line, alternative https://www.emacswiki.org/emacs/CommentingCode
  (bind-key "C-/"  ; same as http://codemirror.net/demo/sublime.html
            (lambda ()
              "Comments or uncomments the region or the current line if there's no active region. http://stackoverflow.com/questions/9688748"
              (interactive)
              (let ((deactivate-mark nil))  ; keep region
                (comment-or-uncomment-region (region-line-beg) (region-line-end)))
              ))

  :bind
  ;; in org-mode, override org-metaup and org-metadown, as move tree is not common
  ("M-<up>" . drag-stuff-up)
  ("M-<down>" . drag-stuff-down)
  )

;;; xcv / cua mode: cut copy paste delete join
(cua-selection-mode -1)  ; load the package for cua copy and cut function, -1 -> don't set the keymap
(setq cua-keep-region-after-copy t)

(defun my-delete-empty-lines ()
  "Replace repeated blank lines to just 1. Works on whole buffer or text selection, respects `narrow-to-region'. URL `http://ergoemacs.org/emacs/elisp_compact_empty_lines.html' Version 2017-09-22"
  (interactive)
  (let ($begin $end)
    (if (region-active-p)
        (setq $begin (region-beginning) $end (region-end))
      (save-excursion
        (save-restriction
          (narrow-to-region $begin $end)
          (progn
            (goto-char (point-min))
            (while (re-search-forward "\n\n+" nil "move")
              (replace-match "\n"))))))))

(defun my-delete-current-line ()
  (delete-region
   (progn (forward-visible-line 0) (point))
   (progn (forward-visible-line 1) (point))))
;; related:
;; http://ergoemacs.org/emacs/emacs_copy_cut_current_line.html
;; https://github.com/purcell/whole-line-or-region
(bind-key* "C-x"  ; need '*' to avoid helm error: "Key sequence C-x X starts with non-prefix key C-x"
           (lambda ()
             "Cut current line, or text selection."
             (interactive)
             (if (use-region-p)
                 (cua-cut-region nil)  ; use cua-cut-region instead of (kill-region (region-beginning) (region-end)) to support rectangle region
               ;; don't use kill-region or kill-whole-line which would do append if last-command is kill.
               ;; (buffer-substring-no-properties (line-beginning-position) (line-beginning-position 2)) doesn't have newline char when cutting last line.
               (kill-new (concat (buffer-substring-no-properties (line-beginning-position) (line-end-position)) "\n"))
               (my-delete-current-line)
               )))
;; related: http://ergoemacs.org/emacs/emacs_copy_cut_current_line.html
(defun my-copy ()
  "Copy current line and them <home> (to approximate sublime text C-c), or text selection."
  (interactive)
  (if (use-region-p)
      ;; use cua-copy-region instead of kill-ring-save or (kill-new (buffer-substring-no-properties (region-beginning) (region-end))) to support rectangle region
      (progn (cua-copy-region nil) (message "Region copied"))
    (kill-new (concat (buffer-substring-no-properties (line-beginning-position) (line-end-position)) "\n"))
    (beginning-of-line)
    (message "Line copied")))
(bind-key "M-c" 'my-copy)
;; To duplicate rectangle region, use multiple-cursors first, then my-duplicate-line-or-region
(defun my-duplicate-line-or-region ()
  (interactive)
  (if (use-region-p)
      ;; from https://www.emacswiki.org/emacs/DuplicateStartOfLineOrRegion
      ;; or just (insert (buffer-substring-no-properties (mark) (point))) for no selection
      (let* ((end (region-end)) (text (buffer-substring (region-beginning) end)))
        (goto-char end)
        (insert text)
        (push-mark end)
        (setq deactivate-mark nil)
        (exchange-point-and-mark))
    (let ((left (buffer-substring-no-properties (point-at-bol) (point))) (right (buffer-substring-no-properties (point) (point-at-bol 2))))
      (insert right) (insert left))))  ; insert twice to keep the cursor's relative position
(bind-key "C-S-c" 'my-duplicate-line-or-region)
(bind-key "C-M-c" 'my-duplicate-line-or-region)  ; C-S-c is N/A in terminal.  original C-M-c runs the command exit-recursive-edit.
(bind-key "C-v" 'yank)
(bind-key "M-x"
          (lambda ()
            "Delete (not kill) the current line OR lines of selected region, including newline char. http://stackoverflow.com/questions/3958528"
            (interactive)
            (save-excursion
              (when (region-active-p) (delete-region (region-beginning) (region-end)))
              (my-delete-current-line))))
(bind-key "M-X"
          (lambda ()  ; join-line-or-lines-in-region https://github.com/rejeep/emacs/blob/master/defuns.el
            "Join this line or the lines in the selected region."
            (interactive)
            (cond ((region-active-p)
                   (let ((min (line-number-at-pos (region-beginning))))
                     (goto-char (region-end))
                     (while (> (line-number-at-pos) min) (join-line))))
                  (t (join-line -1)))))  ; modification: join with next line, instead of prev line

;; ;;d fix numeric arg and last line issue
(bind-key "<f9>"  ; = http://codemirror.net (minor difference: codemirror sort doesn't include cursor line if cursor is at line beginning)
          (lambda (arg)  ; sort-lines doesn't expand to full line if the selection is not perfect.
            (interactive "p")
            (when (use-region-p)
              (if (< (point) (mark))  ; simplify it? ;;d
                  (let ((beg (point)))
                    (goto-char (mark))
                    (forward-line arg)
                    (push-mark)
                    (goto-char beg)
                    (beginning-of-line))
                (let ((end (point)))
                  (goto-char (mark))
                  (beginning-of-line)
                  (push-mark)
                  (goto-char end)
                  (forward-line arg)))
              (let ((deactivate-mark nil))  ; keep region
                (call-interactively 'sort-lines)))))

;; http://ergoemacs.org/emacs/emacs_new_empty_buffer.html
;; https://emacs.stackexchange.com/questions/35803/how-to-get-emacs-to-prompt-when-a-new-buffer-is-changed-on-save
;; https://emacs.stackexchange.com/questions/2191/copy-contents-of-current-buffer-in-a-temp-buffer-and-operate-on-that
(defun new-buffer-with-selected-string ()
  "Create a new empty buffer. New buffer will be named “untitled” or “untitled<2>”, “untitled<3>”, etc."
  (interactive)
  (let (($buf (generate-new-buffer "untitled"))
        ($str (if (use-region-p) (buffer-substring-no-properties (region-beginning) (region-end)) "")))
    (switch-to-buffer $buf)
    (undo-tree-mode 1)  ; https://www.reddit.com/r/emacs/comments/4akvr3/how_to_enable_undotreemode_in_fundamentalmode/
    (insert $str)
    (setq buffer-offer-save t)
    $buf))
(bind-key "C-n" #'new-buffer-with-selected-string)

(defun my-rm/delete-this-buffer-and-file ()  ; dangerous action -> no shortcut
  "Removes file connected to current buffer and kills buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (when (yes-or-no-p "Are you sure you want to remove this file? ")
        (delete-file filename)
        (kill-buffer buffer)
        (message "File '%s' successfully removed" filename)))))

(defun my-rename-current-buffer-file ()  ; https://stackoverflow.com/a/37456354
  "Renames current buffer and file it is visiting."
  (interactive)
  (let* ((name (buffer-name))
         (filename (buffer-file-name))
         (basename (file-name-nondirectory filename)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " (file-name-directory filename) basename nil basename)))
        (if (get-buffer new-name)
            (error "A buffer named '%s' already exists!" new-name)
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (message "File '%s' successfully renamed to '%s'"
                   name (file-name-nondirectory new-name)))))))

;; https://www.youtube.com/watch?v=-4O-ZYjQxks http://capitaomorte.github.io/yasnipqpet/
;; snippets https://www.reddit.com/r/emacs/comments/3xtmq5/any_cool_yasnippets_to_share_with_us/
;; Snippet Expansion with YASnippet http://cupfullofcode.com/blog/2013/02/26/snippet-expansion-with-yasnippet/index.html
(use-package yasnippet
  :mode ("\\.yasnippet" . snippet-mode)
  :init
  ;; redefine yas-load-directory to resolve an error in Linux: "No such file or directory"
  ;; need with-eval-after-load in :init to prevent startup error.
  (with-eval-after-load "yasnippet"  ; use eval-after-load to make sure the new defun overrides the old one
    (defun yas-load-directory (top-level-dir &optional use-jit interactive) nil))
  (yas-global-mode 1)
  (yas--define-parents 'minibuffer-inactive-mode '(org-mode))  ; https://emacs.stackexchange.com/questions/10960/one-yasnippet-snippet-for-multiple-modes
  (define-key yas-minor-mode-map (kbd "C-c &") 'undefined)

  (add-hook 'org-mode-hook  ; adapted from http://orgmode.org/manual/Conflicts.html no need to set yas/trigger-key
            (lambda ()
              ;; syntax of ?_ is changed by org-mode itself: as part of the definition of the mode (see org.el) -> do it in hook https://emacs.stackexchange.com/questions/17284
              (modify-syntax-entry ?_ "_" org-mode-syntax-table)  ; to fix syntax subword movement (inconsistent between forward and backward) for words_with_underscore, origin (wrong) value = "w"
              (define-key yas/keymap [tab] 'yas/next-field-or-maybe-expand)))  ; cannot put it in org entry where yas/keymap is not defined yet

  ;; define yasnippet templates using elisp https://emacs.stackexchange.com/questions/34840  =_single_config_file=
  ;; How To Define Templates In YASnippet - Emacs http://emacs.livejournal.com/30321.html
  (yas-define-snippets
   'org-mode
   ;; no "s" as "<s" is used
   '(
     (".c"   "#+BEGIN_SRC C++\n$0\n#+END_SRC\n"    "#+BEGIN_SRC C++ #+END_SRC")
     (".py"  "#+BEGIN_SRC python\n$0\n#+END_SRC\n" "#+BEGIN_SRC python #+END_SRC")
     (".py3" "#+BEGIN_SRC python :python python3\n$0\n#+END_SRC\n" "#+BEGIN_SRC python :python python3 #+END_SRC")
     (".pys"  "src_python{$0}" "src_python{ }")
     (".sh"  "#+BEGIN_SRC shell\n$0\n#+END_SRC\n"  "#+BEGIN_SRC shell #+END_SRC")
     ("approx" "≈" "≈")
     ("exists" "∃" "∃")
     ("forall" "∀" "∀")
     ("degree" "°" "°")
     ("neq" "≠" "≠")
     ("notexists" "∄" "∄")
     ;; use ✔ ✘ instead of + - which are used for bullet points; they are not a keyword -> can be used in headings.
     ("vv" "✔" "✔")  ; alt: ☑ ☺
     ("xx" "✘" "✘")  ; alt: ☒ ☹
     ("bb" "❱" "❱")  ; = "better than", get from http://xahlee.info/comp/unicode_matching_brackets.html
     ("ac" "ACTION:" "ACTION:")
     ("ch" "CHALLENGE:" "CHALLENGE:")
     ("co" "COZ:" "COZ:")
     ("e" "+ Events:\n  - \n+ Done & Thanks:\n  - \n+ Problems & Solutions:\n  - " "Event log")
     ("er" "ERROR:" "ERROR:")
     ("go" "GOAL:" "GOAL:")
     ("le" "LESSON:" "LESSON:")
     ("pr" "PROBLEM:" "PROBLEM:")
     ("re" "REASON:" "REASON:")
     ("so" "SOLUTION:" "SOLUTION:")
     ("to" "TODO:" "TODO:")
     ("d" "DONE" "DONE")
     ("m" "MISSED" "MISSED")
     ("=" "$1 = ${1:$(calc-eval (replace-regexp-in-string \"p\" \"perm\" (replace-regexp-in-string \"c\" \"choose\" yas-text)))}$0" "calc-eval")  ; https://www.reddit.com/r/emacs/comments/6u0gmx
     ))
  (yas-define-snippets
   'c++-mode
   '(("F"  "FLAGS_"  "FLAGS_")
     ))
  (yas-define-snippets
   'sh-mode  ; for shell-script
   '(("F"  "FLAGS_"  "FLAGS_")
     ))
  (yas-define-snippets
   'python-mode
   '(("F"  "FLAGS."  "FLAGS.")
     ))
  )

;; A Package in a league of its own: Helm http://tuhdo.github.io/helm-intro.html
;; Helm wiki https://github.com/emacs-helm/helm/wiki
;; Open Files In Different Ways https://www.emacswiki.org/emacs/OpenFilesInDifferentWays
;; ref. http://stackoverflow.com/a/12708839  https://github.com/aronasorman/dotfiles/blob/master/.emacs.d/init.el
;; note:
;;   space: fall back to matchplugin detected http://emacs.stackexchange.com/questions/4243/
;; TODO try: ace-isearch combines ace-jump-mode and helm-swoop http://sachachua.com/blog/2015/01/emacs-kaizen-ace-isearch-combines-ace-jump-mode-helm-swoop/
;; TODO Create Buffer?
(use-package helm
  :config
  (setq helm-map (make-keymap))  ; https://stackoverflow.com/questions/7459019/is-there-a-way-to-reset-the-emacs-keymap
  (setq helm-buffer-map helm-map)  ; remove helm-buffer-map, which is redundant to me https://github.com/emacs-helm/helm/blob/master/helm-buffers.el
  ;; From https://gist.github.com/antifuchs/9238468
  (setq
   helm-allow-mouse t                   ; mouse-1 select candidate, mouse-2 execute default action, mouse-3 popup menu actions https://github.com/emacs-helm/helm/issues/1746
   helm-mode-line-string ""             ; original value is outdated and annoying
   helm-follow-mode-persistent t        ; https://github.com/emacs-helm/helm/issues/210 https://github.com/emacs-helm/helm/issues/530
   helm-net-prefer-curl t               ; o/w helm-google has no result; helm-google-suggest-use-curl-p is deprecated
   helm-buffer-max-length 40            ; some source codes has long file names (default = 20)
   helm-dabbrev-cycle-threshold 0       ; show helm completion at the very beginning without dabbrev cycle
   ;; helm-buffers-fuzzy-matching t        ; the default pattern search is better (more controlable) than fuzzy search
   ;; helm-move-to-line-cycle-in-source t  ; move to end or beginning of source when reaching top or bottom of source
   ;; helm-candidate-number-limit 1000     ; original value was 100, too small for shell history
   ;; helm-ff-skip-boring-files t
   ;; helm-ff-auto-update-initial-value t  ; auto update when only one candidate directory is matched. https://groups.google.com/forum/#!topic/emacs-helm/K3FuveL5uTY inconvenient if i wanna go to top level dir
   helm-dabbrev-separator-regexp "\\s-\\|\t\\|[(\[\{\"'`=<$,@.#+]\\|\\s\\\\|^\n\\|^"  ; remove ";" from separator to match tags
   helm-ff-transformer-show-only-basename nil  ; to show full file path in recentf/helm-locate in helm-mini https://emacs.stackexchange.com/questions/40934
   )
  (set-face-attribute 'helm-source-header nil :height 0.8)

  ;; (add-to-list 'helm-sources-using-default-as-input 'helm-source-google-suggest) doesn't support region
  (defun helm-google-suggest-region-or-symbol ()  ; ref: helm-google-suggest-at-point https://github.com/emacs-helm/helm/pull/1614
    (interactive)
    (helm :sources 'helm-source-google-suggest
          :buffer "*helm google*"
          :input (if (use-region-p) (buffer-substring-no-properties (region-beginning) (region-end)) (thing-at-point 'symbol))))
  (helm-mode)
  (advice-add 'helm-org-goto-marker :after (lambda (arg) (my-recenter-no-redraw)))  ; recenter after any jump, and during follow. lambda is needed to match the num of arg http://stackoverflow.com/a/36896945/550243
  (with-eval-after-load "helm-ring" (helm-attrset 'follow 1 helm-source-mark-ring))  ; https://emacs.stackexchange.com/questions/26296

  (defun helm-copy-to-buffer ()  ; re-define helm-copy-to-buffer to delete active region (if any)
    (interactive)
    (with-helm-alive-p
      (helm-run-after-exit
       (lambda (cands)
         (with-helm-current-buffer
           (if (region-active-p) (delete-region (region-beginning) (region-end)))  ; added to delete region if any.
           (insert (mapconcat (lambda (c)
                                (format "%s" c))
                              cands "\n"))))
       (helm-marked-candidates))))

  (defun helm-grep-cur-dir-tree ()  ; http://stackoverflow.com/questions/30142296/
    "Recursively search current directory."
    (interactive)
    (setq  ; re-use minibuffer-history history
     minibuffer-history
     (let ((helm-grep-history minibuffer-history))
       (helm-do-grep-1
        (list default-directory)
        t  ; recurse
        nil
        '("*")  ; all files, e.g., txt, cc
        ;; ""  ; no default keyword string
        (if (use-region-p) (buffer-substring-no-properties (region-beginning) (region-end))
          (thing-at-point 'symbol))  ; default-input = region / symbol
        )
       helm-grep-history)))

  (defun helm-map-M-g ()  ; http://emacs.stackexchange.com/questions/28790/
    (interactive)
    (if (eq helm-pattern "")
        (previous-history-element 1)  ; empty search field: activate previous search
      ;; non-empty search field: go to next match
      (call-interactively 'helm-next-line)  ; need "call-interactively" to retain follow-mode
      ))

  ;; helm-my-imenu always shows the whole lines (like folding), while original helm-imenu and helm-semantic-or-imenu may truncate some lines in C++ mode.
  ;; from https://www.reddit.com/r/emacs/comments/3mtus3/how_to_display_a_list_of_classes_functions_etc/
  ;; TODO full syntax highlighting?
  ;; TODO try helm-imenu-in-all-buffers
  (require 'helm-imenu)  ; needed, o/w (wrong-type-argument arrayp nil)
  (defun helm-my-imenu-transformer (cands)
    (with-helm-current-buffer
      (save-excursion
        (cl-loop for (func-name . mrkr) in cands
                 collect
                 (cons (format "%4d %s"  ; similar to helm-swoop
                               (line-number-at-pos mrkr)
                               (progn (goto-char mrkr) (buffer-substring mrkr (line-end-position))))
                       (cons func-name mrkr))))))
  (defvar helm-my-imenu-source  (helm-make-source "helm imenu" 'helm-imenu-source
                                  :candidate-transformer 'helm-my-imenu-transformer
                                  :follow 1))
  (defvar helm-my-imenu-history nil)
  (defun helm-my-imenu ()
    (interactive)
    (deactivate-mark)  ; to avoid distorted region with (setq cua-keep-region-after-copy t)
    ;; (jit-lock-fontify-now)  ; needed to have a full syntax highlighting, as emacs fontlock only the visited part of buffer for better performances on large buffer. https://groups.google.com/forum/#!topic/emacs-helm/YwqsyRRHjY4 but it takes too long time for large files, e.g., get_sherlock_model.cc, TODO make it fontify small helm buffer only? http://oremacs.com/2015/04/22/swiper-0.3.0/
    (let ((imenu-auto-rescan t)
          (str (thing-at-point 'symbol))
          (helm-execute-action-at-once-if-one
           helm-imenu-execute-action-at-once-if-one))
      (helm :sources 'helm-my-imenu-source
            :candidate-number-limit 9999  ; same as https://github.com/emacs-helm/helm/blob/master/helm-semantic.el
            :preselect str
            :buffer "*helm imenu*"
            :history 'helm-my-imenu-history)
      ))

  (defvar helm-my-doc-files
    (helm-build-sync-source "My Doc: ~/Google Drive/_doc/"  ; ref: defvar helm-source-ido-virtual-buffers in https://github.com/emacs-helm/helm/blob/master/helm-buffers.el
      :candidates (lambda ()
                    (with-helm-current-buffer
                      ;; avoid duplication with helm-source-files-in-current-dir, ref: helm-files-in-current-dir-source in https://github.com/emacs-helm/helm/blob/master/helm-files.el
                      (unless (string-match "/Google Drive/_doc/\\'" (helm-current-directory)) (directory-files "~/Google Drive/_doc" t))
                      ))
      :keymap helm-generic-files-map
      :action (helm-actions-from-type-file)
      :requires-pattern 1  ; to speedup helm-mini on Linux
      :match-part 'helm-basename  ; only match basename
      ))

  (require 'recentf)  ; for recentf-list
  (recentf-mode 1)
  (defvar helm-my-delayed-recentf  ; replace helm-source-recentf, which is slow on Linux when the list contain contain something long to load (e.g., cc file in workspace) https://github.com/emacs-helm/helm/issues/1894
    (helm-build-sync-source "Recent"
      :candidates recentf-list
      :keymap helm-generic-files-map
      :action (helm-actions-from-type-file)
      :requires-pattern 1  ; to speedup helm-mini on Linux
      :match-part 'helm-basename  ; only match basename
      ))

  (require 'helm-for-files)
  (defvar helm-file-basename-in-current-dir
    (helm-make-source "Current Dir" 'helm-files-in-current-dir-source
      :match-part 'helm-basename))  ; only match basename

  ;; (defvar helm-source-locate
  ;;   (helm-make-source "Locate" 'helm-locate-source
  ;;     :pattern-transformer 'helm-locate-pattern-transformer
  ;;     ;; :match-part is only used here to tell helm which part
  ;;     ;; of candidate to highlight.
  ;;     :match-part (lambda (candidate)
  ;;                   (if (or (string-match-p " -b\\'" helm-pattern)
  ;;                           (and helm-locate-fuzzy-match
  ;;                                (not (string-match "\\s-" helm-pattern))))
  ;;                       (helm-basename candidate)
  ;;                       candidate))))

  (defvar helm-my-mini-history nil)  ; separate histories for different Helm prompts http://emacs.stackexchange.com/questions/12338
  (setq helm-source-buffers-list (helm-make-source "Buffers" 'helm-source-buffers))  ; copy from helm-mini() in https://github.com/emacs-helm/helm/blob/master/helm-buffers.el
  (require 'helm-for-files)  ; for helm-source-files-in-current-dir
  (setq helm-my-mini-sources
        '(helm-source-buffers-list
          helm-my-delayed-recentf  ; recentf is more commonly used than files-in-current-dir -> higher position
          helm-file-basename-in-current-dir
          helm-my-doc-files
          helm-source-locate
          ;; helm-source-buffer-not-found = create file, can also be done by C-x C-f
          ))
  (defun helm-my-mini ()  ; to use my own helm-my-mini-history
    (interactive)
    (helm :sources helm-my-mini-sources
          :buffer "*helm my mini*"
          :history 'helm-my-mini-history
          ))

  ;; adapted from https://github.com/emacs-helm/helm/blob/master/helm-net.el
  ;; added ":match 'identity" and ":fuzzy-match t" to disable filtering
  ;; usage: auto-correct, e.g.,
  ;;   cronical diese -> chronic disease
  ;;   arbitary -> arbitrary
  ;;   sacrifies -> sacrifice
  ;;   symton -> symptoms
  ;;   fructuate -> fluctuate
  ;;   be cla -> be calm
  ;;   curiousity -> curiosity
  ;;   hueuristic -> heuristic
  ;;   vunability -> vulnerability (no other dictionary can make that correction)
  (setq helm-source-google-suggest  ; use setq to re-defvar https://lists.gnu.org/archive/html/help-gnu-emacs/2009-06/msg00397.html
        (helm-build-sync-source "Google Suggest"
          :candidates (lambda () (funcall helm-google-suggest-default-function))
          ;; :action 'helm-google-suggest-actions  ; commented out old default action (search on google), as i want default action = insert the string, like helm-dabbrev
          :action '(("insert" . (lambda (cands)
                                  (with-helm-current-buffer
                                    (if (region-active-p) (delete-region (region-beginning) (region-end)))  ; added to delete region if any.
                                    (insert cands)))))
          :volatile t
          :match 'identity  ; to disable filter https://groups.google.com/forum/#!topic/emacs-helm/dgtL2Pe_PXI
          :fuzzy-match t
          :keymap helm-map
          :requires-pattern 3
          ))

  (defun helm-previous-10-line ()
    (interactive)
    (let ((old-pos (helm-candidate-number-at-point)))
      (helm-previous-line 10)
      (if (= old-pos (helm-candidate-number-at-point))  ; jump to previous section if at the top
          (helm-previous-line))))
  (defun helm-next-10-line ()
    (interactive)
    (let ((old-pos (helm-candidate-number-at-point)))
      (helm-next-line 10)
      (if (= old-pos (helm-candidate-number-at-point))  ; jump to next section if at the bottom
          (helm-next-line))))

  ;; Better Helm https://github.com/hatschipuh/better-helm
  ;; no (helm-ido-like-hide-modelines), ok to show buffer modelines
  ;; no (helm-ido-like-hide-helm-modeline), as helm-modeline may have useful info, e.g., num candidates

  ;; helm-ido-like-load-ido-like-bottom-buffer
  ;; popup helm-buffer at the bottom
  (setq helm-split-window-in-side-p t)
  (add-to-list 'display-buffer-alist
               '("\\`\\*helm.*\\*\\'"
                 (display-buffer-in-side-window)
                 (window-height . 0.3)))
  ;; same for helm swoop
  (setq helm-swoop-split-with-multiple-windows nil
        helm-swoop-split-direction 'split-window-vertically
        helm-swoop-split-window-function 'helm-default-display-buffer)
  ;; dont display the header line
  (setq helm-display-header-line nil)
  ;; input in header line
  (setq helm-echo-input-in-header-line t)
  (add-hook 'helm-minibuffer-set-up-hook 'helm-hide-minibuffer-maybe)

  ;; Backspace goes to the upper folder if you are not inside a filename, and Return will select a file or navigate into the directory if it is one.
  (defun helm-ido-like-find-files-up-one-level-maybe ()
    (interactive)
    (if (looking-back "/" 1)
        (call-interactively 'helm-find-files-up-one-level)
      (delete-char -1)))
  (defun helm-ido-like-find-files-navigate-forward ()
    "Adjust how helm-execute-persistent actions behaves, depending on context."
    (interactive)
    (let ((sel (helm-get-selection)))
      (if (file-directory-p sel)
          ;; the current dir needs to work to
          ;; be able to select directories if needed
          (cond ((and (stringp sel)
                      (string-match "\\.\\'" (helm-get-selection)))
                 (helm-maybe-exit-minibuffer))
                (t
                 (helm-execute-persistent-action)))
        (helm-maybe-exit-minibuffer))))
  (with-eval-after-load 'helm-files
    (define-key helm-read-file-map (kbd "<backspace>") 'helm-ido-like-find-files-up-one-level-maybe)
    (define-key helm-read-file-map (kbd "DEL") 'helm-ido-like-find-files-up-one-level-maybe)
    (define-key helm-find-files-map (kbd "<backspace>") 'helm-ido-like-find-files-up-one-level-maybe)
    (define-key helm-find-files-map (kbd "DEL") 'helm-ido-like-find-files-up-one-level-maybe)
    (define-key helm-find-files-map (kbd "<return>") 'helm-ido-like-find-files-navigate-forward)
    (define-key helm-read-file-map (kbd "<return>") 'helm-ido-like-find-files-navigate-forward)
    (define-key helm-find-files-map (kbd "RET") 'helm-ido-like-find-files-navigate-forward)
    (define-key helm-read-file-map (kbd "RET") 'helm-ido-like-find-files-navigate-forward))

  :bind
  ("M-w"     . helm-my-mini)  ; so that switch to prev file = M-w M-q
  ("M-q M-r" . helm-my-imenu)
  ("M-q M-v" . helm-show-kill-ring)
  ("M-q M-g" . helm-google-suggest-region-or-symbol)
  ("M-q M-f" . helm-grep-cur-dir-tree)
  ;; ("<f1> a" . helm-apropos)  ; the same as helm-mode-describe-function?
  ("C-o" . helm-find-files)  ; original C-o runs the command open-line ≈ C-e RET
  ("M-q M-w" . helm-M-x)     ; if use M-x, M-X would be wasted.  Avoid using M-x / C-c M-x, as M-x = delete line
  ("M-q <tab>" . helm-dabbrev)  ; original M-/ runs the command dabbrev (not very useful if we have company), company doesn't include dabbrev
  ;; TODO helm-all-mark-rings cannot show context lines? https://github.com/emacs-helm/helm/issues/261
  ("M-q M-m" . helm-mark-ring)  ; or helm-all-mark-rings?
  :bind  ; cannot combine with above list in v25, due to: "Wrong type argument: listp, :map"
  (:map helm-map  ; defined in https://github.com/emacs-helm/helm/blob/master/helm.el
        ;; rule: unify movement shortcuts for helm and buffer
        ;; helm-yank-text-at-point is not useful, buggy in helm-swoop
        ("<up>"   . helm-previous-line)
        ("<down>" . helm-next-line)
        ("M-g" . helm-map-M-g)
        ("M-p" . helm-previous-line)
        ("M-n" . helm-next-line)
        ("M-i" . helm-previous-source)
        ("M-u" . helm-next-source)
        ("M-k" . helm-previous-10-line)
        ("M-j" . helm-next-10-line)
        ("M-," . previous-history-element)
        ("M-m" . next-history-element)  ; good to insert default-input / word under cursor https://emacs.stackexchange.com/questions/17754 https://github.com/emacs-helm/helm/issues/491
        ("M-q M-i" . helm-previous-page)
        ("M-q M-u" . helm-next-page)
        ("M-z" . helm-select-action)  ; list actions, useful?
        ("<backtab>" . helm-previous-line)
        ("<tab>" . helm-next-line)       ; = Chrome address bar, also enable left-hand-only-mode, not compatible with yasnippet?
        ("M-SPC" . helm-copy-to-buffer)  ; for helm-google-suggest.  M-SPC originally bind to helm-toggle-visible-mark, which can be called by C-SPC.
        ("M-q M-k" . helm-beginning-of-buffer)
        ("M-q M-j" . helm-end-of-buffer)
        ("M-w" . helm-maybe-exit-minibuffer)
        ("RET" . helm-maybe-exit-minibuffer)
        ("M-RET" . helm-maybe-exit-minibuffer)  ; convenient as Meta is on usually inside helm
        ("C-w" . helm-buffer-run-kill-buffers)  ; only applicable in helm-buffer
        ("C-g" . helm-keyboard-quit)
        )
  :bind
  (:map helm-find-files-map  ; https://github.com/emacs-helm/helm/blob/master/helm-files.el
        ("M-p" . nil)  ; originally bind to helm-ff-run-switch-to-history
        ("C-o" . helm-point-file-in-dired)
        )
  :bind
  (:map shell-mode-map
        ("M-q M-e" . helm-comint-input-ring)  ; similar to C-r in shell for reverse-search-history
        )
  :bind
  (:map helm-generic-files-map
        ("M-i" . nil)  ; originally bind to helm-ff-properties-persistent (which shows file properties)
        ))

;; https://github.com/cute-jumper/ace-jump-helm-line
(use-package ace-jump-helm-line
  :after helm  ; instead of (with-eval-after-load 'helm ...)
  :config
  ;; doesn't load package if use ":bind" here, ref: https://github.com/syl20bnr/spacemacs/blob/master/layers/%2Bcompletion/helm/packages.el
  (define-key helm-map (kbd "M-;") 'ace-jump-helm-line)
  (setq ace-jump-helm-line-default-action 'select)
  )

(use-package helm-c-yasnippet
  :config
  (setq helm-yas-display-key-on-candidate t)  ; to learn the snippet key
  :bind
  ("M-q M-t" . helm-yas-complete)
  )

;; helm-swoop looks like occur, but moving simultaneously. https://plus.sandbox.google.com/107988747892328588078/posts/38NZQR9EBAa
;; helm-swoop is better than isearch C-s + risearch C-r
;; http://sachachua.com/blog/2015/01/emacs-kaizen-helm-swoop-editing/
;; https://github.com/ShingoFukuyama/helm-swoop/#configure-pre-input-search-query
;; https://github.com/ShingoFukuyama/helm-swoop#config
;; TODO how about selected pre-input so that typing any char will replace it?
;; While searching, C-c C-e activates edit-mode, in which you can edit the buffer from within the helm-swoop buffer
;; helm-resume http://emacs.stackexchange.com/questions/10701/ cannot keep previous helm-swoop keyword if there is any other helm used
;; helm-multi-swoop needs to first select files for search
;; helm-swoop-back-to-last-point should be superseeded by "move back"
;; alt: isearch+ https://www.emacswiki.org/emacs/IsearchPlus
(use-package helm-swoop  ; and isearch
  :init  ; for isearch
  (setq search-whitespace-regexp "[-_ \t\n]+")  ; for isearch-forward, make these equivalent: space newline tab hyphen underscore http://ergoemacs.org/emacs/emacs_make_modern.html

  (add-hook 'isearch-mode-end-hook
            (lambda ()
              (when (and isearch-success (> (length isearch-string) 0)) (my-shift-select isearch-other-end))
              (my-recenter-no-redraw)))
  (defun my-isearch-with-region ()
    "Use region as the isearch text. This pairs nicely with expand-region. http://stackoverflow.com/a/32002122/550243"
    (when mark-active
      (let ((region (funcall region-extract-function nil)))
        (deactivate-mark)
        (isearch-push-state)
        (isearch-yank-string region))))
  (add-hook 'isearch-mode-hook #'my-isearch-with-region)

  ;; http://stackoverflow.com/questions/11052678/emacs-combine-iseach-forward-and-recenter-top-bottom
  (defadvice isearch-repeat-forward (after isearch-repeat-forward-recenter activate) (my-recenter-no-redraw))
  (defadvice isearch-repeat-backward (after isearch-repeat-backward-recenter activate) (my-recenter-no-redraw))
  (ad-activate 'isearch-repeat-forward)
  (ad-activate 'isearch-repeat-backward)

  :config
  (defun helm-my-swoop()  ; http://stackoverflow.com/questions/38823152
    (interactive)
    ;; (setq minibuffer-default (funcall minibuffer-default-add-function))
    (setq search-ring (let ((minibuffer-history search-ring)) (helm-swoop) minibuffer-history)))  ; re-use isearch history

  :bind
  ("M-g" . helm-my-swoop)  ; M-g is easier than C-f, and swoop is more useful
  ("M-G" . helm-multi-swoop-all)
  ("C-f" . isearch-forward)
  ("C-M-f" . isearch-backward)

  :bind
  (:map helm-swoop-map
        ("M-i" . nil)  ; originally bind to helm-multi-swoop-all-from-helm-swoop
        ("C-a" . helm-multi-swoop-all-from-helm-swoop)
        ("M-G" . helm-previous-line)
        )

  :bind
  (:map isearch-mode-map
        ("C-s" . nil)
        ("C-r" . nil)
        ("C-w" . nil)
        ("M-y" . nil)
        ("M-n" . nil)
        ("M-p" . nil)
        ("M-e" . nil)
        ("M-j" . isearch-ring-advance)
        ("M-k" . isearch-ring-retreat)
        ("M-g" . helm-swoop-from-isearch)
        ("C-d" . my-isearch-yank-word-or-char-from-beginning)
        ("C-v" . isearch-yank-kill)  ; http://tahirhassan.blogspot.com/2014/01/emacs-cua-mode-and-isearch.html
        ("C-f" . isearch-repeat-forward)
        ("C-M-f" . isearch-repeat-backward)
        ("<escape>" . isearch-abort)
        ("<backspace>" . isearch-del-char)  ; https://www.reddit.com/r/emacs/comments/2adj9w
        ("M-SPC" . isearch-edit-string)
        )
  )

(use-package swiper
  :config
  (setq swiper-action-recenter t)
  :bind
  ("M-q G" . swiper)
  )
;; https://github.com/abo-abo/swiper-helm
(use-package swiper-helm
  :config
  (add-to-list 'helm-boring-buffer-regexp-list "\\`\\*swiper")  ; https://www.reddit.com/r/emacs/comments/4r2p0f/hide_some_buffers_in_helmbufferslist/
  (setq swiper-helm-display-function 'helm-default-display-buffer)
  (defun helm-my-swiper ()  ; use minibuffer-history by default
    (interactive)
    (if (and (buffer-file-name) (not (ignore-errors (file-remote-p (buffer-file-name)))) (> (buffer-size) 10000000))  ; use grep for large file (> 10MB), otherwise reboot is needed, ref: https://github.com/abo-abo/swiper/issues/416
        (progn
          (when (and (buffer-modified-p) (y-or-n-p "Save before grep? "))
            (save-buffer))
          (setq  ; re-use minibuffer-history history
           minibuffer-history
           (let ((helm-grep-history minibuffer-history))
             (helm-do-grep-1
              (list (buffer-file-name))
              nil  ; not recurse
              nil
              '("*")  ; all extensions
              (if (use-region-p) (buffer-substring-no-properties (region-beginning) (region-end)))
              )
             helm-grep-history)))
      (swiper-helm (if (use-region-p) (buffer-substring-no-properties (region-beginning) (region-end))))))
  (add-to-list 'display-buffer-alist
               '("\\`\\*swiper.*\\*\\'"
                 (display-buffer-in-side-window)
                 (window-height . 0.3)))
  :bind
  ("M-q g" . helm-my-swiper))

;; https://emacs.stackexchange.com/questions/975/getting-number-of-occurrences-during-incremental-search-c-s-isearch-forward https://stackoverflow.com/questions/22479140/how-to-display-number-of-matches-in-incremental-search
(use-package anzu
  :init
  (global-anzu-mode)
  :config
  (setq anzu-cons-mode-line-p nil)
  (global-set-key [remap query-replace] 'anzu-query-replace)
  (global-set-key [remap query-replace-regexp] 'anzu-query-replace-regexp))

;; (use-package isearch
;;   :ensure nil
;;   :bind
;;   (:map isearch-mode-map
;;         ("M-j" . isearch-ring-advance)
;;         ("M-k" . isearch-ring-retreat)
;;         :map minibuffer-local-isearch-map
;;         ("M-j" . next-history-element)
;;         ("M-k" . previous-history-element))
;;   :config
;;   (setq
;;    isearch-allow-scroll t
;;    lazy-highlight-cleanup nil
;;    lazy-highlight-initial-delay 0))

;; Region set by expand-region doesn't get unset by motion https://github.com/magnars/expand-region.el/issues/185 my workaround: unset region after copy
(use-package expand-region  ; and selection / mark
  :init
  (defun my-shift-select (other-end)  ; selection's highlight should override flycheck's http://stackoverflow.com/questions/38912681/programmatically-do-shift-selection
    (let ((pos (point)))
      (goto-char other-end)
      (setq-local transient-mark-mode (cons 'only (unless (eq transient-mark-mode 'lambda) transient-mark-mode)))
      (cl-letf (((symbol-function 'message) #'ignore)) (push-mark nil nil t))  ; suppress message http://stackoverflow.com/questions/41328667
      (goto-char pos)))

  (defun convert-to-shift-selection ()
    (when (use-region-p)
      (let ((pos (point)))
        (goto-char (mark))
        (setq-local transient-mark-mode (cons 'only (unless (eq transient-mark-mode 'lambda) transient-mark-mode)))
        (cl-letf (((symbol-function 'message) #'ignore)) (push-mark nil nil t))  ; suppress message http://stackoverflow.com/questions/41328667
        (goto-char pos))))

  (defadvice er/expand-region (around fill-out-region activate)
    (if (or (eq last-command 'er/expand-region) (and (not (eq (get-text-property (point) 'face) 'org-date)) (not (eq (get-text-property (1- (point)) 'face) 'org-date))))
        (progn ad-do-it (convert-to-shift-selection))
      ;; https://emacs.stackexchange.com/questions/38048/function-to-highlight-timestamp-generated-from-org-time-stamp
      (let ((beg (previous-property-change (point) nil (line-beginning-position))))
        (when (= beg (line-beginning-position)) (setq beg (point)))
        (goto-char beg)
        (let ((end (next-property-change beg nil (line-end-position))))  ; if replace beg by (point), then "end = true end + 1" when "point = end - 1"
          (my-shift-select (1+ end))
          ))))

  (defun your-mark-org-timestamp ()
    (interactive)
    ;; (when (eq (get-text-property (point) 'face) 'org-date)
    (when (or (eq (get-text-property (point) 'face) 'org-date) (eq (get-text-property (1- (point)) 'face) 'org-date))
      (let ((beg (previous-property-change (point) nil (line-beginning-position))))
        (when (= beg (line-beginning-position)) (setq beg (point)))
        (goto-char beg)
        (let ((end (next-property-change beg nil (line-end-position))))  ; if replace beg by (point), then "end = true end + 1" when "point = end - 1"
          (my-shift-select (1+ end))
          ))))

  (defun goto-closest-imenu-item (direction)
    "Jump to the closest imenu item on the current buffer.
     If direction is 1, jump to next imenu item.
     If direction is -1, jump to previous imenu item.
     See https://emacs.stackexchange.com/questions/30673
     Adapted from `which-function' in https://github.com/typester/emacs/blob/master/lisp/progmodes/which-func.el"
    (setq imenu--index-alist nil) (imenu--make-index-alist t)  ; to force rescan, (setq imenu-auto-rescan t) doesn't work, ref: https://github.com/emacs-helm/helm/blob/master/helm-imenu.el
    (let ((alist imenu--index-alist)
          (minoffset (point-max))
          offset pair mark imstack destination)
      ;; Elements of alist are either ("name" . marker), or
      ;; ("submenu" ("name" . marker) ... ). The list can be
      ;; arbitrarily nested.
      (while (or alist imstack)
        (if alist
            (progn
              (setq pair (car-safe alist)
                    alist (cdr-safe alist))
              (cond ((atom pair))     ; skip anything not a cons
                    ((imenu--subalist-p pair)
                     (setq imstack   (cons alist imstack)
                           alist     (cdr pair)))
                    ((number-or-marker-p (setq mark (cdr pair)))
                     (if (> (setq offset (* (- mark (point)) direction)) 0)
                         (if (< offset minoffset)  ; find the closest item
                             (setq minoffset offset
                                   destination mark))))))
          (setq alist     (car imstack)
                imstack   (cdr imstack))))
      (when destination (imenu-default-goto-function "" destination ""))))
  (bind-key "M-u"
            (lambda (arg)
              (interactive "^p")
              (cond ((eq major-mode 'shell-mode) (comint-next-prompt arg))
                    ((eq major-mode 'org-mode) (org-next-visible-heading arg))
                    (t (goto-closest-imenu-item 1)))
              (my-recenter-no-redraw)))
  (bind-key "M-i"
            (lambda (arg)
              (interactive "^p")
              (cond ((eq major-mode 'shell-mode) (comint-previous-prompt arg))
                    ((eq major-mode 'org-mode) (org-previous-visible-heading arg))
                    (t (goto-closest-imenu-item -1)))
              (my-recenter-no-redraw)))

  (defun my-goto-buffer-beg () (interactive "^") (beginning-of-buffer))
  (defun my-goto-buffer-end () (interactive "^") (end-of-buffer))
  (defun my-shift-select-all () (interactive) (mark-whole-buffer) (convert-to-shift-selection))

  :bind
  ("C-a" . my-shift-select-all)
  ("M-2" . your-mark-org-timestamp)
  ("M-r" . er/expand-region)    ; original M-r runs the command move-to-window-line-top-bottom
  ("M-R" . er/contract-region)
  ("C-<up>"   . my-goto-buffer-beg)  ; same as sublime and http://codemirror.net
  ("C-<down>" . my-goto-buffer-end)
  ("M-q M-k" . my-goto-buffer-beg)  ; use C-c instead of M-q so that it doesn't conflict with helm keymap
  ("M-q M-j" . my-goto-buffer-end)
  )

;; (use-package wrap-region
;;   :config
;;   (defun wrap-region-trigger (arg key)
;;     "Called when trigger key is pressed."
;;       (let* ((deactivate-mark nil) (wrapper (wrap-region-find key)))
;;         (if (and wrapper
;;                  (region-active-p)
;;                  (if wrap-region-only-with-negative-prefix (< arg 0) t))
;;             (if (wrap-region-insert-tag-p key)
;;                 (wrap-region-with-tag)
;;               (wrap-region-with-punctuations
;;                (wrap-region-wrapper-left wrapper)
;;                (wrap-region-wrapper-right wrapper)))
;;           (wrap-region-fallback key))))
;;   (wrap-region-global-mode t)
;;   (setq wrap-region-only-with-negative-prefix t)
;;   )

;; https://github.com/zk-phi/phi-search
;; C-s / C-r phi-search-again
;; RET phi-search-complete
;; C-RET phi-search-complete-at-beginning
;; issues: no history; C-h/l can't cancel, while C-f/b can
(use-package phi-search
  :bind
  (:map phi-search-default-map
        ("C-l" . nil)  ; originally bind to phi-search-recenter
        ("M-v" . nil)  ; originally bind to phi-search-scroll-down
        ("M-i" . phi-search-scroll-up)
        ("M-u" . phi-search-scroll-down)
        ))

;; default location for the mc/list-file: ~/.emacs.d/.mc-lists.el http://emacs.stackexchange.com/questions/9915
;; mc/edit-lines is useful (only) when it's easy to select a big region
;; ✔ instant feedback https://news.ycombinator.com/item?id=15199642
(use-package multiple-cursors  ; mc
  :defer 0  ; otherwise config won't be involved when calling mc/edit-lines
  :config
  (defun mc/cursor-is-bar () nil)  ; mc/cursor-is-bar doesn't work on Mac Emacs 25.1.1 https://github.com/magnars/multiple-cursors.el/pull/253
  (set-face-foreground 'mc/cursor-face "brown")  ; default is black, which is the same as normal text foreground
  (setq mc/always-run-for-all t)  ; to "run-for-all" for every command except for those that are listed in mc/cmds-to-run-once https://github.com/magnars/multiple-cursors.el#unknown-commands
  (setq-local mc-count "")  ; init value == "" to minimize less useful info on modeline, "local" to make it buffer-dependent
  (put 'mc-count 'risky-local-variable t)  ; needed to show and update the count dynamically

  (setq original-cursor-color (face-background 'cursor))
  (defun mc-set-cursor ()  ; http://emacs-fu.blogspot.com/2009/12/changing-cursor-color-and-shape.html
    (if multiple-cursors-mode  ; cannot use (> (mc/num-cursors) 1) which doesn't work after left/right movement with num-curors==2
        (set-cursor-color "red")  ; to warn user that mc mode is on
      (set-cursor-color original-cursor-color)))
  (add-hook 'post-command-hook 'mc-set-cursor)

  (add-hook 'multiple-cursors-mode-enabled-hook  (lambda () (setq-local mc-count mc/mode-line)))
  (add-hook 'multiple-cursors-mode-disabled-hook (lambda () (setq-local mc-count "")))
  ;; https://emacs.stackexchange.com/questions/20987/multiple-cursors-package-makes-emacs-run-slow
  (add-to-list 'mc/unsupported-minor-modes 'highlight-thing-mode)  ; highlight-thing-mode will override mc selection highlight, confusing esp. if you use ctrl-d

  ;; https://github.com/magnars/multiple-cursors.el/issues/75
  ;; don't use advice-add on mc/mark-next-like-this, which is used by many other mc functions.
  ;; ;;d cycle to the last cursor at the beginning of each fun
  (defun my-mc/mark-next-like-this-with-cycle ()
    (interactive)
    (mc/mark-next-like-this 1)
    (mc/cycle-forward))
  (defun my-mc/unmark-next-like-this-with-cycle ()
    (interactive)
    (if (= (mc/num-cursors) 1)
        (error "Only one cursor.")
      (mc/cycle-backward)
      (mc/unmark-next-like-this)))
  (defun my-mc/skip-to-next-like-this-with-cycle ()
    (interactive)
    (if (= (mc/num-cursors) 1)
        (error "Only one cursor.")
      (mc/cycle-backward)
      (mc/skip-to-next-like-this)
      (mc/cycle-forward)))
  ;; ref: https://github.com/magnars/multiple-cursors.el/blob/master/mc-mark-more.el
  ;; ✘ cannot wrap back to buffer beginning (coz set of cursors is represented by a continuous region), on the contrary, evil-ex-find-next is ok, see https://github.com/LonoCloud/evil/blob/master/evil-search.el
  (defun my-mc/forward-only-ctrl-d (&optional count)
    (interactive "p")
    (while (< 0 count)
      (if (region-active-p)
          (progn
            (ignore-errors (mc/mark-next-like-this 1))
            (if (> (mc/num-cursors) 1)
                (mc/cycle-forward)
              (message "No match.")))  ; reformat it to "Search failed: ..."
        (when (= (mc/num-cursors) 1)  ; C-d has no well-defined behavior when there is no region and in mc mode
          (mc--select-thing-at-point 'symbol)
          (if (region-active-p)
              (convert-to-shift-selection)
            (skip-chars-forward "\t ")  ; select \t and spaces if no word/symbol selected
            (let ((end (point)))
              (skip-chars-backward "\t ")
              (my-shift-select end)
              ))))
      (setq count (1- count))))
  (defun my-mc/redo-disabled ()
    (interactive)
    (error "Redo disabled, as it might screw with your cursors."))  ; https://github.com/magnars/multiple-cursors.el#known-limitations

  ;; use mc--default-cmds-to-run-once instead of mc/cmds-to-run-once, so that they won't be saved/duplicated in .mc-lists.el  =_single_config_file=
  (add-to-list 'mc--default-cmds-to-run-once #'my-mc/mark-next-like-this-with-cycle)
  (add-to-list 'mc--default-cmds-to-run-once #'my-mc/skip-to-next-like-this-with-cycle)
  (add-to-list 'mc--default-cmds-to-run-once #'my-mc/unmark-next-like-this-with-cycle)
  (add-to-list 'mc--default-cmds-to-run-once #'my-mc/forward-only-ctrl-d)
  (add-to-list 'mc--default-cmds-to-run-once #'my-mc/redo-disabled)
  (add-to-list 'mc--default-cmds-to-run-once #'nonshift-avy-goto-char)
  (add-to-list 'mc--default-cmds-to-run-once #'helm-M-x)      ; e.g., for mc/insert-numbers, mc/edit-lines
  (add-to-list 'mc--default-cmds-to-run-once #'helm-my-mini)  ; to switch file
  (add-to-list 'mc--default-cmds-to-run-once #'my-escape)
  (add-to-list 'mc--default-cmds-to-run-once #'my-prefix-cancel)

  :bind
  ("C-d" . my-mc/forward-only-ctrl-d)
  ;; no shift-selection needed -> can use C-M- modifiers
  ;; no shortcut for mc/mark-previous-like-this to simplify the UI, i.e., always need to start with top cursor
  ("C-M-n" . my-mc/mark-next-like-this-with-cycle)    ; as the movement is similar to M-n
  ("C-M-p" . my-mc/unmark-next-like-this-with-cycle)  ; = sublime's C-u https://www.sublimetext.com/docs/2/multiple_selection_with_the_keyboard.html
  ("C-M-j" . my-mc/skip-to-next-like-this-with-cycle) ; sublime uses C-k C-d, which is not convenient http://stackoverflow.com/questions/11548308
  ("M-q a"   . mc/mark-all-like-this)
  ("M-q M-a" . mc/mark-all-like-this-dwim)
  ("M-q M-n" . mc/edit-lines)  ; useful after selecting a block of text, e.g., by expand-region
  ("C-M-<down-mouse-1>" . mc/add-cursor-on-click)  ; "C-<mouse-1>" is used and cannot be overrided in both Mac and Ubuntu https://github.com/magnars/multiple-cursors.el#binding-mouse-events (C-M-n + mouse click has a similar effect)

  :bind
  (:map mc/keymap  ; https://github.com/magnars/multiple-cursors.el/blob/master/multiple-cursors-core.el
        ("C-s" . nil)
        ("C-r" . nil)
        ("C-v" . nil)
        ("RET" . nil)  ; originally bind to "disable mc mode"; set it to nil for inserting newline
        ("M-," . mc/cycle-backward)  ; original cmd my-prev-symbol is not useful in mc mode
        ("M-m" . mc/cycle-forward)
        ("C-f" . phi-search)
        ("C-M-f" . phi-search-backward)
        ("M-z" . my-mc/redo-disabled)
        ;; C-c prefix for mode specific keys
        ("C-c M-1" . mc/insert-numbers)  ; insert a number sequence (starting with 0) http://stackoverflow.com/questions/29838244
        ("C-c M-2" . mc/insert-letters)
        ("C-c M-3" . mc-hide-unmatched-lines-mode)
        ("M-q M-d" . undefined)
        ("M-q M-e" . undefined)
        ("M-q M-r" . undefined)
        ("M-q M-g" . undefined)
        ("M-q M-f" . undefined)
        ("M-q M-m" . undefined)
        ("M-:" . undefined)
        ))

;; kmacro-edit-lossage doesn’t work out of the box. It gives “Key sequence C-h l is not defined”.  This fixes the problem https://github.com/br4ndur/dot-emacs/blob/master/branduren.org
(defun kmacro-edit-lossage-fixed ()
  "Edit most recent 300 keystrokes as a keyboard macro."
  (interactive)
  (kmacro-push-ring)
  (edit-kbd-macro 'view-lossage))

;; can also use it to switch window https://emacs.stackexchange.com/questions/3458/how-to-switch-between-windows-quickly
;; https://github.com/abo-abo/avy/blob/master/avy.el
(use-package avy
  :config
  (setq avy-keys (number-sequence ?a ?z))
  (setq avy-case-fold-search nil)  ; do not ignore case
  (setq avy-dispatch-alist '((?C . avy-action-copy)))  ; ref: http://emacs.stackexchange.com/questions/27979/use-avy-to-select-distant-word-or-line-and-paste-at-point
  ;; (setq avy-background t) is too distracting
  (defun my-noshift-avy-goto-char ()
    (interactive)
    (deactivate-mark)
    (call-interactively 'avy-goto-char))
  (defun my-shift-avy-goto-char ()
    (interactive)
    (setq this-command-keys-shift-translated t)
    (handle-shift-selection)
    (call-interactively 'avy-goto-char))
  :bind
  ("M-;" . my-noshift-avy-goto-char)  ; original M-; runs the command comment-dwim, e.g., add an end-of-line comment in cc mode, not very useful.  Both the shortcut and the command needs 2 hands.
  ("M-:" . my-shift-avy-goto-char)
  ("M-q M-;" . avy-goto-line)  ; if you enter a digit for avy-goto-line, it will switch to goto-line with that digit already entered.
  )


;;; programming
;; Emacs配置C语言编程环境 - 牛牛龙 http://yulongniu.bionutshell.org/blog/2014/12/02/emacs-config-c/
;; emacs-pills :: compilation feedback with colors https://www.youtube.com/watch?v=ZnWN7htqT48

(defun bracket-delete ()
  "Delete backward 1 character, but if it's a \"quote\" or bracket ()[]{}【】「」 etc, delete bracket pair.
Modified from `http://ergoemacs.org/emacs/emacs_delete_backward_char_or_bracket_text.html'
Version 2017-07-02"
  (interactive)
  (cond
   ((looking-back "\\s)" 1)
    (xah-delete-backward-bracket-pair)
    )
   ((looking-back "\\s(" 1)
    (progn
      (backward-char)
      (forward-sexp)
      (xah-delete-backward-bracket-pair)
      ))
   ((looking-back "\\s\"" 1)
    (if (nth 3 (syntax-ppss))
        (progn
          (backward-char)
          (xah-delete-forward-bracket-pairs))
      (xah-delete-backward-bracket-pair)
      ))
   (t
    (delete-char -1))))
(bind-key "M-q <backspace>" 'bracket-delete)
(defun xah-delete-backward-bracket-pair ()
  "Delete the matching brackets/quotes to the left of cursor.
After the command, mark is set at the left matching bracket position, so you can `exchange-point-and-mark' to select it.
This command assumes the left of point is a right bracket, and there's a matching one before it.
What char is considered bracket or quote is determined by current syntax table.
URL `http://ergoemacs.org/emacs/emacs_delete_backward_char_or_bracket_text.html'
Version 2017-07-02"
  (interactive)
  (let (( $p0 (point)) $p1)
    (forward-sexp -1)
    (setq $p1 (point))
    (goto-char $p0)
    (delete-char -1)
    (goto-char $p1)
    (delete-char 1)
    (push-mark (point) t)
    (goto-char (- $p0 2))))
(defun xah-delete-forward-bracket-pairs ( &optional @delete-inner-text-p)
  "Delete the matching brackets/quotes to the right of cursor.
If *delete-inner-text-p is true, also delete the inner text.
After the command, mark is set at the left matching bracket position, so you can `exchange-point-and-mark' to select it.
This command assumes the char to the right of point is a left bracket or quote, and have a matching one after.
What char is considered bracket or quote is determined by current syntax table.
URL `http://ergoemacs.org/emacs/emacs_delete_backward_char_or_bracket_text.html'
Version 2017-07-02"
  (interactive)
  (if @delete-inner-text-p
      (progn
        (mark-sexp)
        (kill-region (region-beginning) (region-end)))
    (let (($pt (point)))
      (forward-sexp)
      (delete-char -1)
      (push-mark (point) t)
      (goto-char $pt)
      (delete-char 1))))

(global-font-lock-mode t)  ; turn on syntax highlighting
;; (setq jit-lock-defer-time 0.05)  ; improve the scrolling speed for large files https://stackoverflow.com/questions/18316665/ http://tsengf.blogspot.com/2012/11/slow-scrolling-speed-in-emacs.html
;; (setq fast-but-imprecise-scrolling t)  ; https://emacs.stackexchange.com/questions/31402/how-to-avoid-scrolling-with-large-files-hanging-for-short-periods-of-time-hold

;; disable all c-electric kbd, e.g., not to override C-d, not to override "=" which disabled delete-selection-mode
(setq c++-mode-map (make-keymap))
(setq c-mode-map (make-keymap))
(setq protobuf-mode-map (make-keymap))
(setq sh-mode-map (make-keymap))  ; e.g., '=' runs the command sh-assignment, which breaks mc with (delete-selection-mode t)
;; (setq-default c-electric-flag nil)
;; (add-hook 'prog-mode-hook (lambda () (use-local-map nil)))  ; https://emacs.stackexchange.com/questions/33177 disable all mode specific kbd for shorter mc cmd lists (c-mode-base-map has many c-electric kbds)

;; http://emacs.stackexchange.com/a/17021
(defun eval-last-sexp-and-replace ()
  (interactive)
  (let ((value (eval (preceding-sexp))))
    (kill-sexp -1)
    (insert (format "%s" value))))
(bind-key "M-q M-E" #'eval-last-sexp-and-replace)

(defun calc-region/line-and-append (&optional start end)
  "calc region/line, and insert result at the end."
  (interactive "r")
  (unless (use-region-p) (setq start (line-beginning-position)) (setq end (line-end-position)))
  (let ((result (calc-eval (replace-regexp-in-string "," "" (buffer-substring-no-properties start end)))))
    (goto-char end)
    (insert " = " result)))
(bind-key "M-q =" #'calc-region/line-and-append)

(defun save-and-run-current-file ()
  "Execute the current file.  For example, if the current buffer is the file x.py, then it'll call 「python x.py」 in a shell. File suffix is used to determine what program to run. If the file is modified or not saved, save it automatically before run. URL `http://ergoemacs.org/emacs/elisp_run_current_file.html' version 2016-01-28"
  (interactive)
  (let ((ξsuffix-map
         ;; (‹extension› . ‹shell program name›)
         `(("php" . "php")
           ("pl" . "perl")
           ("py" . "python")
           ("py3" . ,(if (string-equal system-type "windows-nt") "c:/Python32/python.exe" "python3"))
           ("rb" . "ruby")
           ("go" . "go run")
           ("js" . "node")  ; node.js
           ("sh" . "bash")
           ("clj" . "java -cp /home/xah/apps/clojure-1.6.0/clojure-1.6.0.jar clojure.main")
           ("ml" . "ocaml")
           ("vbs" . "cscript")
           ("tex" . "pdflatex")
           ("latex" . "pdflatex")
           ("java" . "javac")
           ("html" . "open")
           ))
        ξfname
        ξfSuffix
        ξprog-name
        ξcmd-str)
    (when (null (buffer-file-name)) (save-buffer))
    (when (buffer-modified-p) (save-buffer))
    (setq ξfname (buffer-file-name))
    (setq ξfSuffix (file-name-extension ξfname))
    (setq ξprog-name (cdr (assoc ξfSuffix ξsuffix-map)))
    (setq ξcmd-str (concat ξprog-name " \""   ξfname "\""))
    (cond
     ((string-equal ξfSuffix "el") (load ξfname))
     ((string-equal ξfSuffix "borg") (run-shell-command (format "borgcfg %s reload" ξfname)))
     ((string-equal ξfSuffix "java")
      (progn
        (shell-command ξcmd-str "*save-and-run-current-file output*" )
        (shell-command
         (format "java %s" (file-name-sans-extension (file-name-nondirectory ξfname))))))
     (t (if ξprog-name
            (progn
              (message "Running...")
              (run-shell-command  ξcmd-str)  ; allow user input, e.g., using sudo (requires password input)
              ;; (shell-command ξcmd-str "*save-and-run-current-file output*")
              )
          (message "No recognized program file suffix for this file."))))))
(global-set-key (kbd "<f5>") 'save-and-run-current-file)


(add-hook 'emacs-lisp-mode-hook
          (lambda ()  ; http://milkbox.net/note/single-file-master-emacs-configuration/
            (eldoc-mode)  ; to display emacs lisp documentation
            (add-hook 'after-save-hook #'check-parens nil t)  ; https://youtu.be/RvPFZL6NJNQ?t=14m31s
            (setq imenu-generic-expression '(("" "^(use-package \\([^ \n]+\\)" 1) ("" "^;;; \\(.+\\)$" 1)))))

(add-hook 'c++-mode-hook
          (lambda ()
            (setq
             ;; default regex problems: no template item, can extract function names, but no item in some .h file (e.g., sherlock_pipeline.h)
             ;; my regex: namespaces (beg + end) + classes + functions with no leading space on first line
             imenu-generic-expression
             '(;; first arg is "", coz nil -> have "Function" prefix for helm-imenu.
               ;; regex first char:  ^/ to exclude all comments, ^# to exclude all preprocessors
               ;; regex second char: ^E to exclude DEFINE* DECLARE* REGISTER*
               ;; regex the rest:    ^= to exclude static variables, must contain '(' or '{' to exclude short and long using statements
               ("" "^\\([^ #\t\n/][^=\n;E][^=\n;]+[\\({,:][^=\n]*\\)$" 1)
               ("" "^\\(TEST.+\\)$" 1)    ; for unit test macros
               ("" "^\\(class .+\\)$" 1)  ; for long class def line that doesn't has '{'
               ("" "^\\(} .+\\)$" 1)      ; for closing namespace "}  // ..."
               ))))

(add-hook 'protobuf-mode-hook
          (lambda ()
            (setq
             imenu-generic-expression
             '(
               ("" "^\\(message .+\\)$" 1)
               ))))

(defun py-help-at-point nil)  ; https://marc.info/?l=python-mode&m=142597554420133&w=2 emacs 25: `*Python Help*` buffer keeps coming up, whenever the cursor stays for more than a second on something. http://grokbase.com/t/python/python-mode/15173ahftp/python-help-buffer
(add-hook'python-mode-hook
 (lambda ()
   (setq tab-width 2)  ; o/w tab-width = 8 https://emacs.stackexchange.com/questions/17563
   (setq imenu-generic-expression '(("" "\\(^d.+\\)$" 1)))  ; first char = 'd' for "def "
   ))

;; (use-package android-mode
;;   :ensure t
;;   :config
;;   (setq android-mode-sdk-dir (getenv "ANDROID_HOME")))

;; https://stackoverflow.com/questions/9288181/converting-from-camelcase-to-in-emacs
;; https://github.com/akicho8/string-inflection
(use-package string-inflection
  :bind
  ;; it's common to run the command with forward-word (M-l)
  ("M-t" . string-inflection-ruby-style-cycle))  ; original M-t runs the command transpose-words

;; F7 for compilation (or 'eval-buffer', 'perl -c', etc.); I think it's the same as in VC++. There the distance is not so important - I hit the key with force, then lean back in my chair to see where it will lead me. http://emacs-fu.blogspot.com/2009/07/keyboard-shortcuts.html
(use-package compile
  :config
  (setq compilation-ask-about-save nil          ; save before compiling
        compilation-always-kill t               ; kill old compile processes before starting the new one
        compilation-scroll-output 'first-error  ; automatically scroll to first error
        )
  (add-to-list 'display-buffer-alist
               '("\\`\\*compilation\\*\\'"
                 (display-buffer-reuse-window display-buffer-at-bottom)
                 (window-height . 0.3)))
  ;; Colorize output of Compilation Mode, see http://stackoverflow.com/a/3072831/355252
  (require 'ansi-color)
  (add-hook 'compilation-filter-hook #'prelude-colorize-compilation-buffer)
  (defun prelude-colorize-compilation-buffer ()
    "Colorize a compilation mode buffer."
    (interactive)
    ;; we don't want to mess with child modes such as grep-mode, ack, ag, etc http://stackoverflow.com/questions/3072648
    (when (eq major-mode 'compilation-mode)
      (let ((inhibit-read-only t))
        (ansi-color-apply-on-region (point-min) (point-max)))))
  (defun next-error-cycle () (interactive) (condition-case nil (next-error) (error (next-error 1 t))) (my-recenter-no-redraw))  ; https://stackoverflow.com/questions/21125015
  :bind
  ("C-j" . next-error-cycle)
  ("C-k" . previous-error)
  :bind
  (:map compilation-mode-map  ; https://github.com/emacs-mirror/emacs/blob/master/lisp/progmodes/compile.el
        ("M-n" . nil)  ; originally bind to compilation-next-error
        ("M-p" . nil)  ; originally bind to compilation-previous-error
        ;; ("M-m" . compilation-next-error)  ; no shift support yet
        ;; ("M-," . compilation-previous-error)
        ("C-c C-c" . kill-compilation)  ; originally bind to compile-goto-error, change it to kill process, i.e., same as shortcut in shell.
        ("RET" . compile-goto-error)
        ))

;; (use-package flycheck  ; https://www.youtube.com/watch?v=iSL4wUKc8O4
;;   :config (hook-into-modes 'flycheck-mode '(prog-mode-hook)))

;; (use-package google-c-style)

;; http://emacs.stackexchange.com/questions/12530
;; don't use global-highlight-symbol-mode coz: it is Very slow in org-mode and disable at the beginning in all modes (bug due to incompatibility?) https://github.com/nschum/highlight-symbol.el/issues/11
;; old code for global mode: (define-globalized-minor-mode my-global-highlight-symbol-mode highlight-symbol-mode (lambda () (highlight-symbol-mode 1))) (my-global-highlight-symbol-mode 1)
;; xx highlight-symbol won't highlight selection https://github.com/nschum/highlight-symbol.el/issues/32
;; vv highlight-symbol-jump shows number of occurrences in minibuffer
;; don't use isearch, which needs an extra key to exit the isearch-mode
(use-package highlight-symbol
  :defer 5  ; otherwise, the first command takes significant delay
  :config
  (defun shift-select-next (str dir)
    (let ((case-fold-search nil))  ; set to case-sensitive
      (if (and (use-region-p) (if (< 0 dir) (< (point) (mark)) (< (mark) (point))))
          (exchange-point-and-mark))  ; fix no jumping when changing search direction
      (unless (re-search-forward str nil t dir)  ; 3rd param = t -> don't show message if not found
        (goto-char (if (< 0 dir) (point-min) (point-max)))  ; copied from highlight-symbol-jump for wrap around search
        (re-search-forward str nil nil dir)))
    ;; http://emacs.stackexchange.com/questions/14310/how-to-select-text-found-by-re-search-forward
    (if (> dir 0)  ; order mark and point according to the search direction, to enable repeated search.
        (progn (setf (point) (match-end 0)) (my-shift-select (match-beginning 0)))
      (setf (point) (match-beginning 0)) (my-shift-select (match-end 0))))
  (defun my-symbol-jump (dir)
    (if (use-region-p)
        (let ((str (buffer-substring-no-properties (region-beginning) (region-end))))
          (setf (point) (if (> dir 0) (region-end) (region-beginning)))
          (shift-select-next str dir)
          (highlight-symbol-count str t)
          (add-to-history minibuffer-history-variable str))
      (highlight-symbol-jump dir)  ; search symbol with boundaries, different from the default isearch
      (add-to-history minibuffer-history-variable (highlight-symbol-get-symbol)))  ; every search (e.g., C-n/p) update history to have a fast jump / re-search
    (my-recenter-no-redraw))
  (defun my-next-symbol () (interactive) (my-symbol-jump 1))
  (defun my-prev-symbol () (interactive) (my-symbol-jump -1))
  (defun my-next-symbol-from-history () (interactive) (shift-select-next (car minibuffer-history) 1))
  (defun my-prev-symbol-from-history () (interactive) (shift-select-next (car minibuffer-history) -1))
  :bind
  ("M-m" . my-next-symbol)
  ("M-," . my-prev-symbol)
  ("M-M" . my-next-symbol-from-history)
  ("M-<" . my-prev-symbol-from-history)
  )

;; https://github.com/fgeller/highlight-thing.el
;; alt: https://www.reddit.com/r/emacs/comments/5a2r14/how_to_get_emacs_to_highlight_all_instances_of/
(use-package highlight-thing
  :defer 3
  :config
  (setq highlight-thing-delay-seconds 0.1)
  (setq highlight-thing-case-sensitive-p t)
  (setq highlight-thing-what-thing 'symbol)
  (setq highlight-thing-prefer-active-region t)
  (setq highlight-thing-exclude-thing-under-point t)
  (set-face-attribute 'highlight-thing nil :background "yellow" :underline nil :inherit nil)
  (advice-add 'highlight-thing-get-active-region :filter-return (lambda (s) (when (> (length s) 2) s)))  ; add length check to avoid significant delay when selecting spaces
  (setq highlight-thing-excluded-major-modes '(org-mode))  ; e.g., highlighting "+" is a bit annoying
  (global-highlight-thing-mode)
  )

;; related: light-symbol-mode: https://github.com/ntschutta/emacs/blob/master/light-symbol.el http://emacsblog.org/2007/04/17/quick-tip-light-symbol-mode/
;; related: https://github.com/kmmbvnr/emacs-config/blob/master/elisp/auto-highlight-symbol.el
;; related: idle-highlight-mode is not compatible with hl-mode on Linux
;; (use-package idle-highlight-mode  :config (setq idle-highlight-idle-time 0))
;; (define-globalized-minor-mode my-global-idle-highlight-mode idle-highlight-mode (lambda () (idle-highlight-mode 1)))  ; http://stackoverflow.com/questions/16048231/how-to-enable-a-non-global-minor-mode-by-default-on-emacs-startup
;; (my-global-idle-highlight-mode 1)

(use-package company
  :defer 5
  :config
  (setq company-show-numbers t)  ; show quick-reference numbers in the tooltip. (Select a completion with M-1 through M-0.)
  (setq company-selection-wrap-around t)  ; to move from last row to first row
  (setq-default company-minimum-prefix-length 3)  ; "setq" is not enough.  company company-minimum-prefix-length should >1 to avoid interfering with "<s" expansion
  (add-hook 'company-mode-hook (lambda () (setq-default company-minimum-prefix-length 3)))  ; needed for Linux (bug?)
  (global-company-mode)  ; use company-mode everywhere
  :bind
  (:map company-active-map  ; see https://github.com/company-mode/company-mode/blob/master/company.el
        ;; rule: minimize num of mainstream kbds in company-active-map to minimize the workflow difference with and without company mode, use C-c prefix kbd for mode specific actions
        ("RET" . nil)    ; RET is originally bind to company-complete-selection
        ("M-n" . nil)
        ("M-p" . nil)
        ("C-h" . nil)    ; originally bind to company-show-doc-buffer (for the selected candidate), use f1 instead
        ("C-w" . nil)    ; originally bind to company-show-location = see its source
        ("C-s" . nil)    ; originally bind to company-search-candidates
        ("C-M-s" . nil)  ; originally bind to company-filter-candidates
        ;; Use arrow keys to select, then <tab> to complete after interaction
        ;; OR M-(digit) to quickly complete with one of the first 10 candidates.
        ("<tab>" . company-complete-selection)  ; originally bind to company-complete-common, which was not good as it made the workflow non-deterministic (one or two tabs).
        ("C-c o" . company-show-location)  ; is it useful? use left/right arrows? ;;d
        ("C-c g" . company-search-candidates)
        ("C-c f" . company-filter-candidates)))

;; new-shell-frame https://github.com/terrycojones/emacs-setup/blob/master/elisp/my-shell.el
;; Emacs shells or Tmux? : emacs https://www.reddit.com/r/emacs/comments/1yzk6n/emacs_shells_or_tmux/

;; goal: make Emacs shell more like a normal shell (except full screen commands like tmux)
;; ref: why I run shells inside Emacs https://snarfed.org/why_i_run_shells_inside_emacs
;; :kbd: C-M-l      go to the top of the last command, but used by Ubuntu for lock screen -> use C-up/down instead
;; :kbd: C-c C-c    send C-c to current subjob
(use-package shell
  :config
  (setq shell-file-name "bash")
  (setq comint-input-autoexpand 'input)       ; auto-expand history references
  (setq comint-prompt-read-only t)
  (setq comint-input-ignoredups t)            ; don't store successive identical inputs in the input history
  (setq comint-scroll-to-bottom-on-input t)   ; always insert at the bottom when typing
  (setq comint-scroll-to-bottom-on-output t)  ; auto scroll down for each new output
  (setq comint-get-old-input (lambda () ""))  ; what to run when i press enter on a line above the current prompt (default = grabs the current line)
  (setq comint-input-ring-size 10000)         ; max shell history size
  (setq comint-buffer-maximum-size 100000)    ; max length of the buffer in lines (original value = 1024)
  (setq comint-move-point-for-output t)  ; ??
  (set-face-attribute 'comint-highlight-prompt nil :inherit nil)  ; doesn't work?
  (setenv "PAGER" "cat")                      ; avoid "WARNING: terminal is not fully functional", e.g., when man man
  (setenv "BASH_ENV" "~/.bashrc")
  ;; do not add "-i" by (setq shell-command-switch "-ic") to source .bashrc http://stackoverflow.com/questions/12224909
  ;; which causes some bash warnings http://stackoverflow.com/questions/12224909 e.g., "bash: cannot set terminal process group (-1): Inappropriate ioctl for device"

  (add-to-list 'display-buffer-alist  ; ref: https://www.gnu.org/software/emacs/manual/html_node/elisp/Display-Action-Functions.html
               '("\\`\\*shell\\*\\'"
                 (display-buffer-reuse-window display-buffer-at-bottom)
                 (window-height . 0.3)))
  ;; related https://tsdh.wordpress.com/2011/10/12/a-quick-pop-up-shell-for-emacs/ https://www.emacswiki.org/emacs/ShellPop
  (defun sh-switch-and-cd ()
    (interactive)
    (let ((dir default-directory))
      (shell)
      (set-process-query-on-exit-flag (get-process "shell") nil)
      (comint-send-string (current-buffer) (concat " cd \"" dir "\"; echo cd \"" dir "\";\n"))))  ; begin with a space -> not saved in the history list

  ;; ref http://stackoverflow.com/questions/16638632/how-can-i-make-an-emacs-shell-command-output-buffer-always-follow-the-bottom
  (defun my-insertion-filter (proc string) (when (buffer-live-p (process-buffer proc)) (with-current-buffer (process-buffer proc) (insert string))))

  ;; modified from http://stackoverflow.com/a/7053298
  ;; "$  " -> inert the output into buffer.  Limitation of inline shell command: no bashrc (e.g., no alias), no long running command (coz the emacs would be freezed)
  (defun sh-send-region-or-line ()
    (interactive)
    (save-some-buffers t)  ; save all files, in case the shell command cause some problem.
    (let ((inline nil) min max command)
      (if (use-region-p)
          (setq min (region-beginning) max (region-end))
        (beginning-of-line)
        (if (string= "Shell-script" mode-name)  ; no prefix '$' in .sh files
            (setq min (point-at-bol))
          (if (setq min (search-forward "$  " (line-end-position) t))  ; t -> it's ok if no "$  "
              (setq inline t)  ; "$  " -> inert the output into buffer
            (setq min (search-forward "$ " (line-end-position) t))))  ; if no "$ " -> error
        (setq max (point-at-eol)))
      (if min (setq command (buffer-substring-no-properties min max))
        (shell)
        (user-error "No '$ ' prefix or selection."))
      (if inline
          (progn
            (forward-line)
            ;; old code: (let ((shell-command-switch "-lc")) (insert (shell-command-to-string command))) doesn't support timeout http://emacs.stackexchange.com/questions/10278
            ;; use "l" instead of "i" in shell-command-switch to avoid following errors: http://stackoverflow.com/questions/27581085 http://emacs.stackexchange.com/questions/5296
            ;;   "bash: cannot set terminal process group (-1): Inappropriate ioctl for device" "bash: no job control in this shell"
            (let ((proc (start-process "emacs-proc" (current-buffer) "bash" "-lc" command))) ;; start an async process
              (set-process-filter proc 'my-insertion-filter)
              (with-timeout (5 (kill-process proc))  ; on timeout, kill the process
                (while (process-live-p proc)  ; while process is running
                  (sit-for .05))  ; let emacs read events and run timmers (and check for timeout)
                (message "%s finished on time." command)))
            )
        (run-shell-command command)
        (forward-line)
        )))

  (defun run-shell-command (command)
    (let ((current-window (get-buffer-window (current-buffer))))
      (shell)
      (let* ((proc (get-process "shell")) (pid (process-running-child-p proc)))
        (when pid
          (let ((p-name (shell-command-to-string (format "ps -o comm= -p %d" pid))))  ; https://emacs.stackexchange.com/questions/40816
            (when (not (string-match "/usr/local/bin/gnubby-ssh" p-name))
              (error "Shell is busy running: %s" p-name))))
        (goto-char (process-mark proc))
        (insert command)  ; cannot do (append-to-buffer (get-buffer "*shell*") min max) due to error: append-to-buffer: Text is read-only
        (comint-send-input)  ; = enter in shell https://groups.google.com/forum/#!topic/gnu.emacs.help/MmMyB-WGxw0
        )
      (select-window current-window)))

  ;; Turn off "Active processes exist" warning http://emacs.stackexchange.com/questions/22275 https://github.com/thorrr/python-goodies/blob/master/python-goodies.el
  ;; because the shell exit prompt made me ignoring the first prompt when closing Emacs that I may sometimes miss important prompts.
  (add-hook 'comint-exec-hook (lambda () (set-process-query-on-exit-flag (get-buffer-process (current-buffer)) nil)))

  (add-hook
   'shell-mode-hook
   (lambda ()
     (when (window-system)  ; calling text-scale-decrease in terminal results in user-error: Cannot decrease the default face height more than it already is
       (text-scale-decrease 1))  ; for ls-ing long filenames
     (yas-minor-mode -1)
     (setq imenu-generic-expression '(("" ".+\$ \\(.+\\)$" 1)))))

  ;; use global keymap instead of org-mode-map, as shell command (e.g., complication) may appear in any file
  ;; not C-x C-e which is not convenience for sending region (which requires C-S-x)
  ;; tests:
  ;; $ ls
  ;; $  pwd
  :bind
  ("M-q M-s" . sh-send-region-or-line)
  ("M-q s"   . sh-switch-and-cd)
  :bind
  (:map comint-mode-map
        ("M-p" . nil)
        ("M-n" . nil)
        ("SPC" . comint-magic-space)
        ("C-<up>" . comint-previous-prompt)  ; = C-c C-p
        ("C-<down>" . comint-next-prompt)    ; = C-c C-n
        ("<up>" . comint-previous-input)  ; up/donw work like in shell
        ("<down>" . comint-next-input)
        ("M-." . comint-insert-previous-argument)  ; same as bash https://lists.gnu.org/archive/html/bug-gnu-emacs/2002-06/msg00303.html
        ("<tab>" . company-complete)
        )
  )

;; https://github.com/adamrt/sane-term
;; sane-term will cycle through term buffers, creating one if there are none.
;; sane-term-create will create a new term buffer.
(use-package sane-term
  :after term  ; to avoid: Symbol’s value as variable is void: term-mode-map
  :config
  ;; shell to use for sane-term
  (setq sane-term-shell-command "/bin/bash")
  ;; sane-term will create first term if none exist
  (setq sane-term-initial-create t)
  ;; `C-d' or `exit' will kill the term buffer.
  (setq sane-term-kill-on-exit t)
  ;; After killing a term buffer, not cycle to another.
  (setq sane-term-next-on-kill nil)

  (defun my-term-toggle-mode ()  ; http://joelmccracken.github.io/entries/switching-between-term-mode-and-line-mode-in-emacs-term/
    "Toggles term between line mode and char mode"
    (interactive)
    (if (term-in-line-mode)
        (progn
          (term-char-mode)
          (read-only-mode -1))
      (term-line-mode)
      (read-only-mode 1)))  ; term-line-mode is not good for editing https://emacs.stackexchange.com/questions/17085
  (defun term-send-tab  () (interactive) (term-send-raw-string "\C-i"))
  (defun cd (dir))  ; to avoid this error when ssh from mac to linux: error in process filter: No such directory found via CDPATH environment variable

  :bind
  (:map term-mode-map
        ("C-c C-c" . my-term-toggle-mode)  ; originally bind to term-interrupt-subjob, i.e., was same as shell-mode
        ("M-p" . nil)
        ("M-n" . nil)
        ("M-r" . nil)
        ("M-s" . nil)
        ("C-d" . nil)  ; originally bind to term-delchar-or-maybe-eof
        )
  (:map term-raw-map
        ;; forward all keystrokes except: copy/paste, switch window/buffer
        ("C-c C-c" . my-term-toggle-mode)  ; originally bind to term-send-raw
        ("<tab>" . term-send-tab)
        ("C-V" . term-paste)  ; https://stackoverflow.com/questions/2886184
        ("M-C" . my-copy)
        ("M-SPC" . nil)
        ("M-w" . nil)
        )
  )

;; https://www.emacswiki.org/emacs/UndoTree
;; http://ergoemacs.org/emacs/emacs_best_redo_mode.html http://pragmaticemacs.com/emacs/advanced-undoredo-with-undo-tree/ http://www.dr-qubit.org/emacs.php#undo-tree TOREAD
(use-package undo-tree
  :defer 0
  :config
  (global-undo-tree-mode)
  (setq undo-tree-visualizer-timestamps t
        undo-tree-visualizer-diff t
        undo-tree-enable-undo-in-region nil)  ; disable region-restricted undo/redo, which is unpredictable, not available in most common editors, and buggy https://emacs.stackexchange.com/questions/37393
  (add-hook 'undo-tree-visualizer-mode-hook (lambda () (setq undo-tree-visualizer-diff t)))  ; https://mail.google.com/mail/u/0/?zx=77451guns65d#sent/1566d68e1a50afc6
  :bind
  (:map undo-tree-map
        ("C-/" . nil)
        ("C-_" . nil)
        ("C-?" . nil)
        ("M-_" . nil)
        ("C-M-z" . undo-tree-visualize)
        ("C-z" . undo-tree-undo)
        ("M-Z" . undo-tree-undo)
        ("C-y" . undo-tree-redo)  ; original C-y runs the command yank
        ("M-z" . undo-tree-redo)  ; original M-z runs the command zap-to-char; cannot use C-S-z which is not available in TTY
        )
  :bind
  (:map undo-tree-visualizer-mode-map
        ;; make undo-tree-visualizer as a version selector (e.g., like helm-swoop as a location selector)
        ("C-g" . undo-tree-visualizer-abort)  ; = no change on buffer
        ([return] . undo-tree-visualizer-quit)
        ))

;; http://emacs.stackexchange.com/questions/17710/use-package-with-config-to-set-variables
(use-package org
  :mode (("\\.txt$" . org-mode))  ; Google Drive doesn't support viewing .org file -> org files use .txt extension
  :ensure org-plus-contrib  ; https://www.reddit.com/r/emacs/comments/5sx7j0/how_do_i_get_usepackage_to_ignore_the_bundled/
  :config
  (setq
   org-adapt-indentation nil         ; prevent demoting heading also shifting text inside sections
   org-confirm-babel-evaluate nil
   org-descriptive-links nil
   org-edit-src-content-indentation 0
   org-edit-src-persistent-message nil
   org-ellipsis "⤵"                 ; a little downward-pointing arrow instead of the usual ellipsis (...) that org displays when there’s stuff under a header
   org-fontify-whole-heading-line t  ; fontify the whole line for headings (with a background color). https://github.com/fniessen/emacs-leuven-theme/blob/master/README.org
   org-link-search-must-match-exact-headline nil  ; do fuzzy text search http://orgmode.org/manual/External-links.html e.g., file:learn.txt::SRS
   org-src-fontify-natively t     ; pretty fontification of source code blocks http://orgmode.org/worg/org-contrib/babel/examples/fontify-src-code-blocks.html
   org-src-tab-acts-natively t    ; TAB as if it were used in the language’s major mode
   org-src-window-setup 'current-window  ; when editing a code snippet, use the current window rather than popping open a new one
   org-startup-folded nil            ; http://emacs.stackexchange.com/questions/9709/keep-the-headlines-expanded-in-org-mode
   org-startup-truncated nil
   )

  (modify-syntax-entry ?' "." org-mode-syntax-table)  ; to fix syntax subword movement (inconsistent between forward and backward) for words', origin (wrong) value = "w p"?
  (modify-syntax-entry ?/ "." org-mode-syntax-table)  ; so that the cursor will stop in the middle of "=/"
  (modify-syntax-entry ?\; "w" org-mode-syntax-table) ; for ;tag syntax

  (put 'org-force-self-insert 'delete-selection t)  ; for inserting '|' inside org-table with region (if any) deletion
  (setf org-blank-before-new-entry '((heading . nil) (plain-list-item . nil)))  ; to avoid newline after M-RET at first list item http://stackoverflow.com/questions/28351465

  ;; Changing format in org-emphasis-alist doesn't affect exports http://emacs.stackexchange.com/questions/5889/
  ;; (add-to-list 'org-emphasis-alist '("*" (:foreground "red")))  ; no bold, as bold doesn't work well for Chinese characters on Mac emacs
  ;; (add-to-list 'org-emphasis-alist '("_" (:foreground "#F08000")))  ; dark orange, no underline which decrease readability

  (org-add-link-type
   "image-url"
   (lambda (path)
     (let ((img (expand-file-name
                 (concat (md5 path) "." (file-name-extension path))
                 temporary-file-directory)))
       (if (file-exists-p img)
           (find-file img)
         (url-copy-file path img)
         (find-file img)))))

  ;; http://stackoverflow.com/questions/22491823/disable-certain-org-mode-markup
  (setq org-emphasis-alist
        '(("*" (:foreground "red") "<b>" "</b>")
          ;; ("/" italic "<i>" "</i>")  i didn't use italic
          ("_" (:foreground "dark orange") "<span style=\"text-decoration:underline;\">" "</span>")
          ("~" org-code "<code>" "</code>" verbatim)
          ("=" org-verbatim "<code>" "</code>" verbatim)
          ))

  (defvar helm-my-org-in-buffer-headings-history nil)
  (defun helm-my-org-in-buffer-headings()
    (interactive)
    (deactivate-mark)  ; to avoid distorted region with (setq cua-keep-region-after-copy t)
    (setq helm-my-org-in-buffer-headings-history
          (let ((minibuffer-history helm-my-org-in-buffer-headings-history))
            (helm-org-in-buffer-headings) minibuffer-history)))
  (with-eval-after-load "helm-org"  ; use eval-after-load to make sure the new defun overrides the old one
    ;; related: worf-goto http://stackoverflow.com/questions/28030946/emacs-org-mode-search-only-headers
    ;; (define-key helm-org-headings-map (kbd "M-1") (lambda () (interactive) (helm-set-pattern "^\\*\\ ")))  ; show top level headings only
    ;; redefine helm-source-org-headings-for-files to add ":follow 1", original function: https://github.com/emacs-helm/helm/blob/master/helm-org.el
    ;; TODO highlight line when following, like helm-swoop
    ;; OR use (symbol-value source)? http://emacs.stackexchange.com/questions/2563/
    (defun helm-source-org-headings-for-files (filenames &optional parents)
      (helm-make-source "Org Headings" 'helm-org-headings-class
        :parents parents
        :candidates filenames
        :follow 1)))

  (setq org-tag-persistent-alist
        '(("drill" . ?d)
          ("crypt" . ?c)))
  ;; TODO disable "*" bullet in any case
  (setq org-list-demote-modify-bullet '(("+" . "-") ("-" . "+")))  ; only use "+" and "-" for bullet, no "*" which is used for headings
  ;; alt: ▪

;;; org-babel
  ;; read user input http://stackoverflow.com/questions/37028455
  (org-babel-do-load-languages
   'org-babel-load-languages  ; http://orgmode.org/manual/Languages.html
   '((shell . t)  ; org-babel-load-languages now uses 'shell' instead of 'sh' https://lists.gnu.org/archive/html/emacs-orgmode/2016-04/msg00298.html
     (python . t)
     (emacs-lisp . t)
     (java . t)
     (C . t)  ; for C, C++ and D http://emacs.stackexchange.com/questions/17673 requires "#+BEGIN_SRC C++" instead of "#+BEGIN_SRC c++"
     ))
  (setq org-babel-default-header-args:python '((:results  . "output")))  ; https://www.reddit.com/r/orgmode/comments/6fsaz3
  (setq org-babel-default-header-args:C++  ; cannot be ":C"
        '((:includes . "<iostream> <string> <map>")  ; http://home.fnal.gov/~neilsen/notebook/orgExamples/org-examples.html#sec-18
          (:results . "verbatim")    ; o/w it may be formatted into a table
          (:flags   . "-std=c++11")  ; todo: switch to c++14
          ))

  ;; https://www.emacswiki.org/emacs/AddKeywords e.g., http://stackoverflow.com/a/28059832
  (font-lock-add-keywords
   'org-mode
   '(("\\(^ *\\$ \\)\\(.+\\)$"  ; a strict syntax for single-line shell cmd to avoid false pos.  Use babel block for multi-line shell command, as multi-line-font-locking is not trival http://stackoverflow.com/questions/9452615
      (1 '(:foreground "#00AA00") t)
      (2 'org-code t))  ; adding 't' at the end will override other font lock, e.g., no bold in `$ reload *xyz*` https://www.gnu.org/software/emacs/manual/html_node/elisp/Search_002dbased-Fontification.html
     ("\\(  // .+\\)"   ; "  // " = end of line comment, at least two spaces is best between code and comments
      (1 'font-lock-comment-face t))
     ("\\(^ *// .+\\)"  ; comment-only line
      (1 'font-lock-comment-face t))
     ;; unicode ref: http://www.fileformat.info/info/unicode/block/geometric_shapes/list.htm https://en.wikipedia.org/wiki/Geometric_Shapes
     ("^ *\\(+\\) " (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "▸"))))  ; "▶" is too distracting
     ("^ *\\(-\\) " (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "▹"))))
     ("\\(✘\\)" (1 '(:foreground "#AA0000")))
     ("\\(✔\\)" (1 '(:foreground "#00AA00")))
     ("\\(;[a-zA-Z_]+\\)" (1 '(:foreground "purple1")))
     ("\\(GOAL:\\|TRIGGER:\\|\\!\\!\\!\\)" (1 '(:foreground "red")))
     ;; all upper case to make it standout even without syntax highlightings, and some of them are also used by log files commonly.
     ;; remove "TODO:" and always use org-mode TODO???
     ;; same color as _emphasis_
     ("\\(IDEA:\\|PROBLEM:\\|ERROR:\\|SOLUTION:\\|LESSON:\\|COZ:\\|ACTION:\\|CHALLENGE:\\|RESULT:\\|REASON:\\|REF:\\|TODO:\\|DONE\\|MISSED\\|e\\.g\\.,\\|i\\.e\\.,\\|vs\\.\\|<->\\|->\\|<-\\|~>\\|<~\\|❱\\)" (1 '(:foreground "dark orange")))
     ("\\(/// \\)" (1 'font-lock-comment-face t))
     ("\"\\(\\(?:.\\|\n\\)*?[^\\]\\)\"" 0 font-lock-string-face)  ; highlight double quoted text https://stackoverflow.com/questions/7155528
     ;; syntax highlight for Org-mode inline source code src_lang{} https://stackoverflow.com/questions/20309842
     ;; removed "weight:" due to: Invalid face attribute :weight (quote normal). Invalid face attribute :weight (quote bold)
     ("\\(src_\\)\\([^[{]+\\)\\(\\[:.*\\]\\){\\([^}]*\\)}"
      (1 '(:foreground "black" :height 10)) ; src_ part
      (2 '(:foreground "cyan" :height 75 :underline "red")) ; "lang" part.
      (3 '(:foreground "#555555" :height 70)) ; [:header arguments] part.
      (4 'org-code) ; "code..." part.
      )
     ;; Allow missing [:header arguments]. Cannot combine with the full version by adding '?', because that produces following error on Linux
     ;;   Error during redisplay: (jit-lock-function 38127) signaled (error "No match 3 in highlight (3 (quote (:foreground \"#555555\" :height 70)))")
     ("\\(src_\\)\\([^[{]+\\){\\([^}]*\\)}"
      (1 '(:foreground "black" :height 10)) ; src_ part
      (2 '(:foreground "cyan" :height 75 :underline "red")) ; "lang" part.
      (3 'org-code) ; "code..." part.
      ))
   t)  ; don't override original highlighting, e.g., org-heading

  (add-hook  ; http://stackoverflow.com/questions/5500035/set-custom-keybinding-for-specific-emacs-mode/
   'org-mode-hook (lambda () (visible-mode)))  ; disable folding entirely to avoid mis-trigger https://lists.gnu.org/archive/html/emacs-orgmode/2009-11/msg00108.html

  ;; delete highlighted region if any http://www.chenblog.xyz/questions/2614710/in-org-mode-how-to-make-return-delete-highlighted-region
  (define-key org-mode-map
    (kbd "RET")
    (lambda() (interactive) (if (region-active-p) (delete-region (region-beginning) (region-end))) (call-interactively 'org-return)))
  (define-key org-mode-map
    (kbd "M-RET")
    (lambda() (interactive) (if (region-active-p) (delete-region (region-beginning) (region-end))) (call-interactively 'org-meta-return)))

  :bind
  ("M-q M-l" . org-forward-sentence)
  ("M-q M-h" . org-backward-sentence)
  ("M-+" . org-increase-number-at-point)  ; original M-+ is not used. C-+ is N/A in terminal; good for mc

  :bind
  (:map org-mode-map
        ("M-q M-r" . helm-my-org-in-buffer-headings)
        ;; original C-M-<arrows> are for lisp movement, not applicable in org-mode -> override them
        ("C-M-<right>"  . org-shiftright)
        ("C-M-<left>"   . org-shiftleft)
        ("C-M-<up>"     . org-shiftup)
        ("C-M-<down>"   . org-shiftdown)
        ("C-c <up>"     . org-move-item-up)
        ("C-c <down>"   . org-move-item-down)
        ([(shift left)]     . nil)
        ([(shift right)]    . nil)
        ([(shift up)]       . nil)
        ([(shift down)]     . nil)
        ([(control return)] . nil)        ; key n/a on TTY
        ;; ([(shift return)]   . nil)        ; key n/a on TTY
        ([(shift control return)] . nil)  ; key n/a on TTY
        ("M-{" . nil)  ; org-backward-element
        ("M-}" . nil)  ; org-forward-element
        ("M-<up>" . nil)
        ("M-<down>" . nil)
        ("M-a" . nil)
        ("M-e" . nil)
        ("C-j" . nil)
        ("C-k" . nil)
        ("C-y" . nil)
        ("C-a" . nil)
        ("C-e" . nil)
        ("M-h" . nil)  ; org-mark-element
        ("C-'" . nil)  ; org-cycle-agenda-files, which is useless and annoying to me
        ))

(use-package org-crypt
  :ensure nil  ; for non-package
  :init
  ;; http://orgmode.org/worg/org-tutorials/encrypting-files.html
  ;; "Searching for program: no such file or directory, gpg" -> need to install gpg
  (require 'org-crypt)
  (org-crypt-use-before-save-magic)
  (setq org-tags-exclude-from-inheritance (quote ("crypt")))
  (setq org-crypt-key nil)  ; use symmetric encryption
  (setq org-crypt-disable-auto-save t)  ; disable auto-save before decrypt; alt: 'encrypt = re-encrypt before auto-save (sensitive-mode doesn't work)
  (add-to-list 'exec-path "/usr/local/bin")  ; fix "Searching for program: no such file or directory, gpg" http://danzorx.tumblr.com/post/11976550618/easypg-for-emacs-on-os-x-or-sometimes-emacs  http://www.moeding.net/archives/35-GNU-Emacs-and-GPGTool-on-Mac.html
  :bind
  (:map org-mode-map
        ("M-'" . org-decrypt-entry)  ; oringal M-' runs the command abbrev-prefix-mark
        ("M-\"" . org-decrypt-entries)
        ;; ("M-'" . epa-encrypt-region)
        ;; ("M-\"" . epa-decrypt-region)
        ))

;; (defun encrypt-region ()
;;   (interactive)
;;   (let ((context (epg-make-context 'OpenPGP)) (region (buffer-substring (region-beginning) (region-end))))
;;     (delete-region (region-beginning) (region-end))
;;     (insert
;;      (epg-encrypt-string context region nil)
;;      'utf-8)))

(use-package ob-go  ; Golang in Babel
  :defer t
  :config
  (add-to-list 'org-babel-load-languages '(go . t))
  (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages))

;; Spaced Repetition https://news.ycombinator.com/item?id=7539390  ;;r
;; do your daily repetitions is what will save you in the long run http://mt.artofmemory.com/forums/spaced-repetition-software-srs-and-flash-cards-maintenance
;; Twenty rules of formulating knowledge https://www.supermemo.com/en/articles/20rules  ;;r
;; bug: org-drill randomly shows nothing https://bitbucket.org/eeeickythump/org-drill/issues/28/org-drill-randomly-shows-nothing
;;   workaround: http://comments.gmane.org/gmane.emacs.orgmode/96414
;; setting e.g., https://github.com/Fuco1/.emacs.d/blob/master/files/org-defs.el
;; customize the delimiters http://www.giovannicarmantini.com/2015/07/putting-some-make-up-on-my-org-mode-flashcards
(use-package org-drill-table
  ;; a hack to solve: (use-package org-drill) returns "package org-drill is unavailable", OR manually download and install https://bitbucket.org/eeeickythump/org-drill/issues/40/problem-this-install-start-org-drill
  :config
  (require 'cl)  ; to avoid Lisp error: (void-function copy-list) http://stackoverflow.com/questions/34983106
  (require 'org-drill)  ; http://orgmode.org/worg/org-contrib/org-drill.html
  (setq org-drill-add-random-noise-to-intervals-p t) ; add random noise
  (setq org-drill-use-visible-cloze-face-p t)
  (setq org-drill-scope 'agenda)  ; or 'file-no-restriction
  (setq org-drill-maximum-items-per-session 1000)
  (setq org-drill-maximum-duration 60)   ; 60 minutes
  (setq org-drill-learn-fraction 0.25)
  (setq org-drill-left-cloze-delimiter "<[")  ; from https://jmm.io/pr/emacs-meetup/#/drillconfig
  (setq org-drill-right-cloze-delimiter "]>") ; the default are "[" and "]" which are common strings, e.g., used in org-mode
  )

;; https://github.com/fniessen/emacs-leuven-theme
;; pros: prettier org-mode source code blocks https://www.reddit.com/r/emacs/comments/415imd/prettier_orgmode_source_code_blocks/
(use-package leuven-theme  ; ui, e.g., hl-line, fringe, paran
  :after org  ; to redefine org-block-*-line
  :config
  (load-theme 'leuven t)
  (global-hl-line-mode)
  (setq global-hl-line-sticky-flag t)
  (set-face-attribute 'hl-line nil :underline nil :background "#E5F1E0")  ; the underline markup will hide underscore and bottom part of some other chars https://debbugs.gnu.org/db/20/20510.html
  (defface right-triangle-face '((t (:foreground "red"))) "Face for `right-triangle-face`.")  ; https://emacs.stackexchange.com/questions/13134/emphasise-the-current-error-in-the-compilation-window
  (set-fringe-bitmap-face 'right-triangle 'right-triangle-face)
  ;; remove org-*-line background, underline and overline, which are too disturbing.  or use (face-attribute 'default :background) instead of nil
  (set-face-attribute 'org-block-begin-line nil :underline nil :background nil :foreground "#00BB00")
  (set-face-attribute 'org-block-end-line   nil :overline  nil :background nil :foreground "#00BB00")
  (set-face-attribute 'org-meta-line nil :background nil)

  (with-eval-after-load 'helm
    (set-face-background 'helm-selection (face-attribute 'hl-line :background))  ; unify line color, esp for TTY mode whose original helm-selection color = dark red
    (set-face-background 'helm-selection-line (face-attribute 'hl-line :background))  ; original color = gray
    )

  ;; (use-package mic-paren)
  ;; (paren-activate)
  ;; (setq paren-match-face "gold")

  ;; http://emacs-fu.blogspot.com/2009/01/balancing-your-parentheses.html
  (show-paren-mode t)
  (setq show-paren-priority -10)  ; cannot be -100 which is lower than hl-line https://emacs.stackexchange.com/questions/17324/how-to-make-the-region-face-take-priority-over-the-show-paren-mode-face
  (setq show-paren-delay 0.1)  ; highlight matching parenthesis
  (set-face-background 'show-paren-match-face "gold")
  (set-face-foreground 'show-paren-mismatch-face "red")
  (defadvice show-paren-function  ; https://emacs.stackexchange.com/questions/28525
      (after show-matching-paren-offscreen activate)
    "If the matching paren is offscreen, show the matching line in the echo area (for few seconds). Has no effect if the character before point is not of the syntax class ')'."
    (interactive)
    (let* ((cb (char-before (point))) (matching-text (and cb (char-equal (char-syntax cb) ?\) ) (blink-matching-open))))))
  )

;; not smooth sometimes?
(use-package beacon
  :defer 6
  :config
  ;; (beacon-mode 1)
  (setq beacon-blink-delay 0)
  (setq beacon-blink-duration 0.5))

;; TODO fix the problem that it doesn't activate sometimes, e.g., after splitting, opening *Help* buffer, see also http://emacs.1067599.n8.nabble.com/Hooks-for-new-buffers-td282982.html
;; alt: https://www.reddit.com/r/emacs/comments/2jjgbi/looking_for_a_better_way_to_indicate_active/
(use-package auto-dim-other-buffers
  :after leuven-theme  ; after the theme to override the default color
  :config
  (auto-dim-other-buffers-mode)
  (defun adob--ignore-buffer (buffer)  ; add helm and swiper buffers
    (let ((buffer-name (buffer-name buffer)))
      (or
       (null buffer)
       (minibufferp buffer)
       (string-match "^ \\*Echo Area" buffer-name)
       (string-match "^\\*helm" buffer-name)
       (string-match "^\\*swiper" buffer-name))))
  (set-face-attribute 'auto-dim-other-buffers-face nil :background "ivory")  ;; or gray92, as gray91 is too dark for 256color TTY mode
  )

;; shows the buffer position in mode line, good for TTY
;; https://www.emacswiki.org/emacs/SmlModeLine http://emacs-fu.blogspot.com/2010/03/showing-buffer-position-in-mode-line.html https://pastebin.com/d775fJxx
(use-package sml-modeline
  :after leuven-theme  ; after the theme to override the default color
  :config
  (defun sml-modeline-create ()  ; https://emacs.stackexchange.com/questions/33488/speed-up-sml-modeline-is-this-possible
    (let* ((wstart (window-start))
           (wend (window-end))
           number-max number-beg number-end
           (sml-begin (or (car sml-modeline-borders) ""))
           (sml-end   (or (cdr sml-modeline-borders) ""))
           (inner-len (- sml-modeline-len (length sml-begin) (length sml-end)))
           bpad-len epad-len
           pos-%
           start end
           string)
      (if (not (or (< wend (save-restriction (widen) (point-max)))
                   (> wstart 1)))
          ""
        (cond
         ((eq sml-modeline-numbers 'percentage)
          (setq number-max (save-restriction (widen) (point-max)))
          (setq number-beg (/ (float wstart) (float number-max)))
          (setq number-end (/ (float wend) (float number-max)))
          (setq start (floor (* number-beg inner-len)))
          (setq end (floor (* number-end inner-len)))
          (setq string
                (concat (format "%02d" (round (* number-beg 100)))
                        "-"
                        (format "%02d" (round (* number-end 100))) "%%")))
         ((eq sml-modeline-numbers 'line-numbers)
          (save-restriction
            (widen)
            (save-excursion (goto-char (point-max))
                            (setq number-max (string-to-number (format-mode-line "%l")))
                            (goto-char wstart)
                            (setq number-beg (string-to-number (format-mode-line "%l")))
                            (goto-char wend)
                            (setq number-end (string-to-number (format-mode-line "%l"))))
            )
          (setq start (floor (* (/ number-beg (float number-max)) inner-len)))
          (setq end   (floor (* (/ number-end (float number-max)) inner-len)))
          (setq string
                (concat "L"
                        (format "%02d" number-beg)
                        "-"
                        (format "%02d" number-end))))
         (t (error "Unknown sml-modeline-numbers=%S" sml-modeline-numbers)))
        (setq inner-len (max inner-len (length string)))
        (setq bpad-len (floor (/ (- inner-len (length string)) 2.0)))
        (setq epad-len (- inner-len (length string) bpad-len))
        (setq pos-% (+ bpad-len (length string) -1))
        (setq string (concat sml-begin
                             (make-string bpad-len 32)
                             string
                             (make-string epad-len 32)
                             sml-end))
        ;;(assert (= (length string) sml-modeline-len) t)
        (when (= start sml-modeline-len) (setq start (1- start)))
        (setq start (+ start (length sml-begin)))
        (setq end   (+ end   (length sml-begin)))
        (when (= start end) (setq end (1+ end)))
        (when (= end pos-%) (setq end (1+ end))) ;; If on % add 1
        (put-text-property start end 'face 'sml-modeline-vis-face string)
        (when (and (= 0 (length sml-begin))
                   (= 0 (length sml-end)))
          (put-text-property 0 start 'face 'sml-modeline-end-face string)
          (put-text-property end sml-modeline-len 'face 'sml-modeline-end-face string))
        string)))
  (setq sml-modeline-borders '("❚" . ""))  ; to unify the background color to make it less distracting
  (defun set-sml-modeline-len () (setq sml-modeline-len (1+ (/ (window-total-width) 2))))  ; 1+ to make it covers center point.
  (set-sml-modeline-len)
  (add-hook 'window-configuration-change-hook #'set-sml-modeline-len)
  (set-face-background 'sml-modeline-vis-face "#FFFFFF")  ; "white" doesn't work on tty
  (sml-modeline-mode 1)
  )

;; sml is better than the original mode-line coz sml shows the full file path and original only shows the filename.
(use-package smart-mode-line
  :after sml-modeline  ; because mode-line-format uses sml-modeline
  :init
  ;; https://emacs.stackexchange.com/questions/16545/make-names-of-major-modes-shorter-in-the-mode-line
  ;; helm-mini major-mode column width also depend on mode-name https://github.com/emacs-helm/helm/pull/1311
  (defcustom mode-name-max-length 12
    "The number of characters after which a major mode name will be
  truncated in the modeline.")
  (defun truncate-mode-name ()
    (setq mode-name (truncate-string-to-width mode-name mode-name-max-length nil nil 't)))
  (add-hook 'after-change-major-mode-hook #'truncate-mode-name)

  (which-function-mode t)  ; show current function name / org-heading on the mode line
  (advice-add 'which-function :filter-return (lambda (s) (when s (truncate-string-to-width s 30))))  ; https://emacs.stackexchange.com/questions/34657/how-to-limit-the-length-of-function-name-shown-by-which-function-mode-t
  :config
  (column-number-mode t)
  (setq sml/no-confirm-load-theme t)  ; before sml/setup to avoid the confirm prompt
  (setq rm-whitelist '(""))  ; Minor modes should not be displayed in mode line. http://ergoemacs.org/emacs/modernization_mode_line.html
  (setq sml/col-number-format "%3c")  ; as I allow long lines.
  (setq sml/modified-char "M")  ; origin value = "x", not clear to me
  (setq-default
   mode-line-format
   '("%e"
     mode-line-front-space
     mc-count  ; shows number of cursors, which is quite useful
     mode-line-mule-info
     mode-line-client
     mode-line-modified
     mode-line-remote
     mode-line-frame-identification
     mode-line-buffer-identification
     sml/pos-id-separator
     (vc-mode vc-mode)
     sml/pre-modes-separator
     mode-line-modes
     mode-line-misc-info
     mode-line-end-spaces
     (:eval (list (sml-modeline-create)))  ; moved it to the end, and remove sml/buffer-identification-filling by not using mode-line-position.
     ))
  ;; (setq sml/sml-modeline-position 'sml/anchor-after-minor-modes)  ; https://github.com/Malabarba/smart-mode-line/issues/11 but no option to place it at the end
  (sml/setup)

  (set-face-attribute 'mode-line-inactive nil :background "gray92" :box nil)  ; original inactive color was too similar to the active color. "gray92" = inactive buffer bg color. require: AFTER sml/setup http://stackoverflow.com/questions/9446673

  ;; https://stackoverflow.com/questions/7271312/change-emacs-window-appearance-when-it-loses-focus
  (copy-face 'mode-line 'my-mode-line-backup)  ; move out from lambda in focus-out-hook to fix: error: Invalid face, my-mode-line-backup
  (copy-face 'hl-line 'my-hl-line-backup)
  (setq my-fringe-background (face-background 'fringe))
  (add-hook 'focus-out-hook
            (lambda ()
              (set-face-attribute 'fringe nil :background "black")
              (set-face-attribute 'auto-dim-other-buffers-face nil :background "ivory2")
              (copy-face 'mode-line-inactive 'mode-line)
              (copy-face 'auto-dim-other-buffers-face 'hl-line)))  ; hide current line
  (add-hook 'focus-in-hook
            (lambda ()
              (set-face-attribute 'fringe nil :background my-fringe-background)
              (set-face-attribute 'auto-dim-other-buffers-face nil :background "ivory")
              (copy-face 'my-mode-line-backup 'mode-line)
              (copy-face 'my-hl-line-backup 'hl-line)))
  )

;; http://ergoemacs.org/emacs/emacs_navigating_keys_for_brackets.html
(defvar xah-brackets nil "string of left/right brackets pairs.")
(setq xah-brackets "()[]{}<>（）［］｛｝⦅⦆〚〛⦃⦄“”‘’‹›«»「」〈〉《》【】〔〕⦗⦘『』〖〗〘〙｢｣⟦⟧⟨⟩⟪⟫⟮⟯⟬⟭⌈⌉⌊⌋⦇⦈⦉⦊❛❜❝❞❨❩❪❫❴❵❬❭❮❯❰❱❲❳〈〉⦑⦒⧼⧽﹙﹚﹛﹜﹝﹞⁽⁾₍₎⦋⦌⦍⦎⦏⦐⁅⁆⸢⸣⸤⸥⟅⟆⦓⦔⦕⦖⸦⸧⸨⸩｟｠⧘⧙⧚⧛⸜⸝⸌⸍⸂⸃⸄⸅⸉⸊᚛᚜༺༻༼༽⏜⏝⎴⎵⏞⏟⏠⏡﹁﹂﹃﹄︹︺︻︼︗︘︿﹀︽︾﹇﹈︷︸")
(defvar xah-left-brackets '("(" "{" "[" "<" "〔" "【" "〖" "〈" "《" "「" "『" "“" "‘" "‹" "«" )
  "List of left bracket chars.")
(progn
  ;; make xah-left-brackets based on xah-brackets
  (setq xah-left-brackets '())
  (dotimes ($x (- (length xah-brackets) 1))
    (when (= (% $x 2) 0)
      (push (char-to-string (elt xah-brackets $x))
            xah-left-brackets)))
  (setq xah-left-brackets (reverse xah-left-brackets)))
(defvar xah-right-brackets '(")" "]" "}" ">" "〕" "】" "〗" "〉" "》" "」" "』" "”" "’" "›" "»")
  "list of right bracket chars.")
(progn
  (setq xah-right-brackets '())
  (dotimes ($x (- (length xah-brackets) 1))
    (when (= (% $x 2) 1)
      (push (char-to-string (elt xah-brackets $x))
            xah-right-brackets)))
  (setq xah-right-brackets (reverse xah-right-brackets)))
(bind-key
 "M-q M-9"
 (lambda ()
   "Move cursor to the previous occurrence of left bracket.
The list of brackets to jump to is defined by `xah-left-brackets'.
URL `http://ergoemacs.org/emacs/emacs_navigating_keys_for_brackets.html'
Version 2015-10-01"
   (interactive)
   (search-backward-regexp (regexp-opt xah-left-brackets) nil t)))
(bind-key
 "M-q M-0"
 (lambda ()
   "Move cursor to the next occurrence of right bracket.
The list of brackets to jump to is defined by `xah-right-brackets'.
URL `http://ergoemacs.org/emacs/emacs_navigating_keys_for_brackets.html'
Version 2015-10-01"
   (interactive)
   (re-search-forward (regexp-opt xah-right-brackets) nil t)))
(defun xah-goto-matching-bracket ()
  "Move cursor to the matching bracket.
If cursor is not on a bracket, call `backward-up-list'.
The list of brackets to jump to is defined by `xah-left-brackets' and `xah-right-brackets'.
URL `http://ergoemacs.org/emacs/emacs_navigating_keys_for_brackets.html'
Version 2016-11-22"
  (if (nth 3 (syntax-ppss))
      (ignore-errors (backward-up-list 1 'ESCAPE-STRINGS 'NO-SYNTAX-CROSSING))
    (cond
     ((eq (char-after) ?\") (forward-sexp))
     ((eq (char-before) ?\") (backward-sexp))
     ((looking-at (regexp-opt xah-left-brackets))
      (forward-sexp))
     ((looking-back (regexp-opt xah-right-brackets) (max (- (point) 1) 1))
      (backward-sexp))
     (t (ignore-errors (backward-up-list 1 'ESCAPE-STRINGS 'NO-SYNTAX-CROSSING))))))
;; ref: https://wwwtech.de/articles/2013/may/emacs:-jump-to-matching-paren-beginning-of-block
;; related: https://www.emacswiki.org/emacs/NavigatingParentheses
(defun my-matching-paren (with-shift)
  "Go to the matching  if on (){}[]<>, similar to vi style of % "
  (set-marker (mark-marker) (point))
  (setq this-command-keys-shift-translated with-shift)
  (handle-shift-selection)
  (xah-goto-matching-bracket)
  (my-recenter-no-redraw))
(bind-key "M-." (lambda () (interactive) (my-matching-paren nil)))  ; original M-. runs the command xref-find-definitions
(bind-key "M->" (lambda () (interactive) (my-matching-paren t)))

(defun xah-forward-quote-smart ()
  "Move cursor to the current or next string quote.
Place cursor at the position after the left quote.
Repeated call will find the next string.
URL `http://ergoemacs.org/emacs/emacs_navigating_keys_for_brackets.html'
Version 2016-11-22"
  (interactive)
  (let (($pos (point)))
    (if (nth 3 (syntax-ppss))
        (progn
          (backward-up-list 1 'ESCAPE-STRINGS 'NO-SYNTAX-CROSSING)
          (forward-sexp)
          (re-search-forward "\\\"" nil t))
      (progn (re-search-forward "\\\"" nil t)))
    (when (<= (point) $pos)
      (progn (re-search-forward "\\\"" nil t)))))

(defun xah-backward-quote ()
  "Move cursor to the previous occurrence of \".
If there are consecutive quotes of the same char, keep moving until none.
Returns `t' if found, else `nil'.
URL `http://ergoemacs.org/emacs/emacs_navigating_keys_for_brackets.html'
Version 2016-07-23"
  (interactive)
  (if (search-backward-regexp "\\\"+" nil t)
      (when (char-before) ; isn't nil, at beginning of buffer
        (while (char-equal (char-before) (char-after))
          (left-char)
          t))
    (progn
      (message "No more quotes before cursor.")
      nil)))

;; https://emacs.stackexchange.com/questions/37634/how-to-replace-matching-parentheses
(defun yf/replace-or-delete-pair (open)
  "Replace pair at point by OPEN and its corresponding closing character.
The closing character is lookup in the syntax table or asked to
the user if not found."
  (interactive
   (list
    (read-char
     (format "Replacing pair %c%c by (or hit RET to delete pair):"
             (char-after)
             (save-excursion
               (forward-sexp 1)
               (char-before))))))
  (if (memq open '(?\n ?\r))
      (delete-pair)
    (let ((close (cdr (aref (syntax-table) open))))
      (when (not close)
        (setq close
              (read-char
               (format "Don't know how to close character %s (#%d) ; please provide a closing character: "
                       (single-key-description open 'no-angles)
                       open))))
      (yf/replace-pair open close))))

(defun yf/replace-pair (open close)
  "Replace pair at point by respective chars OPEN and CLOSE.
If CLOSE is nil, lookup the syntax table. If that fails, signal
an error."
  (let ((close (or close
                   (cdr-safe (aref (syntax-table) open))
                   (error "No matching closing char for character %s (#%d)"
                          (single-key-description open t)
                          open)))
        (parens-require-spaces))
    (insert-pair 1 open close))
  (delete-pair)
  (backward-char 1))

(bind-key "M-1" 'yf/replace-or-delete-pair)

(if (eq system-type 'darwin)  ; MacOS-only customization
    (progn
      (setq ns-pop-up-frames nil)  ; when you double-click on a file in the Mac Finder open it as a buffer in the existing Emacs frame, rather than creating a new frame just for that file
      (setq helm-locate-command "mdfind -name %s %s")  ; use Spotlight to search file https://github.com/syl20bnr/spacemacs/issues/3280 alt: https://www.emacswiki.org/emacs/LocateFilesAnywhere
      (setq ring-bell-function (lambda () (message "################################################################################")))  ; mac visible-bell is weird https://www.reddit.com/r/emacs/comments/3omsr2/weird_display_issue_in_os_x/ https://www.reddit.com/r/emacs/comments/1a6z4n/can_i_make_emacs_beep_less/

      ;; PROBLEM: /usr/local/bin is not in Emacs shell path.
      ;; SOL: http://stackoverflow.com/questions/2266905/emacs-is-ignoring-my-path-when-it-runs-a-compile-command
      ;; cannot use string-trim instead of replace-regexp-in-string (as the function is not loaded at this point)
      ;; alt: use exec-path-from-shell package http://emacs.stackexchange.com/questions/10722 https://github.com/purcell/exec-path-from-shell
      (let ((path-from-shell (replace-regexp-in-string "[[:space:]\n]*$" "" (shell-command-to-string "$SHELL -l -c 'echo $PATH'"))))
        (setenv "PATH" path-from-shell)
        (setq exec-path (split-string path-from-shell path-separator)))
      ;; re-map modifiers
      (setq mac-control-modifier 'meta
            mac-option-modifier  'hyper     ; don't rely on the middle modifier key, which is captured by OS on Linux; nil -> insert Greek symbols, which is annoying when i type it accidentally
            mac-command-modifier 'control)  ; default mac-command = super
      ;; (setq frame-resize-pixelwise t)  ; get proper maximisation working on Mac
      )

  ;; Linux-only customization
  (setq visible-bell t))
