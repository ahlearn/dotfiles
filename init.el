;; An Emacs configuration for a stubborn modern-GUI-editor user.
;;
;; Config rules:
;; + Modernization: e.g., http://ergoemacs.org/emacs/emacs_modernization.html
;;   - Eliminate the use of marker, which is not conventional nowadays; use shift selection instead
;;   - Easy Moving From Vscode To Emacs https://krsoninikhil.github.io/2018/12/15/easy-moving-from-vscode-to-emacs/
;; + Geared toward (in descending order) Mac OS, Linux, and Terminal.
;; + Minimize mode-specific settings, e.g., don't use highlight-thing-excluded-major-modes
;;
;; Keybinding rules:
;; + Mostly compatible with default Mac OS X system key bindings http://www.hcs.harvard.edu/~jrus/site/system-bindings.html
;; + Use CUA shortcuts
;; + Use meta-key-based (instead of ctrl-key-based) keybindings, because Emacs Keys are Painful http://ergoemacs.org/emacs/emacs_kb_shortcuts_pain.html
;; + Minimize number of shortcuts by merging similar functions, e.g., use helm-mini instead of helm-buffers-list, helm-locate, etc
;; + Minimize use of function keys, which is not convenient, e.g., don't use <f11> as the prefix key
;;
;; Coding style:
;; + Names of most user-defined and interactive functions are prefixed by "my-"
;; + Minimize the use of require or load, use eval-after-load instead
;; + Single self-contained config file (packages will be downloaded automatically)

;; (setq debug-on-error t)  ; uncomment this line for debug

;;; Speedup, startup hooks
(setq file-name-handler-alist-original file-name-handler-alist) (setq file-name-handler-alist nil)  ; reduced startup time by about 15% https://emacs.stackexchange.com/questions/34342 https://www.reddit.com/r/emacs/comments/3kqt6e
(setq inhibit-startup-screen t)  ; no splash screen
(setq initial-scratch-message nil)
(setq my-doc-directory "~/gdrive/_doc/")
(tool-bar-mode -1)
(scroll-bar-mode -1)
(tooltip-mode -1)
(setq gc-cons-threshold-original gc-cons-threshold) (setq gc-cons-threshold (* 1024 1024 100))  ; reduced startup time by about 25% https://www.reddit.com/r/emacs/comments/3kqt6e
;; temporarily increase memory threshold so that GC runs less often, which speeds up some operations https://github.com/hrs/sensible-defaults.el/blob/master/sensible-defaults.el
;; http://bling.github.io/blog/2016/01/18/why-are-you-changing-gc-cons-threshold/
(defun my-minibuffer-setup-hook ()
  (setq gc-cons-threshold 100000000)  ; The default is 800kb - Spacemacs uses 100mb http://emacs.stackexchange.com/questions/19705
  (yas-minor-mode 1)  ; make yasnippet also available in minibuffer / helm
  )
(defun my-minibuffer-exit-hook () (setq gc-cons-threshold gc-cons-threshold-original))
(add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-hook)
(add-hook 'minibuffer-exit-hook #'my-minibuffer-exit-hook)
(run-with-idle-timer
 1 nil
 (lambda ()
   (require 'recentf) (recentf-mode 1)  ; to run "Cleaning up the recentf list" earlier
   (setq gc-cons-threshold gc-cons-threshold-original)  ; Reset `gc-cons-threshold' to its default value. http://emacs.stackexchange.com/a/16595/12987
   (setq file-name-handler-alist file-name-handler-alist-original)
   (makunbound 'file-name-handler-alist-original)
   (message "init-time = %s, last session = %s" (emacs-init-time) (shell-command-to-string (concat "tail -n1 " my-doc-directory "z.emacs_sessions.txt")))
   ))
(add-hook 'focus-out-hook #'garbage-collect)  ; Collect garbage when Emacs is focused out to make it faster https://github.com/elnawe/.emacs.d/blob/master/configuration.org
(add-hook 'kill-emacs-hook (lambda () (append-to-file (concat "\n" system-name " " (format-time-string "%a, %Y/%m/%d, %l:%M %p")) nil (concat my-doc-directory "z.emacs_sessions.txt")) t))
(add-hook
 'emacs-startup-hook
 '(lambda ()
    (unless (buffer-file-name)  ; open following file only when current buffer is scratch https://emacs.stackexchange.com/questions/825
      (if (eq system-type 'gnu/linux)
          (find-file (concat my-doc-directory "work.txt"))
        (find-file (concat my-doc-directory "personal.txt")))
      (beginning-of-buffer)
      )
    (if (eq system-type 'darwin)
        (use-package openwith
          :config
          ;; https://emacs.stackexchange.com/questions/3105/how-to-use-an-external-program-as-the-default-way-to-open-pdfs-from-emacs
          (setq openwith-associations
                '(("\\.pdf\\'" "/Applications/Preview.app/Contents/MacOS/Preview" (file))
                  ("\\.mp3\\'" "/Applications/VLC.app/Contents/MacOS/VLC" (file))))
          (openwith-mode t)
          ))
    ))

;;; Display
(setq initial-major-mode 'org-mode)  ; i seldom use the default lisp-interaction-mode (which rebinds C-j)
(add-to-list 'initial-frame-alist '(fullscreen . maximized))  ; better than manually setting width and height http://emacs.stackexchange.com/a/3008
(setq-default frame-title-format "%b (%f)") ; show file path in frame title http://stackoverflow.com/a/12009623 note: don't rely on it, as it is N/A in terminal
;; (menu-bar-mode -1)  ; TODO try out menu
(setq-default cursor-type '(bar . 3)) (blink-cursor-mode t)
(setq blink-cursor-blinks 0)  ; blinks forever
(delete-selection-mode t)  ; delete seleted text when typing https://www.emacswiki.org/emacs/DeleteSelectionMode
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
(setq-default show-trailing-whitespace t)  ; http://trey-jackson.blogspot.com/2008/03/emacs-tip-12-show-trailing-whitespace.html
(setq-default indent-tabs-mode nil)        ; use space instead of tab https://www.emacswiki.org/emacs/NoTabs as we have auto-indent in every mode anyway
(setq-default tab-width 2)
(setq sh-basic-offset 2 sh-indentation 2)  ; indent with 2 spaces for .sh files
(setq-default indicate-empty-lines t)
(setq sentence-end-double-space nil)  ; don't assume that sentences should have two spaces after periods
(defalias 'yes-or-no-p 'y-or-n-p)     ; answering just 'y' or 'n' will do

;;; for emacs -nw, terminal mode, TTY
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


;;; use-package
;; user manual https://github.com/jwiegley/use-package/blob/master/use-package.org
(require 'package)  ; https://www.emacswiki.org/emacs/ELPA https://github.com/durantschoon/.emacs.d/tree/boilerplate-sane-defaults_v1.0
;; (add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)  ; safer to get stable version first https://github.com/magnars/.emacs.d/pull/7
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)  ; stable Melpa doesn't have some packages -> need non-stable Melpa repository, url copied from http://melpa.org/#/getting-started
(add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)  ; for installing org-plus-contrib https://emacs.stackexchange.com/questions/17710
;; http://elpa.gnu.org/packages/ is on the list by default after version 24
(setq load-prefer-newer t)  ; load newer elisp
(package-initialize)
(unless (package-installed-p 'use-package)  ; install use-package if it isn't installed yet.
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile (require 'use-package))  ; load use-package
(setq use-package-always-ensure t)  ; install all packages automatically; for non-package, override it to nil
;; (setq use-package-always-defer t)  ; cause too many dependence bugs

(require 'cl)  ; for using cl-letf, and remove-if http://ergoemacs.org/emacs/elisp_filter_list.html
(require 'subr-x)  ; for string-trim

;; https://github.com/jwiegley/use-package/blob/master/bind-key.el TODO clean up this file by better bind-key syntax
(use-package bind-key  ; mouse, scroll, and basic key bindings
  :init
  ;; unbind useless keybindings, ("M-SPC" . nil) doesn't work
  (global-unset-key (kbd "M-SPC"))  ; change it to main prefix key, similar to idea of spacemacs, original M-SPC runs the command just-one-space
  (global-unset-key (kbd "C-<prior>"))  ; scroll-left is useless
  (global-unset-key (kbd "C-<next>"))   ; scroll-right is useless
  ;; rule: C-c = mode-dependent prefix
  ;; C-c C-c is a common action (e.g., to send "C-c" in shell), better to have a new easy shortcut, e.g., M-c M-c  http://emacs.stackexchange.com/questions/14322
  (define-key key-translation-map (kbd "M-c") (kbd "C-c"))
  (define-key key-translation-map (kbd "C-c") (kbd "M-c"))
  (define-key key-translation-map (kbd "C-S-c") (kbd "M-C"))
  (define-key key-translation-map (kbd "M-C") (kbd "C-S-c"))        ; for org-mode
  (define-key key-translation-map (kbd "C-i") (kbd "<tab>"))          ; in TTY, tab key generates C-i instead of <tab>
  (define-key key-translation-map (kbd "<return>") (kbd "RET"))       ; RET is more common than <return>
  (define-key key-translation-map (kbd "<S-tab>") (kbd "<backtab>"))  ; unify key strings
  (define-key key-translation-map (kbd "M-<kp-enter>") (kbd "M-RET")) ; for numpad enter
  (define-key key-translation-map (kbd "M-<enter>")    (kbd "M-RET")) ; for numpad enter
  (define-key key-translation-map (kbd "M-ESC") (kbd "<escape>"))     ; so that i can keep pressing meta key
  (define-key key-translation-map (kbd "M-q") (kbd "<escape>"))       ; M-q is easier
  (global-set-key (kbd "M-SPC C-x") ctl-x-map)  ; cannot use (bind-key "M-SPC C-x" #'ctl-x-map) which return "Wrong type argument: commandp, ctl-x-map". ref: http://ergoemacs.org/emacs/emacs_dvorak_C-x.html
  (defun my-prefix-cancel () (interactive) (message "Prefix canceled."))  ; single ESC to cancel prefix key, cannot use keyboard-quit which will quit mc mode too.
  (defun my-save-all-buffers () (interactive) (save-some-buffers t))
  (setq kmacro-ring-max 30)  ; default is 8
  ;; kmacro-edit-lossage doesn’t work out of the box. It gives “Key sequence C-h l is not defined”.  This fixes the problem https://github.com/br4ndur/dot-emacs/blob/master/branduren.org
  (defun kmacro-edit-lossage-fixed ()
    "Edit most recent 300 keystrokes as a keyboard macro."
    (interactive)
    (kmacro-push-ring)
    (edit-kbd-macro 'view-lossage))
  (defun my-recenter-no-redraw ()
    "Like `recenter', but no redrawing."  ; avoid flashing in terminal http://stackoverflow.com/a/36896945/550243
    (let ((recenter-redisplay nil)) (recenter nil))  ; recenter without arg -> go to middle of the screen
    )
  (defun my-escape ()
    (interactive)
    (if (eq last-command 'mwheel-scroll)
        (progn
          (exchange-point-and-mark t)  ; go back to pre-scroll position
          (my-recenter-no-redraw)
          (beacon-blink))
      (if (eq last-command 'keyboard-quit)
          ;; double esc = keyboard-escape-quit + widen = keyboard-quit + delete-other-windows + ...
          (progn
            (widen)
            (when (and (boundp 'vdiff--session) (not (null vdiff--session))) (vdiff-quit))
            (keyboard-escape-quit)  ; = original "ESC ESC ESC"
            (call-interactively 'lazy-highlight-cleanup)  ; for isearch
            (my-recenter-no-redraw)
            (beacon-blink))
        ;; single esc = C-g = keyboard-quit, same as all other apps like closing a dialog box in Chrome (solving a problem mentioned in https://www.reddit.com/r/emacs/comments/4a0421 )
        ;; Use unread-command-events instead of calling keyboard-quit directly, in case C-g is rebound, e.g., in company-mode / minibuffer-local-map http://superuser.com/a/945245/59765
        (setq unread-command-events (listify-key-sequence "\C-g"))
        )))

  ;; double-click on the quotation mark -> select all text in double quotes
  ;; double-click on the '_' -> select the whole variable
  ;; scroll down and keep cursor is not possible?
  (setq mouse-wheel-scroll-amount '(5 ((shift) . 5) ((control) . nil)))  ; http://stackoverflow.com/a/445881 https://www.emacswiki.org/emacs/SmoothScrolling
  (setq mouse-wheel-progressive-speed nil)  ; don't accelerate scrolling http://stackoverflow.com/a/445881 https://www.emacswiki.org/emacs/SmoothScrolling
  (setq mouse-drag-copy-region nil)  ; highlight does not alter kill ring
  (setq mouse-yank-at-point t)  ; when middle-clicking the mouse to yank from the clipboard, insert the text where point is, not where the mouse cursor is
  (defadvice mouse-set-point (around glue-underscore activate)
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
  (advice-add 'mwheel-scroll :before (lambda (arg) (if (not (eq last-command 'mwheel-scroll)) (set-marker (mark-marker) (point)))))

  :bind
  ("M-SPC <escape>" . my-prefix-cancel)
  ("C-c <escape>" . my-prefix-cancel)
  ("<f1> <escape>" . my-prefix-cancel)
  ("M-SPC C-q" . quoted-insert)     ; original kbd of quoted-insert = C-q
  ("M-SPC M-e" . eval-last-sexp)    ; original kbd of eval-last-sexp = C-x C-e
  ("M-SPC e"   . eval-expression)   ; original kbd of eval-expression = M-:
  ("<f11>" . kmacro-start-macro-or-insert-counter)  ; orignally binded to <f3>
  ("<f12>" . kmacro-end-or-call-macro)              ; orignally binded to <f4>
  ("M-SPC <f11>" . kmacro-edit-lossage)
  ;; other-window is a common function, but original binding (C-x o) is not convenient.
  ;; C-` is N/A in TTY (= C-@ = C-SPC), M-` is used by Ubuntu to flip through windows in the switcher. M-v is not easy enough.
  ("M-z" . other-window)  ; original M-z runs the command zap-to-char
  ;; Bind C-+ and C-- to increase and decrease text size, respectively.
  ;; original keys: C-x C-+, C-x C--, and C-x C-0.
  ;; from https://github.com/hrs/sensible-defaults.el/blob/master/sensible-defaults.el
  ("C-0" . text-scale-adjust)  ; = (lambda () (interactive) (text-scale-set 0)), -> use M-num (or C-u num) instead of C-num for num arg
  ("C-+" . text-scale-increase)
  ("C-=" . text-scale-increase)
  ("C-_" . text-scale-decrease)
  ("C--" . text-scale-decrease)
  ;; remap useful file commands http://ergoemacs.org/emacs/emacs_kb_shortcuts_pain.html
  ("C-s" . my-save-all-buffers)
  ("C-M-s" . write-file)  ; = save-as
  ("C-q" . save-buffers-kill-terminal)
  ("C-w" . kill-this-buffer)
  ("M-v" . indent-for-tab-command)
  ("M-RET" . newline-and-indent)
  ("<backspace>" . my-escape)  ; disable <backspace>
  ("<escape>" . my-escape)
  (:map query-replace-map  ; query-replace-map is keymap-parent of map-y-or-n-p (used by save-buffers-kill-terminal) http://superuser.com/questions/795763/how-to-make-emacs-quit-the-minibuffer-with-one-press-of-esc
        ("<escape>" . keyboard-quit)  ; originally bind to exit-prefix
        )
  )

;; https://tumashu.github.io/pyim/documents/a-chinese-input-method-which-support-quanpin,-shuangpin,-wubi-and-cangjie./
;; https://github.com/tumashu/pyim
(use-package pyim  ; and utf-8, font, Chinese input method
  :init
  ;; (use-package cnfonts :config (cnfonts-enable) (cnfonts-edit-profile))  ; for finding fonts https://github.com/tumashu/cnfonts
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
                ((eq system-type 'gnu/linux)  '("Monospace" "WenQuanYi Zen Hei Mono"))  ; "Menlo" is N/A
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
  (set-buffer-file-coding-system 'utf-8)
  :config
  (use-package pyim-cangjie5dict :config (pyim-cangjie5-enable))
  (setq default-input-method "pyim")
  (setq pyim-default-scheme 'cangjie)
  ;; (setq-default pyim-english-input-switch-functions '(pyim-probe-dynamic-english))  ; 无痛中英文切换：光标前是汉字字符时，才能输入中文
  (setq-default pyim-punctuation-half-width-functions '(pyim-probe-punctuation-line-beginning pyim-probe-punctuation-after-punctuation))
  (setq pyim-page-tooltip 'popup)  ; 使用 pupup-el 来绘制选词框
  (setq pyim-page-length 10)  ; 选词框显示候选词個數
  :bind
  ;; ("M-/" . pyim-convert-code-at-point) ;与 pyim-probe-dynamic-english 配合，强制将光标前的拼音字符串转换为中文
  ("M-SPC SPC" . toggle-input-method)
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
(use-package backup-walker)
(defvar user-temporary-file-directory "~/.emacs.d/backups/")  ; http://www.martinaulbach.net/linux/software/21-my-emacs-configuration-file
(setq backup-directory-alist `(("." . ,user-temporary-file-directory)))  ; http://pages.sachachua.com/.emacs.d/Sacha.html
(setq tramp-backup-directory-alist backup-directory-alist)
;; Remember password when connected to remote sites via Tramp http://stackoverflow.com/questions/840279/passwords-in-emacs-tramp-mode-editing
;; Emacs "tramp" service (ssh connection) constantly asks for the log in password without this
(setq password-cache-expiry nil)
(setq backup-by-copying t)         ; prevents links from being made to point at the backup file rather than the original.  It's the safest but slowest bet. http://stackoverflow.com/questions/151945  and to stop emacs's backup changing the file's creation date of the original file http://ergoemacs.org/emacs/emacs_make_modern.html
(setq delete-old-versions 'never)  ; disk space is cheap. save lots.
(setq version-control t)           ; keep multiple numbered backup files, rather than a single unnumbered backup file.
(setq vc-make-backup-files t)      ; vc = version-control, can use ediff-backup to diff
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)))
(setq delete-by-moving-to-trash t)  ; don’t really delete the file
(use-package osx-trash
  :if (eq system-type 'darwin) :config (osx-trash-setup)) ; Provide OS X system trash support for GNU Emacs 24, makes delete-by-moving-to-trash do what you expect it to do.
(setq custom-file (make-temp-file ""))  ; disabled the custom file; don't call "(load custom-file)" coz i don't use custom file https://www.reddit.com/r/emacs/comments/4q4ixw
(setq auto-revert-interval 1)  ; reduce the delay on auto-reloading from 5 seconds down to 1 second.  setq before loading autorevert was faster https://emacs.stackexchange.com/questions/102/#comment48662_106
(global-auto-revert-mode 1)    ; http://stackoverflow.com/questions/1480572 but not prompt for revert if changed???
(setq create-lockfiles nil)    ; disable lockfile which is not useful (as i only open one emacs at a time), and cost overhead (to cloud sync) https://www.gnu.org/software/emacs/manual/html_node/emacs/Interlocking.html#Interlocking
(use-package saveplace
  :config
  (setq-default save-place t)
  (setq save-place-file "~/.emacs.d/saveplace")
  (when (>= emacs-major-version 25) (save-place-mode t))  ; http://emacs.stackexchange.com/questions/14670
  )
(setq history-delete-duplicates t)
(setq-default bidi-display-reordering nil)  ; to navigate long lines faster https://emacs.stackexchange.com/questions/598/how-do-i-prevent-extremely-long-lines-making-emacs-slow https://www.reddit.com/r/emacs/comments/7wezb4/how_can_i_make_line_rendering_faster/ https://www.reddit.com/r/emacs/comments/6hzq1j
(setq-default bidi-paragraph-direction 'left-to-right)  ; https://www.reddit.com/r/emacs/comments/88yzp4/better_way_to_run_terminals_in_emacs/

;; https://www.reddit.com/r/emacs/comments/3srwz6/idelike_go_back/
;; auto-mark adapted from https://www.emacswiki.org/emacs/AutoMark
;; removed auto-mark-ignore-move-on-sameline coz line-number-at-pos is costly https://emacs.stackexchange.com/questions/3821
;; confirmed marks:
;;   original point before C-v
(use-package back-button  ; and auto-mark
  :init
  (setq global-mark-ring-max 256  ; original values = 16, too small for edit marks
        mark-ring-max 256)
  ;; (defun auto-mark-pre-command-handle ()
  ;;   (setq auto-mark-previous-buffer-size (buffer-size)
  ;;         auto-mark-previous-point (point)))
  ;; (defun auto-mark-post-command-handle ()
  ;;   (when (and (not (member this-command auto-mark-command-ignore-alist))
  ;;              auto-mark-previous-point  ; check auto-mark-previous-point to allow nil value, to avoid extra point-min mark
  ;;              (or (/= auto-mark-previous-buffer-size (buffer-size))  ; edit or not
  ;;                  (/= auto-mark-previous-point (point)))  ; move or not
  ;;              (or (null mark-ring) (not (= auto-mark-previous-point (marker-position (car mark-ring))))))  ; only push when the position is different than the previous one, to avoid duplicate marks
  ;;     (setq mark-ring (cons (copy-marker auto-mark-previous-point) mark-ring))))  ; push to mark-ring directly instead of push-mark which may not update mark-ring, also good for functions that use (let ((deactivate-mark nil)) ...)
  ;; (setq auto-mark-command-ignore-alist '(back-button-local-backward back-button-local-forward))
  ;; ;; (make-local-variable 'auto-mark-previous-buffer-size)
  ;; ;; (make-local-variable 'auto-mark-previous-point)
  ;; (setq-local auto-mark-previous-buffer-size 0)
  ;; (setq-local auto-mark-previous-point 0)
  ;; (add-hook 'pre-command-hook 'auto-mark-pre-command-handle nil t)
  ;; (add-hook 'post-command-hook 'auto-mark-post-command-handle nil t)
  :config
  (back-button-mode 1)
  (advice-add 'back-button-visible-mark-show :after (lambda (arg) (hl-line-highlight) (deactivate-mark) (my-recenter-no-redraw) (beacon-blink)))  ; cannot add advice to 'back-button-local which has some delay
  (defun shift-back-button-local-backward-recent ()
    (interactive)
    (exchange-point-and-mark)
    (convert-to-shift-selection)
    (my-recenter-no-redraw))
  :bind
  ("C-h" . back-button-local-backward)
  ("C-S-h" . shift-back-button-local-backward-recent)
  ("C-l" . back-button-local-forward)
  )

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
  ;; to fix: Region set by expand-region doesn't get unset by motion https://github.com/magnars/expand-region.el/issues/185
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
  (setq expand-region-fast-keys-enabled nil)  ; disable: "Type r to expand again, - to contract, 0 to reset" which is annoying when i wanna replace region by "r..."
  :bind
  ("C-a" . my-shift-select-all)
  ("M-r" . er/expand-region)    ; original M-r runs the command move-to-window-line-top-bottom
  ("M-R" . er/contract-region)
  )

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
  (add-hook 'dired-mode-hook 'auto-revert-mode)  ; auto refresh dired when file changes http://pragmaticemacs.com/emacs/automatically-revert-buffers/
  :bind
  (:map dired-mode-map
        ("e" . wdired-change-to-wdired-mode)
        ("h" . dired-up-directory)
        ("l" . dired-find-alternate-file)  ; visit w/o a new buffer, using emacs
        ("j" . dired-next-line)
        ("k" . dired-previous-line)
        ("o" . dired-open-file)  ; using system app
        ))

;;; frame and window
;; resize Emacs (GUI) window to exactly half the screen https://emacs.stackexchange.com/questions/30420
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
(bind-key "M-SPC M-SPC M-SPC"   #'toggle-frame-maximized)
(bind-key "M-SPC M-SPC M-k"    #'my-frame-resize-half-up)
(bind-key "M-SPC M-SPC M-j"  #'my-frame-resize-half-down)
(bind-key "M-SPC M-SPC M-h"  #'my-frame-resize-half-left)
(bind-key "M-SPC M-SPC M-l" #'my-frame-resize-half-right)

(setq winner-dont-bind-my-keys t)  ; don't use default binding (C-c <left>/<right>) coz C-c is used for mode-specific functions only.
(winner-mode)  ; https://github.com/emacs-mirror/emacs/blob/master/lisp/winner.el
(bind-key "C-k" #'winner-undo)
(bind-key "C-j" #'winner-redo)
(bind-key "C-M-w" #'delete-window)
(bind-key "M-SPC M-SPC \\" #'split-window-horizontally)
(bind-key "M-SPC M-SPC -" #'split-window-vertically)

;; http://stackoverflow.com/a/28876359 more fine-grain than subword-mode
(use-package syntax-subword  ; and navigation / movement / jump
  :defer 0
  :init
  (global-visual-line-mode 1)  ; enable Word Wrap, o/w may split a word into 2 parts
  (setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))  ; disabled by default https://www.gnu.org/software/emacs/manual/html_node/emacs/Visual-Line-Mode.html  alt: use whitespace-mode to display hard newlines https://vlevit.org/en/blog/tech/visual-line-wrap

  ;; delete instead of kill, ref: http://ergoemacs.org/emacs/emacs_kill-ring.html http://ergoemacs.org/emacs/emacs_delete_whole_line.html https://www.emacswiki.org/emacs/BackwardDeleteWord
  ;; do not use (setq kill-ring (cdr kill-ring)) to remove last item from kill-ring, as it screws up the OS clipboard http://stackoverflow.com/questions/637351
  (defun my-delete-subword (arg) (if (use-region-p) (delete-region (region-beginning) (region-end)) (delete-region (point) (progn (syntax-subword-forward arg) (point)))))
  (defun my-delete-prev-subword () (interactive) (my-delete-subword -1))
  (defun my-delete-next-subword () (interactive) (my-delete-subword 1))

  (add-hook 'post-command-hook
            (lambda ()
              ;; use called-handle-shift-selection to make sure count-words-region is called only after interactive^ commands, i.e., C-n/p is not affected.
              ;; use use-region-p instead of region-active-p to skip the empty region case
              (when (and (boundp 'called-handle-shift-selection) called-handle-shift-selection (use-region-p) (not (active-minibuffer-window)))
                (setq-local called-handle-shift-selection nil)
                (call-interactively 'count-words-region))))  ; call-interactively to print the result message.
  (advice-add 'handle-shift-selection :around
              (lambda (orig-fun &rest args)
                (setq-local was-active (use-region-p))  ; https://emacs.stackexchange.com/questions/30414/how-to-move-the-cursor-to-the-beginning-end-of-a-shift-selected-region-by-a-left
                (setq-local called-handle-shift-selection t)
                ;; don't update mark-ring, and suppress "Mark set" message
                (cl-letf (((symbol-function 'push-mark) (lambda ( &optional LOCATION NOMSG ACTIVATE) (set-marker (mark-marker) (point) (current-buffer)) (set-mark (mark t)))))
                  (apply orig-fun args))))

  (bind-key "M-g" (lambda (arg)  ; original M-l runs the command downcase-word
                    (interactive "^p")
                    (if (and was-active (not (use-region-p)))
                        (goto-char (region-end))
                      (syntax-subword-forward arg)
                      )))
  ;; bind M-h to syntax-subword-backward instead of delete-backward-char, because if all hjkl are movement key then it's ok to mistype sometimes.
  (bind-key "M-s" (lambda (arg)  ; original M-h runs the command mark-paragraph / org-mark-element; need * to override org-mode rebind.
                    (interactive "^p")
                    (if (and was-active (not (use-region-p)))
                        (goto-char (region-beginning))
                      (syntax-subword-backward arg)
                      )))

  (defun term-to-term-line-mode()
    (when (and (boundp 'term-raw-map) (term-in-char-mode))  ; term-in-char-mode is a macro, cannot be used in boundp
      (term-line-mode)
      (setq cursor-type '(bar . 3))
      (read-only-mode 1)))  ; term-line-mode is not good for editing https://emacs.stackexchange.com/questions/17085

  ;; go to next/prev block-start. related: http://ergoemacs.org/emacs/emacs_move_by_paragraph.html http://whattheemacsd.com/setup-html-mode.el-01.html http://endlessparentheses.com/meta-binds-part-2-a-peeve-with-paragraphs.html
  ;; do not use "*" so that it can be redefined in helm-map
  (bind-key "M-j" (lambda ()  ; original M-j runs the command indent-new-comment-line
                    "Next block-start."
                    (interactive "^")
                    (term-to-term-line-mode)
                    (unless (search-forward-regexp "\n[\t\n ]*\n" nil t) (goto-char (point-max)))
                    ))
  (bind-key "M-k" (lambda ()  ; original M-k runs the command kill-sentence
                    "Previous block-start."
                    (interactive "^")
                    (term-to-term-line-mode)
                    (skip-chars-backward "\n\t ")
                    (if (search-backward-regexp "\n[\t ]*\n" nil t)  ; go to start of prev empty line
                        (progn (skip-chars-forward "\n\t ")  ; go to the block
                               (skip-chars-backward "\t "))  ; go to line-beginning-position, (move-beginning-of-line) doesn't work.
                      (goto-char (point-min))
                      )))

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
              (term-to-term-line-mode)
              (cond ((eq major-mode 'shell-mode) (comint-next-prompt arg))
                    ((eq major-mode 'org-mode) (org-next-visible-heading arg))
                    (t (goto-closest-imenu-item 1)))
              (my-recenter-no-redraw)))
  (bind-key "M-i"
            (lambda (arg)
              (interactive "^p")
              (term-to-term-line-mode)
              (cond ((eq major-mode 'shell-mode) (comint-previous-prompt arg))
                    ((eq major-mode 'org-mode) (org-previous-visible-heading arg))
                    (t (goto-closest-imenu-item -1)))
              (my-recenter-no-redraw)))

  (defun my-goto-buffer-beg () (interactive "^") (term-to-term-line-mode) (beginning-of-buffer))
  (defun my-goto-buffer-end () (interactive "^") (term-to-term-line-mode) (end-of-buffer))
  (defun my-shift-select-all () (interactive) (mark-whole-buffer) (convert-to-shift-selection))

  ;; do not use viper-forward-Word which cannot eat multiple "\n"
  (defun my-next-space-seperated-word ()
    (interactive "^")
    (term-to-term-line-mode)
    (skip-chars-forward "^\n\t ") (skip-chars-forward "\n\t ")  ; == (search-forward-regexp "[^\t\n ][\t\n ]+" nil t)
    )
  (defun my-prev-space-seperated-word ()
    (interactive "^")
    (term-to-term-line-mode)
    (skip-chars-backward "\n\t ") (skip-chars-backward "^\n\t ")
    )
  (bind-key "M-a" (lambda ()
                    "Move point to the first non-whitespace character on this line. If point was already at that position, move point to beginning of line."
                    (interactive "^")
                    (if (> (save-excursion (beginning-of-visual-line) (point)) (save-excursion (back-to-indentation) (point)))
                        (let ((oldpos (point))) (beginning-of-visual-line) (and (= oldpos (point)) (back-to-indentation)))
                      (let ((oldpos (point))) (back-to-indentation) (and (= oldpos (point)) (beginning-of-visual-line))))))
  (bind-key "M-e" (lambda ()
                    "Move point to the end-of-visual-line. If point was already at that position, move point to end of line."
                    (interactive "^")
                    (let ((oldpos (point))) (end-of-visual-line) (and (= oldpos (point)) (end-of-line)))))

  :bind
  ("<C-backspace>" . my-delete-prev-subword)
  ("<C-delete>" . my-delete-next-subword)
  ("M-H" . my-delete-prev-subword)  ; useful for deleting empty spaces
  ("M-D" . my-delete-next-subword)
  ("M-d" . delete-char)
  ("M-h" . delete-backward-char)
  ("M-n" . next-line)
  ("M-p" . previous-line)
  ("M-f" . forward-char)
  ("M-b" . backward-char)
  ("C-M-j" . scroll-up-command)
  ("C-M-k" . scroll-down-command)
  ("M-o" . my-next-space-seperated-word)
  ("M-y" . my-prev-space-seperated-word)
  ("C-<up>"   . my-goto-buffer-beg)  ; same as sublime and http://codemirror.net
  ("C-<down>" . my-goto-buffer-end)
  ("M-SPC M-k" . my-goto-buffer-beg)
  ("M-SPC M-j" . my-goto-buffer-end)
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
  :defer 5
  :config
  (keyfreq-mode t)
  (keyfreq-autosave-mode t))

;; https://github.com/justbur/emacs-vdiff/blob/master/vdiff.el
;; shows deleted line as empty line -> much better than ediff
(use-package vdiff
  :config
  (setq
   vdiff-disable-folding t
   vdiff-auto-refine t
   vdiff-only-highlight-refinements nil  ; changed-line highlight is still useful, as refinements don't include symbol changes
   vdiff-subtraction-fill-char ? )  ; default = '-'
  (set-face-background 'vdiff-refine-changed (face-attribute 'diff-added :background))  ; default = yellow, change to light green to reduce num of colors
  (defun transpose-windows ()  ; from https://www.emacswiki.org/emacs/TransposeWindows
    "Transpose two windows.  If more or less than two windows are visible, error."
    (interactive)
    (unless (= 2 (count-windows))
      (error "There are not 2 windows."))
    (let* ((windows (window-list))
           (w1 (car windows))
           (w2 (nth 1 windows))
           (w1b (window-buffer w1))
           (w2b (window-buffer w2)))
      (set-window-buffer w1 w2b)
      (set-window-buffer w2 w1b)))
  (defun my-diff-cur-file-on-disk ()
    (interactive)
    ;; set y-or-n-p to nil -> do not compare with auto-save file, ref: https://github.com/jwiegley/emacs-release/blob/master/lisp/vc/ediff.el
    ;; don't use flet, which is deprecated
    (cl-letf (((symbol-function 'y-or-n-p) #'ignore))
      (vdiff-current-file))
    (transpose-windows)  ; keep current buffer on the left
    )
  :bind
  ("M-SPC M-d" . my-diff-cur-file-on-disk)  ; M-d is a bit similar to C-/ s d
  :bind
  (:map vdiff-mode-map
        ("C-c M-j" . vdiff-next-hunk)
        ("C-c M-k" . vdiff-previous-hunk)
        ("C-c M-v" . vdiff-receive-changes)  ; similar to C-v, it changes the current buffer
        ("C-c C-c" . vdiff-send-changes)  ; similar to C-c, it doesn't change the current buffer
        ))

;; https://github.com/bburns/clipmon
;; https://www.reddit.com/r/emacs/comments/2uu3wc/emacs_as_a_clipboard_manager_with_clipmon/
(use-package clipmon  ; xcv, cua mode, cut copy paste join delete, line-wise edit
  :init
  (setq clipmon-timer-interval 1)  ; check system clipboard every 1 sec, default = 2
  (setq kill-ring-max 1000)  ; default = 60
  (clipmon-mode 1)

  (setq save-interprogram-paste-before-kill t)  ; If you have something on the system clipboard, and then kill something in Emacs, then by default whatever you had on the system clipboard is gone and there is no way to get it back. Setting the following option makes it so that when you kill something in Emacs, whatever was previously on the system clipboard is pushed into the kill ring.  https://github.com/raxod502/radian/blob/master/init.el
  (cua-selection-mode -1)  ; load the package for cua copy and cut function, -1 -> don't set the keymap
  (setq cua-keep-region-after-copy t)

  ;; http://emacsredux.com/blog/2013/03/27/copy-filename-to-the-clipboard/ https://github.com/bbatsov/prelude
  (defun my-copy-file-path-to-clipboard ()  ; note: I replaced "name" by "path", as it copies the full path, not just the filename.
    "Copy the current buffer file path to the clipboard."
    (interactive)
    (let ((filename (if (equal major-mode 'dired-mode) default-directory (buffer-file-name))))
      (when filename (kill-new filename) (message "Copied buffer file path '%s' to the clipboard." filename))))

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
  (bind-key "M-C" 'my-duplicate-line-or-region)
  (bind-key "C-M-c" 'my-duplicate-line-or-region)  ; C-S-c is N/A in terminal.  original C-M-c runs the command exit-recursive-edit.
  (bind-key "C-v" 'yank)
  (bind-key "M-x"
            (lambda ()
              "Delete (not kill) the current line OR lines of selected region, including newline char. http://stackoverflow.com/questions/3958528"
              (interactive)
              (save-excursion
                (when (use-region-p) (delete-region (region-beginning) (region-end)))
                (my-delete-current-line))))
  (bind-key "M-X"
            (lambda ()  ; join-line-or-lines-in-region https://github.com/rejeep/emacs/blob/master/defuns.el
              "Join this line or the lines in the selected region."
              (interactive)
              (cond ((use-region-p)
                     (let ((min (line-number-at-pos (region-beginning))))
                       (goto-char (region-end))
                       (while (> (line-number-at-pos) min) (join-line))))
                    (t (join-line -1)))))  ; modification: join with next line, instead of prev line

  (defun my-delete-empty-lines ()
    "Replace repeated blank lines to just 1. Works on whole buffer or text selection, respects `narrow-to-region'. URL `http://ergoemacs.org/emacs/elisp_compact_empty_lines.html' Version 2017-09-22"
    (interactive)
    (let ($begin $end)
      (if (use-region-p)
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

  (defun my-get-clipboard-contents-as-one-line ()  ; good for pasting title+url
    ;; related package for link-capture: org-mac-link http://heikkil.github.io/blog/2015/05/08/notes-from-browser-window/ but no Linux version
    (let ((text (current-kill 0)))  ; don't manipulating the kill ring directly, e.g., (car kill-ring).
      (replace-regexp-in-string " *$" "\n" (replace-regexp-in-string "\n" " " text))))
  (bind-key "M-SPC v" (lambda () (interactive) (insert (my-get-clipboard-contents-as-one-line))))
  )

;; https://github.com/atykhonov/google-translate
(use-package google-translate  ; and google-search and url address
  :init
  (define-globalized-minor-mode  ; https://stackoverflow.com/questions/16048231/how-to-enable-a-non-global-minor-mode-by-default-on-emacs-startup
    global-goto-address-mode
    goto-address-mode
    (lambda ()
      ;; todo use (eq major-mode 'helm-mode) ??
      (when (or (not (stringp mode-name))  ; "mode-name" is not a string for html-mode
                (not (string= "Hmm" mode-name)))  ; disable for helm-mini
        (goto-address-mode))))
  (global-goto-address-mode)  ; clickable URLs
  :config
  (setq
   google-translate-default-source-language "en"
   google-translate-default-target-language "zh-TW"
   google-translate-output-destination 'echo-area)
  (defun google-translate--get-b-d1 () (list 427110 1469889687))  ; bug-fix https://github.com/atykhonov/google-translate/issues/52
  (defun my-google-translate-at-point ()
    (interactive)
    ;; don't use url-hexify-string, which doesn't support Chinese char
    (let ((word (if (use-region-p) (buffer-substring-no-properties (region-beginning) (region-end)) (thing-at-point 'word))))
      ;; for mac: press twice -> also open Dictionary.app which works offline http://larkery-blog-blog.tumblr.com/post/465585528/emacs-dictionaryapp
      (if (and (eq system-type 'darwin) (eq last-command 'my-google-translate-at-point))
          (shell-command (concat "open \"dict:///" word "\"")))
      (google-translate-at-point)))
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
  (defun my-new-chrome-tab ()
    (interactive)
    (if (window-system)
        ;; use about:blank to open a new tab https://productforums.google.com/forum/#!topic/chrome/PmCbBZ06gBA
        (if (eq system-type 'darwin)
            ;; (browse-url "https://www.google.com") doesn't have address bar focus
            ;; cannot open about:blank using browse-url, which doesn't support non-url parameter.
            (shell-command (concat "/Applications/Google\\ Chrome.app/Contents/MacOS/Google\\ Chrome 'about:blank' && open -a Google\\ Chrome"))
          ;; 2>/dev/null to silent the message: "Failed to launch GPU process."
          ;; alt: https://emacs.stackexchange.com/questions/40829/
          ;;   cannot use (shell-command "xdotool search --class 'google-chrome' windowactivate") which always activate oldest window (wanted most recent window)
          (shell-command "google-chrome 'about:blank' 2>/dev/null"))
      (error "Not in window-system")))

  :bind
  ("<f3>" . google-region-or-goto-address-at-point)
  ("C-t" . my-new-chrome-tab)
  ("<f2>" . my-google-translate-at-point)
  ("M-SPC <f2>" . google-translate-at-point-reverse)
  )

;; related: smart-shift: has annoying keychord function, smart-shift-left/right not useful due to auto-format
;; related: http://stackoverflow.com/questions/2423834/move-line-region-up-and-down-in-emacs
;; (drag-stuff-global-mode 1)  ; doesn't work
(use-package drag-stuff  ; and indent
  :init
  (defun region-line-beg ()
    (if (use-region-p)
        (save-excursion (goto-char (region-beginning)) (line-beginning-position))
      (line-beginning-position)))
  (defun region-line-end ()
    (if (use-region-p)
        (save-excursion (goto-char (region-end)) (line-end-position))
      (line-end-position)))
  (bind-key "M-\\"  ; ref: http://emacsredux.com/blog/2013/03/27/indent-region-or-buffer/
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
  ;; fix comment-region (original binding: M-;), which doesn't work well for multi-line, alternative https://www.emacswiki.org/emacs/CommentingCode
  (bind-key "C-/"  ; same as http://codemirror.net/demo/sublime.html
            (lambda ()
              "Comments or uncomments the region or the current line if there's no active region. http://stackoverflow.com/questions/9688748"
              (interactive)
              (let ((deactivate-mark nil))  ; keep region
                (comment-or-uncomment-region (region-line-beg) (region-line-end)))
              ))
  :bind
  ("M-<up>" . drag-stuff-up)
  ("M-<down>" . drag-stuff-down)
  ("M-<right>" . keyboard-indent)
  ("M-<left>" . keyboard-unindent)
  )

(defun my-sudo-edit (&optional arg)  ; From http://emacsredux.com/blog/2013/04/21/edit-files-as-root/
  "Edit currently visited file as root. With a prefix ARG prompt for a file to visit. Will also prompt for a file to visit if current buffer is not visiting a file."
  (interactive "P")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:" (ido-read-file-name "Find file(as root): ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

;; http://ergoemacs.org/emacs/emacs_new_empty_buffer.html
;; https://emacs.stackexchange.com/questions/35803/how-to-get-emacs-to-prompt-when-a-new-buffer-is-changed-on-save
;; https://emacs.stackexchange.com/questions/2191/copy-contents-of-current-buffer-in-a-temp-buffer-and-operate-on-that
(defun my-new-buffer-with-selected-string ()
  "Create a new empty buffer. New buffer will be named “untitled” or “untitled<2>”, “untitled<3>”, etc."
  (interactive)
  (let (($buf (generate-new-buffer "untitled"))
        ($str (if (use-region-p) (buffer-substring-no-properties (region-beginning) (region-end)) "")))
    (switch-to-buffer $buf)
    (undo-tree-mode 1)  ; https://www.reddit.com/r/emacs/comments/4akvr3/how_to_enable_undotreemode_in_fundamentalmode/
    (insert $str)
    (setq buffer-offer-save t)
    $buf))
(bind-key "C-n" #'my-new-buffer-with-selected-string)

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

(defun revert-buffer-no-confirm ()  ; https://emacs.stackexchange.com/questions/10348
  "Revert buffer without confirmation."
  (interactive) (revert-buffer t t))
(bind-key "C-r" #'revert-buffer-no-confirm)

(defun region-or-symbol-at-point ()
  (if (use-region-p) (buffer-substring-no-properties (region-beginning) (region-end)) (thing-at-point 'symbol)))

;; A Package in a league of its own: Helm http://tuhdo.github.io/helm-intro.html
;; Helm wiki https://github.com/emacs-helm/helm/wiki
;; Open Files In Different Ways https://www.emacswiki.org/emacs/OpenFilesInDifferentWays
;; ref. http://stackoverflow.com/a/12708839  https://github.com/aronasorman/dotfiles/blob/master/.emacs.d/init.el
;; note:
;;   space: fall back to matchplugin detected http://emacs.stackexchange.com/questions/4243/
;; TODO try: ace-isearch combines ace-jump-mode and helm-swoop http://sachachua.com/blog/2015/01/emacs-kaizen-ace-isearch-combines-ace-jump-mode-helm-swoop/
(use-package helm
  :config
  (setq helm-map (make-keymap))  ; https://stackoverflow.com/questions/7459019/is-there-a-way-to-reset-the-emacs-keymap
  (setq helm-buffer-map helm-map)  ; remove helm-buffer-map, which is redundant to me https://github.com/emacs-helm/helm/blob/master/helm-buffers.el
  ;; From https://gist.github.com/antifuchs/9238468
  (setq
   helm-always-two-windows t            ; maximizes current buffer when helm is called, so that helm-swoop can be used for buffer at bottom window
   helm-allow-mouse t                   ; mouse-1 select candidate, mouse-2 execute default action, mouse-3 popup menu actions https://github.com/emacs-helm/helm/issues/1746
   helm-mode-line-string ""             ; original value is outdated and annoying
   helm-follow-mode-persistent t        ; https://github.com/emacs-helm/helm/issues/210 https://github.com/emacs-helm/helm/issues/530
   helm-buffer-max-length 40            ; some source codes has long file names (default = 20)
   helm-dabbrev-cycle-threshold 0       ; disable no-popup-cycling, to always shows a list of candidates in a popup
   ;; helm-buffers-fuzzy-matching t        ; the default pattern search is better (more controlable) than fuzzy search
   ;; helm-move-to-line-cycle-in-source t  ; move to end or beginning of source when reaching top or bottom of source
   ;; helm-candidate-number-limit 1000     ; original value was 100, too small for shell history
   ;; helm-ff-skip-boring-files t
   ;; helm-ff-auto-update-initial-value t  ; auto update when only one candidate directory is matched. https://groups.google.com/forum/#!topic/emacs-helm/K3FuveL5uTY inconvenient if i wanna go to top level dir
   helm-dabbrev-separator-regexp "\\s-\\|\t\\|[(\[\{\"'`=<$,@.#+]\\|\\s\\\\|^\n\\|^"  ; remove ";" from separator to match tags
   helm-ff-transformer-show-only-basename nil  ; to show full file path in recentf/helm-locate in helm-mini https://emacs.stackexchange.com/questions/40934
   )
  (set-face-attribute 'helm-source-header nil :height 0.5)
  (add-to-list 'display-buffer-alist
               '("\\`\\*helm.*\\*\\'"
                 (display-buffer-reuse-window display-buffer-at-bottom)
                 (window-height . 0.25)))
  (advice-add 'helm-mouse-select-candidate :after (lambda (arg) (helm-exit-minibuffer)))  ; do default action immediately after mouse click
  ;; (add-to-list 'helm-sources-using-default-as-input 'helm-source-google-suggest) doesn't support region
  (defun helm-google-suggest-region-or-symbol ()  ; ref: helm-google-suggest-at-point https://github.com/emacs-helm/helm/pull/1614
    (interactive)
    (when (not (use-region-p)) (mc--select-thing-at-point 'symbol) (convert-to-shift-selection))
    (helm :sources 'helm-source-google-suggest
          :buffer "*helm google*"
          :input (region-or-symbol-at-point)))
  (helm-mode)
  (advice-add 'helm-org-goto-marker :after (lambda (arg) (my-recenter-no-redraw)))  ; recenter after any jump, and during follow. lambda is needed to match the num of arg http://stackoverflow.com/a/36896945/550243
  (with-eval-after-load "helm-ring" (helm-attrset 'follow 1 helm-source-mark-ring))  ; https://emacs.stackexchange.com/questions/26296

  (defun helm-copy-to-buffer ()  ; re-define helm-copy-to-buffer to delete active region (if any)
    (interactive)
    (with-helm-alive-p
      (helm-run-after-exit
       (lambda (cands)
         (with-helm-current-buffer
           (if (use-region-p) (delete-region (region-beginning) (region-end)))  ; added to delete region if any.
           (insert (mapconcat (lambda (c)
                                (format "%s" c))
                              cands "\n"))))
       (helm-marked-candidates))))

  ;; helm-swoop is for buffers, helm-grep is for files on-disk.
  ;; i don't use helm-ag as i don't wanna install extra software.
  (defun helm-grep-cur-dir-tree ()  ; http://stackoverflow.com/questions/30142296/
    "Recursively search current directory."
    (interactive)
    (setq  ; re-use minibuffer-history history
     minibuffer-history
     (let ((helm-grep-history minibuffer-history))
       (helm-do-grep-1
        (list default-directory)
        nil  ; no recurse to minimize delay
        nil  ; use default backend
        '("*")  ; all files, e.g., txt, cc
        (if (use-region-p) (buffer-substring-no-properties (region-beginning) (region-end))
          (thing-at-point 'symbol))  ; default-input = region / symbol
        )
       helm-grep-history)))

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
    (widen)  ; need it or cannot jump out the narrowed view (if any)
    (deactivate-mark)  ; to avoid distorted region with (setq cua-keep-region-after-copy t)
    ;; (jit-lock-fontify-now)  ; needed to have a full syntax highlighting, as emacs fontlock only the visited part of buffer for better performances on large buffer. https://groups.google.com/forum/#!topic/emacs-helm/YwqsyRRHjY4 but it takes too long time for large .cc files, TODO make it fontify small helm buffer only? http://oremacs.com/2015/04/22/swiper-0.3.0/
    (let ((imenu-auto-rescan t))
      (helm :sources 'helm-my-imenu-source
            :candidate-number-limit 9999  ; same as https://github.com/emacs-helm/helm/blob/master/helm-semantic.el
            ;; :preselect str
            :buffer "*helm imenu*"
            :history 'helm-my-imenu-history)
      ))

  (defvar helm-my-doc-files
    (helm-build-sync-source (concat "My Doc: " my-doc-directory)  ; ref: defvar helm-source-ido-virtual-buffers in https://github.com/emacs-helm/helm/blob/master/helm-buffers.el
      :candidates (lambda ()
                    (with-helm-current-buffer
                      ;; avoid duplication with helm-source-files-in-current-dir, ref: helm-files-in-current-dir-source in https://github.com/emacs-helm/helm/blob/master/helm-files.el
                      (unless (string-match "_doc/\\'" (helm-current-directory)) (directory-files my-doc-directory t))
                      ))
      :keymap helm-generic-files-map
      :action (helm-actions-from-type-file)
      :requires-pattern 1  ; to speedup helm-mini on Linux
      :match-part 'helm-basename  ; only match basename
      ))

  (require 'recentf)  ; needed for helm-my-delayed-recentf
  (defvar helm-my-delayed-recentf  ; replace helm-source-recentf, which is slow on Linux when the list contain contain something long to load (e.g., cc file in workspace) https://github.com/emacs-helm/helm/issues/1894
    (helm-build-sync-source "Recent"
      :candidates recentf-list
      :keymap helm-generic-files-map
      :action (helm-actions-from-type-file)
      :requires-pattern 1  ; to speedup helm-mini on Linux
      :match-part 'helm-basename  ; only match basename
      ))

  (require 'helm-for-files)  ; for helm-files-in-current-dir-source
  (defvar helm-file-basename-in-current-dir
    (helm-make-source "Current Dir" 'helm-files-in-current-dir-source
      :match-part 'helm-basename))  ; only match basename
  (defvar helm-my-mini-history nil)  ; separate histories for different Helm prompts http://emacs.stackexchange.com/questions/12338
  (setq helm-source-buffers-list (helm-make-source "Buffers" 'helm-source-buffers))  ; copy from helm-mini() in https://github.com/emacs-helm/helm/blob/master/helm-buffers.el
  (setq helm-my-mini-sources
        '(helm-source-buffers-list
          helm-file-basename-in-current-dir
          helm-my-delayed-recentf
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
                                    (if (use-region-p) (delete-region (region-beginning) (region-end)))  ; added to delete region if any.
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

  ;; a hack to show default-input (if any), helm maintainer didn't like it https://github.com/emacs-helm/helm/issues/491
  (add-hook 'helm-minibuffer-set-up-hook
            (lambda ()
              (unless (string= "" helm-input)
                (helm-set-pattern helm-input)
                (when (string= helm--prompt "Swoop: ")  ; "Swoop: " == helm-swoop-prompt
                  ;; select pre-input string (if any), so that typing any char will replace the pre-input string. helm doesn't support it by design https://github.com/emacs-helm/helm/issues/491
                  (setq this-command-keys-shift-translated t)
                  (handle-shift-selection)
                  (beginning-of-line)
                  ))))

  ;; Better Helm https://github.com/hatschipuh/better-helm
  ;; no (helm-ido-like-hide-modelines), ok to show buffer modelines
  ;; no (helm-ido-like-hide-helm-modeline), as helm-modeline may have useful info, e.g., num candidates
  (setq helm-swoop-split-window-function 'helm-default-display-buffer)  ; otherwise, helm-swoop uses half of the frame
  (setq helm-echo-input-in-header-line t)

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
    (setq helm-find-files-map (make-sparse-keymap))  ; originally defined in https://github.com/emacs-helm/helm/blob/master/helm-files.el
    (define-key helm-find-files-map (kbd "<backspace>") 'helm-ido-like-find-files-up-one-level-maybe)
    (define-key helm-find-files-map (kbd "DEL") 'helm-ido-like-find-files-up-one-level-maybe)
    (define-key helm-find-files-map (kbd "<return>") 'helm-ido-like-find-files-navigate-forward)
    (define-key helm-find-files-map (kbd "RET") 'helm-ido-like-find-files-navigate-forward)
    (setq helm-read-files-map 'helm-find-files-map))

  :bind
  ("M-w"       . helm-my-mini)  ; so that switch to prev file = M-w M-q
  ("<mouse-3>" . helm-my-mini)
  ("M-SPC M-m" . helm-my-imenu)
  ("M-SPC M-v" . helm-show-kill-ring)
  ("M-SPC M-l" . helm-google-suggest-region-or-symbol)
  ("M-SPC M-f" . helm-grep-cur-dir-tree)
  ("C-o" . helm-find-files)   ; original C-o runs the command open-line ≈ C-e RET
  ("M-SPC M-x" . helm-M-x)    ; if use M-x, M-X would be wasted.  Avoid using M-x / C-c M-x, as M-x = delete line
  ("<tab>" . helm-dabbrev)    ; better than dabbrev, which is not very useful if we have company, company doesn't include dabbrev
  ;; TODO helm-all-mark-rings cannot show context lines? https://github.com/emacs-helm/helm/issues/261
  ("M-SPC M-p" . helm-mark-ring)  ; or helm-all-mark-rings?
  ;; ("<f1> a" . helm-apropos)  ; the same as helm-mode-describe-function?
  :bind  ; cannot combine with above list in v25, due to: "Wrong type argument: listp, :map"
  (:map helm-map  ; defined in https://github.com/emacs-helm/helm/blob/master/helm.el
        ;; rule: unify movement shortcuts for helm and buffer
        ;; helm-yank-text-at-point is not useful, buggy in helm-swoop
        ("<up>"   . helm-previous-line)
        ("<down>" . helm-next-line)
        ("M-p" . helm-previous-line)
        ("M-n" . helm-next-line)
        ("M-i" . helm-previous-source)
        ("M-u" . helm-next-source)
        ("M-k" . helm-previous-10-line)
        ("M-j" . helm-next-10-line)
        ("M-," . previous-history-element)
        ("M-m" . next-history-element)  ; good to insert default-input / word under cursor https://emacs.stackexchange.com/questions/17754 https://github.com/emacs-helm/helm/issues/491
        ("C-M-k" . helm-previous-page)
        ("C-M-j" . helm-next-page)
        ("M-z" . helm-select-action)  ; list actions, useful?
        ("<backtab>" . helm-previous-line)
        ("<tab>" . helm-next-line)       ; = Chrome address bar, also enable left-hand-only-mode, not compatible with yasnippet?
        ("M-v" . helm-copy-to-buffer)    ; similar to C-v
        ("M-SPC M-k" . helm-beginning-of-buffer)
        ("M-SPC M-j" . helm-end-of-buffer)
        ("M-w" . helm-maybe-exit-minibuffer)
        ("RET" . helm-maybe-exit-minibuffer)
        ("M-RET" . helm-maybe-exit-minibuffer)  ; convenient as Meta is on usually inside helm
        ("C-w" . helm-buffer-run-kill-buffers)  ; only applicable in helm-buffer
        ("C-g" . helm-keyboard-quit)
        )
  :bind
  (:map shell-mode-map
        ("C-r" . helm-comint-input-ring)  ; similar to C-r in shell for reverse-search-history
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
  ("M-SPC <tab>" . helm-yas-complete)
  )

;; tips
;;   add prefix C/M-num to get num lines of context
;;   while searching, C-c C-e activates edit-mode, in which you can edit the buffer from within the helm-swoop buffer
;; http://sachachua.com/blog/2015/01/emacs-kaizen-helm-swoop-editing/
;; https://github.com/ShingoFukuyama/helm-swoop/#configure-pre-input-search-query
;; https://github.com/ShingoFukuyama/helm-swoop#config
;; helm-multi-swoop needs to first select files for search
;; helm-swoop-back-to-last-point should be superseeded by "move back"
;; problem:
;;   cannot disable regex
;; alt: isearch+ https://www.emacswiki.org/emacs/IsearchPlus
(use-package helm-swoop  ; and isearch
  :init
  (setq  ; for isearch-forward
   search-whitespace-regexp "[-_ \t\n]+"  ; make these equivalent for searching in codes: space newline tab hyphen underscore http://ergoemacs.org/emacs/emacs_make_modern.html
   lazy-highlight-cleanup nil
   lazy-highlight-initial-delay 0)
  (add-hook 'isearch-mode-end-hook (lambda () (when (and isearch-success (> (length isearch-string) 0)) (my-shift-select isearch-other-end))))
  (defun isearch-within-visible-portion-of-cur-buf ()  ; https://stackoverflow.com/questions/11569635/isearch-occur-visible-area-in-emacs
    (interactive)
    (save-restriction
      (narrow-to-region (window-start) (window-end))
      (isearch-forward)
      (message nil)))  ; to clear the minibuffer "I-search:" string
  (defun isearch-abort-all ()  ; to abort failed isearch by one command
    (interactive)
    (while isearch-mode (isearch-abort)))

  (defun my-helm-swoop ()
    (interactive)
    (if (and (use-region-p) (string-match "\n" (buffer-substring-no-properties (region-beginning) (region-end)))) (narrow-to-region (region-beginning) (region-end)))
    (helm-swoop)
    )
  (setq helm-swoop-move-to-line-cycle nil)  ; make it the same as other helm

  :bind
  ("M-l" . my-helm-swoop)  ; M-g is easier than C-f, and swoop is more useful
  ("M-L" . helm-multi-swoop-all)
  ("C-f" . isearch-within-visible-portion-of-cur-buf)
  ("C-M-f" . isearch-forward)  ; e.g., for full buffer word highlight

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
        ("M-n" . isearch-repeat-forward)
        ("M-p" . isearch-repeat-backward)
        ("M-e" . nil)
        ("M-m" . isearch-yank-word-or-char)
        ;; ("M-," . previous-history-element)
        ("M-j" . isearch-ring-advance)
        ("M-k" . isearch-ring-retreat)
        ("C-v" . isearch-yank-kill)  ; http://tahirhassan.blogspot.com/2014/01/emacs-cua-mode-and-isearch.html
        ("C-f" . isearch-repeat-forward)
        ("C-S-f" . isearch-repeat-backward)  ; "C-F" doesn't work?
        ("C-M-f" . isearch-repeat-backward)
        ("<escape>" . isearch-abort-all)
        ("<backspace>" . isearch-del-char)  ; del-char instead of cycle backward https://www.reddit.com/r/emacs/comments/2adj9w
        )
  )

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
  :defer 1  ; otherwise config won't be involved when calling mc/edit-lines, bug?
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
      (if (use-region-p)
          (progn
            (ignore-errors (mc/mark-next-like-this 1))
            (if (> (mc/num-cursors) 1)
                (mc/cycle-forward)
              (message "No match.")))  ; reformat it to "Search failed: ..."
        (when (= (mc/num-cursors) 1)  ; C-d has no well-defined behavior when there is no region and in mc mode
          (mc--select-thing-at-point 'symbol)
          (if (use-region-p)
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
  ("C-M-p" . my-mc/unmark-next-like-this-with-cycle)  ; = sublime's C-u https://www.sublimetext.com/docs/2/multiple_selection_with_the_keyboard.html
  ("C-M-n" . my-mc/mark-next-like-this-with-cycle)    ; as the movement is similar to M-n
  ("C-M-u" . my-mc/skip-to-next-like-this-with-cycle) ; sublime uses C-k C-d, which is not convenient http://stackoverflow.com/questions/11548308
  ("M-SPC C-a"   . mc/mark-all-like-this)
  ("M-SPC M-a" . mc/mark-all-like-this-dwim)
  ("M-SPC a" . mc/edit-lines)  ; useful after selecting a block of text, e.g., by expand-region
  ("C-M-<down-mouse-1>" . mc/add-cursor-on-click)  ; "C-<mouse-1>" is used and cannot be overrided in both Mac and Ubuntu https://github.com/magnars/multiple-cursors.el#binding-mouse-events (C-M-n + mouse click has a similar effect)
  ("C-M-v" . yank-rectangle)  ; paste multiple string copied by multiple cursors after exiting multiple-cursors https://emacs.stackexchange.com/questions/10879 related: https://emacs.stackexchange.com/questions/10836

  :bind
  (:map mc/keymap  ; https://github.com/magnars/multiple-cursors.el/blob/master/multiple-cursors-core.el
        ("C-s" . nil)  ; phi-search
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
        ))

;; can also use it to switch window https://emacs.stackexchange.com/questions/3458/how-to-switch-between-windows-quickly
;; https://github.com/abo-abo/avy/blob/master/avy.el
(use-package avy
  :config
  (setq avy-keys (number-sequence ?a ?z))
  (setq avy-case-fold-search nil)  ; do not ignore case
  (setq avy-dispatch-alist '((?C . avy-action-copy)))  ; ref: http://emacs.stackexchange.com/questions/27979/use-avy-to-select-distant-word-or-line-and-paste-at-point
  ;; (setq avy-background t) is too distracting
  (defun my-noshift-avy-goto ()
    (interactive)
    (deactivate-mark)
    (call-interactively 'avy-goto-char-2))  ; avy-goto-char may have too many candidates
  (defun my-shift-avy-goto ()
    (interactive)
    (setq this-command-keys-shift-translated t)
    (handle-shift-selection)
    (call-interactively 'avy-goto-char-2))
  :bind
  ("M-;" . my-noshift-avy-goto)  ; original M-; runs the command comment-dwim, e.g., add an end-of-line comment in cc mode, not very useful.  Both the shortcut and the command needs 2 hands.
  ("M-:" . my-shift-avy-goto)
  ("M-SPC M-;" . avy-goto-line)  ; if you enter a digit for avy-goto-line, it will switch to goto-line with that digit already entered.
  )


;;; programming
;; Emacs配置C语言编程环境 - 牛牛龙 http://yulongniu.bionutshell.org/blog/2014/12/02/emacs-config-c/
;; compilation feedback with colors https://www.youtube.com/watch?v=ZnWN7htqT48

(global-font-lock-mode t)  ; turn on syntax highlighting
;; (setq jit-lock-defer-time 0.05)  ; improve the scrolling speed for large files https://stackoverflow.com/questions/18316665/ http://tsengf.blogspot.com/2012/11/slow-scrolling-speed-in-emacs.html
;; (setq fast-but-imprecise-scrolling t)  ; https://emacs.stackexchange.com/questions/31402/how-to-avoid-scrolling-with-large-files-hanging-for-short-periods-of-time-hold

;; disable all c-electric kbd; not to override C-d; not to override "=" which disabled delete-selection-mode
(setq c++-mode-map (make-keymap))
(setq c-mode-map (make-keymap))
(setq protobuf-mode-map (make-keymap))
(setq makefile-mode-map (make-keymap))  ; https://github.com/jwiegley/emacs-release/blob/master/lisp/progmodes/make-mode.el
(setq sh-mode-map (make-keymap))  ; e.g., '=' runs the command sh-assignment, which breaks mc with (delete-selection-mode t)
;; (setq-default c-electric-flag nil)
;; (add-hook 'prog-mode-hook (lambda () (use-local-map nil)))  ; https://emacs.stackexchange.com/questions/33177 disable all mode specific kbd for shorter mc cmd lists (c-mode-base-map has many c-electric kbds)

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
             ;; default regex problems: no template item, can extract function names, but no item in some .h file
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
            (setq imenu-generic-expression '(("" "^\\( *message .+\\)$" 1)))))

(defun py-help-at-point nil)  ; https://marc.info/?l=python-mode&m=142597554420133&w=2 emacs 25: `*Python Help*` buffer keeps coming up, whenever the cursor stays for more than a second on something. http://grokbase.com/t/python/python-mode/15173ahftp/python-help-buffer
(add-hook 'python-mode-hook
          (lambda ()
            (setq-default tab-width 2)  ; o/w tab-width = 8 https://emacs.stackexchange.com/questions/17563
            (setq imenu-generic-expression '(("" "\\(^d.+\\)$" 1)))  ; first char = 'd' for "def "
            ))
;; Make sure the equals sign stands out in keyword arguments (adapted from emacs cookbook)
(defface font-lock-equals-keyword-arg-face
  '((t (:foreground "OrangeRed1")))
  "Font Lock mode face used for the equals in bla=5, foo=7 argument lists."
  :group 'font-lock-faces)
(defface font-lock-equals-assignment-face
  '((t (:foreground "LimeGreen")))
  "Font Lock mode face used for the equals in assignments, e.g. x = 56."
  :group 'font-lock-faces)
(font-lock-add-keywords 'python-mode
                        '((" \\(=\\) " 1 'font-lock-equals-assignment-face)
                          ("[^ ]\\(=\\)[^ ]" 1 'font-lock-equals-keyword-arg-face)))

;; https://stackoverflow.com/questions/9288181/converting-from-camelcase-to-in-emacs
;; https://github.com/akicho8/string-inflection
(use-package string-inflection
  :bind
  ;; it's common to run the command with forward-word (M-l)
  ("M-t" . string-inflection-ruby-style-cycle))  ; original M-t runs the command transpose-words

;; F7 for compilation (or 'eval-buffer', 'perl -c', etc.); same as in VC++. There the distance is not so important - I hit the key with force, then lean back in my chair to see where it will lead me. http://emacs-fu.blogspot.com/2009/07/keyboard-shortcuts.html
(use-package compile
  :config
  ;; Allow me to run multiple "compilations" (e.g., compile, test, grep) at the same time by renaming the compilation buffer
  (setq-default compilation-buffer-name-function
                (lambda (the-major-mode)
                  (format "*%s: %s*" (downcase the-major-mode) (buffer-name))))  ; use buffer-name instead of buffer-file-name, which may be too long and unreadable
  (setq compilation-ask-about-save nil          ; save before compiling
        compilation-always-kill t               ; kill old compile processes before starting the new one
        compilation-scroll-output 'first-error  ; automatically scroll to first error
        )
  (add-to-list 'display-buffer-alist
               '("\\`\\*compilation.+\\'"
                 (display-buffer-reuse-window display-buffer-at-bottom)
                 (window-height . 0.25)))
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

  ;; (add-hook 'compilation-mode-hook
  ;;           (lambda ()
  ;;             (setq-local imenu-generic-expression compilation-error-regexp-alist)))

  (defun next-error-cycle () (interactive) (condition-case nil (next-error) (error (next-error 1 t))) (my-recenter-no-redraw))  ; https://stackoverflow.com/questions/21125015

  ;; (gethash "*compilation*" compile-buf-to-src-buf)
  ;; (hash-table-count compile-buf-to-src-buf)
  ;; https://stackoverflow.com/questions/17659212/dont-display-compilation-buffer-in-emacs-until-the-process-exits-with-error-o
  (setq compilation-finish-functions
        ;; related https://emacs.stackexchange.com/questions/62/hide-compilation-window
        (lambda (compile-buf outstr)
          (message "Process in %s: %s" compile-buf (string-trim outstr))  ; trim to remove "\n" at the end
          (with-current-buffer (gethash compile-buf compile-buf-to-src-buf) ; compile-src-buffer
            (remhash compile-buf compile-buf-to-src-buf)
            (if (string-match "finished" outstr)
                (setq mode-line-process (format #("✔" 0 1 (face '(:foreground "green")))))
              (setq mode-line-process (format #("✘" 0 1 (face '(:foreground "red")))))
              (display-buffer compile-buf))  ; show it, but don't switch focus
            )
          t))
  ;; related https://emacs.stackexchange.com/questions/9949/how-to-access-the-original-buffer-when-running-m-x-compile-and-friends
  ;; use hash-table instead of buffer-local-var which is tedious to use
  (setq compile-buf-to-src-buf (make-hash-table :test 'equal))
  (defadvice compilation-start (around inhibit-display (command &optional mode name-function highlight-regexp))
    (message "Running: %s" (string-trim command))  ; trim to remove "\n" at the end
    (if (not (string-match "^\\(find\\|grep\\)" command))
        (cl-flet (((symbol-function 'display-buffer) #'ignore)
                  ((symbol-function 'set-window-point) #'ignore)
                  ((symbol-function 'goto-char) #'ignore))
          (save-window-excursion
            (puthash ad-do-it (current-buffer) compile-buf-to-src-buf)))
      ad-do-it))
  (ad-activate 'compilation-start)

  :bind
  ("M-SPC M-u" . next-error-cycle)
  ("M-SPC M-i" . previous-error)
  :bind
  (:map compilation-mode-map  ; https://github.com/emacs-mirror/emacs/blob/master/lisp/progmodes/compile.el
        ("M-n" . nil)  ; originally bind to compilation-next-error
        ("M-p" . nil)  ; originally bind to compilation-previous-error
        ("M-u" . compilation-next-error)
        ("M-i" . compilation-previous-error)
        ("C-c C-c" . kill-compilation)  ; originally bind to compile-goto-error, change it to kill process, i.e., same as shell.
        ("RET" . compile-goto-error)
        ))

;; (use-package flycheck  ; https://www.youtube.com/watch?v=iSL4wUKc8O4
;;   :config (hook-into-modes 'flycheck-mode '(prog-mode-hook)))

;; http://emacs.stackexchange.com/questions/12530
;; don't use global-highlight-symbol-mode coz: it is Very slow in org-mode and disabled at the beginning in all modes (bug due to incompatibility?) https://github.com/nschum/highlight-symbol.el/issues/11
;; old code for global mode: (define-globalized-minor-mode my-global-highlight-symbol-mode highlight-symbol-mode (lambda () (highlight-symbol-mode 1))) (my-global-highlight-symbol-mode 1)
;; xx highlight-symbol won't highlight selection https://github.com/nschum/highlight-symbol.el/issues/32
;; vv highlight-symbol-jump shows number of occurrences in minibuffer
;; don't use isearch, which needs an extra key to exit the isearch-mode
(use-package highlight-symbol  ; only for highlight-symbol-jump function
  :config
  (defun shift-select-next (str dir)
    (let ((case-fold-search nil))  ; set to case-sensitive
      (if (and (use-region-p) (if (< 0 dir) (< (point) (mark)) (< (mark) (point))))
          (exchange-point-and-mark))  ; to fix the problem no jumping when changing search direction
      (unless (re-search-forward str nil t dir)  ; 3rd param = t -> don't show message if not found
        (goto-char (if (< 0 dir) (point-min) (point-max)))  ; copied from highlight-symbol-jump for wrap around search
        (re-search-forward str nil nil dir)))
    ;; http://emacs.stackexchange.com/questions/14310/how-to-select-text-found-by-re-search-forward
    (if (> dir 0)  ; order mark and point according to the search direction, to enable repeated search.
        (progn (setf (point) (match-end 0)) (my-shift-select (match-beginning 0)))
      (setf (point) (match-beginning 0)) (my-shift-select (match-end 0)))
    (highlight-symbol-count str t)
    (my-recenter-no-redraw))
  (defun my-symbol-jump (dir)
    (if (use-region-p)
        ;; no need to use highlight-symbol-border-pattern (which is used in highlight-symbol-get-symbol) when we have a region
        (let ((str (regexp-quote (buffer-substring-no-properties (region-beginning) (region-end)))))
          (shift-select-next str dir)
          (add-to-history minibuffer-history-variable str))
      (highlight-symbol-jump dir)  ; search symbol with boundaries, different from the default isearch
      (add-to-history minibuffer-history-variable (highlight-symbol-get-symbol))  ; every search (e.g., C-n/p) update history to have a fast jump / re-search
      (my-recenter-no-redraw)))
  (defun my-next-symbol () (interactive) (my-symbol-jump 1))
  (defun my-prev-symbol () (interactive) (my-symbol-jump -1))
  (defun my-next-symbol-last-searched () (interactive) (shift-select-next (car minibuffer-history) 1))
  (defun my-prev-symbol-last-searched () (interactive) (shift-select-next (car minibuffer-history) -1))
  :bind
  ("M-m" . my-next-symbol)
  ("M-," . my-prev-symbol)
  ("M-M" . my-next-symbol-last-searched)
  ("M-<" . my-prev-symbol-last-searched)
  )

;; src https://github.com/fgeller/highlight-thing.el
;; alt: https://www.reddit.com/r/emacs/comments/5a2r14/how_to_get_emacs_to_highlight_all_instances_of/
(use-package highlight-thing
  :defer 3
  :config
  (setq highlight-thing-delay-seconds 0.1
        highlight-thing-case-sensitive-p t
        highlight-thing-prefer-active-region t
        highlight-thing-exclude-thing-under-point t
        highlight-thing-what-thing 'symbol
        highlight-thing-ignore-list '("+" "-" " " "  " "\n"))
  (global-highlight-thing-mode)
  )

;; related: light-symbol-mode: https://github.com/ntschutta/emacs/blob/master/light-symbol.el http://emacsblog.org/2007/04/17/quick-tip-light-symbol-mode/
;; related: https://github.com/kmmbvnr/emacs-config/blob/master/elisp/auto-highlight-symbol.el
;; related: idle-highlight-mode is not compatible with hl-mode on Linux
;; (use-package idle-highlight-mode  :config (setq idle-highlight-idle-time 0))
;; (define-globalized-minor-mode my-global-idle-highlight-mode idle-highlight-mode (lambda () (idle-highlight-mode 1)))  ; http://stackoverflow.com/questions/16048231/how-to-enable-a-non-global-minor-mode-by-default-on-emacs-startup
;; (my-global-idle-highlight-mode 1)

(use-package company
  :defer 3
  :config
  (setq company-show-numbers t)  ; show quick-reference numbers in the tooltip. (Select a completion with M-1 through M-0.)
  (setq company-selection-wrap-around t)  ; to move from last row to first row
  (setq-default company-minimum-prefix-length 3)  ; "setq" is not enough.  company company-minimum-prefix-length should >1 to avoid interfering with "<s" expansion
  (add-hook 'company-mode-hook (lambda () (setq-default company-minimum-prefix-length 3)))  ; needed for Linux (bug?)
  (global-company-mode)  ; use company-mode everywhere
  ;; :bind
  ;; ("<tab>" . company-complete-common-or-cycle)
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

;; https://github.com/kyagi/shell-pop-el
(use-package shell-pop  ; and term
  :config
  (setq shell-pop-shell-type (quote ("ansi-term" "*ansi-term*" (lambda nil (ansi-term shell-pop-term-shell))))
        shell-pop-window-size 25
        shell-pop-full-span t
        shell-pop-autocd-to-working-dir nil)
  (shell-pop--set-shell-type 'shell-pop-shell-type shell-pop-shell-type)  ; need to do this manually or not picked up by shell-pop http://pragmaticemacs.com/emacs/pop-up-a-quick-shell-with-shell-pop/

  (setq term-buffer-maximum-size 0)  ; buffer everything https://stackoverflow.com/questions/15468816
  (defun shell-send-region-or-line ()  ; similar to http://stackoverflow.com/a/7053298
    (interactive)
    (save-some-buffers t)  ; save all files, in case the shell command cause some problem.
    (if (string= "term-mode" major-mode)  ; similar to https://www.enigmacurry.com/2008/12/26/emacs-ansi-term-tricks/
        (progn
          (shell-pop-out)
          (shell-pop-up nil))  ; open a new term
      (let ((min nil) (max nil))
        (if (use-region-p)
            (setq min (region-beginning) max (region-end))
          (beginning-of-line)
          (if (string= "Shell-script" mode-name)  ; prefix '$' is not needed in .sh files
              (setq min (point-at-bol))
            (setq min (search-forward "$ " (line-end-position) t)))
          (setq max (point-at-eol)))
        (let ((current-window (get-buffer-window (current-buffer)))
              (command (if min (concat (buffer-substring-no-properties min max) "\n") nil)))  ; need to get the command before calling shell-pop, which changes buffer
          (shell-pop nil)
          (when min
            (comint-send-string "*ansi-term-1*" command)
            (select-window current-window)
            (unless (use-region-p) (forward-line)))))))
  (defun term-send-tab  () (interactive) (term-send-raw-string "\C-i"))
  (defun term-to-term-char-mode ()
    (interactive)
    (term-char-mode)
    (setq cursor-type 'box)
    (read-only-mode -1))
  (add-hook
   'term-mode-hook
   (lambda ()
     (setq show-trailing-whitespace nil)  ; i don't care about trailing whitespace in terminal
     (setq imenu-generic-expression '(("" "^\\$ \\(.*\\)$" 1)))))
  (add-hook 'shell-pop-in-after-hook (lambda () (term-to-term-char-mode) (set-process-query-on-exit-flag (get-buffer-process (current-buffer)) nil)))

  :bind
  ;; https://github.com/emacs-mirror/emacs/blob/master/lisp/term.el
  (:map term-mode-map  ; similar to shell-mode
        ("RET" . term-to-term-char-mode)
        ("C-c C-c" . nil)  ; originally bind to term-interrupt-subjob, i.e., was same as shell-mode
        ("M-p" . nil)
        ("M-n" . nil)
        ("M-r" . nil)
        ("M-s" . nil)
        ("C-d" . nil)  ; originally bind to term-delchar-or-maybe-eof
        )
  (:map term-raw-map
        ;; forward all keystrokes except: copy/paste, window functions
        ;; ("C-c C-c" . nil)  ; originally bind to term-send-raw
        ("<tab>" . term-send-tab)
        ("C-v" . term-paste)  ; https://stackoverflow.com/questions/2886184
        ("M-c" . my-copy)
        ("M-SPC" . nil)
        ;; movements
        ("M-j" . nil)
        ("M-k" . nil)
        ("M-u" . nil)
        ("M-i" . nil)
        ("M-y" . nil)
        ("M-o" . nil)
        ;; window/frame functions
        ("<escape>" . nil)
        ("C-g" . keyboard-quit)  ; for expanding window by <escape> x 2
        ("C-s" . nil)
        ("C-q" . nil)
        ("C-o" . nil)
        ("M-z" . nil)
        ("M-w" . nil)
        ("C-w" . nil)
        ("C-M-w" . nil)
        )
  :bind
  ("M-SPC M-s" . shell-send-region-or-line)
  )

;; https://www.emacswiki.org/emacs/UndoTree
;; http://www.dr-qubit.org/undo-tree.html
;; http://ergoemacs.org/emacs/emacs_best_redo_mode.html
;; http://pragmaticemacs.com/emacs/advanced-undoredo-with-undo-tree/
(use-package undo-tree
  :init
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
        ("M-SPC M-z" . undo-tree-visualize)
        ("C-z" . undo-tree-undo)
        ("C-S-z" . undo-tree-redo)
        ("C-M-z" . undo-tree-redo)  ; in case C-S-z doesn't work in terminal
        )
  :bind
  (:map undo-tree-visualizer-mode-map
        ;; make undo-tree-visualizer as a version selector (e.g., like helm-swoop as a location selector)
        ("C-g" . undo-tree-visualizer-abort)  ; = no change on buffer
        ("RET" . undo-tree-visualizer-quit)
        ))


;; "(use-package FEATURE-NAME", where FEATURE-NAME is what goes in the (provide ..) line of a package. For org-mode, that line is (provide 'org). http://emacs.stackexchange.com/questions/17710
(use-package org
  :mode (("\\.txt$" . org-mode))  ; Google Drive doesn't support viewing .org file -> org files use .txt extension
  :ensure org-plus-contrib  ; https://www.reddit.com/r/emacs/comments/5sx7j0/how_do_i_get_usepackage_to_ignore_the_bundled/ https://emacs.stackexchange.com/questions/7890
  :config
  (setq
   org-adapt-indentation nil         ; prevent shifting text inside a section when demoting the section heading
   org-confirm-babel-evaluate nil
   org-descriptive-links nil
   org-edit-src-content-indentation 0
   org-edit-src-persistent-message nil
   org-fontify-whole-heading-line t  ; fontify the whole line for headings (with a background color). https://github.com/fniessen/emacs-leuven-theme/blob/master/README.org
   org-highlight-links nil        ; i don't need those highlights (e.g., [[ ... ]]) https://emacs.stackexchange.com/questions/34290 and it seems to speedup file load time too
   org-link-search-must-match-exact-headline nil  ; do fuzzy text search http://orgmode.org/manual/External-links.html e.g., file:learn.txt::SRS
   org-list-demote-modify-bullet '(("+" . "-") ("-" . "+"))  ; only use "+" and "-" for bullet, no "*" which is used for headings. TODO disable "*" bullet in any case
   org-src-fontify-natively t     ; pretty fontification of source code blocks http://orgmode.org/worg/org-contrib/babel/examples/fontify-src-code-blocks.html
   org-src-tab-acts-natively t    ; TAB as if it were used in the language’s major mode
   org-src-window-setup 'current-window  ; when editing a code snippet, use the current window rather than popping open a new one
   org-startup-folded nil         ; http://emacs.stackexchange.com/questions/9709/keep-the-headlines-expanded-in-org-mode
   org-startup-truncated nil
   org-emphasis-alist  ; http://stackoverflow.com/questions/22491823/disable-certain-org-mode-markup
   ;; no bold, as bold doesn't work well for Chinese characters on Mac emacs
   '(("*" (:foreground "pink" :background "pink") "<b>" "</b>")  ; "test" is better than "highlighting"
     ("_" (:foreground "dark orange") "<span style=\"text-decoration:underline;\">" "</span>")  ; no underline which decrease readability
     ("~" org-verbatim "<code>" "</code>" verbatim)
     ;; ("=" ...)  ; =...= is common in sample code notes -> high FPR -> disable it
     ;; ("/" ...)  ; /.../ is common in path
     ))

  ;; to check syntax table -> <f1> s
  (modify-syntax-entry ?' "." org-mode-syntax-table)  ; to fix syntax subword movement (inconsistent between forward and backward) for words' , origin (wrong) value = "w p"?
  (modify-syntax-entry ?/ "." org-mode-syntax-table)  ; to make a word stop in the middle of "=/", e.g., in "file=/path/..."
  (modify-syntax-entry ?\n ">" org-mode-syntax-table) ; to unify org and prog modes: to make a word stop in between SPCs and RETs. \n = C-j in syntax table
  (modify-syntax-entry ?\; "w" org-mode-syntax-table) ; for ;tag syntax

  (defvar helm-my-org-in-buffer-headings-history nil)
  (defun helm-my-org-in-buffer-headings()
    (interactive)
    (widen)
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

;;; org-babel
  ;; read user input http://stackoverflow.com/questions/37028455
  (org-babel-do-load-languages
   'org-babel-load-languages  ; http://orgmode.org/manual/Languages.html
   '((shell . t)  ; uses 'shell' instead of 'sh' https://lists.gnu.org/archive/html/emacs-orgmode/2016-04/msg00298.html
     (python . t)
     (emacs-lisp . t)
     (java . t)
     (C . t)  ; for C, C++ and D http://emacs.stackexchange.com/questions/17673 requires "#+BEGIN_SRC C++" instead of "#+BEGIN_SRC c++"
     ))
  (setq
   org-babel-default-header-args:python  ; e.g., https://www.reddit.com/r/orgmode/comments/6fsaz3/
   '((:python . "python3")  ; python2 will be deprecated soon
     (:results . "output")
     )
   org-babel-default-header-args:C++  ; cannot be ":C"
   '((:includes . "<iostream> <string> <map>")  ; http://home.fnal.gov/~neilsen/notebook/orgExamples/org-examples.html#sec-18
     (:results . "verbatim")    ; o/w results may be formatted into a table https://orgmode.org/worg/org-contrib/babel/header-args.html
     (:flags   . "-std=c++11")  ; todo: switch to c++14
     ))

  ;; https://www.emacswiki.org/emacs/AddKeywords e.g., http://stackoverflow.com/a/28059832
  (font-lock-add-keywords
   'org-mode
   '(("\\(^ *\\$ \\)\\(.+\n\\)"
      ;; a strict syntax for single-line shell cmd to avoid false pos.
      ;; use \n instead of $ in the regex to avoid *org-emphasis* overriding end of line color.
      ;; for multi-line shell command -> use babel block, as multi-line-font-locking is not trival http://stackoverflow.com/questions/9452615
      (1 '(:foreground "#00AA00") t)
      (2 'org-code t))  ; adding 't' at the end will override other font lock, e.g., no bold in `$ reload *xyz*` https://www.gnu.org/software/emacs/manual/html_node/elisp/Search_002dbased-Fontification.html
     ;; unicode ref: http://www.fileformat.info/info/unicode/block/geometric_shapes/list.htm https://en.wikipedia.org/wiki/Geometric_Shapes
     ("^ *\\(+\\) " (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "▸"))))  ; "▶" is too distracting
     ("^ *\\(-\\) " (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "▹"))))
     ("\\(✘\\)" (1 '(:foreground "#BB0000")))
     ("\\(✔\\)" (1 '(:foreground "#00BB00")))
     ("\\(;[0-9a-z-]+\\)" (1 '(:foreground "purple1")))  ; ;tag uses only lower case to minimize num of choice; use '-' instead of '_' because 1) '-' is used in English word, 2) minimize num of choice for faster search, 3) easier to type
     ("\\(// \\)" (1 '(:foreground "#00CC00")))  ; comment
     ("\"\\(\\(?:.\\|\n\\)*?[^\\]\\)\"" 0 font-lock-string-face)  ; highlight double quoted text https://stackoverflow.com/questions/7155528
     ("\\(e\\.g\\.,\\|i\\.e\\.,\\|vs\\.\\|<->\\|->\\|<-\\|~>\\|<~\\|❱\\|<2.\\{13\\}>\\)" (1 '(:foreground "chocolate")))  ; common functional strings
     ;; syntax highlight for Org-mode inline source code src_lang{} https://stackoverflow.com/questions/20309842
     ;; removed "weight:" due to: Invalid face attribute :weight (quote normal). Invalid face attribute :weight (quote bold)
     ("\\(src_\\)\\([^[{]+\\)\\(\\[:.*\\]\\){\\([^}]*\\)}"
      (1 '(:foreground "blue" :height 50))   ; src_ part
      (2 '(:foreground "blue" :height 50))    ; "lang" part.
      (3 '(:foreground "#555555" :height 70)) ; [:header arguments] part.
      (4 'org-code) ; "code..." part.
      )
     ;; Allow missing [:header arguments]. Cannot combine with the full version by adding '?', because that produces following error on Linux
     ;;   Error during redisplay: (jit-lock-function 38127) signaled (error "No match 3 in highlight (3 (quote (:foreground \"#555555\" :height 70)))")
     ("\\(src_\\)\\([^[{]+\\){\\([^}]*\\)}"
      (1 '(:foreground "blue" :height 50)) ; src_ part
      (2 '(:foreground "blue" :height 50)) ; "lang" part.
      (3 'org-code) ; "code..." part.
      )
     )
   t)  ; override original highlighting, e.g., org-heading

  (add-hook 'org-mode-hook  ; adapted from http://orgmode.org/manual/Conflicts.html no need to set yas/trigger-key
            (lambda ()
              ;; syntax of ?_ is changed by org-mode itself: as part of the definition of the mode (see org.el) -> do it in hook https://emacs.stackexchange.com/questions/17284
              (modify-syntax-entry ?_ "_" org-mode-syntax-table)  ; to fix syntax subword movement (inconsistent between forward and backward) for words_with_underscore, origin (wrong) value = "w"
              (visible-mode)  ; disable folding entirely to avoid mis-trigger https://lists.gnu.org/archive/html/emacs-orgmode/2009-11/msg00108.html
              ))

  (put 'org-force-self-insert 'delete-selection t)  ; for inserting '|' inside org-table with region deletion
  (setf org-blank-before-new-entry '((heading . nil) (plain-list-item . nil)))  ; to avoid newline after M-RET at first list item http://stackoverflow.com/questions/28351465
  ;; delete highlighted region if any http://www.chenblog.xyz/questions/2614710/in-org-mode-how-to-make-return-delete-highlighted-region
  (define-key org-mode-map
    (kbd "RET")
    (lambda() (interactive) (if (use-region-p) (delete-region (region-beginning) (region-end))) (call-interactively 'org-return)))
  (define-key org-mode-map
    (kbd "M-RET")
    (lambda() (interactive) (if (use-region-p) (delete-region (region-beginning) (region-end))) (call-interactively 'org-meta-return)))

  :bind
  ("M-SPC M-o" . org-forward-sentence)
  ("M-SPC M-y" . org-backward-sentence)
  ;; alt: https://emacs.stackexchange.com/questions/20481/incrementing-characters-in-emacs-next-to-numbers
  ("M-_" . org-decrease-number-at-point)  ; original M-_ is not used. M-- is used as negative-argument
  ("M-+" . org-increase-number-at-point)  ; original M-+ is not used. C-+ is N/A in terminal; good for mc
  :bind
  (:map org-mode-map
        ("M-v" . org-cycle)
        ("M-S-v" . org-shifttab)
        ("M-SPC M-m" . helm-my-org-in-buffer-headings)
        ("C-c M-t" . org-table-transpose-table-at-point)  ; new kbd
        ("C-c M-g" . org-metaright)  ; originally bind to M-<right>, not convenient
        ("C-c M-s" . org-metaleft)
        ("C-c M-L" . org-shiftmetaright)
        ("C-c M-S" . org-shiftmetaleft)
        ("C-c M-k" . org-metaup)
        ("C-c M-j" . org-metadown)
        ("C-c M-o" . org-shiftright)
        ("C-c M-y" . org-shiftleft)
        ("C-c M-i" . org-shiftup)
        ("C-c M-u" . org-shiftdown)
        ("C-c C-n" . nil)  ; org-next-visible-heading, M-u is better
        ("C-c C-p" . nil)  ; similar to C-c C-n
        ([(shift left)]     . nil)  ; org-shiftleft
        ([(shift right)]    . nil)  ; org-shiftright
        ([(shift up)]       . nil)  ; org-shiftup
        ([(shift down)]     . nil)  ; org-shiftdown
        ([(control return)] . nil)  ; key n/a on TTY
        ;; ([(shift return)]   . nil)        ; key n/a on TTY
        ([(shift control return)] . nil)  ; key n/a on TTY
        ;; rebind org-metaup and org-metadown, as move tree is not common
        ("M-<up>" . nil)
        ("M-<down>" . nil)
        ("M-<left>" . nil)
        ("M-<right>" . nil)
        ("<tab>" . nil)
        ("<backtab>" . nil)
        ("M-{" . nil)
        ("M-}" . nil)
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

;; src https://github.com/Sodel-the-Vociferous/inline-crypt-el/blob/master/inline-crypt.el
(use-package inline-crypt
  :config
  (add-to-list 'display-buffer-alist  ; ref: https://www.gnu.org/software/emacs/manual/html_node/elisp/Display-Action-Functions.html
               '("\\`\\*inline-crpyt\\*\\'"
                 (display-buffer-reuse-window display-buffer-at-bottom)
                 (window-height . 0.25)))
  (defconst inline-crypt-openssl-args
    '("-a" ; Use base64 encoding, i.e., ASCII text based https://linuxconfig.org/using-openssl-to-encrypt-messages-and-files-on-linux
      "-salt" ; Salt, for flavour and security -> U2FsdGVkX decodes to Salted__ https://stackoverflow.com/questions/23205167/why-does-this-ciphertext-always-start-with-the-same-characters
      "-pass" "stdin" ; Use the first line of stdin as the password
      "-md" "sha256"  ; The key derivation uses message digest that was changed in openssl 1.1: Use SHA256 not MD5 as default digest https://stackoverflow.com/questions/39637388
      ))
  :bind
  ("C-e" . inline-crypt-encrypt-region)
  ("C-S-e" . inline-crypt-decrypt-region)
  )

;; (use-package ob-go  ; Golang in Babel
;;   :defer t
;;   :config
;;   (add-to-list 'org-babel-load-languages '(go . t))
;;   (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages))

;; src https://github.com/fniessen/emacs-leuven-theme
;; pros: prettier org-mode source code blocks https://www.reddit.com/r/emacs/comments/415imd/prettier_orgmode_source_code_blocks/
(use-package leuven-theme
  :after org  ; to redefine org-block-*-line
  :config
  (load-theme 'leuven t)
  (defface right-triangle-face '((t (:foreground "red"))) "Face for `right-triangle-face`.")  ; https://emacs.stackexchange.com/questions/13134/emphasise-the-current-error-in-the-compilation-window
  (set-fringe-bitmap-face 'right-triangle 'right-triangle-face)
  ;; remove org-*-line background, underline and overline, which are too disturbing.  or use (face-attribute 'default :background) instead of nil
  (set-face-attribute 'org-block-begin-line nil :underline nil :background nil :foreground "#00BB00")
  (set-face-attribute 'org-block-end-line   nil :overline  nil :background nil :foreground "#00BB00")
  (set-face-attribute 'org-block nil :background "#FFFFF0")  ; lighter than original color: FFFFE0
  (set-face-attribute 'org-meta-line nil :background nil)

  ;; (use-package mic-paren)
  ;; (paren-activate)
  ;; (setq paren-match-face "gold")
  )

;; highlight parentheses surrounding point https://www.emacswiki.org/emacs/HighlightParentheses https://github.com/tsdh/highlight-parentheses.el
(use-package highlight-parentheses  ; pairs, brackets
  :after leuven-theme
  :defer 3
  :init
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
   "M-SPC M-9"
   (lambda ()
     "Move cursor to the previous occurrence of left bracket.
The list of brackets to jump to is defined by `xah-left-brackets'.
URL `http://ergoemacs.org/emacs/emacs_navigating_keys_for_brackets.html'
Version 2015-10-01"
     (interactive)
     (search-backward-regexp (regexp-opt xah-left-brackets) nil t)))
  (bind-key
   "M-SPC M-0"
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

  :config
  (setq
   hl-paren-colors nil
   hl-paren-background-colors '("green"))
  (global-highlight-parentheses-mode)

  ;; http://emacs-fu.blogspot.com/2009/01/balancing-your-parentheses.html
  (show-paren-mode t)
  (setq show-paren-priority -10)  ; cannot be -100 which is lower than hl-line https://emacs.stackexchange.com/questions/17324/how-to-make-the-region-face-take-priority-over-the-show-paren-mode-face
  (setq show-paren-delay 0.1)  ; highlight matching parenthesis
  (set-face-background 'show-paren-match "gold")
  (set-face-foreground 'show-paren-mismatch "red")
  (defadvice show-paren-function  ; https://emacs.stackexchange.com/questions/28525
      (after show-matching-paren-offscreen activate)
    "If the matching paren is offscreen, show the matching line in the echo area (for few seconds). Has no effect if the character before point is not of the syntax class ')'."
    (interactive)
    (let* ((cb (char-before (point))) (matching-text (and cb (char-equal (char-syntax cb) ?\) ) (blink-matching-open))))))

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
  ;; adapted from http://ergoemacs.org/emacs/emacs_delete_backward_char_or_bracket_text.html
  (defun pair-del-pairs-or-region-2-ends ()
    "Delete bracket pair if it's a \"quote\" or bracket ()[]{}【】「」 etc, no op if no pair and no selection."
    (interactive)
    (if (use-region-p)
        (let ((deactivate-mark nil)
              (ξp1 (region-beginning))
              (ξp2 (region-end)))
          (goto-char ξp2)
          (delete-char -1)
          (goto-char ξp1)
          (delete-char 1)
          (goto-char (- ξp2 2))
          (my-shift-select ξp1)
          )
      (cond  ; no text selection
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
        (message "No bracket pair.")))))
  (bind-key "M-SPC <backspace>" 'pair-del-pairs-or-region-2-ends)
  (defun xah-delete-backward-bracket-pair ()
    "Delete the matching brackets/quotes to the left of cursor.
After the command, mark is set at the left matching bracket position, so you can `exchange-point-and-mark' to select it.
This command assumes the left of point is a right bracket, and there's a matching one before it.
What char is considered bracket or quote is determined by current syntax table.
URL `http://ergoemacs.org/emacs/emacs_delete_backward_char_or_bracket_text.html'
Version 2017-07-02"
    (interactive)
    (let (($p0 (point)) $p1)
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
  ;; adapted from https://www.reddit.com/r/emacs/comments/4531i9
  ;; modified to keep selection (if any), the latest version has extra function that is not necessary to me.
  ;; related: yf/replace-or-delete-pair https://emacs.stackexchange.com/questions/37634/how-to-replace-matching-parentheses
  (defun pair-insert-bracket-pair (φleft-bracket φright-bracket)
    "Wrap or Insert a matching bracket and place cursor in between.
If there's a text selection, wrap brackets around it. Else, smartly decide wrap or insert. (basically, if there's no char after cursor, just insert bracket pair.)
φleft-bracket ＆ φright-bracket are strings.
URL `http://ergoemacs.org/emacs/elisp_insert_brackets_by_pair.html'
Version 2015-04-19"
    (if (use-region-p)
        (progn
          (let ((deactivate-mark nil)
                (ξp1 (region-beginning))
                (ξp2 (region-end)))
            (goto-char ξp2)
            (insert φright-bracket)
            (goto-char ξp1)
            (insert φleft-bracket)
            (goto-char (+ ξp2 2))
            (my-shift-select ξp1)
            ))
      (progn ; no text selection
        (if
            (or
             (looking-at "[^-_[:alnum:]]")
             (eq (point) (point-max)))
            (progn
              (insert φleft-bracket φright-bracket)
              (search-backward φright-bracket ))
          (progn
            (let (ξp1 ξp2)
              ;; basically, want all alphanumeric, plus hyphen and underscore, but don't want space or punctuations. Also want chinese.
              (skip-chars-backward "-_[:alnum:]")
              (setq ξp1 (point))
              (skip-chars-forward "-_[:alnum:]")
              (setq ξp2 (point))
              (goto-char ξp2)
              (insert φright-bracket)
              (goto-char ξp1)
              (insert φleft-bracket)
              (goto-char (+ ξp2 (length φleft-bracket)))))))))
  (defun pair-insert-paren () (interactive) (pair-insert-bracket-pair "(" ")") )
  (defun pair-insert-square-bracket () (interactive) (pair-insert-bracket-pair "[" "]") )
  (defun pair-insert-angle-bracket () (interactive) (pair-insert-bracket-pair "<" ">") )
  (defun pair-insert-brace () (interactive) (pair-insert-bracket-pair "{" "}") )
  (defun pair-insert-double-quote () (interactive) (pair-insert-bracket-pair "\"" "\"") )
  (defun pair-insert-single-quote () (interactive) (pair-insert-bracket-pair "'" "'") )

  :bind
  ("M-SPC (" . pair-insert-paren)
  ("M-SPC [" . pair-insert-square-bracket)
  ("M-SPC <" . pair-insert-angle-bracket)
  ("M-SPC {" . pair-insert-brace)
  ("M-SPC \"" . pair-insert-double-quote)
  ("M-SPC '" . pair-insert-single-quote)
  )

;; not smooth sometimes?
(use-package beacon
  :defer 6
  :config
  ;; (beacon-mode 1)
  (setq beacon-blink-delay 0)
  (setq beacon-blink-duration 0.5))

;; shows the buffer position in mode line, good for TTY
;; https://www.emacswiki.org/emacs/SmlModeLine http://emacs-fu.blogspot.com/2010/03/showing-buffer-position-in-mode-line.html https://pastebin.com/d775fJxx
(use-package sml-modeline
  :defer 1
  :config
  (setq sml-modeline-borders '("❚" . ""))  ; to unify the background color to make it less distracting
  (defun set-sml-modeline-len () (setq sml-modeline-len (1+ (/ (window-total-width) 2))))  ; 1+ to make it covers center point.
  (set-sml-modeline-len)
  (add-hook 'window-configuration-change-hook #'set-sml-modeline-len)
  (set-face-background 'sml-modeline-vis-face "#FFFFFF")  ; "white" doesn't work on tty
  (sml-modeline-mode 1)
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
  )

;; sml is better than the original mode-line coz sml shows the full file path and original only shows the filename.
(use-package smart-mode-line  ; and hl-line
  :after sml-modeline  ; because mode-line-format uses sml-modeline
  :config
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
  (setq rm-whitelist '(""))           ; minor modes should not be displayed in mode line. http://ergoemacs.org/emacs/modernization_mode_line.html
  (setq sml/col-number-format "%3c")  ; as I allow long lines with large col number
  (setq sml/modified-char "M")        ; origin value = "x", not clear to me
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

  (global-hl-line-mode)
  (setq global-hl-line-sticky-flag t)
  (set-face-attribute 'hl-line nil :underline nil :background "#E5F1E0")  ; the underline markup will hide underscore and bottom part of some other chars https://debbugs.gnu.org/db/20/20510.html
  (with-eval-after-load 'helm
    (set-face-background 'helm-selection (face-attribute 'hl-line :background))  ; unify line color, esp for TTY mode whose original helm-selection color = dark red
    (set-face-foreground 'helm-selection nil)
    )
  ;; https://stackoverflow.com/questions/7271312/change-emacs-window-appearance-when-it-loses-focus
  (copy-face 'mode-line 'my-mode-line-backup)  ; move out from lambda in focus-out-hook to fix: error: Invalid face, my-mode-line-backup
  (copy-face 'hl-line 'my-hl-line-backup)
  (setq my-fringe-background-backup (face-background 'fringe))
  (set-face-attribute 'mode-line-inactive nil :background "gray92" :box nil)  ; original inactive color was too similar to the active color. "gray92" = inactive buffer bg color. require: AFTER sml/setup http://stackoverflow.com/questions/9446673
  (add-hook 'focus-out-hook
            (lambda ()
              (set-face-attribute 'fringe nil :background "black")
              (copy-face 'mode-line-inactive 'mode-line)
              (set-face-attribute 'hl-line nil :background "white")  ; hide current line
              ))
  (add-hook 'focus-in-hook
            (lambda ()
              (set-face-attribute 'fringe nil :background my-fringe-background-backup)
              (copy-face 'my-mode-line-backup 'mode-line)
              (copy-face 'my-hl-line-backup 'hl-line)))
  )


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
