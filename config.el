(setq user-full-name "Peng Ye"
      user-mail-address "yepeng230@gmail.com")

;; (defun peng/set-fonts ()
;;   (interactive)
;;   (set-face-attribute 'default nil :font (font-spec :family "FiraCode Nerd Font" :size 18))
;;   ;;(set-fontset-font t 'unicode (font-spec :family "Noto Sans CJK SC" :size 16) nil 'prepend)
;;   (set-fontset-font t '(#x2ff0 . #x9ffc) (font-spec :family "Noto Sans CJK SC" :size 18) nil 'prepend)
;;   )
;; (add-hook! 'window-setup-hook :append 'peng/set-fonts)
;;(setq doom-font (font-spec :family "FiraCode Nerd Font" :size 12 :weight 'medium))
(defun font-installed-p (font-name)
  "Check if font with FONT-NAME is available."
  (find-font (font-spec :name font-name)))
(defun centaur-setup-fonts ()
  "Setup fonts."
  (when (display-graphic-p)
    ;; Set default font
    (cl-loop for font in '("Cascadia Code" "Fira Code" "Jetbrains Mono"
                           "SF Mono" "Hack" "Source Code Pro" "Menlo"
                           "Monaco" "DejaVu Sans Mono" "Consolas")
             when (font-installed-p font)
             return (set-face-attribute 'default nil
                                        :family font
                                        :height 110))

    ;; Set mode-line font
    (cl-loop for font in '("Menlo" "SF Pro Display" "Helvetica")
             when (font-installed-p font)
             return (progn
                      (set-face-attribute 'mode-line nil :family font :height 120)
                      (when (facep 'mode-line-active)
                        (set-face-attribute 'mode-line-active nil :family font :height 120))
                      (set-face-attribute 'mode-line-inactive nil :family font :height 120)))

    ;; Specify font for all unicode characters
    (cl-loop for font in '("Segoe UI Symbol" "Symbola" "Symbol")
             when (font-installed-p font)
             return (if (< emacs-major-version 27)
                        (set-fontset-font "fontset-default" 'unicode font nil 'prepend)
                      (set-fontset-font t 'symbol (font-spec :family font) nil 'prepend)))

    ;; Emoji
    (cl-loop for font in '("Noto Color Emoji" "Apple Color Emoji" "Segoe UI Emoji")
             when (font-installed-p font)
             return (cond
                     ((< emacs-major-version 27)
                      (set-fontset-font "fontset-default" 'unicode font nil 'prepend))
                     ((< emacs-major-version 28)
                      (set-fontset-font t 'symbol (font-spec :family font) nil 'prepend))
                     (t
                      (set-fontset-font t 'emoji (font-spec :family font) nil 'prepend))))

    ;; Specify font for Chinese characters
    (cl-loop for font in '("LXGW Neo Xihei" "WenQuanYi Micro Hei Mono" "LXGW WenKai Screen"
                           "LXGW WenKai Mono" "PingFang SC" "Microsoft Yahei UI" "Simhei")
             when (font-installed-p font)
             return (progn
                      (setq face-font-rescale-alist `((,font . 1.3)))
                      (set-fontset-font t 'han (font-spec :family font))))))

(centaur-setup-fonts)
(add-hook 'window-setup-hook #'centaur-setup-fonts)
(add-hook 'server-after-make-frame-hook #'centaur-setup-fonts)

(setq doom-theme 'doom-tokyo-night)

(setq display-line-numbers-type 'relative)

(setq org-directory "~/org/"
      org-roam-directory "~/org/roam")

(if (display-graphic-p)
    (after! evil
      (defalias 'evil-insert-state 'evil-emacs-state)
      (define-key evil-emacs-state-map (kbd "<escape>") 'evil-normal-state)
      (setq evil-emacs-state-cursor 'bar)))

(setq-default
 delete-by-moving-to-trash t                      ; Delete files to trash
 window-combination-resize t                      ; take new window space from all other windows (not just current)
 x-stretch-cursor t)                              ; Stretch cursor to the glyph width

(setq undo-limit 80000000                         ; Raise undo-limit to 80Mb
      evil-want-fine-undo t                       ; By default while in insert all changes are one big blob. Be more granular
      auto-save-default t                         ; Nobody likes to loose work, I certainly don't
      truncate-string-ellipsis ""                ; Unicode ellispis are nicer than "...", and also save /precious/ space
      password-cache-expiry nil                   ; I can trust my computers ... can't I?
      scroll-preserve-screen-position 'always     ; Don't have `point' jump around
      scroll-margin 2                             ; It's nice to maintain a little margin
      confirm-kill-processes nil                  ; exit emacs without notification when use EAF
      confirm-kill-emacs nil
      display-time-default-load-average nil)      ; I don't think I've ever found this useful

(display-time-mode 1)                             ; Enable time in the mode-line

(unless (string-match-p "^Power N/A" (battery))   ; On laptops...
  (display-battery-mode 1))                       ; it's nice to know how much power you have

(global-subword-mode 1)                           ; Iterate through CamelCase words

(add-to-list 'default-frame-alist '(height . 24))
(add-to-list 'default-frame-alist '(width . 80))

(setq-default custom-file (expand-file-name ".custom.el" doom-private-dir))
(when (file-exists-p custom-file)
  (load custom-file))

(setq evil-vsplit-window-right t
      evil-split-window-below t)

(defadvice! prompt-for-buffer (&rest _)
  :after '(evil-window-split evil-window-vsplit)
  (consult-buffer))

(map! :map evil-window-map
      "SPC" #'rotate-layout
      ;; Navigation
      "<left>"     #'evil-window-left
      "<down>"     #'evil-window-down
      "<up>"       #'evil-window-up
      "<right>"    #'evil-window-right
      ;; Swapping windows
      "C-<left>"       #'+evil/window-move-left
      "C-<down>"       #'+evil/window-move-down
      "C-<up>"         #'+evil/window-move-up
      "C-<right>"      #'+evil/window-move-right)

(after! browse-url
  (setq browse-url-browser-function 'eaf-open-browser
        browse-url-generic-program "eaf-open-browser"))

(setq window-divider-default-right-width 24
      window-divider-default-places 'right-only
      x-underline-at-descent-line t)

;; (setq doom-theme 'doom-peacock)

(use-package! eaf
  :defer t
  :commands (eaf-open eaf-open-browser eaf-open-browser-other-window eaf-open-browser-with-history)
  :load-path "~/.config/emacs/.local/straight/repos/emacs-application-framework"
  :hook (eaf-mode . centaur-tabs-mode)
  :init
  ;; maps
  (map! :leader :desc "eaf search it"
        "o s e" #'eaf-search-it)
  (map! :leader :desc "eaf browser with history"
        "o s b" #'eaf-open-browser-with-history)
  :custom
  (eaf-browser-continue-where-left-off t)
  (eaf-browser-enable-adblocker t)
  :config
  (require 'eaf-browser)
  (require 'eaf-pdf-viewer)
  (require 'eaf-file-manager)
  (require 'eaf-pyqterminal)
  (require 'eaf-evil)
  (setq browse-url-browser-function 'eaf-open-browser)
  (setq eaf-evil-leader-key "C-SPC")
  (defalias 'browse-web #'eaf-open-browser)
  (eaf-bind-key scroll_up "C-n" eaf-pdf-viewer-keybinding)
  (eaf-bind-key scroll_down "C-p" eaf-pdf-viewer-keybinding)
  (eaf-bind-key nil "M-q" eaf-browser-keybinding)
  (setq eaf-browser-default-search-engine "duckduckgo")
  (setq eaf-proxy-type "http")
  (setq eaf-proxy-host "127.0.0.1")
  (setq eaf-proxy-port "7890")
  )

(use-package! calctex
  :defer t
  :commands (calctex-mode calc)
  :init
  (add-hook 'calc-mode-hook #'calctex-mode)
  :config
  (setq calctex-additional-latex-packages "
\\usepackage[usenames]{xcolor}
\\usepackage{soul}
\\usepackage{adjustbox}
\\usepackage{amsmath}
\\usepackage{amssymb}
\\usepackage{siunitx}
\\usepackage{cancel}
\\usepackage{mathtools}
\\usepackage{mathalpha}
\\usepackage{xparse}
\\usepackage{arevmath}"
        calctex-additional-latex-macros
        (concat calctex-additional-latex-macros
                "\n\\let\\evalto\\Rightarrow"))
  (defadvice! no-messaging-a (orig-fn &rest args)
    :around #'calctex-default-dispatching-render-process
    (let ((inhibit-message t) message-log-max)
      (apply orig-fn args)))
  ;; Fix hardcoded dvichop path (whyyyyyyy)
  (let ((vendor-folder (concat (file-truename doom-local-dir)
                               "straight/"
                               (format "build-%s" emacs-version)
                               "/calctex/vendor/")))
    (setq calctex-dvichop-sty (concat vendor-folder "texd/dvichop")
          calctex-dvichop-bin (concat vendor-folder "texd/dvichop")))
  (unless (file-exists-p calctex-dvichop-bin)
    (message "CalcTeX: Building dvichop binary")
    (let ((default-directory (file-name-directory calctex-dvichop-bin)))
      (call-process "make" nil nil nil))))

(setq calc-angle-mode 'rad  ; radians are rad
      calc-symbolic-mode t) ; keeps expressions like \sqrt{2} irrational for as long as possible

(after! text-mode
  (add-hook! 'text-mode-hook
    (unless (derived-mode-p 'org-mode)
      ;; Apply ANSI color codes
      (with-silent-modifications
        (ansi-color-apply-on-region (point-min) (point-max) t)))))

(use-package! org-transclusion
  :after org
  :commands org-transclusion-mode
  :init
  (map! :after org :map org-mode-map
        "<f12>" #'org-transclusion-mode))

(use-package! org-pandoc-import
  :after org)

(map! :map evil-org-mode-map
      :after evil-org
      :n "g <up>" #'org-backward-heading-same-level
      :n "g <down>" #'org-forward-heading-same-level
      :n "g <left>" #'org-up-element
      :n "g <right>" #'org-down-element)

(add-hook 'org-mode-hook 'turn-on-org-cdlatex)

(defadvice! org-edit-latex-emv-after-insert ()
  :after #'org-cdlatex-environment-indent
  (org-edit-latex-environment))

(setq org-re-reveal-theme "white"
      org-re-reveal-transition "slide"
      org-re-reveal-plugins '(markdown notes math search zoom))

(setq org-beamer-theme "[progressbar=foot]metropolis")

(use-package! org-modern
  :hook (org-mode . global-org-modern-mode)
  :config
  (setq org-modern-label-border 0.3))

(after! ox-latex
  (setq org-latex-pdf-process '("latexmk -xelatex -quiet -shell-escape -f %f"))
  (setq org-latex-listings 'minted)
  (add-to-list 'org-latex-classes
               '("article"
                 "\\documentclass[10pt,a4paper]{report}
\\usepackage{graphicx}
\\usepackage{xcolor}
\\usepackage{xeCJK}
\\usepackage{lmodern}
\\usepackage{verbatim}
\\usepackage{minted}
\\usepackage{fixltx2e}
\\usepackage{longtable}
\\usepackage{float}
\\usepackage{tikz}
\\usepackage{wrapfig}
\\usepackage{soul}
\\usepackage{textcomp}
\\usepackage{listings}
\\usepackage{geometry}
\\usepackage{algorithm}
\\usepackage{algorithmic}
\\usepackage{marvosym}
\\usepackage{wasysym}
\\usepackage{latexsym}
\\usepackage{natbib}
\\usepackage{fancyhdr}
\\usepackage[xetex,colorlinks=true,CJKbookmarks=true,
linkcolor=blue,
urlcolor=blue,
menucolor=blue]{hyperref}
\\usepackage{fontspec,xunicode,xltxtra}
\\newfontinstance\\MONO{\\fontnamemono}
\\newcommand{\\mono}[1]{{\\MONO #1}}
\\setCJKmainfont[Scale=0.9]{Adobe Fangsong Std}%中文字体
\\setCJKmonofont[Scale=0.9]{Adobe Fangsong Std}
\\hypersetup{unicode=true}
\\geometry{a4paper, textwidth=6.5in, textheight=10in,
marginparsep=7pt, marginparwidth=.6in}
\\punctstyle{kaiming}
\\title{}
\\fancyfoot[C]{\\bfseries\\thepage}
\\chead{\\MakeUppercase\\sectionmark}
\\pagestyle{fancy}
\\tolerance=1000
[NO-DEFAULT-PACKAGES]
[NO-PACKAGES]
[EXTRA]"
                 ("\\chapter{%s}" . "\\chapter*{%s}")
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
  )

(after! +lookup
  (set-lookup-handlers! 'eaf-open-browser-other-window
    :modes '(emacs-lisp-mode
             c++-mode
             markdown-mode
             org-mode)))

(after! pyim
  (require 'pyim-cregexp-utils)
  (require 'pyim-liberime)
  ;; 如果使用 popup page tooltip, 就需要加载 popup 包。
  ;; (require 'popup nil t)
  ;; (setq pyim-page-tooltip 'popup)

  ;; 如果使用 pyim-dregcache dcache 后端，就需要加载 pyim-dregcache 包。
  ;; (require 'pyim-dregcache)
  ;; (setq pyim-dcache-backend 'pyim-dregcache)

  ;; 加载 basedict 拼音词库。
  (pyim-basedict-enable)

  ;; 将 Emacs 默认输入法设置为 pyim.
  (setq default-input-method "pyim")

  ;; 显示 5 个候选词。
  (setq pyim-page-length 5)

  ;; 金手指设置，可以将光标处的编码（比如：拼音字符串）转换为中文。
  (global-set-key (kbd "M-j") 'pyim-convert-string-at-point)

  ;; 按 "C-<return>" 将光标前的 regexp 转换为可以搜索中文的 regexp.
  (define-key minibuffer-local-map (kbd "C-<return>") 'pyim-cregexp-convert-at-point)

  ;; 设置 pyim 默认使用的输入法策略，我使用全拼。
  (pyim-default-scheme 'ziranma-shuangpin)
  ;; (pyim-default-scheme 'wubi)
  ;; (pyim-default-scheme 'cangjie)

  ;; 设置 pyim 是否使用云拼音。
  (setq pyim-cloudim 'baidu)

  ;; 设置 pyim 探针
  ;; 我自己使用的中英文动态切换规则是：
  ;; 1. 光标只有在注释里面时，才可以输入中文。
  ;; 2. 光标前是汉字字符时，才能输入中文。
  ;; 3. 使用 M-j 快捷键，强制将光标前的拼音字符串转换为中文。
  (setq-default pyim-english-input-switch-functions
                '(pyim-probe-org-structure-template))

  (setq-default pyim-punctuation-half-width-functions
                '(pyim-probe-punctuation-line-beginning
                  pyim-probe-punctuation-after-punctuation))

  ;; 开启代码搜索中文功能（比如拼音，五笔码等）
  (pyim-isearch-mode 1)
)

(use-package! pyim-basedict
  :after pyim)

(use-package! xclip
  :config
  (xclip-mode 1)
  )

(use-package! image-roll
  :config
  (add-hook 'Tex-PDF-mode-hook 'pdf-view-roll-minor-mode)
  )
