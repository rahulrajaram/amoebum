(defpackage :ptui.test.terminal-diversity
  (:use :cl :fiveam)
  (:export #:ptui-terminal-diversity-suite #:run-all))

(in-package :ptui.test.terminal-diversity)

(def-suite ptui-terminal-diversity-suite
  :description "Terminal capability detection across diverse terminal emulator profiles.")

(in-suite ptui-terminal-diversity-suite)

;;; -----------------------------------------------------------------------
;;; Helper
;;; -----------------------------------------------------------------------

(defun make-env (terms)
  (lambda (name)
    (cdr (assoc name terms :test #'string=))))

;;; -----------------------------------------------------------------------
;;; 1. Standard xterm profiles
;;; -----------------------------------------------------------------------

(test ptui-caps-xterm-plain
  "TERM=xterm -> x16 color, mouse=t, alt-screen=t"
  (let ((caps (ptui.term.caps:probe-terminal-caps
               :env (make-env '(("TERM" . "xterm"))))))
    (is (string= "xterm" (ptui.term.caps:terminal-caps-term caps)))
    (is (eq nil (ptui.term.caps:terminal-caps-256colorp caps)))
    (is (eq nil (ptui.term.caps:terminal-caps-truecolorp caps)))
    (is (eq t   (ptui.term.caps:terminal-caps-mousep caps)))
    (is (eq t   (ptui.term.caps:terminal-caps-alt-screenp caps)))
    (is (eq :x16 (ptui.core.color:resolve-color-mode caps)))))

(test ptui-caps-xterm-256color
  "TERM=xterm-256color -> 256color=t, truecolor=nil"
  (let ((caps (ptui.term.caps:probe-terminal-caps
               :env (make-env '(("TERM" . "xterm-256color"))))))
    (is (eq t   (ptui.term.caps:terminal-caps-256colorp caps)))
    (is (eq nil (ptui.term.caps:terminal-caps-truecolorp caps)))
    (is (eq t   (ptui.term.caps:terminal-caps-mousep caps)))
    (is (eq t   (ptui.term.caps:terminal-caps-alt-screenp caps)))
    (is (eq :x256 (ptui.core.color:resolve-color-mode caps)))))

(test ptui-caps-xterm-256color-plus-truecolor
  "TERM=xterm-256color, COLORTERM=truecolor -> truecolor"
  (let ((caps (ptui.term.caps:probe-terminal-caps
               :env (make-env '(("TERM" . "xterm-256color")
                                ("COLORTERM" . "truecolor"))))))
    (is (eq t (ptui.term.caps:terminal-caps-truecolorp caps)))
    (is (eq t (ptui.term.caps:terminal-caps-256colorp caps)))
    (is (eq :truecolor (ptui.core.color:resolve-color-mode caps)))))

;;; -----------------------------------------------------------------------
;;; 2. tmux / screen profiles
;;; -----------------------------------------------------------------------

(test ptui-caps-screen-plain
  "TERM=screen -> x16, no 256color, mouse=t"
  (let ((caps (ptui.term.caps:probe-terminal-caps
               :env (make-env '(("TERM" . "screen"))))))
    (is (string= "screen" (ptui.term.caps:terminal-caps-term caps)))
    (is (eq nil (ptui.term.caps:terminal-caps-256colorp caps)))
    (is (eq nil (ptui.term.caps:terminal-caps-truecolorp caps)))
    (is (eq t   (ptui.term.caps:terminal-caps-mousep caps)))
    (is (eq :x16 (ptui.core.color:resolve-color-mode caps)))))

(test ptui-caps-screen-256color
  "TERM=screen-256color -> x256, 256=t"
  (let ((caps (ptui.term.caps:probe-terminal-caps
               :env (make-env '(("TERM" . "screen-256color"))))))
    (is (eq t   (ptui.term.caps:terminal-caps-256colorp caps)))
    (is (eq nil (ptui.term.caps:terminal-caps-truecolorp caps)))
    (is (eq :x256 (ptui.core.color:resolve-color-mode caps)))))

(test ptui-caps-tmux-plain
  "TERM=tmux -> x16, mouse=t"
  (let ((caps (ptui.term.caps:probe-terminal-caps
               :env (make-env '(("TERM" . "tmux"))))))
    (is (string= "tmux" (ptui.term.caps:terminal-caps-term caps)))
    (is (eq nil (ptui.term.caps:terminal-caps-256colorp caps)))
    (is (eq nil (ptui.term.caps:terminal-caps-truecolorp caps)))
    (is (eq t   (ptui.term.caps:terminal-caps-mousep caps)))
    (is (eq :x16 (ptui.core.color:resolve-color-mode caps)))))

(test ptui-caps-tmux-256color
  "TERM=tmux-256color -> x256"
  (let ((caps (ptui.term.caps:probe-terminal-caps
               :env (make-env '(("TERM" . "tmux-256color"))))))
    (is (eq t   (ptui.term.caps:terminal-caps-256colorp caps)))
    (is (eq nil (ptui.term.caps:terminal-caps-truecolorp caps)))
    (is (eq :x256 (ptui.core.color:resolve-color-mode caps)))))

(test ptui-caps-tmux-256color-plus-truecolor
  "TERM=tmux-256color, COLORTERM=truecolor -> truecolor"
  (let ((caps (ptui.term.caps:probe-terminal-caps
               :env (make-env '(("TERM" . "tmux-256color")
                                ("COLORTERM" . "truecolor"))))))
    (is (eq t (ptui.term.caps:terminal-caps-truecolorp caps)))
    (is (eq t (ptui.term.caps:terminal-caps-256colorp caps)))
    (is (eq :truecolor (ptui.core.color:resolve-color-mode caps)))))

;;; -----------------------------------------------------------------------
;;; 3. Modern terminal emulators
;;; -----------------------------------------------------------------------

(test ptui-caps-iterm2-profile
  "TERM=xterm-256color, COLORTERM=truecolor, TERM_PROGRAM=iTerm.app -> truecolor"
  (let ((caps (ptui.term.caps:probe-terminal-caps
               :env (make-env '(("TERM"         . "xterm-256color")
                                ("COLORTERM"    . "truecolor")
                                ("TERM_PROGRAM" . "iTerm.app"))))))
    (is (eq t (ptui.term.caps:terminal-caps-truecolorp caps)))
    (is (eq t (ptui.term.caps:terminal-caps-256colorp caps)))
    (is (eq :truecolor (ptui.core.color:resolve-color-mode caps)))))

(test ptui-caps-kitty-profile
  "TERM=xterm-kitty, COLORTERM=truecolor -> truecolor, 256=t (truecolor implies 256)"
  (let ((caps (ptui.term.caps:probe-terminal-caps
               :env (make-env '(("TERM"      . "xterm-kitty")
                                ("COLORTERM" . "truecolor"))))))
    (is (eq t (ptui.term.caps:terminal-caps-truecolorp caps)))
    ;; truecolor implies 256colorp in probe-terminal-caps
    (is (eq t (ptui.term.caps:terminal-caps-256colorp caps)))
    (is (eq :truecolor (ptui.core.color:resolve-color-mode caps)))))

(test ptui-caps-alacritty-profile
  "TERM=alacritty, COLORTERM=truecolor -> truecolor (TERM has no '256color' substring)"
  (let ((caps (ptui.term.caps:probe-terminal-caps
               :env (make-env '(("TERM"      . "alacritty")
                                ("COLORTERM" . "truecolor"))))))
    (is (eq t (ptui.term.caps:terminal-caps-truecolorp caps)))
    ;; truecolor implies 256colorp
    (is (eq t (ptui.term.caps:terminal-caps-256colorp caps)))
    (is (eq :truecolor (ptui.core.color:resolve-color-mode caps)))))

(test ptui-caps-24bit-colorterm-variant
  "TERM=xterm-256color, COLORTERM=24bit -> truecolor (24bit variant)"
  (let ((caps (ptui.term.caps:probe-terminal-caps
               :env (make-env '(("TERM"      . "xterm-256color")
                                ("COLORTERM" . "24bit"))))))
    (is (eq t (ptui.term.caps:terminal-caps-truecolorp caps)))
    (is (eq t (ptui.term.caps:terminal-caps-256colorp caps)))
    (is (eq :truecolor (ptui.core.color:resolve-color-mode caps)))))

;;; -----------------------------------------------------------------------
;;; 4. Degraded / edge cases
;;; -----------------------------------------------------------------------

(test ptui-caps-dumb-terminal
  "TERM=dumb -> x16, mouse=nil, alt-screen=nil"
  (let ((caps (ptui.term.caps:probe-terminal-caps
               :env (make-env '(("TERM" . "dumb"))))))
    (is (string= "dumb" (ptui.term.caps:terminal-caps-term caps)))
    (is (eq nil (ptui.term.caps:terminal-caps-256colorp caps)))
    (is (eq nil (ptui.term.caps:terminal-caps-truecolorp caps)))
    (is (eq nil (ptui.term.caps:terminal-caps-mousep caps)))
    (is (eq nil (ptui.term.caps:terminal-caps-alt-screenp caps)))
    (is (eq :x16 (ptui.core.color:resolve-color-mode caps)))))

(test ptui-caps-missing-term-defaults-to-dumb
  "TERM absent -> defaults to dumb behavior (no color, no mouse, no alt-screen)"
  (let ((caps (ptui.term.caps:probe-terminal-caps
               ;; env returns nil for every key
               :env (make-env '()))))
    (is (string= "dumb" (ptui.term.caps:terminal-caps-term caps)))
    (is (eq nil (ptui.term.caps:terminal-caps-256colorp caps)))
    (is (eq nil (ptui.term.caps:terminal-caps-truecolorp caps)))
    (is (eq nil (ptui.term.caps:terminal-caps-mousep caps)))
    (is (eq nil (ptui.term.caps:terminal-caps-alt-screenp caps)))
    (is (eq :x16 (ptui.core.color:resolve-color-mode caps)))))

(test ptui-caps-vt100-profile
  "TERM=vt100 -> x16, mouse=t (interactive but no color)"
  (let ((caps (ptui.term.caps:probe-terminal-caps
               :env (make-env '(("TERM" . "vt100"))))))
    (is (string= "vt100" (ptui.term.caps:terminal-caps-term caps)))
    (is (eq nil (ptui.term.caps:terminal-caps-256colorp caps)))
    (is (eq nil (ptui.term.caps:terminal-caps-truecolorp caps)))
    (is (eq t   (ptui.term.caps:terminal-caps-mousep caps)))
    (is (eq :x16 (ptui.core.color:resolve-color-mode caps)))))

(test ptui-caps-linux-console
  "TERM=linux -> x16, mouse=t (Linux virtual console)"
  (let ((caps (ptui.term.caps:probe-terminal-caps
               :env (make-env '(("TERM" . "linux"))))))
    (is (string= "linux" (ptui.term.caps:terminal-caps-term caps)))
    (is (eq nil (ptui.term.caps:terminal-caps-256colorp caps)))
    (is (eq nil (ptui.term.caps:terminal-caps-truecolorp caps)))
    (is (eq t   (ptui.term.caps:terminal-caps-mousep caps)))
    (is (eq :x16 (ptui.core.color:resolve-color-mode caps)))))

(test ptui-caps-rxvt-unicode-256color
  "TERM=rxvt-unicode-256color -> x256"
  (let ((caps (ptui.term.caps:probe-terminal-caps
               :env (make-env '(("TERM" . "rxvt-unicode-256color"))))))
    (is (eq t   (ptui.term.caps:terminal-caps-256colorp caps)))
    (is (eq nil (ptui.term.caps:terminal-caps-truecolorp caps)))
    (is (eq :x256 (ptui.core.color:resolve-color-mode caps)))))

;;; -----------------------------------------------------------------------
;;; 5. Color mode resolution precedence
;;; -----------------------------------------------------------------------

(test ptui-color-mode-precedence-truecolor-wins
  "truecolor > 256color > x16 precedence"
  (let* ((truecolor-caps (ptui.term.caps:probe-terminal-caps
                          :env (make-env '(("TERM"      . "xterm-256color")
                                          ("COLORTERM" . "truecolor")))))
         (x256-caps (ptui.term.caps:probe-terminal-caps
                     :env (make-env '(("TERM" . "xterm-256color")))))
         (x16-caps (ptui.term.caps:probe-terminal-caps
                    :env (make-env '(("TERM" . "xterm"))))))
    (is (eq :truecolor (ptui.core.color:resolve-color-mode truecolor-caps)))
    (is (eq :x256      (ptui.core.color:resolve-color-mode x256-caps)))
    (is (eq :x16       (ptui.core.color:resolve-color-mode x16-caps)))))

;;; -----------------------------------------------------------------------
;;; 6. color->sgr output across modes
;;; -----------------------------------------------------------------------

(test ptui-sgr-pure-red-truecolor
  "Pure red (255,0,0) in truecolor mode -> 38;2;255;0;0"
  (let ((color (ptui.core.color:make-color-rgb 255 0 0)))
    (is (string= "38;2;255;0;0"
                 (ptui.core.color:color->sgr color :mode :truecolor :fg-or-bg :fg)))))

(test ptui-sgr-pure-red-x256
  "Pure red (255,0,0) in x256 mode -> 38;5;196"
  (let ((color (ptui.core.color:make-color-rgb 255 0 0)))
    (is (string= "38;5;196"
                 (ptui.core.color:color->sgr color :mode :x256 :fg-or-bg :fg)))))

(test ptui-sgr-pure-red-x16
  "Pure red (255,0,0) in x16 mode -> 31 (standard red)"
  (let ((color (ptui.core.color:make-color-rgb 255 0 0)))
    ;; x16 index: brightp = (30*255 >= 12800) = 7650 >= 12800 => nil
    ;; base = 1 (R>=128) + 0 + 0 = 1
    ;; code = 30 + 1 = 31
    (is (string= "31"
                 (ptui.core.color:color->sgr color :mode :x16 :fg-or-bg :fg)))))

(test ptui-sgr-pure-white-truecolor
  "Pure white (255,255,255) in truecolor mode -> 38;2;255;255;255"
  (let ((color (ptui.core.color:make-color-rgb 255 255 255)))
    (is (string= "38;2;255;255;255"
                 (ptui.core.color:color->sgr color :mode :truecolor :fg-or-bg :fg)))))

(test ptui-sgr-pure-white-x256
  "Pure white (255,255,255) in x256 mode -> 38;5;231"
  (let ((color (ptui.core.color:make-color-rgb 255 255 255)))
    ;; x256 index: 16 + 36*round(255*5/255) + 6*round(255*5/255) + round(255*5/255)
    ;;           = 16 + 36*5 + 6*5 + 5 = 16 + 180 + 30 + 5 = 231
    (is (string= "38;5;231"
                 (ptui.core.color:color->sgr color :mode :x256 :fg-or-bg :fg)))))

(test ptui-sgr-pure-white-x16
  "Pure white (255,255,255) in x16 mode -> bright white fg"
  (let ((color (ptui.core.color:make-color-rgb 255 255 255)))
    ;; x16 index: brightp = (30*255 + 59*255 + 11*255 = 25500) >= 12800 => t
    ;; base = 1+2+4 = 7; idx = 8+7 = 15
    ;; fg: not (< 15 8), so (+ 82 15) = 97
    (is (string= "97"
                 (ptui.core.color:color->sgr color :mode :x16 :fg-or-bg :fg)))))

(test ptui-sgr-background-mode
  "Background mode changes prefix codes"
  (let ((red   (ptui.core.color:make-color-rgb 255 0 0))
        (white (ptui.core.color:make-color-rgb 255 255 255)))
    ;; truecolor bg: prefix 48
    (is (string= "48;2;255;0;0"
                 (ptui.core.color:color->sgr red :mode :truecolor :fg-or-bg :bg)))
    ;; x256 bg: prefix 48
    (is (string= "48;5;196"
                 (ptui.core.color:color->sgr red :mode :x256 :fg-or-bg :bg)))
    ;; x16 bg red: idx=1, (< 1 8) => (+ 40 1) = 41
    (is (string= "41"
                 (ptui.core.color:color->sgr red :mode :x16 :fg-or-bg :bg)))
    ;; x16 bg white: idx=15, not (< 15 8), so (+ 92 15) = 107
    (is (string= "107"
                 (ptui.core.color:color->sgr white :mode :x16 :fg-or-bg :bg)))))

(test ptui-sgr-default-color
  "Default color token returns reset codes"
  (is (string= "39" (ptui.core.color:color->sgr :default :mode :truecolor :fg-or-bg :fg)))
  (is (string= "49" (ptui.core.color:color->sgr :default :mode :truecolor :fg-or-bg :bg)))
  (is (string= "39" (ptui.core.color:color->sgr :default :mode :x256 :fg-or-bg :fg)))
  (is (string= "49" (ptui.core.color:color->sgr :default :mode :x16  :fg-or-bg :bg))))

;;; -----------------------------------------------------------------------
;;; 7. Case-insensitive COLORTERM matching
;;; -----------------------------------------------------------------------

(test ptui-caps-colorterm-mixed-case
  "COLORTERM=TrueColor (mixed case) -> truecolor=t"
  (let ((caps (ptui.term.caps:probe-terminal-caps
               :env (make-env '(("TERM"      . "xterm")
                                ("COLORTERM" . "TrueColor"))))))
    (is (eq t (ptui.term.caps:terminal-caps-truecolorp caps)))))

(test ptui-caps-colorterm-upper-case
  "COLORTERM=TRUECOLOR (upper case) -> truecolor=t"
  (let ((caps (ptui.term.caps:probe-terminal-caps
               :env (make-env '(("TERM"      . "xterm")
                                ("COLORTERM" . "TRUECOLOR"))))))
    (is (eq t (ptui.term.caps:terminal-caps-truecolorp caps)))))

(test ptui-caps-colorterm-24bit-upper-case
  "COLORTERM=24BIT (upper case) -> truecolor=t"
  (let ((caps (ptui.term.caps:probe-terminal-caps
               :env (make-env '(("TERM"      . "xterm")
                                ("COLORTERM" . "24BIT"))))))
    (is (eq t (ptui.term.caps:terminal-caps-truecolorp caps)))))

;;; -----------------------------------------------------------------------
;;; Entry point
;;; -----------------------------------------------------------------------

(defun run-all ()
  (run! 'ptui-terminal-diversity-suite))
