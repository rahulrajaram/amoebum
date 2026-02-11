(defpackage :ptui.backend.ncurses
  (:use :cl)
  (:export #:make-ncurses-backend))

(in-package :ptui.backend.ncurses)

(defclass ncurses-backend (ptui.backend.protocol:terminal-backend)
  ((started-p :initform nil :accessor backend-started-p)
   (colors-enabled-p :initform nil :accessor backend-colors-enabled-p)
   (default-colors-enabled-p :initform nil :accessor backend-default-colors-enabled-p)
   (max-colors :initform 0 :accessor backend-max-colors)
   (pair-limit :initform 0 :accessor backend-pair-limit)
   (next-pair :initform 1 :accessor backend-next-pair)
   (pair-cache :initform (make-hash-table :test 'equal) :accessor backend-pair-cache))
  (:documentation "Real ncurses backend using cl-charms (charms/ll)."))

(defun make-ncurses-backend (&key caps)
  (make-instance 'ncurses-backend
                 :caps (or caps (ptui.term.caps:probe-terminal-caps))))

(defun %x256-from-rgb (r g b)
  (+ 16
     (* 36 (round (* (max 0 (min 255 r)) 5) 255))
     (* 6 (round (* (max 0 (min 255 g)) 5) 255))
     (round (* (max 0 (min 255 b)) 5) 255)))

(defun %x16-from-rgb (r g b)
  ;; Match ptui.core.color's quantization: luminance split + 3-bit cube.
  (let* ((brightp (>= (+ (* 30 r) (* 59 g) (* 11 b)) 12800))
         (base (+ (if (>= r 128) 1 0)
                  (if (>= g 128) 2 0)
                  (if (>= b 128) 4 0))))
    (if brightp
        (+ 8 base)
      base)))

(defun %rgb->idx (backend color)
  (cond
    ((or (null color) (eq color ptui.core.color:color-default))
     (if (backend-default-colors-enabled-p backend) -1 nil))
    ((typep color 'ptui.core.color:color-rgb)
     (let* ((r (ptui.core.color::color-rgb-r color))
            (g (ptui.core.color::color-rgb-g color))
            (b (ptui.core.color::color-rgb-b color)))
       (if (>= (backend-max-colors backend) 256)
           (%x256-from-rgb r g b)
           (%x16-from-rgb r g b))))
    (t
     nil)))

(defun %clamp-color-idx (backend idx &key (fgp t))
  (let ((maxc (backend-max-colors backend)))
    (cond
      ((null idx)
       (if (backend-default-colors-enabled-p backend)
           -1
           (if fgp charms/ll:COLOR_WHITE charms/ll:COLOR_BLACK)))
      ((= idx -1)
       (if (backend-default-colors-enabled-p backend)
           -1
           (if fgp charms/ll:COLOR_WHITE charms/ll:COLOR_BLACK)))
      ((>= maxc 256)
       (max 0 (min 255 idx)))
      ((>= maxc 16)
       (max 0 (min 15 idx)))
      (t
       (max 0 (min 7 (mod idx 8)))))))

(defun %pair-for (backend fg-idx bg-idx)
  (if (not (backend-colors-enabled-p backend))
      0
      (let* ((key (list fg-idx bg-idx))
             (cached (gethash key (backend-pair-cache backend))))
        (when cached
          (return-from %pair-for cached))
        (let ((next (backend-next-pair backend))
              (limit (backend-pair-limit backend)))
          (when (or (<= limit 0) (>= next limit))
            ;; Pair space exhausted; degrade gracefully to pair 0.
            (return-from %pair-for 0))
          (let ((rc (charms/ll:init-pair (the integer next)
                                         (the integer fg-idx)
                                         (the integer bg-idx))))
            (when (eql rc charms/ll:ERR)
              (return-from %pair-for 0))
            (setf (gethash key (backend-pair-cache backend)) next)
            (incf (backend-next-pair backend))
            next)))))

(defun %attrs->mask (attrs)
  (let ((mask charms/ll:A_NORMAL))
    (unless attrs
      (return-from %attrs->mask mask))
    (when (ptui.core.types:attrs-boldp attrs) (setf mask (logior mask charms/ll:A_BOLD)))
    (when (ptui.core.types:attrs-underlinep attrs) (setf mask (logior mask charms/ll:A_UNDERLINE)))
    (when (ptui.core.types:attrs-invertp attrs) (setf mask (logior mask charms/ll:A_REVERSE)))
    (when (ptui.core.types:attrs-dimp attrs) (setf mask (logior mask charms/ll:A_DIM)))
    ;; :italic and :strike are best-effort no-ops in curses.
    mask))

(defun %apply-style (backend fg bg attrs)
  (let* ((fg-idx (%clamp-color-idx backend (%rgb->idx backend fg) :fgp t))
         (bg-idx (%clamp-color-idx backend (%rgb->idx backend bg) :fgp nil))
         (pair (%pair-for backend fg-idx bg-idx))
         (attr-mask (%attrs->mask attrs))
         (pair-mask (if (and (backend-colors-enabled-p backend) (> pair 0))
                        (charms/ll:color-pair pair)
                        0))
         (mask (logior attr-mask pair-mask)))
    (charms/ll:attrset mask)))

(defun %key-event (key &key (ctrlp nil) (altp nil) (shiftp nil) (text? nil))
  (ptui.core.events:make-key-event key :ctrlp ctrlp :altp altp :shiftp shiftp :text? text?))

(defun %decode-getch (ch)
  (cond
    ((eql ch charms/ll:KEY_RESIZE) (list (%key-event :resize)))
    ((eql ch charms/ll:KEY_UP) (list (%key-event :up)))
    ((eql ch charms/ll:KEY_DOWN) (list (%key-event :down)))
    ((eql ch charms/ll:KEY_LEFT) (list (%key-event :left)))
    ((eql ch charms/ll:KEY_RIGHT) (list (%key-event :right)))
    ((or (eql ch charms/ll:KEY_ENTER) (= ch 10) (= ch 13)) (list (%key-event :enter)))
    ((= ch 27) (list (%key-event :escape)))
    ((or (eql ch charms/ll:KEY_BACKSPACE) (= ch 127) (= ch 8)) (list (%key-event :backspace)))
    ((or (= ch 9) (eql ch charms/ll:KEY_STAB)) (list (%key-event :tab)))
    ((= ch 3) (list (%key-event :ctrl-c :ctrlp t)))
    ((and (integerp ch) (<= 32 ch) (<= ch 126))
     (list (%key-event :text :text? (string (code-char ch)))))
    (t
     nil)))

(defmethod ptui.backend.protocol:backend-init ((backend ncurses-backend))
  (let ((init-ok nil))
    (unwind-protect
         (progn
           ;; Initialize curses and basic input behavior.
           (charms/ll:initscr)
           (charms/ll:raw)
           (charms/ll:noecho)
           (charms/ll:keypad charms/ll:*stdscr* charms/ll:TRUE)
           (charms/ll:nodelay charms/ll:*stdscr* charms/ll:TRUE)
           (ignore-errors (charms/ll:curs-set 0))
           ;; Colors are best-effort: initialize if available, but never claim truecolor.
           (when (not (eql (charms/ll:has-colors) charms/ll:FALSE))
             (when (not (eql (charms/ll:start-color) charms/ll:ERR))
               (setf (backend-colors-enabled-p backend) t
                     (backend-pair-limit backend) (max 0 charms/ll:*color-pairs*)
                     (backend-max-colors backend) (max 0 charms/ll:*colors*))
               (when (not (eql (charms/ll:use-default-colors) charms/ll:ERR))
                 (setf (backend-default-colors-enabled-p backend) t))))
           (setf (backend-started-p backend) t)
           (setf init-ok t)
           nil)
      (unless init-ok
        (ignore-errors (charms/ll:endwin))
        (setf (backend-started-p backend) nil)))))

(defmethod ptui.backend.protocol:backend-shutdown ((backend ncurses-backend))
  (declare (ignore backend))
  (ignore-errors (charms/ll:curs-set 1))
  (ignore-errors (charms/ll:endwin))
  nil)

(defmethod ptui.backend.protocol:backend-poll-events ((backend ncurses-backend))
  (declare (ignore backend))
  (let ((events '()))
    (loop for ch = (charms/ll:getch)
          while (not (eql ch charms/ll:ERR)) do
            (dolist (ev (%decode-getch ch))
              (push ev events)))
    (nreverse events)))

(defmethod ptui.backend.protocol:backend-size ((backend ncurses-backend))
  (declare (ignore backend))
  (multiple-value-bind (rows cols) (charms/ll:get-maxyx charms/ll:*stdscr*)
    (ptui.core.types:make-size (max 0 cols) (max 0 rows))))

(defmethod ptui.backend.protocol:backend-commit ((backend ncurses-backend) draw-ops)
  (let ((count 0))
    (dolist (op draw-ops)
      (incf count)
      (case (ptui.render.diff::draw-op-kind op)
        (:move
         (charms/ll:move (ptui.render.diff::draw-op-row op)
                         (ptui.render.diff::draw-op-col op)))
        (:style
         (%apply-style backend
                       (ptui.render.diff::draw-op-fg op)
                       (ptui.render.diff::draw-op-bg op)
                       (ptui.render.diff::draw-op-attrs op)))
        (:write
         (charms/ll:addstr (ptui.render.diff::draw-op-text op)))
        (:clear-eol
         (charms/ll:clrtoeol))
        (:clear-screen
         (charms/ll:clear))
        (:hide-cursor
         (ignore-errors (charms/ll:curs-set 0)))
        (:show-cursor
         (ignore-errors (charms/ll:curs-set 1)))
        ((:enter-alt :exit-alt)
         ;; No-op: curses owns the screen state.
         nil)
        (otherwise
         nil)))
    (charms/ll:refresh)
    count))
