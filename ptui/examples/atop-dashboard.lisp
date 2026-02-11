(defpackage :ptui.examples.atop-dashboard
  (:use :cl)
  (:export #:main
           #:collect-host-snapshot
           #:build-atop-model
           #:make-atop-dashboard-state))

(in-package :ptui.examples.atop-dashboard)

(defstruct (cpu-counters (:constructor make-cpu-counters
                            (&key (user 0) (nice 0) (system 0) (idle 0)
                                  (iowait 0) (irq 0) (softirq 0) (steal 0))))
  (user 0 :type integer)
  (nice 0 :type integer)
  (system 0 :type integer)
  (idle 0 :type integer)
  (iowait 0 :type integer)
  (irq 0 :type integer)
  (softirq 0 :type integer)
  (steal 0 :type integer))

(defstruct (memory-counters (:constructor make-memory-counters
                               (&key (total-kb 0) (available-kb 0)
                                     (swap-total-kb 0) (swap-free-kb 0))))
  (total-kb 0 :type integer)
  (available-kb 0 :type integer)
  (swap-total-kb 0 :type integer)
  (swap-free-kb 0 :type integer))

(defstruct (filesystem-counters (:constructor make-filesystem-counters
                                   (&key (mount-count 0) (rw-mount-count 0)
                                         (root-device "n/a")
                                         (root-fstype "n/a"))))
  (mount-count 0 :type integer)
  (rw-mount-count 0 :type integer)
  (root-device "n/a" :type string)
  (root-fstype "n/a" :type string))

(defstruct (disk-counters (:constructor make-disk-counters
                             (&key (read-ios 0) (write-ios 0)
                                   (read-sectors 0) (write-sectors 0)
                                   (inflight 0) (io-ms 0)
                                   (rotational-devices 0)
                                   (ssd-devices 0))))
  (read-ios 0 :type integer)
  (write-ios 0 :type integer)
  (read-sectors 0 :type integer)
  (write-sectors 0 :type integer)
  (inflight 0 :type integer)
  (io-ms 0 :type integer)
  (rotational-devices 0 :type integer)
  (ssd-devices 0 :type integer))

(defstruct (network-counters (:constructor make-network-counters
                                (&key (iface-count 0) (rx-bytes 0) (tx-bytes 0)
                                      (rx-packets 0) (tx-packets 0)
                                      (fastest-link-mbps 0))))
  (iface-count 0 :type integer)
  (rx-bytes 0 :type integer)
  (tx-bytes 0 :type integer)
  (rx-packets 0 :type integer)
  (tx-packets 0 :type integer)
  (fastest-link-mbps 0 :type integer))

(defstruct (tcpip-counters (:constructor make-tcpip-counters
                              (&key (tcp-in-segs 0) (tcp-out-segs 0)
                                    (tcp-retrans-segs 0) (tcp-active-opens 0)
                                    (tcp-passive-opens 0) (tcp-curr-estab 0)
                                    (ip-in-receives 0) (ip-in-delivers 0)
                                    (ip-out-requests 0))))
  (tcp-in-segs 0 :type integer)
  (tcp-out-segs 0 :type integer)
  (tcp-retrans-segs 0 :type integer)
  (tcp-active-opens 0 :type integer)
  (tcp-passive-opens 0 :type integer)
  (tcp-curr-estab 0 :type integer)
  (ip-in-receives 0 :type integer)
  (ip-in-delivers 0 :type integer)
  (ip-out-requests 0 :type integer))

(defstruct (host-snapshot (:constructor make-host-snapshot
                             (&key (timestamp-ms 0)
                                   (cpu (make-cpu-counters))
                                   (memory (make-memory-counters))
                                   (filesystem (make-filesystem-counters))
                                   (disk (make-disk-counters))
                                   (network (make-network-counters))
                                   (tcpip (make-tcpip-counters)))))
  (timestamp-ms 0 :type integer)
  (cpu (make-cpu-counters) :type cpu-counters)
  (memory (make-memory-counters) :type memory-counters)
  (filesystem (make-filesystem-counters) :type filesystem-counters)
  (disk (make-disk-counters) :type disk-counters)
  (network (make-network-counters) :type network-counters)
  (tcpip (make-tcpip-counters) :type tcpip-counters))

(defstruct (cpu-model (:constructor make-cpu-model
                           (&key (usage-pct 0.0) (user-pct 0.0)
                                 (system-pct 0.0) (idle-pct 100.0))))
  (usage-pct 0.0 :type real)
  (user-pct 0.0 :type real)
  (system-pct 0.0 :type real)
  (idle-pct 100.0 :type real))

(defstruct (memory-model (:constructor make-memory-model
                              (&key (total-kb 0) (used-kb 0) (available-kb 0)
                                    (used-pct 0.0)
                                    (swap-total-kb 0) (swap-used-kb 0))))
  (total-kb 0 :type integer)
  (used-kb 0 :type integer)
  (available-kb 0 :type integer)
  (used-pct 0.0 :type real)
  (swap-total-kb 0 :type integer)
  (swap-used-kb 0 :type integer))

(defstruct (filesystem-model (:constructor make-filesystem-model
                                  (&key (mount-count 0) (rw-mount-count 0)
                                        (root-device "n/a")
                                        (root-fstype "n/a"))))
  (mount-count 0 :type integer)
  (rw-mount-count 0 :type integer)
  (root-device "n/a" :type string)
  (root-fstype "n/a" :type string))

(defstruct (disk-model (:constructor make-disk-model
                            (&key (read-iops 0.0) (write-iops 0.0)
                                  (read-kib-s 0.0) (write-kib-s 0.0)
                                  (inflight 0) (busy-pct 0.0)
                                  (rotational-devices 0)
                                  (ssd-devices 0))))
  (read-iops 0.0 :type real)
  (write-iops 0.0 :type real)
  (read-kib-s 0.0 :type real)
  (write-kib-s 0.0 :type real)
  (inflight 0 :type integer)
  (busy-pct 0.0 :type real)
  (rotational-devices 0 :type integer)
  (ssd-devices 0 :type integer))

(defstruct (network-model (:constructor make-network-model
                               (&key (iface-count 0)
                                     (rx-kib-s 0.0) (tx-kib-s 0.0)
                                     (rx-pps 0.0) (tx-pps 0.0)
                                     (fastest-link-mbps 0))))
  (iface-count 0 :type integer)
  (rx-kib-s 0.0 :type real)
  (tx-kib-s 0.0 :type real)
  (rx-pps 0.0 :type real)
  (tx-pps 0.0 :type real)
  (fastest-link-mbps 0 :type integer))

(defstruct (tcpip-model (:constructor make-tcpip-model
                             (&key (tcp-in-segs 0) (tcp-out-segs 0)
                                   (tcp-retrans-segs 0)
                                   (tcp-retrans-pct 0.0)
                                   (tcp-active-opens 0)
                                   (tcp-passive-opens 0)
                                   (tcp-curr-estab 0)
                                   (ip-in-receives 0)
                                   (ip-in-delivers 0)
                                   (ip-out-requests 0))))
  (tcp-in-segs 0 :type integer)
  (tcp-out-segs 0 :type integer)
  (tcp-retrans-segs 0 :type integer)
  (tcp-retrans-pct 0.0 :type real)
  (tcp-active-opens 0 :type integer)
  (tcp-passive-opens 0 :type integer)
  (tcp-curr-estab 0 :type integer)
  (ip-in-receives 0 :type integer)
  (ip-in-delivers 0 :type integer)
  (ip-out-requests 0 :type integer))

(defstruct (atop-model (:constructor make-atop-model
                            (&key (collected-at-ms 0)
                                  (cpu (make-cpu-model))
                                  (memory (make-memory-model))
                                  (filesystem (make-filesystem-model))
                                  (disk (make-disk-model))
                                  (network (make-network-model))
                                  (tcpip (make-tcpip-model)))))
  (collected-at-ms 0 :type integer)
  (cpu (make-cpu-model) :type cpu-model)
  (memory (make-memory-model) :type memory-model)
  (filesystem (make-filesystem-model) :type filesystem-model)
  (disk (make-disk-model) :type disk-model)
  (network (make-network-model) :type network-model)
  (tcpip (make-tcpip-model) :type tcpip-model))

(defstruct (atop-dashboard-state (:constructor make-atop-dashboard-state
                                   (&key
                                     (snapshot nil)
                                     (model (make-atop-model))
                                     (pausedp nil)
                                     (show-help-p nil)
                                     (refresh-ms 1000)
                                     (last-refresh-ms 0)
                                     (status-line "collecting...")
                                     (collect-fn #'collect-host-snapshot)
                                     (model-fn #'build-atop-model)
                                     (now-ms-fn #'ptui.util.time:monotonic-ms))))
  snapshot
  (model (make-atop-model) :type atop-model)
  (pausedp nil :type boolean)
  (show-help-p nil :type boolean)
  (refresh-ms 1000 :type integer)
  (last-refresh-ms 0 :type integer)
  (status-line "collecting..." :type string)
  (collect-fn #'collect-host-snapshot :type function)
  (model-fn #'build-atop-model :type function)
  (now-ms-fn #'ptui.util.time:monotonic-ms :type function))

(defun %ensure-state (state)
  (if (and state (typep state 'atop-dashboard-state))
      state
      (make-atop-dashboard-state)))

(defun %safe-parse-integer (value)
  (cond
    ((integerp value) value)
    ((stringp value)
     (let ((trimmed (string-trim '(#\Space #\Tab #\Newline #\Return) value)))
       (ignore-errors (parse-integer trimmed :junk-allowed t))))
    (t nil)))

(defun %split-words (line)
  (remove-if (lambda (token) (string= token ""))
             (uiop:split-string (or line "")
                                :separator '(#\Space #\Tab))))

(defun %read-lines (path)
  (handler-case
      (with-open-file (in path :direction :input :if-does-not-exist nil)
        (if in
            (loop for line = (read-line in nil nil)
                  while line
                  collect line)
            '()))
    (error ()
      '())))

(defun %safe-directory (pattern)
  (handler-case
      (directory pattern)
    (error ()
      '())))

(defun %path-interface-name (path)
  (let* ((parts (remove-if (lambda (token) (string= token ""))
                           (uiop:split-string (namestring path)
                                              :separator '(#\/))))
         (net-pos (position "net" parts :test #'string=)))
    (when (and net-pos
               (< (1+ net-pos) (length parts)))
      (nth (1+ net-pos) parts))))

(defun %net-link-speeds (read-lines-fn directory-fn)
  (let ((pairs '()))
    (dolist (path (funcall directory-fn #P"/sys/class/net/*/speed"))
      (let* ((iface (%path-interface-name path))
             (line (first (funcall read-lines-fn (namestring path))))
             (speed (%safe-parse-integer line)))
        (when (and iface speed (> speed 0))
          (push (cons iface speed) pairs))))
    pairs))

(defun %prefixp (prefix text)
  (uiop:string-prefix-p prefix text))

(defun %parse-proc-stat-lines (lines)
  (let ((line (find-if (lambda (candidate) (%prefixp "cpu " candidate)) lines)))
    (if (null line)
        (make-cpu-counters)
        (let* ((tokens (%split-words line))
               (numbers (mapcar #'%safe-parse-integer (rest tokens))))
          (labels ((nth-num (idx)
                     (or (nth idx numbers) 0)))
            (make-cpu-counters
             :user (nth-num 0)
             :nice (nth-num 1)
             :system (nth-num 2)
             :idle (nth-num 3)
             :iowait (nth-num 4)
             :irq (nth-num 5)
             :softirq (nth-num 6)
             :steal (nth-num 7)))))))

(defun %parse-meminfo-lines (lines)
  (let ((table (make-hash-table :test #'equal)))
    (dolist (line lines)
      (let ((colon (position #\: line)))
        (when colon
          (let* ((key (subseq line 0 colon))
                 (value (subseq line (1+ colon)))
                 (number (%safe-parse-integer value)))
            (when number
              (setf (gethash key table) number))))))
    (make-memory-counters
     :total-kb (or (gethash "MemTotal" table) 0)
     :available-kb (or (gethash "MemAvailable" table)
                       (gethash "MemFree" table)
                       0)
     :swap-total-kb (or (gethash "SwapTotal" table) 0)
     :swap-free-kb (or (gethash "SwapFree" table) 0))))

(defun %parse-mounts-lines (lines)
  (let ((mount-count 0)
        (rw-count 0)
        (root-device "n/a")
        (root-fstype "n/a"))
    (dolist (line lines)
      (let ((tokens (%split-words line)))
        (when (>= (length tokens) 4)
          (incf mount-count)
          (let ((device (nth 0 tokens))
                (mountpoint (nth 1 tokens))
                (fstype (nth 2 tokens))
                (options (nth 3 tokens)))
            (unless (%prefixp "ro" options)
              (incf rw-count))
            (when (string= mountpoint "/")
              (setf root-device device
                    root-fstype fstype))))))
    (make-filesystem-counters
     :mount-count mount-count
     :rw-mount-count rw-count
     :root-device root-device
     :root-fstype root-fstype)))

(defun %skip-disk-device-p (name)
  (or (%prefixp "loop" name)
      (%prefixp "ram" name)))

(defun %parse-diskstats-lines (lines)
  (let ((read-ios 0)
        (write-ios 0)
        (read-sectors 0)
        (write-sectors 0)
        (inflight 0)
        (io-ms 0))
    (dolist (line lines)
      (let ((tokens (%split-words line)))
        (when (>= (length tokens) 14)
          (let ((name (nth 2 tokens)))
            (unless (%skip-disk-device-p name)
              (incf read-ios (or (%safe-parse-integer (nth 3 tokens)) 0))
              (incf read-sectors (or (%safe-parse-integer (nth 5 tokens)) 0))
              (incf write-ios (or (%safe-parse-integer (nth 7 tokens)) 0))
              (incf write-sectors (or (%safe-parse-integer (nth 9 tokens)) 0))
              (incf inflight (or (%safe-parse-integer (nth 11 tokens)) 0))
              (incf io-ms (or (%safe-parse-integer (nth 12 tokens)) 0)))))))
    (make-disk-counters
     :read-ios read-ios
     :write-ios write-ios
     :read-sectors read-sectors
     :write-sectors write-sectors
     :inflight inflight
     :io-ms io-ms)))

(defun %sys-block-rotation-summary (read-lines-fn directory-fn)
  (let ((rotational 0)
        (ssd 0))
    (dolist (path (funcall directory-fn #P"/sys/block/*/queue/rotational"))
      (let* ((line (first (funcall read-lines-fn (namestring path))))
             (flag (%safe-parse-integer line)))
        (cond
          ((= flag 1) (incf rotational))
          ((= flag 0) (incf ssd)))))
    (values rotational ssd)))

(defun %parse-net-dev-lines (lines net-speeds)
  (let ((iface-count 0)
        (rx-bytes 0)
        (tx-bytes 0)
        (rx-packets 0)
        (tx-packets 0)
        (max-speed 0))
    (dolist (line lines)
      (let ((colon (position #\: line)))
        (when colon
          (let* ((iface (string-trim '(#\Space #\Tab) (subseq line 0 colon)))
                 (numbers (%split-words (subseq line (1+ colon)))))
            (unless (string= iface "lo")
              (incf iface-count)
              (incf rx-bytes (or (%safe-parse-integer (nth 0 numbers)) 0))
              (incf rx-packets (or (%safe-parse-integer (nth 1 numbers)) 0))
              (incf tx-bytes (or (%safe-parse-integer (nth 8 numbers)) 0))
              (incf tx-packets (or (%safe-parse-integer (nth 9 numbers)) 0))
              (let ((speed (cdr (assoc iface net-speeds :test #'string=))))
                (when speed
                  (setf max-speed (max max-speed speed)))))))))
    (make-network-counters
     :iface-count iface-count
     :rx-bytes rx-bytes
     :tx-bytes tx-bytes
     :rx-packets rx-packets
     :tx-packets tx-packets
     :fastest-link-mbps max-speed)))

(defun %snmp-section-table (lines protocol)
  (loop for header in lines
        for value in (rest lines)
        while value
        when (and (%prefixp (concatenate 'string protocol ":") header)
                  (%prefixp (concatenate 'string protocol ":") value))
          do (let* ((keys (rest (%split-words header)))
                    (vals (rest (%split-words value)))
                    (table (make-hash-table :test #'equal)))
               (loop for key in keys
                     for raw in vals do
                       (setf (gethash key table) (or (%safe-parse-integer raw) 0)))
               (return table))
        do (setf lines (rest lines))
        finally (return (make-hash-table :test #'equal))))

(defun %parse-snmp-lines (lines)
  (let ((tcp (%snmp-section-table lines "Tcp"))
        (ip (%snmp-section-table lines "Ip")))
    (make-tcpip-counters
     :tcp-in-segs (or (gethash "InSegs" tcp) 0)
     :tcp-out-segs (or (gethash "OutSegs" tcp) 0)
     :tcp-retrans-segs (or (gethash "RetransSegs" tcp) 0)
     :tcp-active-opens (or (gethash "ActiveOpens" tcp) 0)
     :tcp-passive-opens (or (gethash "PassiveOpens" tcp) 0)
     :tcp-curr-estab (or (gethash "CurrEstab" tcp) 0)
     :ip-in-receives (or (gethash "InReceives" ip) 0)
     :ip-in-delivers (or (gethash "InDelivers" ip) 0)
     :ip-out-requests (or (gethash "OutRequests" ip) 0))))

(defun collect-host-snapshot (&key
                                (read-lines-fn #'%read-lines)
                                (directory-fn #'%safe-directory)
                                (now-ms-fn #'ptui.util.time:monotonic-ms))
  (let* ((net-speeds (%net-link-speeds read-lines-fn directory-fn))
         (cpu (%parse-proc-stat-lines (funcall read-lines-fn "/proc/stat")))
         (memory (%parse-meminfo-lines (funcall read-lines-fn "/proc/meminfo")))
         (filesystem (%parse-mounts-lines (funcall read-lines-fn "/proc/self/mounts")))
         (disk (%parse-diskstats-lines (funcall read-lines-fn "/proc/diskstats")))
         (network (%parse-net-dev-lines (funcall read-lines-fn "/proc/net/dev")
                                        net-speeds))
         (tcpip (%parse-snmp-lines (funcall read-lines-fn "/proc/net/snmp"))))
    (multiple-value-bind (rotational ssd)
        (%sys-block-rotation-summary read-lines-fn directory-fn)
      (setf (disk-counters-rotational-devices disk) rotational
            (disk-counters-ssd-devices disk) ssd))
    (make-host-snapshot
     :timestamp-ms (funcall now-ms-fn)
     :cpu cpu
     :memory memory
     :filesystem filesystem
     :disk disk
     :network network
     :tcpip tcpip)))

(defun %cpu-total (cpu)
  (+ (cpu-counters-user cpu)
     (cpu-counters-nice cpu)
     (cpu-counters-system cpu)
     (cpu-counters-idle cpu)
     (cpu-counters-iowait cpu)
     (cpu-counters-irq cpu)
     (cpu-counters-softirq cpu)
     (cpu-counters-steal cpu)))

(defun %clamp-pct (value)
  (max 0.0 (min 100.0 value)))

(defun %safe-delta (curr prev)
  (max 0 (- curr prev)))

(defun %elapsed-seconds (previous current)
  (if previous
      (let ((delta-ms (%safe-delta (host-snapshot-timestamp-ms current)
                                   (host-snapshot-timestamp-ms previous))))
        (if (<= delta-ms 0)
            1.0
            (/ delta-ms 1000.0)))
      1.0))

(defun build-atop-model (previous current)
  (let* ((elapsed (%elapsed-seconds previous current))
         (cpu-prev (and previous (host-snapshot-cpu previous)))
         (cpu-now (host-snapshot-cpu current))
         (cpu-total-now (%cpu-total cpu-now))
         (cpu-total-prev (if cpu-prev (%cpu-total cpu-prev) cpu-total-now))
         (cpu-total-delta (max 1 (%safe-delta cpu-total-now cpu-total-prev)))
         (cpu-idle-now (+ (cpu-counters-idle cpu-now)
                          (cpu-counters-iowait cpu-now)))
         (cpu-idle-prev (if cpu-prev
                            (+ (cpu-counters-idle cpu-prev)
                               (cpu-counters-iowait cpu-prev))
                            cpu-idle-now))
         (cpu-idle-delta (%safe-delta cpu-idle-now cpu-idle-prev))
         (cpu-user-delta (%safe-delta (cpu-counters-user cpu-now)
                                      (if cpu-prev (cpu-counters-user cpu-prev) 0)))
         (cpu-system-delta (%safe-delta (cpu-counters-system cpu-now)
                                        (if cpu-prev (cpu-counters-system cpu-prev) 0)))
         (cpu-usage (%clamp-pct (* 100.0 (- 1.0 (/ cpu-idle-delta cpu-total-delta)))))
         (cpu-user-pct (%clamp-pct (* 100.0 (/ cpu-user-delta cpu-total-delta))))
         (cpu-system-pct (%clamp-pct (* 100.0 (/ cpu-system-delta cpu-total-delta))))
         (memory (host-snapshot-memory current))
         (mem-total (max 1 (memory-counters-total-kb memory)))
         (mem-available (max 0 (memory-counters-available-kb memory)))
         (mem-used (max 0 (- mem-total mem-available)))
         (mem-used-pct (%clamp-pct (* 100.0 (/ mem-used mem-total))))
         (swap-total (max 0 (memory-counters-swap-total-kb memory)))
         (swap-free (max 0 (memory-counters-swap-free-kb memory)))
         (swap-used (max 0 (- swap-total swap-free)))
         (filesystem (host-snapshot-filesystem current))
         (disk-now (host-snapshot-disk current))
         (disk-prev (and previous (host-snapshot-disk previous)))
         (disk-read-ios (%safe-delta (disk-counters-read-ios disk-now)
                                     (if disk-prev (disk-counters-read-ios disk-prev) 0)))
         (disk-write-ios (%safe-delta (disk-counters-write-ios disk-now)
                                      (if disk-prev (disk-counters-write-ios disk-prev) 0)))
         (disk-read-sectors (%safe-delta (disk-counters-read-sectors disk-now)
                                         (if disk-prev (disk-counters-read-sectors disk-prev) 0)))
         (disk-write-sectors (%safe-delta (disk-counters-write-sectors disk-now)
                                          (if disk-prev (disk-counters-write-sectors disk-prev) 0)))
         (disk-io-ms (%safe-delta (disk-counters-io-ms disk-now)
                                  (if disk-prev (disk-counters-io-ms disk-prev) 0)))
         (network-now (host-snapshot-network current))
         (network-prev (and previous (host-snapshot-network previous)))
         (rx-bytes (%safe-delta (network-counters-rx-bytes network-now)
                                (if network-prev (network-counters-rx-bytes network-prev) 0)))
         (tx-bytes (%safe-delta (network-counters-tx-bytes network-now)
                                (if network-prev (network-counters-tx-bytes network-prev) 0)))
         (rx-packets (%safe-delta (network-counters-rx-packets network-now)
                                  (if network-prev (network-counters-rx-packets network-prev) 0)))
         (tx-packets (%safe-delta (network-counters-tx-packets network-now)
                                  (if network-prev (network-counters-tx-packets network-prev) 0)))
         (tcp (host-snapshot-tcpip current))
         (tcp-out (max 1 (tcpip-counters-tcp-out-segs tcp))))
    (make-atop-model
     :collected-at-ms (host-snapshot-timestamp-ms current)
     :cpu (make-cpu-model
           :usage-pct cpu-usage
           :user-pct cpu-user-pct
           :system-pct cpu-system-pct
           :idle-pct (%clamp-pct (- 100.0 cpu-usage)))
     :memory (make-memory-model
              :total-kb mem-total
              :used-kb mem-used
              :available-kb mem-available
              :used-pct mem-used-pct
              :swap-total-kb swap-total
              :swap-used-kb swap-used)
     :filesystem (make-filesystem-model
                  :mount-count (filesystem-counters-mount-count filesystem)
                  :rw-mount-count (filesystem-counters-rw-mount-count filesystem)
                  :root-device (filesystem-counters-root-device filesystem)
                  :root-fstype (filesystem-counters-root-fstype filesystem))
     :disk (make-disk-model
            :read-iops (/ disk-read-ios elapsed)
            :write-iops (/ disk-write-ios elapsed)
            :read-kib-s (/ (/ (* disk-read-sectors 512.0) 1024.0) elapsed)
            :write-kib-s (/ (/ (* disk-write-sectors 512.0) 1024.0) elapsed)
            :inflight (disk-counters-inflight disk-now)
            :busy-pct (%clamp-pct (* 100.0 (/ disk-io-ms (* elapsed 1000.0))))
            :rotational-devices (disk-counters-rotational-devices disk-now)
            :ssd-devices (disk-counters-ssd-devices disk-now))
     :network (make-network-model
               :iface-count (network-counters-iface-count network-now)
               :rx-kib-s (/ (/ rx-bytes 1024.0) elapsed)
               :tx-kib-s (/ (/ tx-bytes 1024.0) elapsed)
               :rx-pps (/ rx-packets elapsed)
               :tx-pps (/ tx-packets elapsed)
               :fastest-link-mbps (network-counters-fastest-link-mbps network-now))
     :tcpip (make-tcpip-model
             :tcp-in-segs (tcpip-counters-tcp-in-segs tcp)
             :tcp-out-segs (tcpip-counters-tcp-out-segs tcp)
             :tcp-retrans-segs (tcpip-counters-tcp-retrans-segs tcp)
             :tcp-retrans-pct (%clamp-pct (* 100.0
                                             (/ (tcpip-counters-tcp-retrans-segs tcp)
                                                tcp-out)))
             :tcp-active-opens (tcpip-counters-tcp-active-opens tcp)
             :tcp-passive-opens (tcpip-counters-tcp-passive-opens tcp)
             :tcp-curr-estab (tcpip-counters-tcp-curr-estab tcp)
             :ip-in-receives (tcpip-counters-ip-in-receives tcp)
             :ip-in-delivers (tcpip-counters-ip-in-delivers tcp)
             :ip-out-requests (tcpip-counters-ip-out-requests tcp)))))

(defun %safe-kib->string (kib)
  (cond
    ((>= kib 1048576) (format nil "~,1f GiB" (/ kib 1048576.0)))
    ((>= kib 1024) (format nil "~,1f MiB" (/ kib 1024.0)))
    (t (format nil "~D KiB" kib))))

(defun %fit-width (text width)
  (ptui.text.layout:truncate-to-width (or text "") (max 0 width)))

(defun %template-cell (&key (fg :default) (bg :default) (boldp nil))
  (ptui.core.types:make-cell
   " "
   fg
   bg
   (ptui.core.types:make-attrs :boldp boldp)))

(defun %partition-dimension (total parts)
  (let ((base (if (> parts 0) (floor total parts) 0))
        (extra (if (> parts 0) (mod total parts) 0)))
    (loop for i from 0 below parts
          collect (+ base (if (< i extra) 1 0)))))

(defun %draw-panel (buf rect title lines)
  (let* ((x (ptui.core.types:rect-x rect))
         (y (ptui.core.types:rect-y rect))
         (w (ptui.core.types:rect-w rect))
         (h (ptui.core.types:rect-h rect))
         (title-cell (%template-cell :fg (ptui.core.color:make-color-rgb 130 210 255) :boldp t))
         (line-cell (%template-cell :fg (ptui.core.color:make-color-rgb 210 210 210))))
    (when (and (>= w 4) (>= h 3))
      (ptui.render.buffer:buffer-draw-border buf rect :border-style :square)
      (ptui.render.buffer:buffer-draw-text
       buf
       (+ x 2)
       y
       (list (list (%fit-width title (max 0 (- w 4))) title-cell)))
      (loop for line in lines
            for row from 0
            while (< row (max 0 (- h 2))) do
              (ptui.render.buffer:buffer-draw-text
               buf
               (1+ x)
               (+ y 1 row)
               (list (list (%fit-width line (max 0 (- w 2))) line-cell)))))))

(defun %cpu-lines (model)
  (let ((cpu (atop-model-cpu model)))
    (list (format nil "usage  ~,1f%" (cpu-model-usage-pct cpu))
          (format nil "user   ~,1f%" (cpu-model-user-pct cpu))
          (format nil "system ~,1f%" (cpu-model-system-pct cpu))
          (format nil "idle   ~,1f%" (cpu-model-idle-pct cpu)))))

(defun %memory-lines (model)
  (let ((mem (atop-model-memory model)))
    (list (format nil "used  ~A / ~A (~,1f%)"
                  (%safe-kib->string (memory-model-used-kb mem))
                  (%safe-kib->string (memory-model-total-kb mem))
                  (memory-model-used-pct mem))
          (format nil "avail ~A" (%safe-kib->string (memory-model-available-kb mem)))
          (format nil "swap  ~A / ~A"
                  (%safe-kib->string (memory-model-swap-used-kb mem))
                  (%safe-kib->string (memory-model-swap-total-kb mem))))))

(defun %filesystem-lines (model)
  (let ((fs (atop-model-filesystem model)))
    (list (format nil "mounts  ~D total / ~D rw"
                  (filesystem-model-mount-count fs)
                  (filesystem-model-rw-mount-count fs))
          (format nil "root fs ~A" (filesystem-model-root-fstype fs))
          (format nil "root dev ~A" (filesystem-model-root-device fs)))))

(defun %disk-lines (model)
  (let ((disk (atop-model-disk model)))
    (list (format nil "read  ~,1f iops  ~,1f KiB/s"
                  (disk-model-read-iops disk)
                  (disk-model-read-kib-s disk))
          (format nil "write ~,1f iops  ~,1f KiB/s"
                  (disk-model-write-iops disk)
                  (disk-model-write-kib-s disk))
          (format nil "busy  ~,1f%  inflight ~D"
                  (disk-model-busy-pct disk)
                  (disk-model-inflight disk))
          (format nil "media rot ~D / ssd ~D"
                  (disk-model-rotational-devices disk)
                  (disk-model-ssd-devices disk)))))

(defun %network-lines (model)
  (let ((network (atop-model-network model)))
    (list (format nil "rx ~,1f KiB/s (~,1f pps)"
                  (network-model-rx-kib-s network)
                  (network-model-rx-pps network))
          (format nil "tx ~,1f KiB/s (~,1f pps)"
                  (network-model-tx-kib-s network)
                  (network-model-tx-pps network))
          (format nil "ifaces ~D  max-link ~D Mbps"
                  (network-model-iface-count network)
                  (network-model-fastest-link-mbps network)))))

(defun %tcpip-lines (model)
  (let ((tcpip (atop-model-tcpip model)))
    (list (format nil "tcp in/out  ~D / ~D"
                  (tcpip-model-tcp-in-segs tcpip)
                  (tcpip-model-tcp-out-segs tcpip))
          (format nil "tcp retrans ~D (~,2f%)"
                  (tcpip-model-tcp-retrans-segs tcpip)
                  (tcpip-model-tcp-retrans-pct tcpip))
          (format nil "opens a/p ~D / ~D  estab ~D"
                  (tcpip-model-tcp-active-opens tcpip)
                  (tcpip-model-tcp-passive-opens tcpip)
                  (tcpip-model-tcp-curr-estab tcpip))
          (format nil "ip in/del/out ~D / ~D / ~D"
                  (tcpip-model-ip-in-receives tcpip)
                  (tcpip-model-ip-in-delivers tcpip)
                  (tcpip-model-ip-out-requests tcpip)))))

(defun %draw-help-overlay (buf cols rows)
  (let* ((w (min 62 (max 24 (- cols 6))))
         (h (min 10 (max 6 (- rows 6))))
         (x (max 1 (floor (- cols w) 2)))
         (y (max 1 (floor (- rows h) 2)))
         (rect (ptui.core.types:make-rect x y w h))
         (title-cell (%template-cell :fg (ptui.core.color:make-color-rgb 255 220 120) :boldp t))
         (line-cell (%template-cell :fg (ptui.core.color:make-color-rgb 230 230 230)))
         (lines '("Controls"
                  "q / Ctrl-C  quit"
                  "p           pause/resume refresh"
                  "? or h      toggle this help"
                  "Panels: CPU, Memory, Filesystem, Disk I/O, Network, TCP/IP")))
    (ptui.render.buffer:buffer-draw-border buf rect :border-style :square)
    (loop for line in lines
          for idx from 0
          while (< idx (max 0 (- h 2))) do
            (ptui.render.buffer:buffer-draw-text
             buf
             (+ x 2)
             (+ y 1 idx)
             (list (list (%fit-width line (max 0 (- w 4)))
                         (if (zerop idx) title-cell line-cell)))))))

(defun %refresh-state-if-needed (state)
  (let* ((now (funcall (atop-dashboard-state-now-ms-fn state)))
         (have-snapshot-p (not (null (atop-dashboard-state-snapshot state))))
         (due-p (or (not have-snapshot-p)
                    (>= (- now (atop-dashboard-state-last-refresh-ms state))
                        (atop-dashboard-state-refresh-ms state)))))
    (when (and due-p (not (atop-dashboard-state-pausedp state)))
      (handler-case
          (let* ((previous (atop-dashboard-state-snapshot state))
                 (current (funcall (atop-dashboard-state-collect-fn state)))
                 (model (funcall (atop-dashboard-state-model-fn state)
                                 previous
                                 current)))
            (setf (atop-dashboard-state-snapshot state) current
                  (atop-dashboard-state-model state) model
                  (atop-dashboard-state-last-refresh-ms state) now
                  (atop-dashboard-state-status-line state)
                  (format nil "refresh ok (~D ms)"
                          (host-snapshot-timestamp-ms current))))
        (error (err)
          (setf (atop-dashboard-state-status-line state)
                (format nil "refresh error: ~A" err)))))
    state))

(defun %render-atop-dashboard (state size)
  (let* ((dashboard-state (%ensure-state state))
         (cols (ptui.core.types:size-cols size))
         (rows (ptui.core.types:size-rows size))
         (model (atop-dashboard-state-model (%refresh-state-if-needed dashboard-state)))
         (buf (ptui.render.buffer:make-buffer cols rows))
         (panel-top 2)
         (panel-bottom (max panel-top (- rows 2)))
         (panel-height (max 0 (- panel-bottom panel-top)))
         (panel-width (max 0 (- cols 2)))
         (col-widths (%partition-dimension panel-width 2))
         (row-heights (%partition-dimension panel-height 3))
         (header-cell (%template-cell :fg (ptui.core.color:make-color-rgb 120 220 255) :boldp t))
         (status-cell (%template-cell :fg (ptui.core.color:make-color-rgb 180 180 180)))
         (hint-cell (%template-cell :fg (ptui.core.color:make-color-rgb 170 170 170))))
    (ptui.render.buffer:buffer-draw-border
     buf
     (ptui.core.types:make-rect 0 0 (max 1 cols) (max 1 rows)))
    (ptui.render.buffer:buffer-draw-text
     buf
     2
     1
     (list
      (list (%fit-width "PTUI Atop Dashboard v1 (Linux /proc + /sys)"
                        (max 0 (- cols 4)))
            header-cell)))
    (loop for row from 0 below 3 do
      (loop for col from 0 below 2 do
        (let* ((x (+ 1 (if (zerop col) 0 (first col-widths))))
               (y (+ panel-top (reduce #'+ row-heights :end row :initial-value 0)))
               (w (nth col col-widths))
               (h (nth row row-heights))
               (rect (ptui.core.types:make-rect x y w h)))
          (case (+ (* row 2) col)
            (0 (%draw-panel buf rect "CPU" (%cpu-lines model)))
            (1 (%draw-panel buf rect "Memory" (%memory-lines model)))
            (2 (%draw-panel buf rect "Filesystem" (%filesystem-lines model)))
            (3 (%draw-panel buf rect "Disk I/O" (%disk-lines model)))
            (4 (%draw-panel buf rect "Network" (%network-lines model)))
            (5 (%draw-panel buf rect "TCP/IP" (%tcpip-lines model)))))))
    (when (> rows 1)
      (ptui.render.buffer:buffer-draw-text
       buf
       2
       (- rows 2)
       (list (list (%fit-width
                    (format nil "status: ~A"
                            (atop-dashboard-state-status-line dashboard-state))
                    (max 0 (- cols 4)))
                   status-cell))))
    (when (> rows 2)
      (ptui.render.buffer:buffer-draw-text
       buf
       2
       (max 0 (- rows 1))
       (list (list (%fit-width
                    (format nil
                            "q quit | p pause/resume | ? help | refresh ~D ms | mode: ~A"
                            (atop-dashboard-state-refresh-ms dashboard-state)
                            (if (atop-dashboard-state-pausedp dashboard-state)
                                "paused"
                                "running"))
                    (max 0 (- cols 4)))
                   hint-cell))))
    (when (atop-dashboard-state-show-help-p dashboard-state)
      (%draw-help-overlay buf cols rows))
    buf))

(defun %toggle-pause (state)
  (setf (atop-dashboard-state-pausedp state)
        (not (atop-dashboard-state-pausedp state)))
  (setf (atop-dashboard-state-status-line state)
        (if (atop-dashboard-state-pausedp state)
            "paused by user"
            "resumed by user"))
  (when (not (atop-dashboard-state-pausedp state))
    (setf (atop-dashboard-state-last-refresh-ms state) 0))
  state)

(defun %toggle-help (state)
  (setf (atop-dashboard-state-show-help-p state)
        (not (atop-dashboard-state-show-help-p state)))
  state)

(defun %on-atop-event (state event)
  (let ((dashboard-state (%ensure-state state)))
    (when (typep event 'ptui.core.events:key-event)
      (let ((key (ptui.core.events:key-event-key event))
            (text (or (ptui.core.events:key-event-text? event) "")))
        (when (and (eql key :text)
                   (or (string= text "p")
                       (string= text "P")
                       (string= text " ")))
          (%toggle-pause dashboard-state))
        (when (and (eql key :text)
                   (or (string= text "?")
                       (string= text "h")
                       (string= text "H")))
          (%toggle-help dashboard-state))))
    dashboard-state))

(defun main (&rest argv)
  (declare (ignore argv))
  (ptui.engine.loop:run #'%render-atop-dashboard
                        :backend :auto
                        :fps 20
                        :initial-state (make-atop-dashboard-state)
                        :on-event #'%on-atop-event))
