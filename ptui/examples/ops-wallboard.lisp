(defpackage :ptui.examples.ops-wallboard
  (:use :cl)
  (:export #:run-ops-wallboard
           #:run-visual-demo))

(in-package :ptui.examples.ops-wallboard)

(defparameter *visual-demo-tick* nil)

(defun %wallboard-tick ()
  (or *visual-demo-tick*
      (get-universal-time)))

(defun %style (&key fg bg boldp dimp)
  (ptui.core.types:make-cell
   " "
   (or fg ptui.core.color:color-default)
   (or bg ptui.core.color:color-default)
   (ptui.core.types:make-attrs :boldp (not (null boldp))
                               :dimp (not (null dimp)))))

(defun %filter-next (mode)
  (case mode
    (:all :app)
    (:app :infra)
    (:infra :data)
    (otherwise :all)))

(defun %filter-prev (mode)
  (case mode
    (:all :data)
    (:app :all)
    (:infra :app)
    (:data :infra)
    (otherwise :all)))

(defun %matches-filter-p (row mode)
  (or (eq mode :all)
      (eq (getf row :tier) mode)))

(defun %severity-badge-segments (severity pulsep)
  (case severity
    (:crit
     (list (list " CRIT "
                 (%style :fg (ptui.core.color:make-color-rgb 255 245 245)
                         :bg (if pulsep
                                 (ptui.core.color:make-color-rgb 220 38 38)
                                 (ptui.core.color:make-color-rgb 153 27 27))
                         :boldp t))))
    (:warn
     (list (list " WARN "
                 (%style :fg (ptui.core.color:make-color-rgb 17 24 39)
                         :bg (ptui.core.color:make-color-rgb 245 158 11)
                         :boldp t))))
    (t
     (list (list "  OK  "
                 (%style :fg (ptui.core.color:make-color-rgb 236 253 245)
                         :bg (ptui.core.color:make-color-rgb 22 163 74)
                         :boldp t))))))

(defun %service-row-text (row)
  (format nil "~A  p95:~3Dms  err:~,1F%  ~A"
          (getf row :name)
          (getf row :p95-ms)
          (getf row :error-rate)
          (getf row :sparkline)))

(defun %render-service-item (row index selectedp)
  (declare (ignore index))
  (let* ((pulsep (oddp (%wallboard-tick)))
         (name (getf row :name))
         (p95-ms (getf row :p95-ms))
         (error-rate (getf row :error-rate))
         (sparkline (getf row :sparkline))
         (severity (getf row :severity))
         (prefix-style (%style :fg (if selectedp
                                       (ptui.core.color:make-color-rgb 125 211 252)
                                       (ptui.core.color:make-color-rgb 71 85 105))
                               :boldp selectedp))
         (name-style (%style :fg (if selectedp
                                     (ptui.core.color:make-color-rgb 248 250 252)
                                     (ptui.core.color:make-color-rgb 203 213 225))
                             :boldp selectedp))
         (metric-style (%style :fg (ptui.core.color:make-color-rgb 148 163 184)))
         (spark-style (%style :fg (case severity
                                    (:crit (ptui.core.color:make-color-rgb 251 113 133))
                                    (:warn (ptui.core.color:make-color-rgb 251 191 36))
                                    (t (ptui.core.color:make-color-rgb 74 222 128)))
                             :boldp t)))
    (ptui.widgets.core:make-text-widget
     (%service-row-text row)
     :styled-segments
     (append
      (list (list (if selectedp "> " "  ") prefix-style))
      (%severity-badge-segments severity pulsep)
      (list
       (list " " metric-style)
       (list (format nil "~18A" name) name-style)
       (list (format nil " p95:~3Dms" p95-ms) metric-style)
       (list (format nil " err:~,1F% " error-rate) metric-style)
       (list sparkline spark-style))))))

(defun %heat-strip-segments ()
  (let* ((chars '("▁" "▂" "▃" "▄" "▅" "▆" "▇" "█" "▇" "▆" "▅" "▄" "▃" "▂"))
         (palette (vector
                   (ptui.core.color:make-color-rgb 59 130 246)
                   (ptui.core.color:make-color-rgb 99 102 241)
                   (ptui.core.color:make-color-rgb 139 92 246)
                   (ptui.core.color:make-color-rgb 168 85 247)
                   (ptui.core.color:make-color-rgb 217 70 239)
                   (ptui.core.color:make-color-rgb 244 63 94)
                   (ptui.core.color:make-color-rgb 239 68 68)
                   (ptui.core.color:make-color-rgb 249 115 22)))
         (offset (mod (%wallboard-tick) (length palette))))
    (loop for ch in chars
          for i from 0
          collect (list ch
                        (%style :fg (aref palette (mod (+ i offset) (length palette)))
                                :boldp t)))))

(defun %ticker-line (messages)
  (let* ((tick (%wallboard-tick))
         (idx (mod tick (length messages)))
         (pulsep (oddp tick))
         (base-style (%style :fg (ptui.core.color:make-color-rgb 254 243 199)
                             :bg (if pulsep
                                     (ptui.core.color:make-color-rgb 146 64 14)
                                     (ptui.core.color:make-color-rgb 120 53 15))
                             :boldp t)))
    (list (list (format nil " LIVE INCIDENT TICKER :: ~A "
                        (nth idx messages))
                base-style))))

(ptui.ui.panel:defpanel ops-wallboard-panel (services incidents deployment-state ticker-messages)
  (:state
    (selected-index 0 :type fixnum)
    (filter-mode :all :type keyword))
  (:data
    (filtered-services
     (remove-if-not (lambda (row) (%matches-filter-p row filter-mode))
                    services)
     :deps (services filter-mode))
    (service-count (length filtered-services) :deps (filtered-services)))
  (:effects
    (clamp-selection
      (when (and (plusp service-count)
                 (> selected-index (1- service-count)))
        (funcall set-selected-index (1- service-count)))
      :deps (selected-index service-count))
    (reset-empty-selection
      (when (zerop service-count)
        (funcall set-selected-index 0))
      :deps (service-count)))
  (:layout
    (:column
      (header :fixed 1
        (ptui.widgets.core:make-text-widget
         "NOC WALLBOARD"
         :styled-segments
         (append
          (list (list " NOC WALLBOARD "
                      (%style :fg (ptui.core.color:make-color-rgb 224 242 254)
                              :bg (ptui.core.color:make-color-rgb 3 105 161)
                              :boldp t))
                (list "  incidents:" (%style :fg (ptui.core.color:make-color-rgb 148 163 184)))
                (list (format nil " ~D " incidents)
                      (%style :fg (ptui.core.color:make-color-rgb 254 242 242)
                              :bg (ptui.core.color:make-color-rgb 185 28 28)
                              :boldp t))
                (list " deploy:" (%style :fg (ptui.core.color:make-color-rgb 148 163 184)))
                (list (format nil " ~A " deployment-state)
                      (%style :fg (ptui.core.color:make-color-rgb 20 83 45)
                              :bg (ptui.core.color:make-color-rgb 134 239 172)
                              :boldp t))
                (list " " (%style)))
          (%heat-strip-segments))))
      (ticker :fixed 1
        (ptui.widgets.core:make-text-widget
         "ticker"
         :styled-segments (%ticker-line ticker-messages)))
      (services-region :flex 1
        (ptui.views:list-view filtered-services #'%render-service-item 12 nil selected-index nil))
      (footer :fixed 1
        (ptui.widgets.core:make-text-widget
         "footer"
         :styled-segments
         (list
          (list (format nil " filter:~A " filter-mode)
                (%style :fg (ptui.core.color:make-color-rgb 196 181 253)
                        :bg (ptui.core.color:make-color-rgb 76 29 149)
                        :boldp t))
          (list (format nil " services:~2D " service-count)
                (%style :fg (ptui.core.color:make-color-rgb 186 230 253)
                        :bg (ptui.core.color:make-color-rgb 12 74 110)
                        :boldp t))
          (list (format nil " selected:~2D " selected-index)
                (%style :fg (ptui.core.color:make-color-rgb 220 252 231)
                        :bg (ptui.core.color:make-color-rgb 22 101 52)
                        :boldp t))
          (list "  keys: up/down filter, left/right tier  "
                (%style :fg (ptui.core.color:make-color-rgb 148 163 184))))))))
  (:keys
    (:up (funcall set-selected-index (max 0 (1- selected-index))))
    (:down (funcall set-selected-index
                    (if (plusp service-count)
                        (min (1- service-count) (1+ selected-index))
                        0)))
    (:left (funcall set-filter-mode (%filter-prev filter-mode)))
    (:right (funcall set-filter-mode (%filter-next filter-mode)))))

(ptui.ui.app:defapp ops-wallboard-app (:fps 8)
  (ops-wallboard-panel
   (list (list :name "api-gateway" :tier :app :severity :ok
               :p95-ms 24 :error-rate 0.1 :sparkline "▁▂▃▃▄▅▄")
         (list :name "payments-core" :tier :app :severity :warn
               :p95-ms 172 :error-rate 2.3 :sparkline "▂▃▄▅▆▅▄")
         (list :name "postgres-primary" :tier :data :severity :ok
               :p95-ms 41 :error-rate 0.0 :sparkline "▁▁▂▂▃▂▂")
         (list :name "redis-cluster" :tier :data :severity :warn
               :p95-ms 88 :error-rate 1.1 :sparkline "▁▂▃▄▅▄▃")
         (list :name "node-exporter" :tier :infra :severity :ok
               :p95-ms 16 :error-rate 0.0 :sparkline "▁▁▁▂▂▂▁")
         (list :name "ingress-edge" :tier :infra :severity :crit
               :p95-ms 310 :error-rate 6.7 :sparkline "▃▄▅▆▇█▇")
         (list :name "search-service" :tier :app :severity :ok
               :p95-ms 57 :error-rate 0.2 :sparkline "▁▂▂▃▄▃▂"))
   3
   "rolling"
   (list "ingress-edge 5xx spike crossing SLO"
         "payments-core queue depth above threshold"
         "redis-cluster replica lag recovering"
         "search-service reindex completed")))

(defun run-ops-wallboard ()
  (run-ops-wallboard-app))

(defun run-visual-demo ()
  (let ((*visual-demo-tick* 424242))
    (run-ops-wallboard-app)))
