(in-package :amoebum)

;;;; NXT-395: deftool history helpers.
;;;;
;;;; This module owns version snapshots, diffing, lifecycle events, and
;;;; rollback for deftool registrations. The behavior is byte-identical to the
;;;; historical inline implementation from `src/macros/deftool.lisp`.

(defstruct (tool-history-entry
            (:constructor make-tool-history-entry
                (&key tool-definition
                 tool-metadata
                 timestamp
                 source-file
                 source-line)))
  tool-definition
  tool-metadata
  timestamp
  source-file
  source-line)

(defun %history-limit ()
  (if (and (integerp *tool-history-max-versions*)
           (> *tool-history-max-versions* 0))
      *tool-history-max-versions*
      10))

(defun %trim-history-entries (entries)
  (let ((limit (%history-limit)))
    (if (> (length entries) limit)
        (subseq entries 0 limit)
        entries)))

(defun %copy-hash-table-shallow (source)
  (let ((copy (make-hash-table :test #'equal)))
    (maphash (lambda (key value)
               (setf (gethash key copy) value))
             source)
    copy))

(defun %copy-tool-definition (tool-definition)
  (when tool-definition
    (let* ((parameters (pseudopod:tool-definition-parameters tool-definition))
           (copied-parameters
             (if (hash-table-p parameters)
                 (%copy-hash-table-shallow parameters)
                 parameters)))
      (pseudopod:make-tool-definition
       :name (pseudopod:tool-definition-name tool-definition)
       :description (pseudopod:tool-definition-description tool-definition)
       :parameters copied-parameters
       :fn (pseudopod:tool-definition-fn tool-definition)))))

(defun %copy-tool-metadata (metadata)
  (when (and metadata (tool-metadata-p metadata))
    (make-tool-metadata
     :name (tool-metadata-name metadata)
     :permission (tool-metadata-permission metadata)
     :dangerous-p (tool-metadata-dangerous-p metadata)
     :category (tool-metadata-category metadata)
     :timeout-seconds (tool-metadata-timeout-seconds metadata)
     :source-file (tool-metadata-source-file metadata)
     :source-line (tool-metadata-source-line metadata)
     :parameter-specs (copy-tree (tool-metadata-parameter-specs metadata))
     :defined-at (tool-metadata-defined-at metadata)
     :mcp-server (tool-metadata-mcp-server metadata))))

(defun %tool-history-entries (tool-name)
  (copy-list (gethash (%tool-name-string tool-name) *tool-history*)))

(defun %push-tool-history-entry (tool-name entry)
  (let* ((key (%tool-name-string tool-name))
         (current (%tool-history-entries key))
         (updated (%trim-history-entries (cons entry current))))
    (setf (gethash key *tool-history*) updated)
    updated))

(defun %push-tool-version-to-history (tool-name tool-definition tool-metadata)
  (when tool-definition
    (%push-tool-history-entry
     tool-name
     (make-tool-history-entry
      :tool-definition (%copy-tool-definition tool-definition)
      :tool-metadata (%copy-tool-metadata tool-metadata)
      :timestamp (get-universal-time)
      :source-file (and (tool-metadata-p tool-metadata)
                        (tool-metadata-source-file tool-metadata))
      :source-line (and (tool-metadata-p tool-metadata)
                        (tool-metadata-source-line tool-metadata))))))

(defun %metadata-value-equal-p (left right)
  (cond
    ((and (pathnamep left) (pathnamep right))
     (string= (namestring left) (namestring right)))
    (t
     (equal left right))))

(defun %tool-metadata->plist (metadata)
  (and (tool-metadata-p metadata)
       (list :name (tool-metadata-name metadata)
             :permission (tool-metadata-permission metadata)
             :dangerous-p (tool-metadata-dangerous-p metadata)
             :category (tool-metadata-category metadata)
             :timeout-seconds (tool-metadata-timeout-seconds metadata)
             :source-file (tool-metadata-source-file metadata)
             :source-line (tool-metadata-source-line metadata)
             :parameter-specs (copy-tree (tool-metadata-parameter-specs metadata))
             :defined-at (tool-metadata-defined-at metadata)
             :mcp-server (tool-metadata-mcp-server metadata))))

(defun %tool-metadata-diff (old-metadata new-metadata)
  (let ((diff '()))
    (labels ((push-diff (field old-value new-value)
               (unless (%metadata-value-equal-p old-value new-value)
                 (push (list :field field :old old-value :new new-value) diff))))
      (push-diff :permission
                 (and (tool-metadata-p old-metadata)
                      (tool-metadata-permission old-metadata))
                 (and (tool-metadata-p new-metadata)
                      (tool-metadata-permission new-metadata)))
      (push-diff :dangerous-p
                 (and (tool-metadata-p old-metadata)
                      (tool-metadata-dangerous-p old-metadata))
                 (and (tool-metadata-p new-metadata)
                      (tool-metadata-dangerous-p new-metadata)))
      (push-diff :category
                 (and (tool-metadata-p old-metadata)
                      (tool-metadata-category old-metadata))
                 (and (tool-metadata-p new-metadata)
                      (tool-metadata-category new-metadata)))
      (push-diff :timeout-seconds
                 (and (tool-metadata-p old-metadata)
                      (tool-metadata-timeout-seconds old-metadata))
                 (and (tool-metadata-p new-metadata)
                      (tool-metadata-timeout-seconds new-metadata)))
      (push-diff :source-file
                 (and (tool-metadata-p old-metadata)
                      (tool-metadata-source-file old-metadata))
                 (and (tool-metadata-p new-metadata)
                      (tool-metadata-source-file new-metadata)))
      (push-diff :source-line
                 (and (tool-metadata-p old-metadata)
                      (tool-metadata-source-line old-metadata))
                 (and (tool-metadata-p new-metadata)
                      (tool-metadata-source-line new-metadata)))
      (push-diff :parameter-specs
                 (and (tool-metadata-p old-metadata)
                      (tool-metadata-parameter-specs old-metadata))
                 (and (tool-metadata-p new-metadata)
                      (tool-metadata-parameter-specs new-metadata)))
      (push-diff :defined-at
                 (and (tool-metadata-p old-metadata)
                      (tool-metadata-defined-at old-metadata))
                 (and (tool-metadata-p new-metadata)
                      (tool-metadata-defined-at new-metadata)))
      (push-diff :mcp-server
                 (and (tool-metadata-p old-metadata)
                      (tool-metadata-mcp-server old-metadata))
                 (and (tool-metadata-p new-metadata)
                      (tool-metadata-mcp-server new-metadata))))
    (nreverse diff)))

(defun %emit-tool-redefined (tool-name old-metadata new-metadata)
  (publish (current-event-bus)
           (make-tool-redefined-event
            :tool-name (%tool-name-string tool-name)
            :old-metadata (%tool-metadata->plist old-metadata)
            :new-metadata (%tool-metadata->plist new-metadata)
            :metadata-diff (%tool-metadata-diff old-metadata new-metadata))))

(defun tool-history (tool-name)
  (let ((entries (%tool-history-entries tool-name)))
    (loop for entry in entries
          for version from 1
          collect (list :version version
                        :timestamp (tool-history-entry-timestamp entry)
                        :source-file (tool-history-entry-source-file entry)
                        :source-line (tool-history-entry-source-line entry)
                        :metadata (tool-history-entry-tool-metadata entry)))))

(defun rollback-tool (tool-name &key (version 1))
  (unless (and (integerp version) (plusp version))
    (error "VERSION must be a positive integer, got ~S." version))
  (let* ((normalized-name (%tool-name-string tool-name))
         (entries (%tool-history-entries normalized-name))
         (target-index (1- version)))
    (when (null entries)
      (error "No tool history exists for ~A." normalized-name))
    (when (>= target-index (length entries))
      (error "Requested version ~D for ~A, but only ~D version~:P available."
             version
             normalized-name
             (length entries)))
    (let* ((target-entry (nth target-index entries))
           (target-definition (tool-history-entry-tool-definition target-entry))
           (target-metadata (tool-history-entry-tool-metadata target-entry))
           (toolset (%ensure-toolset))
           (current-definition (pseudopod:find-tool toolset normalized-name))
           (current-metadata (gethash normalized-name *tool-metadata*))
           (remaining
             (loop for entry in entries
                   for index from 0
                   unless (= index target-index)
                     collect entry))
           (current-entry
             (and current-definition
                  (make-tool-history-entry
                   :tool-definition (%copy-tool-definition current-definition)
                   :tool-metadata (%copy-tool-metadata current-metadata)
                   :timestamp (get-universal-time)
                   :source-file (and (tool-metadata-p current-metadata)
                                     (tool-metadata-source-file current-metadata))
                   :source-line (and (tool-metadata-p current-metadata)
                                     (tool-metadata-source-line current-metadata)))))
           (updated-history
             (%trim-history-entries
              (if current-entry
                  (cons current-entry remaining)
                  remaining))))
      (unless target-definition
        (error "History entry ~D for ~A is missing a tool definition."
               version
               normalized-name))
      (setf (gethash normalized-name *tool-history*) updated-history)
      (pseudopod:register-tool toolset (%copy-tool-definition target-definition))
      (let ((restored-metadata (%copy-tool-metadata target-metadata)))
        (if restored-metadata
            (setf (gethash normalized-name *tool-metadata*) restored-metadata)
            (remhash normalized-name *tool-metadata*))
        (%emit-tool-redefined normalized-name current-metadata restored-metadata)
        restored-metadata))))
