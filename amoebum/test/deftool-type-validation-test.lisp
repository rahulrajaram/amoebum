(in-package :amoebum/test)

(def-suite deftool-type-validation-suite :in amoebum-suite
  :description "deftool type-schema round-trip validation coverage (I202).")

(in-suite deftool-type-validation-suite)

(defun %schema-types-list (schema)
  (let ((raw (gethash "type" schema)))
    (sort (copy-list (amoebum::%normalized-schema-types raw)) #'string<)))

(test cl-type-to-json-schema-covers-core-type-cases
  (labels ((assert-schema (type-spec expected-types
                    &key format minimum maximum enum)
             (let ((schema (amoebum::cl-type-to-json-schema type-spec)))
               (is (equal (sort (copy-list expected-types) #'string<)
                          (%schema-types-list schema))
                   "Expected type ~S to map to ~S, got ~S."
                   type-spec
                   expected-types
                   (%schema-types-list schema))
               (when format
                 (is (string= format (gethash "format" schema))
                     "Expected format ~S for ~S." format type-spec))
               (when minimum
                 (is (= minimum (gethash "minimum" schema))
                     "Expected minimum ~S for ~S." minimum type-spec))
               (when maximum
                 (is (= maximum (gethash "maximum" schema))
                     "Expected maximum ~S for ~S." maximum type-spec))
               (when enum
                 (is (equal enum (gethash "enum" schema))
                     "Expected enum ~S for ~S." enum type-spec)))))
    (assert-schema 'string '("string"))
    (assert-schema 'integer '("integer"))
    (assert-schema 'boolean '("boolean"))
    (assert-schema 'pathname '("string") :format "path")
    (assert-schema 'list '("array"))
    (assert-schema 'null '("null"))
    (assert-schema '(integer 0 100) '("integer") :minimum 0 :maximum 100)
    (assert-schema '(or string pathname) '("string"))
    (assert-schema '(or null integer) '("integer" "null"))
    (assert-schema '(or null pathname) '("null" "string") :format "path")
    (assert-schema '(member :a :b :c) '("string") :enum '("a" "b" "c"))))

(test validate-type-to-schema-mapping-round-trips-known-types
  (dolist (type-spec '(string
                       integer
                       boolean
                       pathname
                       list
                       null
                       (integer 0 100)
                       (or string pathname)
                       (or null integer)
                       (or null pathname)
                       (member :a :b :c)))
    (is-true (amoebum::%validate-type-to-schema-mapping
              "i202-known-types"
              'value
              type-spec)
             "Expected %validate-type-to-schema-mapping to pass for ~S."
             type-spec)))

(test validate-type-to-schema-mapping-warns-on-unmapped-types
  (let ((saw-warning nil))
    (handler-bind ((amoebum::unmapped-type-warning
                     (lambda (condition)
                       (declare (ignore condition))
                       (setf saw-warning t)
                       (let ((restart (find-restart 'muffle-warning)))
                         (when restart
                           (invoke-restart restart)))
                       nil)))
      (is-false (amoebum::%validate-type-to-schema-mapping
                 "i202-unknown-type"
                 'payload
                 'hash-table)))
    (is-true saw-warning)))

(test deftool-macroexpansion-signals-unmapped-type-warning
  (let ((saw-warning nil))
    (handler-bind ((amoebum::unmapped-type-warning
                     (lambda (condition)
                       (declare (ignore condition))
                       (setf saw-warning t)
                       (let ((restart (find-restart 'muffle-warning)))
                         (when restart
                           (invoke-restart restart)))
                       nil)))
      (amoebum::reset-deftool-compile-validation-state)
      (macroexpand-1
       '(amoebum:deftool i202-unmapped-tool
            ((payload hash-table :description "Payload data"))
          "I202 unmapped type warning test tool."
          :ok)))
    (is-true saw-warning)))
