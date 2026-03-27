(in-package :pseudopod/test)

;;; ---------------------------------------------------------------------------
;;; HTTP Error Dispatch Table Tests (FP-Refine Phase 2, Target 2)
;;; ---------------------------------------------------------------------------

(def-suite http-error-dispatch-suite :in pseudopod-suite
  :description "Tests for HTTP status error dispatch tables and classifier functions.")

(in-suite http-error-dispatch-suite)

;;; --- Table Structure Tests ---

(test http-status-error-classes-has-two-entries
  (is (= 2 (length pseudopod::+http-status-error-classes+))))

(test dexador-error-type-classes-has-three-entries
  (is (= 3 (length pseudopod::+dexador-error-type-classes+))))

(test http-error-class-conditions-has-three-entries
  (is (= 3 (length pseudopod::+http-error-class-conditions+))))

;;; --- HTTP Status Classifier Tests ---

(test classify-401-as-auth
  (is (eq :auth (pseudopod::%classify-http-status 401))))

(test classify-403-as-auth
  (is (eq :auth (pseudopod::%classify-http-status 403))))

(test classify-408-as-timeout
  (is (eq :timeout (pseudopod::%classify-http-status 408))))

(test classify-504-as-timeout
  (is (eq :timeout (pseudopod::%classify-http-status 504))))

(test classify-500-as-api
  (is (eq :api (pseudopod::%classify-http-status 500))))

(test classify-200-as-api
  (is (eq :api (pseudopod::%classify-http-status 200))))

(test classify-nil-status-as-api
  (is (eq :api (pseudopod::%classify-http-status nil))))

(test classify-string-status-as-api
  (is (eq :api (pseudopod::%classify-http-status "401"))))

;;; --- Error Message Builder Tests ---

(test auth-error-message-format
  (let ((msg (pseudopod::%http-error-message :auth "request" 401 "body")))
    (is (search "unauthorized" msg))
    (is (search "401" msg))))

(test timeout-error-message-format
  (let ((msg (pseudopod::%http-error-message :timeout "streaming request" 408 "body")))
    (is (search "timed out" msg))
    (is (search "streaming request" msg))))

(test api-error-message-format
  (let ((msg (pseudopod::%http-error-message :api "request" 500 "body")))
    (is (search "failed" msg))
    (is (search "500" msg))))

;;; --- Error Initargs Builder Tests ---

(test auth-initargs-include-status-and-body
  (let ((args (pseudopod::%http-error-initargs :auth 401 "body" "msg" nil)))
    (is (string= "msg" (getf args :message)))
    (is (= 401 (getf args :status-code)))
    (is (string= "body" (getf args :body)))))

(test timeout-initargs-omit-status-and-body
  (let ((args (pseudopod::%http-error-initargs :timeout 408 "body" "msg" nil)))
    (is (string= "msg" (getf args :message)))
    (is (null (getf args :status-code)))
    (is (null (getf args :body)))))

;;; --- Integration: Error Signaling ---

(test signal-http-status-401-signals-auth-error
  (handler-case
      (pseudopod::%signal-http-status-error 401 "unauthorized" :cause nil :streamp nil)
    (pseudopod:pseudopod-auth-error (e)
      (is (search "unauthorized" (pseudopod:pseudopod-error-message e))))
    (:no-error (&rest args)
      (declare (ignore args))
      (fail "Expected pseudopod-auth-error to be signaled"))))

(test signal-http-status-408-signals-timeout-error
  (handler-case
      (pseudopod::%signal-http-status-error 408 "timeout" :cause nil :streamp t)
    (pseudopod:pseudopod-timeout-error (e)
      (is (search "timed out" (pseudopod:pseudopod-error-message e)))
      (is (search "streaming" (pseudopod:pseudopod-error-message e))))
    (:no-error (&rest args)
      (declare (ignore args))
      (fail "Expected pseudopod-timeout-error to be signaled"))))

(test signal-http-status-500-signals-api-error
  (handler-case
      (pseudopod::%signal-http-status-error 500 "internal" :cause nil :streamp nil)
    (pseudopod:pseudopod-api-error (e)
      (is (search "failed" (pseudopod:pseudopod-error-message e))))
    (:no-error (&rest args)
      (declare (ignore args))
      (fail "Expected pseudopod-api-error to be signaled"))))

(test signal-http-status-streaming-label
  (handler-case
      (pseudopod::%signal-http-status-error 500 "err" :cause nil :streamp t)
    (pseudopod:pseudopod-api-error (e)
      (is (search "streaming request" (pseudopod:pseudopod-error-message e))))
    (:no-error (&rest args)
      (declare (ignore args))
      (fail "Expected pseudopod-api-error to be signaled"))))
