(in-package :pseudopod/test)

;;; ---------------------------------------------------------------------------
;;; Content-Part Coercion Dispatch Table Tests (FP-Refine Phase 2, Target 1)
;;; ---------------------------------------------------------------------------

(def-suite content-part-coercion-suite :in pseudopod-suite
  :description "Tests for content-part coercion dispatch tables and pure handler functions.")

(in-suite content-part-coercion-suite)

;;; --- Helper ---

(defun %make-hash (&rest pairs)
  (let ((ht (make-hash-table :test #'equal)))
    (loop for (k v) on pairs by #'cddr
          do (setf (gethash k ht) v))
    ht))

;;; --- Table Structure Tests ---

(test openai-coercion-table-has-four-entries
  (is (= 4 (length pseudopod::+openai-content-part-coercers+))))

(test anthropic-coercion-table-has-four-entries
  (is (= 4 (length pseudopod::+anthropic-content-part-coercers+))))

(test openai-coercion-table-entries-are-functions
  (dolist (entry pseudopod::+openai-content-part-coercers+)
    (is (fboundp (cdr entry)))))

(test anthropic-coercion-table-entries-are-functions
  (dolist (entry pseudopod::+anthropic-content-part-coercers+)
    (is (fboundp (cdr entry)))))

;;; --- OpenAI Per-Type Dispatch Tests ---

(test openai-coerce-text-part-extracts-text
  (let ((hash (%make-hash "type" "text" "text" "hello world")))
    (let ((result (pseudopod::%openai-coerce-text-part hash)))
      (is (hash-table-p result))
      (is (string= "text" (gethash "type" result)))
      (is (string= "hello world" (gethash "text" result))))))

(test openai-coerce-text-part-falls-back-to-content
  (let ((hash (%make-hash "type" "text" "content" "from-content")))
    (let ((result (pseudopod::%openai-coerce-text-part hash)))
      (is (string= "from-content" (gethash "text" result))))))

(test openai-coerce-text-part-empty-defaults
  (let ((hash (%make-hash "type" "text")))
    (let ((result (pseudopod::%openai-coerce-text-part hash)))
      (is (string= "" (gethash "text" result))))))

(test openai-coerce-image-part-with-data-uri
  (let ((hash (%make-hash "type" "image"
                           "media_type" "image/png"
                           "data" "base64data")))
    (let ((result (pseudopod::%openai-coerce-image-part hash)))
      (is (hash-table-p result))
      (is (string= "image_url" (gethash "type" result)))
      (let ((url-obj (gethash "image_url" result)))
        (is (search "data:image/png;base64,base64data" (gethash "url" url-obj)))))))

(test openai-coerce-image-part-fallback-text
  (let ((hash (%make-hash "type" "image" "text" "alt-text")))
    (let ((result (pseudopod::%openai-coerce-image-part hash)))
      (is (string= "text" (gethash "type" result)))
      (is (string= "alt-text" (gethash "text" result))))))

(test openai-coerce-image-part-fallback-bracket
  (let ((hash (%make-hash "type" "image")))
    (let ((result (pseudopod::%openai-coerce-image-part hash)))
      (is (string= "[image]" (gethash "text" result))))))

(test openai-coerce-image-url-part-with-url
  (let ((hash (%make-hash "type" "image_url" "url" "https://example.com/img.png")))
    (let ((result (pseudopod::%openai-coerce-image-url-part hash)))
      (is (string= "image_url" (gethash "type" result)))
      (is (string= "https://example.com/img.png"
                    (gethash "url" (gethash "image_url" result)))))))

(test openai-coerce-image-url-part-no-url-falls-back
  (let ((hash (%make-hash "type" "image_url")))
    (let ((result (pseudopod::%openai-coerce-image-url-part hash)))
      (is (string= "[image]" (gethash "text" result))))))

;;; --- OpenAI Full Dispatch Tests ---

(test openai-coerce-dispatches-text-type
  (let ((hash (%make-hash "type" "text" "text" "dispatched")))
    (let ((result (pseudopod::%openai-coerce-content-part hash)))
      (is (string= "dispatched" (gethash "text" result))))))

(test openai-coerce-dispatches-input-image-type
  (let ((hash (%make-hash "type" "input_image"
                           "media_type" "image/jpeg"
                           "data" "jpeg-data")))
    (let ((result (pseudopod::%openai-coerce-content-part hash)))
      (is (string= "image_url" (gethash "type" result))))))

(test openai-coerce-unknown-type-passes-through
  (let ((hash (%make-hash "type" "custom_block" "data" "preserved")))
    (let ((result (pseudopod::%openai-coerce-content-part hash)))
      (is (string= "custom_block" (gethash "type" result)))
      (is (string= "preserved" (gethash "data" result))))))

(test openai-coerce-nil-input-returns-nil
  (is (null (pseudopod::%openai-coerce-content-part nil))))

(test openai-coerce-string-input-makes-text
  (let ((result (pseudopod::%openai-coerce-content-part "simple")))
    (is (string= "text" (gethash "type" result)))
    (is (string= "simple" (gethash "text" result)))))

;;; --- Anthropic Per-Type Dispatch Tests ---

(test anthropic-coerce-text-block-extracts-text
  (let ((hash (%make-hash "type" "text" "text" "anthropic-text")))
    (let ((result (pseudopod::%anthropic-coerce-text-block-part hash)))
      (is (hash-table-p result))
      (is (string= "text" (gethash "type" result)))
      (is (string= "anthropic-text" (gethash "text" result))))))

(test anthropic-coerce-text-block-falls-back-to-content
  (let ((hash (%make-hash "type" "text" "content" "via-content")))
    (let ((result (pseudopod::%anthropic-coerce-text-block-part hash)))
      (is (string= "via-content" (gethash "text" result))))))

(test anthropic-coerce-image-block-with-data
  (let ((hash (%make-hash "type" "image"
                           "media_type" "image/png"
                           "data" "png-b64")))
    (let ((result (pseudopod::%anthropic-coerce-image-block-part hash)))
      (is (hash-table-p result))
      (is (string= "image" (gethash "type" result)))
      (let ((source (gethash "source" result)))
        (is (string= "base64" (gethash "type" source)))
        (is (string= "image/png" (gethash "media_type" source)))
        (is (string= "png-b64" (gethash "data" source)))))))

(test anthropic-coerce-image-block-fallback-text
  (let ((hash (%make-hash "type" "image" "text" "image-alt")))
    (let ((result (pseudopod::%anthropic-coerce-image-block-part hash)))
      (is (string= "text" (gethash "type" result)))
      (is (string= "image-alt" (gethash "text" result))))))

(test anthropic-coerce-image-block-no-data-no-text-returns-nil
  (let ((hash (%make-hash "type" "image")))
    (is (null (pseudopod::%anthropic-coerce-image-block-part hash)))))

;;; --- Anthropic Full Dispatch Tests ---

(test anthropic-coerce-dispatches-text-type
  (let ((result (pseudopod::%anthropic-coerce-content-part-block "hello")))
    (is (string= "text" (gethash "type" result)))
    (is (string= "hello" (gethash "text" result)))))

(test anthropic-coerce-dispatches-image-url-type
  (let ((hash (%make-hash "type" "image_url"
                           "url" "data:image/jpeg;base64,jdata")))
    (let ((result (pseudopod::%anthropic-coerce-content-part-block hash)))
      (is (string= "image" (gethash "type" result))))))

(test anthropic-coerce-unknown-type-extracts-text-fallback
  (let ((hash (%make-hash "type" "custom" "text" "fallback-text")))
    (let ((result (pseudopod::%anthropic-coerce-content-part-block hash)))
      (is (string= "text" (gethash "type" result)))
      (is (string= "fallback-text" (gethash "text" result))))))

(test anthropic-coerce-unknown-type-no-text-returns-nil
  (let ((hash (%make-hash "type" "custom" "data" "binary")))
    (is (null (pseudopod::%anthropic-coerce-content-part-block hash)))))

(test anthropic-coerce-nil-input-returns-nil
  (is (null (pseudopod::%anthropic-coerce-content-part-block nil))))

;;; --- Shared Dispatcher Tests ---

(test dispatch-content-part-coercion-returns-nil-for-unknown
  (let ((table '(("text" . pseudopod::%openai-coerce-text-part))))
    (is (null (pseudopod::%dispatch-content-part-coercion
               "unknown" table (%make-hash "type" "unknown"))))))

(test dispatch-content-part-coercion-calls-matched-handler
  (let ((table '(("text" . pseudopod::%openai-coerce-text-part))))
    (let ((result (pseudopod::%dispatch-content-part-coercion
                   "text" table (%make-hash "type" "text" "text" "matched"))))
      (is (string= "matched" (gethash "text" result))))))
