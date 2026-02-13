(defpackage :pseudopod
  (:use :cl)
  (:nicknames :moonshot-common-lisp)
  (:export
   #:*default-base-url*
   #:*default-model*
   #:*default-api-key-file*
   #:client
   #:make-client
   #:client-api-key
   #:client-base-url
   #:client-model
   #:client-temperature
   #:client-max-tokens
   #:client-top-p
   #:client-timeout-seconds
   #:read-api-key
   #:chat-completion
   #:stream-chat-completion
   #:print-streamed-completion
   #:main))
