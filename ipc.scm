(module ipc 
  (update-track update-pattern process-ipc-messages)
  
  (import chicken scheme)
  (use extras medea unix-sockets ports srfi-18 data-structures)
  (use data)

  ;; use a named let loop to  process all messages in a blocking style 
  ;; for processing in a separate thread. takes a socket and the target 
  ;; data structure assoc for in-place updates
  (define (process-ipc-messages inport pattern)
    ;; create and return a new thread
    (make-thread 
     ;; and pass it a thunk
     (lambda ()
       ;; first, accept a connection on our unix domain socket and keep values
       ;; now loop while stuff pours in
       (let loop ([json (read-json inport consume-trailing-whitespace: #f)])
         (let ([type (alist-ref 'type json)]
               [data (alist-ref 'data json)])
           (cond [(equal? type "track") (update-track data pattern)]
                 [(equal? type "init")   (update-pattern data pattern)]
                 [else (pp json)]))
         (loop (read-json inport consume-trailing-whitespace: #f)))))))
