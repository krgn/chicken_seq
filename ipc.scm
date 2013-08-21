(module ipc 
  (update-track update-pattern make-default-pattern process-ipc-messages)
  
  (import chicken scheme)
  (use extras medea unix-sockets ports srfi-18 data-structures)

  (define (make-default-pattern)
    (list 
     (cons 0 (list (cons 'name "bd")     (cons 'latency 0) (cons 'steps (vector 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))) 
     (cons 1 (list (cons 'name "rattle") (cons 'latency 0) (cons 'steps (vector 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))))
     (cons 2 (list (cons 'name "hats")   (cons 'latency 0) (cons 'steps (vector 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))))
     (cons 3 (list (cons 'name "snare")  (cons 'latency 0) (cons 'steps (vector 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))))
     (cons 4 (list (cons 'name "hands")  (cons 'latency 0) (cons 'steps (vector 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))))
     (cons 5 (list (cons 'name "cym")    (cons 'latency 0) (cons 'steps (vector 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))))
     (cons 6 (list (cons 'name "hb")     (cons 'latency 0) (cons 'steps (vector 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))))
     (cons 7 (list (cons 'name "clap")   (cons 'latency 0) (cons 'steps (vector 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))))
     (cons 8 (list (cons 'name "cong")   (cons 'latency 0) (cons 'steps (vector 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))))))

  (define (update-pattern data pattern)
    (let ([tracks (alist-ref 'tracks data)])
      ;; we just take the json apart, and apply the new data to our update-track function
      (for-each (lambda (track) (update-track track pattern)) (vector->list tracks))))

  ;; update the global pattern (mutate!)
  (define (update-track track pattern)
    (let* ([id (string->number (alist-ref 'id track))]
           [new-steps (alist-ref 'steps track)])
      (alist-update! 'steps new-steps (alist-ref id pattern))))

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
