(module seq-ipc (update-track make-default-pattern process-ipc-messages)
  (import chicken scheme)
  (use medea zmq ports srfi-18 data-structures)

  (define (make-default-pattern)
    (list 
     (cons 0 (list (cons 'name "bd")     (cons 'latency 0.0) (cons 'steps (vector 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))) 
     (cons 1 (list (cons 'name "rattle") (cons 'latency 0.0) (cons 'steps (vector 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))))
     (cons 2 (list (cons 'name "hats")   (cons 'latency 0.0) (cons 'steps (vector 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))))
     (cons 3 (list (cons 'name "snare")  (cons 'latency 0.0) (cons 'steps (vector 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))))
     (cons 4 (list (cons 'name "hands")  (cons 'latency 0.0) (cons 'steps (vector 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))))
     (cons 5 (list (cons 'name "cym")    (cons 'latency 0.0) (cons 'steps (vector 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))))
     (cons 6 (list (cons 'name "hb")     (cons 'latency 0.0) (cons 'steps (vector 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))))
     (cons 7 (list (cons 'name "clap")   (cons 'latency 0.0) (cons 'steps (vector 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))))
     (cons 8 (list (cons 'name "cong")   (cons 'latency 0.0) (cons 'steps (vector 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))))))

  ;; update the global pattern (mutate!)
  (define (update-track track pattern)
    (let* ((id (alist-ref 'id track))
           (new-steps (alist-ref 'steps track)))
      (alist-update! 'steps new-steps (alist-ref id pattern))))

  ;; use a named let loop to  process all messages in a blocking style 
  ;; for processing in a separate thread. takes a socket and the target 
  ;; data structure assoc for in-place updates
  (define (process-ipc-messages socket pattern)
     (let loop ((msg ""))
       ;; parse json from zmq message body and update the respective track
       (let ((track (with-input-from-string msg read-json)))
         (if track (update-track track pattern))
         (print "received and processed a message: " msg))
       (loop (receive-message* socket)))))
