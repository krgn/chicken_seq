(module ipc 
  (update-track make-default-pattern process-song-updates)
  
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

  ;; update the global pattern (mutate!)
  (define (update-track track pattern)
    (let* ((id (string->number (alist-ref 'id track)))
           (new-steps (alist-ref 'steps track)))
      (alist-update! 'steps new-steps (alist-ref id pattern))))

  ;; use a named let loop to  process all messages in a blocking style 
  ;; for processing in a separate thread. takes a socket and the target 
  ;; data structure assoc for in-place updates
  (define (process-song-updates inport pattern)
    ;; create and return a new thread
    (make-thread 
     ;; and pass it a thunk
     (lambda ()
       ;; first, accept a connection on our unix domain socket and keep values
       ;; now loop while stuff pours in
       (let loop ([track (read-json inport consume-trailing-whitespace: #f)])
         ;; parse the (hopefully) json
         (if track (update-track track pattern))
         (loop (read-json inport consume-trailing-whitespace: #f)))))))

;; ---- Server Process that hogs a socket and waits for a connection ----
;; (define sock (socket af/inet sock/stream))
;; (socket-bind sock (inet-address "127.0.0.1" 8124))
;; (socket-listen sock 1)

;; ---- here comes the juicy part: ----
;; ---- warning, this blocks until a connection has been established.----
;; ---- also, accept() returns the file descriptor for the connection, so don't use /sock/
;; ---- for (socket-send ..) later on, as its going to produce a broken pipe error
;; (define a-connection (socket-accept sock)) 

;; ---- now we can simply send some stuff to the connection: ----
;; (socket-send a-connection (u8vector->blob (u8vector 1 2 3 4 5 6 7 8 9)))
