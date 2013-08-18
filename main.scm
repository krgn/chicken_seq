(use srfi-18 srfi-19 zmq)
(use midi seq-ipc)

(define (main)
  (let* (;;(device (open-device "/dev/midi1"))
        (pattern (make-default-pattern))
        (clock-socket (make-socket 'push (make-context 4)))
        (song-update-socket (make-socket 'pull (make-context 4)))
        (ipc-thread (make-thread 
                     (lambda () (process-ipc-messages song-update-socket pattern))))) ; make a push type socket

    (bind-socket clock-socket "tcp://127.0.0.1:8888") ; the sender/pusher binds
    (connect-socket song-update-socket "tcp://127.0.0.1:8889") ; the sender/pusher binds
    
    (thread-start! ipc-thread)

    (print "entering main loop")
    (let loop ((c 0))
      (begin
        ;; now, we move on with midi and trigger messages 
        ;; ------
        ;; send a trigger message 
        (pp (alist-ref 'steps (alist-ref 0 pattern)))
        ;; (send-message socket (modulo c 16))
        ;; (write-midi device (make-note 1 30 100))
        (thread-sleep! 1))
      (loop (+ c 1)))))

(main)
