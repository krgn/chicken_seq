(use srfi-18)
(use srfi-19)
(use zmq)
(use midi)
 
(define (main)
  (let ((device (open-device "/dev/midi1"))
        (socket (make-socket 'push (make-context 4)))) ; make a push type socket
    (bind-socket socket "tcp://127.0.0.1:8888") ; the sender/pusher binds
    (thread-start! 
     (let loop ()
       (when #t
         (send-message socket "tick")
         (write-midi device (make-note 1 30 100))
         (thread-sleep! 1)
         (send-message socket "tock")
         (write-midi device (make-note 1 30 0))
         (thread-sleep! 1)
         (loop))))))

(main)
