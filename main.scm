(use srfi-18 srfi-19 srfi-34 args)
(use midi data socket unix-sockets ipc)

(define (main)
  ;; won't proceed past this point unless node server has started and created the sockets
  (let-values ([(clock-inport clock-outport) (unix-connect "/tmp/clock-socket")]
               [(song-update-inport song-update-outport) (unix-connect "/tmp/song-update-socket")])
    (let* ((device (open-device "/dev/midi1"))
           (current-bpm 120)
           (current-step 0)
           (sixteenth-note (sixteenth-by-bpm current-bpm))
           (pattern (make-default-pattern))
           (ipc-thread (process-ipc-messages song-update-inport pattern)))

      ;; ------------------------------------------------------------ ;;
      ;; start the thread containing our loop to block for and process
      ;; incoming messages from node.js
      (print "starting IPC thread")
      (thread-start! ipc-thread)

      (print "entering main loop")
      ;; ------------------------------------------------------------ ;;
      ;; enter the main sequencer loop
      (let main-loop ((counter 0))        ;counter is always in milliseconds
        (begin
          ;; every time we're on an exactt sixteenth step we increment or reset our current-step
          (if (zero? (modulo counter (sixteenth-by-bpm current-bpm)))
              (begin 
                (if (not (= counter (bar-in-ms current-bpm)))
                    (set! current-step (inexact->exact (/ counter sixteenth-note))))
                (write current-step clock-outport)))

          ;; we exectute this function for each track in in pattern
          (for-each (lambda (track)
                      (let ((latency (instrument-latency (cdr track)))
                            (steps (instrument-steps (cdr track))))
                        ;; if the current step in the current track is non-zero we see if we have to trigger something
                        (unless (zero? (vector-ref steps current-step))
                          ;; the 0-latency case first
                          (cond [(and (zero? latency)
                                      (zero? (modulo counter (sixteenth-by-bpm current-bpm))))
                                 (write-midi device (make-note #x01 30 100))]
                                [(and (not (zero? latency))
                                      (= latency (modulo counter (sixteenth-by-bpm current-bpm))))
                                 (write-midi device (make-note #x01 30 100))]))))
                    pattern)
          (thread-sleep! 0.001))          ;sleep for one millisecond
        (let ((bar-len-ms (bar-in-ms current-bpm)))
          (cond [(>= counter bar-len-ms) (main-loop 0)]
                [(< counter bar-len-ms) (main-loop (+ counter 1) )]))))))
(main)

