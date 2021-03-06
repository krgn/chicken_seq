(use srfi-18 srfi-19 srfi-34 directory-utils)
(use rawmidi song-data socket unix-sockets ipc)

(define (bail-out error)
  (begin
    (print error)
    (exit 1)))

(define (chicken-seq)
  (define device-file)			;which midi device to use
  (define clock-socket-path)		;which unix socket to use for clock events
  (define song-update-socket-path)	;which unix socket to use for reading updates from web clients

  ;; error handling; bail out of the target device does not exist
  (let* ([env-var (get-environment-variable "SEQ_MIDI_DEVICE")]
         [default-device "/dev/midi1"])
    (cond [(and env-var (file-exists/directory? env-var))
           (set! device-file env-var)]
          [(and (not env-var) (file-exists/directory? default-device))
           (set! device-file default-device)]
          [else (bail-out "No midi device found.")]))

  ;; set up values for IPC socket paths
  (let ([clock-socket-env-var (get-environment-variable "CLOCK_SOCKET")]
        [song-socket-env-var (get-environment-variable "SONG_UPDATE_SOCKET")]
        [clock-socket-default "/tmp/clock-socket"]
        [song-socket-default "/tmp/song-update-socket"])
    ;; deal with clock socket: 1st check there is env var and the file exists, 2nd check default, else bail out
    (cond [(and clock-socket-env-var (file-exists/directory? clock-socket-env-var))
           (set! clock-socket-path clock-socket-env-var)]
          [(and (not clock-socket-env-var) (file-exists/directory? clock-socket-default))
           (set! clock-socket-path clock-socket-default)]
          [else (bail-out "Clock socket not found. Server running?")])
    ;; same here: 1st check there is env var and the file exists, 2nd check default, else bail out
    (cond [(and song-socket-env-var (file-exists/directory? song-socket-env-var))
           (set! song-update-socket-path song-socket-env-var)]
          [(and (not song-socket-env-var) (file-exists/directory? song-socket-default))
           (set! song-update-socket-path song-socket-default)]
          [else (bail-out "song-update-socket does not exist. Server running?")]))
  
  ;; won't proceed past this point unless node server has started and created the sockets
  (let-values ([(clock-inport clock-outport) (unix-connect clock-socket-path)]
               [(song-update-inport song-update-outport) (unix-connect song-update-socket-path)])
    (let* ([device (open-device device-file)]
           (current-bpm 120)
           (current-step 0)
           (last-notes-triggered '())
           (sixteenth-note (sixteenth-by-bpm current-bpm))
           (pattern (make-default-pattern))
           (ipc-thread (process-ipc-messages song-update-inport pattern)))

      ;; ------------------------------------------------------------ ;;
      ;; start the thread containing our loop to block for and process
      ;; incoming messages from node.js
      (thread-start! ipc-thread)

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

          ;; ;; first, send all midi note offs 
          ;; (for-each (lambda (note) (write-midi device (make-note #x01 note 0))) last-notes-triggered)
          ;; (set! last-notes-triggered '()) ;reset the list

          ;; we exectute this function for each track in in pattern
          (for-each (lambda (track)
                      (let ((latency (instrument-latency (cdr track)))
                            (steps (instrument-steps (cdr track))))

                        ;; if the current step in the current track is non-zero we see if we have to trigger something
                        (unless (zero? (vector-ref steps current-step))
                            ;; the 0-latency case first
                            (cond [(and (zero? latency)
                                        (zero? (modulo counter (sixteenth-by-bpm current-bpm))))
                                   (write-midi device (make-note #x90 (instrument-note (cdr track)) 100))]
                                  [(and (not (zero? latency))
                                        (= latency (modulo counter (sixteenth-by-bpm current-bpm))))
                                   (write-midi device (make-note #x90 (instrument-note (cdr track)) 100))]))))
                    pattern)
          (thread-sleep! 0.001))          ;sleep for one millisecond
        (let ((bar-len-ms (bar-in-ms current-bpm)))
          (cond [(>= counter bar-len-ms) (main-loop 0)]
                [(< counter bar-len-ms) (main-loop (+ counter 1) )]))))))
(chicken-seq)
