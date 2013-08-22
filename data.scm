(module data
  (update-track update-pattern make-default-pattern
   instrument-name instrument-note instrument-latency instrument-steps
   instrument-name-set! instrument-note-set! instrument-latency-set! instrument-steps-set!)
  
  (import chicken scheme)
  (use extras medea unix-sockets ports srfi-18 data-structures)

  (define-record instrument name note latency steps)

  (define (update-pattern data pattern)
    (let ([tracks (alist-ref 'tracks data)])
      ;; we just take the json apart, and apply the new data to our update-track function
      (for-each (lambda (track) (update-track track pattern)) (vector->list tracks))))

  ;; update the global pattern (mutate!)
  (define (update-track track pattern)
    (let* ([id (string->number (alist-ref 'id track))]
           [new-steps (alist-ref 'steps track)])
      (instrument-steps-set! (alist-ref id pattern) new-steps)))

  (define (make-default-pattern)
    (list 
     (cons 0 (make-instrument "bd" 61 88 (make-vector 16 0)))
     (cons 1 (make-instrument "rattle" 61 0 (make-vector 16 0)))
     (cons 2 (make-instrument "hats" 70 0 (make-vector 16 0)))
     (cons 3 (make-instrument "snare" 62 37 (make-vector 16 0)))
     (cons 4 (make-instrument "hands" 67 0 (make-vector 16 0)))
     (cons 5 (make-instrument "cym" 63 0 (make-vector 16 0)))
     (cons 6 (make-instrument "hb" 61 0 (make-vector 16 0)))
     (cons 7 (make-instrument "clap" 71 0 (make-vector 16 0)))
     (cons 8 (make-instrument "cong" 64 0 (make-vector 16 0))))))
