(module rawmidi 
  ;; exposes ..
  (open-device close-device write-midi make-note bar-in-ms quaver-by-bpm sixteenth-by-bpm)

  (import chicken scheme)
  (use posix srfi-4)

  (define (bar-in-ms bpm)
    (inexact->exact 
     (* (* (/ 60 bpm) 4) 1000)))

  (define (quaver-by-bpm bpm)
    (inexact->exact 
     (* (/ 60 bpm) 1000)))

  (define (sixteenth-by-bpm bpm)
    (inexact->exact 
     (* (/ (/ 60 bpm) 4) 1000)))

  (define (open-device path)
    (let ((fd (file-open path open/wronly)))
      (file-control fd fcntl/setfd 0)
      fd))

  (define (close-device device)
    (file-close device))

  (define (write-midi device blob)
    (file-write device blob 3))               ;we assume we only have to write 3-byte vectors for now

  (define (make-note ch note vel)
    (u8vector->blob (u8vector ch note vel))))
