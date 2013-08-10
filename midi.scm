(module midi (open-device close-device write-midi make-note)
  (import chicken scheme)
  (use posix)
  (use srfi-4)

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
