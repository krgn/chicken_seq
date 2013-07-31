(import foreign)
(import srfi-14)
(import foreigners)

(foreign-declare "#include \"rawmidi.h\"")

(define-foreign-record-type (midi-port "snd_rawmidi_t")
   (constructor: make-midi-port)
   (destructor: free-midi-port))

(define open-midi-port
  (foreign-lambda* 
   int ((midi-port input)
        (midi-port output)
        (c-string file)
        (c-string device)
        (int mode))
   "C_return(rawmidi_hw_open(&input, &output, file, device, mode));"))

(define write-note 
  (foreign-lambda size_t "rawmidi_hw_write" midi-port u8vector size_t ))

(define print-device-info
  (foreign-lambda int "rawmidi_hw_print_info" c-string))

(define (main)
  (let ((outport (make-midi-port))
        (inport (make-midi-port))
        (file "/dev/midi2")
        (device "UM-1"))
    ;; (print (open-midi-port inport outport file device 0))
    ;; (print (write-note outport '#u8(#x90 60 100) 3))))
    (print-device-info file)))

(main)
