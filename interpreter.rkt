(define memSize 1000)

;; The memory where all the data is stored
(define mem (make-vector memSize 0))
;; Pointer that points to the next free index in the memory vector
(define free 0)

(define (error e)
  (display e)
  (newline)
  exit
  )

;; Allocates a specific amount (n) of memory in the vector
;; This is simply done by increasing the free pointer
(define (malloc n)
  (if (> (+ n free 1) memSize)
      (error "Memory Overflow in malloc")
      (begin
        (ref! free 0 n)
        (set! free (+ free n 1))
        (- free n)
        )
      )
  )

;;;;;;;;;;;;;;;;;; Memory Dump ;;;;;;;;;;;;;;;;;;
;; Prints the content of the memory to the console in an understandable format
(define (dumpMem)
  (if (= (vector-ref mem 0) 0)
      (display "Memory is empty")
      (dumpMemRek 0)
      )
  )

(define (ref p i)
  (vector-ref mem (+ p i 1))
  )

(define (ref! p i v)
  (vector-set! mem (+ p i) v)
  )


(define (dumpObjArgs begin end)
  (display " - ")
  (let ((i 0)
        (argAmount (vector->list (make-vector (+ 1 (- end begin)))))
        )
    (for-each (lambda (x)
                (display (vector-ref mem (+ begin i)))
                (display " ")
                (set! i (+ i 1))
                )
              argAmount)
    )
  (newline)
  )


(define (dumpMemRek i)
  (if (not (>= i memSize))
      (let* ((recLen (vector-ref mem i))
             (tagIndex (+ i 1))
             (argBegin (+ i 2))
             (argEnd (+ argBegin (- recLen 2)))
             )
        (display tagIndex) (display ": ") (display (vector-ref mem tagIndex))
             
        (if (and (not (= recLen 1)) (not (= recLen 0)))
            (dumpObjArgs argBegin argEnd)
            (begin
              (newline)
              (dumpMemRek (+ i (vector-ref mem i) 1))
              )
            )
        )
      )
  )

;;;;;;;;;;;;;;;;;; TESTS ;;;;;;;;;;;;;;;;;;
;; This function should throw an overflow error while trying (malloc 1)
(define (mallocTest)
  (malloc 3)
  (dumpMem)
  (malloc 4)
  (dumpMem)
  (malloc 1)
  (dumpMem)
  )

(define (setTest)
  (define ptr (malloc 1))
  (ref! ptr 0 'true)
  (set! ptr (malloc 1))
  (ref! ptr 0 'false)
  (set! ptr (malloc 3))
  (ref! ptr 0 'pair)
  (set! ptr (malloc 1))
  (ref! ptr 0 'false)
  (dumpMem)
  )

(setTest)