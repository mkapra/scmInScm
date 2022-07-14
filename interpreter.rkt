(define memSize 100)

;; The memory where all the data is stored
(define mem (make-vector memSize 0))
;; Pointer that points to the next free index in the memory vector
(define free 0)

;;;;;;;;;;;;;;;;;; Getter ;;;;;;;;;;;;;;;;;;
(define (tag? n t)
  (eq? (vector-ref mem n) t)
  )

(define (i-pair? n)
  (tag? n 'pair)
  )

(define (number->value p)
  (if (tag? p 'number)
      (ref p 1)
      (error "Pointer is not a number")
      )
  )

(define (i-car p)
  (if (tag? p 'pair)
      (ref p 1)
      (error "Pointer is not a pair")
      )
  )

(define (i-cdr p)
  (if (tag? p 'pair)
      (ref p 2)
      (error "Pointer is not a pair")
      )
  )

(define (symbol->name p)
  (if (tag? p 'symbol)
      (ref p 1)
      (error "Pointer is not a symbol")
      )
  )

;;;;;;;;;;;;;;;;;; Constructors ;;;;;;;;;;;;;;;;;;
(define (i-number value)
  (define ptr (malloc 2))
  (ref! ptr 0 'number)
  (ref! ptr 1 value)
  ptr
  )

(define (i-cons h t)
  (define ptr (malloc 3))
  (ref! ptr 0 'pair)
  (ref! ptr 1 h)
  (ref! ptr 2 t)
  ptr
  )

(define (i-symbol v)
  (define ptr (malloc 2))
  (ref! ptr 0 'symbol)
  (ref! ptr 1 v)
  ptr
  )
        
;;;;;;;;;;;;;;;;;; Memory Dump ;;;;;;;;;;;;;;;;;;
;; Prints the content of the memory to the console in an understandable format
(define (dumpMem)
  (if (= (vector-ref mem 0) 0)
      (display "Memory is empty")
      (dumpMemRek 0)
      )
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
        (if (> recLen 0)
            (begin
              (display tagIndex) (display ": ") (display (vector-ref mem tagIndex))

              ;; Print args only if they exist (this is the case if the recLen is greater than 1
              (if (or (> recLen 1))
                  (dumpObjArgs argBegin argEnd)
                  (begin
                    (newline)
                    )
                  )
              )
            )
        
        (dumpMemRek (+ i (vector-ref mem i) 1))
        )
      )
  )

;;;;;;;;;;;;;;;;;; Memory operations ;;;;;;;;;;;;;;;;;;
(define (ref p i)
  (vector-ref mem (+ p i))
  )

(define (ref! p i v)
  (vector-set! mem (+ p i) v)
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

(define (error e)
  (newline)
  (display "ERROR: ")
  (display e)
  (newline)
  exit
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
  (ref! ptr 1 1)
  (ref! ptr 2 2)
  (set! ptr (malloc 1))
  (ref! ptr 0 'false)
  (dumpMem)
  )

(define (consTest)
  (let* ((nr1 (i-number 200))
         (nr2 (i-number 50))
         (nr3 (i-number 70))
         (p1 (i-cons nr1 nr2))
         (p2 (i-cons p1 nr3))
         )
    (dumpMem)
    ;;(display (number->value (i-car (i-car p2))))(newline)
    ;;(display (number->value (i-cdr p2)))
    )
  )

(consTest)