(define (error . messages)
  (dumpMem)
  (newline)
  (for-each (lambda (x) (display x) (display " ")) messages)
  (newline)
  (exit) ;; This function is undefined. Scheme will exit here
  )

(define memSize 1000)

;; The memory where all the data is stored
(define mem (make-vector memSize 0))
;; Pointer that points to the next free index in the memory vector
(define free 0)

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
      (error "malloc" "Memory Overflow in malloc")
      (begin
        (ref! free 0 n)
        (set! free (+ free n 1))
        (- free n)
        )
      )
  )

;;;;;;;;;;;;;;;;;; Getter ;;;;;;;;;;;;;;;;;;
(define (tag p) (ref p 0))
(define (tag? n t) (eq? (vector-ref mem n) t))
(define (tag! p t) (ref! p 0 t))

(define (i-number? n)
  (tag? n 'number)
  )

(define (i-symbol? n)
  (tag? n 'symbol)
  )

(define (i-pair? n)
  (tag? n 'pair)
  )

(define (i-null? n)
  (tag? n 'null)
  )

(define (i-bool? n)
  (tag? n 'bool)
  )

(define (i-primitive? n)
  (tag? n 'primitiv)
  )

(define (symbol->name p)
  (if (tag? p 'symbol)
      (ref! p 1)
      (error "symbol->name" "Pointer is not a symbol")
      )
  )

(define (number->value p)
  (if (tag? p 'number)
      (ref p 1)
      (error "number->value" "Pointer is not a number" p)
      )
  )

(define (i-car p)
  (if (tag? p 'pair)
      (ref p 1)
      (error "i-car" "Pointer is not a pair" p)
      )
  )

(define (i-cdr p)
  (if (tag? p 'pair)
      (ref p 2)
      (error "i-cdr" "Pointer is not a pair")
      )
  )

(define (bool->value p)
  (if (tag? p 'bool)
      (ref p 1)
      )
  )

(define (symbol->name p)
  (if (tag? p 'symbol)
      (ref p 1)
      (error "symbol->name" "Pointer is not a symbol")
      )
  )

;;;;;;;;;;;;;;;;;; Constructors ;;;;;;;;;;;;;;;;;;
(define i-undefined
  (let ((ptr (malloc 1)))
    (tag! ptr 'undefined)
    ptr
    )
  )

(define i-null
  (let ((ptr (malloc 1)))
    (tag! ptr 'null)
    ptr
    )
  )

; numbers
(define (new-number value)
  (define ptr (malloc 2))
  (ref! ptr 0 'number)
  (ref! ptr 1 value)
  ptr
  )

; pairs
(define (i-cons h t)
  (define ptr (malloc 3))
  (ref! ptr 0 'pair)
  (ref! ptr 1 h)
  (ref! ptr 2 t)
  ptr
  )

; symbols
(define (i-symbol v)
  (define ptr (malloc 2))
  (ref! ptr 0 'symbol)
  (ref! ptr 1 v)
  ptr
  )

; booleans
(define (i-bool v)
  (define ptr (malloc 2))
  (ref! ptr 0 'bool)
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

;;;;;;;;;;;;;;;;;; Bindings and environment ;;;;;;;;;;;;;;;;;;
(define i-epsilon (i-symbol 'epsilon))

(define (new-binding symbol value compose)
  (if (tag? symbol 'symbol)
      (let ((p (malloc 4)))
        (ref! p 0 'bind)
        (ref! p 1 symbol)
        (ref! p 2 value)
        (ref! p 3 compose)
        p
        )
      (error "new-binding" "symbol pointer is not a symbol")
      )
  )

(define (binding->value symbol binding)
  (if (tag? binding 'bind)
      (let ((bindSymbol (ref binding 1)))
        (cond ((eq? (symbol->name symbol) (symbol->name bindSymbol))
               (ref binding 2))
              ((eq? (ref binding 3) i-epsilon)
               #f)
              (else (binding->value symbol (ref binding 3))))
        )
      (error "binding->value" "Pointer is not a binding")
      )
  )

(define (new-environment binding)
  (if (or (tag? binding 'bind) (eq? binding i-epsilon))
      (let ((ptr (malloc 2)))
        (ref! ptr 0 'environment)
        (ref! ptr 1 binding)
        ptr
        )
      (error "new-environment" "Pointer is not a binding")
      )
  )

(define (environment->binding env)
  (if (tag? env 'environment)
      (ref env 1)
      (error "environment->binding" "Pointer is not an environment")
      )
  )

(define (environment-set! env binding)
  (if (tag? env 'environment)
      (ref! env 1 binding)
      (error "environment-set" "Pointer is not an environment")
      )
  )

(define (add-variable var value env)
  (if (tag? env 'environment)
      (environment-set! env (new-binding var value (environment->binding env)))
      (error "add-variable" "Pointer is not an environment")
      )
  )

(define (variable->value var env) 
  (binding->value var (ref env 1))
  )

(define i-environment (new-environment i-epsilon))

(define (new-primitive f)
  (let ((ptr (malloc 2)))
    (ref! ptr 0 'primitiv)
    (ref! ptr 1 f)
    ptr
    )
  )

(define (primitive->f p)
  (if (i-primitive? p)
      (ref p 1)
      (error "primitive->f" "Pointer is not a primitive" p)
      )
  )

(define (add-primitive name f)
  (let ((s (i-symbol name))
        (p (new-primitive f)))
    (add-variable s p i-environment)
    )
  )

;;;;;;;;;;;;;;;;;; Primitives ;;;;;;;;;;;;;;;;;;
(define (i-plus env values)
  (new-number
   (let loop ((sum 0)
              (v values))
     (if (eq? (tag v) 'pair)
         (loop (+ sum (number->value (i-eval env (i-car v))))
               (i-cdr v))
         sum))))
(add-primitive '+ i-plus)

(define (i-minus env values)
  (new-number
   (let loop ((result 0)
              (v values))
     (if (eq? (tag v) 'pair)
         (loop (- result (number->value (i-eval env (i-car v))))
               (i-cdr v))
         result))))
(add-primitive '- i-minus)

(define (i-mul env values)
  (new-number
   (let loop ((result 0)
              (v values))
     (if (eq? (tag v) 'pair)
         (loop (* result (number->value (i-eval env (i-car v))))
               (i-cdr v))
         result))))
(add-primitive '* i-mul)

(define (i-div env values)
  (new-number
   (let loop ((result 0)
              (v values))
     (if (eq? (tag v) 'pair)
         (loop (/ result (number->value (i-eval env (i-car v))))
               (i-cdr v))
         result))))
(add-primitive '/ i-div)

(define (i-define env values)
  (add-variable (i-car values) (i-car (i-cdr values)) i-environment)
  i-undefined
  )
(add-primitive 'define i-define)

(define (i-if env exp)
  (if (bool->value (i-eval env (i-car exp)))
      (i-eval env (i-car (i-cdr exp)))
      (if (eq? (i-cdr (i-cdr exp)) i-null)
          i-undefined
          (i-eval env (i-car (i-cdr (i-cdr exp)))))
      )
  )
(add-primitive 'if i-if)

(define (i-not env exp)
  (if (bool->value (i-eval env (i-car exp)))
      (i-bool #f)
      (i-bool #t)
      )
  )
(add-primitive 'not i-not)

(define (i-and env exp)
  (if (bool->value (i-eval env (i-car exp)))
      (if (eq? (i-cdr exp) i-null)
          (i-bool #t)
          (i-and env (i-cdr exp))
          )
      (i-car exp)
      )
  )
(add-primitive 'and i-and)

(define (i-or env exp)
  (if (bool->value (i-eval env (i-car exp)))
      (i-bool #t)
      (if (eq? (i-cdr exp) i-null)
          (i-bool #f)
          (i-or env (i-cdr exp))
          )
      )
  )
(add-primitive 'or i-or)

(define (i-quote env exp)
  (display "Not implemented")
  )
(add-primitive 'quote i-quote)

;;;;;;;;;;;;;;;;;; eval = apply ;;;;;;;;;;;;;;;;;;
(define (i-eval env exp)
  (cond ((tag? exp 'pair) (i-apply env (variable->value (i-car exp) env) (i-cdr exp)))
        ((tag? exp 'number) exp)
        ((i-bool? exp) exp)
        ((i-symbol? exp) (variable->value exp env))
        )
  )

(define (i-apply env func arglist)
  (if (i-primitive? func)
      ((primitive->f func) env arglist)
      (error "i-apply" "Pointer is not a primitive" func (tag func))
      )
  )

;;;;;;;;;;;;;;;;;; Input ;;;;;;;;;;;;;;;;;;
(define (expr->i-expr in)
  (cond
    ((pair? in) (i-cons (expr->i-expr (car in)) (expr->i-expr (cdr in))))
    ((number? in) (new-number in))
    ((symbol? in) (i-symbol in))
    ((boolean? in) (i-bool in))
    ((null? in) i-null)
    ((tag? in 'undefined) (if #f #t))
    )
  )

(define (i-read)
  (expr->i-expr (read))
  )

(define (read-eval-print return)
  (define (i-exit env values) (return 0))
  (add-primitive 'exit i-exit)
  (let loop ()
    (newline)
    (display "i-scheme> ")
    (i-display (i-eval i-environment (i-read)))
    (loop)))

(define (i-scheme)
  (display "This is i-scheme version 1.0")
  (call-with-current-continuation read-eval-print)
  (display "Bye!")
  (newline))

;;;;;;;;;;;;;;;;;; Output ;;;;;;;;;;;;;;;;;;
(define (i-expr->expr p)
  (cond
    ((i-pair? p) (cons (i-expr->expr (i-car p)) (i-expr->expr (i-cdr p))))
    ((i-number? p) (number->value p))
    ((i-symbol? p) (symbol->name p))
    ((i-bool? p) (bool->value p))
    ((i-null? p) '())
    (else ""))
  )

(define (i-display p)
  (display (i-expr->expr p))
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
  (let* ((nr1 (new-number 200))
         (nr2 (new-number 50))
         (nr3 (new-number 70))
         (p1 (i-cons nr1 nr2))
         (p2 (i-cons p1 nr3))
         )
    (dumpMem)
    ;;(display (number->value (i-car (i-car p2))))(newline)
    ;;(display (number->value (i-cdr p2)))
    )
  )

(define (envTest)
  (let ((n1 (new-number 1))
        (n2 (new-number 2))
        (n3 (new-number 3))
        (s1 (i-symbol 'number))
        (s2 (i-symbol 'number))
        (s3 (i-symbol 'number))
        (s4 (i-symbol 'number))

        (b1 (new-binding s1 n1 i-epsilon))
        (b2 (new-binding s2 n2 b1))
        )
    (dumpMem)
    (define e1 (new-environment b2))
    (variable->value s1 e1)
    (add-variable s4 n3 e1)
    (add-variable s4 n3 i-environment)
    (variable->value s4 e1)
    
    ; Should return false
    ;(binding->value s3 b2)
    ; Should return ptr to n2
    ;(binding->value s2 b2)
    )
  )

(i-scheme)