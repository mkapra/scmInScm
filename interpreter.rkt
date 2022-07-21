;; Error function that takes an error and multiple messages. The error and the messages are printed to the console
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
;; Pointer that points to the next free index in the memory
(define free 0)

;;;;;;;;;;;;;;;;;; Memory operations ;;;;;;;;;;;;;;;;;;
;; Get the value at the given index in the memory
(define (ref p i)
  (vector-ref mem (+ p i))
  )

;; Set the value at the given index in the memory
(define (ref! p i v)
  (vector-set! mem (+ p i) v)
  )

;; Allocates a specific amount (n) of memory in the vector. This is simply done by increasing the free pointer. The
;; return value is the index of the first element in the newly allocated memory.
;;
;; This function also sets the size of the block in a index before the resverved block. This causes the interpreter to
;; allocate n + 1 chunks of memory. This makes it simpler for debugging and is hidden from the user.
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
;; Returns the tag of the given address p
(define (tag p) (ref p 0))
;; Returns if the tag matches the given tag t
(define (tag? n t) (eq? (tag n) t))
;; Sets the tag t on the given address p
(define (tag! p t) (ref! p 0 t))

;; Some type checks
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

(define (i-undefined? n)
  (tag? n 'undefined)
  )

(define (i-bool? n)
  (tag? n 'bool)
  )

(define (i-primitive? n)
  (tag? n 'primitiv)
  )

(define (i-lambda? n)
  (tag? n 'lambda)
  )

(define (i-binding? n)
  (tag? n 'bind)
  )

(define (i-environment? n)
  (tag? n 'environment)
  )

(define (number->value p)
  (if (tag? p 'number)
      (ref p 1)
      (error "number->value" "Pointer is not a number" p)
      )
  )

(define (pair->car p)
  (if (tag? p 'pair)
      (ref p 1)
      (error "pair->car" "Pointer is not a pair" p)
      )
  )

(define (pair->cdr p)
  (if (tag? p 'pair)
      (ref p 2)
      (error "pair->cdr" "Pointer is not a pair")
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
      (error "symbol->name" "Pointer is not a symbol" p)
      )
  )

;; lambda
(define (lambda->arglist p)
  (if (i-lambda? p)
      (ref p 1)
      (error "lambda->arglist" "Pointer is not a lambda" p)
      )
  )

(define (lambda->body p)
  (if (i-lambda? p)
      (ref p 2)
      (error "lambda->body" "Pointer is not a lambda" p)
      )
  )

(define (lambda->env p)
  (if (i-lambda? p)
      (ref p 3)
      (error "lambda->env" "Pointer is not a lambda" p)
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
(define (new-pair h t)
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

; lambda
(define (new-lambda arglist body env)
  (let ((ptr (malloc 4)))
    (ref! ptr 0 'lambda)
    (ref! ptr 1 arglist)
    (ref! ptr 2 body)
    (ref! ptr 3 env)
    ptr
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

(define (dumpObjArgs begin end)
  (display " - ")
  (let ((i 0)
        (argAmount (vector->list (make-vector (+ 1 (- end begin))))))
    (for-each (lambda (x)
                (display (vector-ref mem (+ begin i)))
                (display " ")
                (set! i (+ i 1))
                )
              argAmount))
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

              ;; Print args only if they exist (this is the case if the recLen is greater than 1)
              (if (or (> recLen 1))
                  (dumpObjArgs argBegin argEnd)
                  (newline))
              )
            )

        (dumpMemRek (+ i (vector-ref mem i) 1))
        )
      )
  )

;;;;;;;;;;;;;;;;;; Bindings and environment ;;;;;;;;;;;;;;;;;;
(define i-epsilon (i-symbol 'epsilon))

;; Creates a new binding in the memory. compose is the binding that should
;; be appended to the generated binding
(define (new-binding symbol value compose)
  (if (i-symbol? symbol)
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

;; Searches for a symbol in a list of bindings beginning at binding
(define (binding->value symbol binding)
  (if (i-binding? binding)
      (let ((bindSymbol (ref binding 1)))
        (cond
          ((eq? (symbol->name symbol) (symbol->name bindSymbol)) (ref binding 2))
          ((eq? (ref binding 3) i-epsilon) #f)
          (else (binding->value symbol (ref binding 3))))
        )
      (error "binding->value" "Pointer is not a binding")
      )
  )

;; Creates a new environment with an initial binding
(define (new-environment binding)
  (if (or (i-binding? binding) (eq? binding i-epsilon))
      (let ((ptr (malloc 2)))
        (ref! ptr 0 'environment)
        (ref! ptr 1 binding)
        ptr
        )
      (error "new-environment" "Pointer is not a binding")
      )
  )

;; Returns the first binding which the environment env references to
(define (environment->binding env)
  (if (i-environment? env)
      (ref env 1)
      (error "environment->binding" "Pointer is not an environment")
      )
  )

(define (environment-set! env binding)
  (if (i-environment? env)
      (ref! env 1 binding)
      (error "environment-set" "Pointer is not an environment")
      )
  )

;; Adds a new binding to the environment containing the variable var and the value value
(define (add-variable var value env)
  (if (i-environment? env)
      (environment-set! env (new-binding var value (environment->binding env)))
      (error "add-variable" "Pointer is not an environment")
      )
  )

(define (variable->value var env)
  (binding->value var (ref env 1))
  )

;; Global environment of the interpreter
(define i-environment (new-environment i-epsilon))

;; Creates a new primitive function f in the memory
(define (new-primitive f)
  (let ((ptr (malloc 2)))
    (ref! ptr 0 'primitiv)
    (ref! ptr 1 f)
    ptr
    )
  )

;; Returns the extracted primitive function from the memory
(define (primitive->f p)
  (if (i-primitive? p)
      (ref p 1)
      (error "primitive->f" "Pointer is not a primitive" p)
      )
  )

;; Creates a new primitive with name name and function f and stores the pointer in a binding of
;; the environment
(define (add-primitive name f) (add-variable (i-symbol name) (new-primitive f) i-environment))

;; Evaluates the given expressions and adds them to the env environment.
;; For the evaluation the call-env environment is used.
(define (add-parameters param-list expr-list call-env env)
  (let loop ((params param-list)
             (exprs expr-list))
    (add-variable (pair->car params) (i-eval call-env (pair->car exprs)) env)
    (if (not (= (pair->cdr params) i-null))
        (loop (pair->cdr params) (pair->cdr exprs))
        )
    )
  )

;; Generates a new environment for a lambda function. This environment contains the evaluated
;; arguments of the function.
(define (new-lambda-environment param-list expr-list call-env def-env)
  (let ((new-env (new-environment (environment->binding def-env))))
    (add-parameters param-list expr-list call-env new-env)
    new-env
    )
  )

;;;;;;;;;;;;;;;;;; Primitives ;;;;;;;;;;;;;;;;;;
;; +
(define (i-plus env values)
  (new-number
   (let loop ((sum 0)
              (v values))
     (if (eq? (tag v) 'pair)
         (loop (+ sum (number->value (i-eval env (pair->car v))))
               (pair->cdr v))
         sum))))
(add-primitive '+ i-plus)

;; -
(define (i-minus env values)
  (new-number (- (number->value (pair->car values)) (number->value (i-plus env (pair->cdr values))))))
(add-primitive '- i-minus)

;; *
(define (i-mul env values)
  (new-number
   (let loop ((result 0)
              (v values))
     (if (eq? (tag v) 'pair)
         (loop (* result (number->value (i-eval env (pair->car v))))
               (pair->cdr v))
         result))))
(add-primitive '* i-mul)

;; /
(define (i-div env values)
  (new-number
   (let loop ((result (number->value (i-eval env (pair->car values))))
              (v (pair->cdr values)))
     (if (eq? (tag v) 'pair)
         (loop (/ result (number->value (i-eval env (pair->car v))))
               (pair->cdr v))
         result))))
(add-primitive '/ i-div)

;; define
(define (i-define env values)
  (add-variable (pair->car values) (i-eval env (pair->car (pair->cdr values))) env)
  i-undefined
  )
(add-primitive 'define i-define)

;; if
(define (i-if env exp)
  (if (bool->value (i-eval env (pair->car exp)))
      (i-eval env (pair->car (pair->cdr exp)))
      (if (eq? (pair->cdr (pair->cdr exp)) i-null)
          i-undefined
          (i-eval env (pair->car (pair->cdr (pair->cdr exp)))))
      )
  )
(add-primitive 'if i-if)

;; not
(define (i-not env exp)
  (if (bool->value (i-eval env (pair->car exp)))
      (i-bool #f)
      (i-bool #t)
      )
  )
(add-primitive 'not i-not)

;; and
(define (i-and env exp)
  (if (bool->value (i-eval env (pair->car exp)))
      (if (eq? (pair->cdr exp) i-null)
          (i-bool #t)
          (i-and env (pair->cdr exp))
          )
      (pair->car exp)
      )
  )
(add-primitive 'and i-and)

;; or
(define (i-or env exp)
  (if (bool->value (i-eval env (pair->car exp)))
      (i-bool #t)
      (if (eq? (pair->cdr exp) i-null)
          (i-bool #f)
          (i-or env (pair->cdr exp))
          )
      )
  )
(add-primitive 'or i-or)

;; begin
(define (i-begin env exp)
  (let ((first (i-eval env (pair->car exp))))
    (if (eq? (pair->cdr exp) i-null)
        first
        (i-begin env (pair->cdr exp))
        )
    )
  )
(add-primitive 'begin i-begin)

;; cons
(define (i-cons env exp)
  (if (eq? (pair->cdr exp) i-null)
      (new-pair (i-eval env (pair->car exp)) (pair->cdr exp))
      (new-pair (i-eval env (pair->car exp)) (i-cons env (pair->cdr exp)))
      )
  )
(add-primitive 'cons i-cons)

;; car
(define (i-car env exp) (pair->car (i-eval env (pair->car exp))))
(add-primitive 'car i-car)
;; cdr
(define (i-cdr env exp) (pair->cdr (i-eval env (pair->car exp))))
(add-primitive 'cdr i-cdr)

;; i-lambda
(define (i-lambda env values)
  (new-lambda (pair->car values) (pair->cdr values) env))
(add-primitive 'lambda i-lambda)

;; quote
(define (i-quote env exp) (pair->car exp))
(add-primitive 'quote i-quote)

;;;;;;;;;;;;;;;;;; eval = apply ;;;;;;;;;;;;;;;;;;
(define (i-eval env exp)
  (cond
    ((tag? exp 'pair) (i-apply env (variable->value (pair->car exp) env) (pair->cdr exp)))
    ((tag? exp 'number) exp)
    ((i-bool? exp) exp)
    ((i-symbol? exp) (variable->value exp env)))
  )

(define (i-apply env func arglist)
  (cond
    ((i-primitive? func) ((primitive->f func) env arglist))
    ((i-lambda? func) (let loop ((func-env (new-lambda-environment (lambda->arglist func) arglist env (lambda->env func))) (exp (lambda->body func)))
                        (cond
                          ((eq? (pair->cdr exp) i-null) (i-eval func-env (pair->car exp)))
                          (else (i-eval func-env (pair->car exp)) (loop func-env (pair->cdr exp)))))))
  )

;;;;;;;;;;;;;;;;;; Input ;;;;;;;;;;;;;;;;;;
(define (expr->i-expr in)
  (cond
    ((pair? in) (new-pair (expr->i-expr (car in)) (expr->i-expr (cdr in))))
    ((number? in) (new-number in))
    ((symbol? in) (i-symbol in))
    ((boolean? in) (i-bool in))
    ((null? in) i-null)
    ((i-undefined?) (if #f #t)))
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
    ((i-lambda? p) (string-append "#<procedure:" (symbol->string (symbol->name (ref 1 (environment->binding i-environment)))) ">"))
    ((i-pair? p) (cons (i-expr->expr (pair->car p)) (i-expr->expr (pair->cdr p))))
    ((i-number? p) (number->value p))
    ((i-symbol? p) (symbol->name p))
    ((i-bool? p) (bool->value p))
    ((i-null? p) '())
    (else ""))
  )

(define (i-display p) (display (i-expr->expr p)))

;; Call entry function of interpreter
(i-scheme)
