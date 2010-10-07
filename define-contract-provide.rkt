(module define-contract-provide scheme
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;
  ;; define/contract/provide macros
  ;;
  ;; take define/contract further
  
  (provide define/contract/provide
           define/provide
           define/contract/provide-struct
           define/contract-parameter
           )
  
  (define-syntax define/contract/provide
    (syntax-rules ()
      [(_ (id . args) contract body ...)
       (begin
         (define/contract (id . args) contract body ...)
         (provide/contract [id contract]))]
      [(_ id contract expr)
       (begin
         (define/contract id contract expr)
         (provide/contract [id contract]))] ))

  (define-syntax define/provide
    (syntax-rules ()
      [(_ (id . args) body ...)
       (begin
         (define (id . args) body ...)
         (provide id))]
      [(_ id expr)
       (begin
         (define id expr)
         (provide id))] ))

  (define-syntax define/contract/provide-struct
    (syntax-rules ()
      [(_ struct-id ([field contract] ...) struct-option ...)
       (begin
         (define-struct struct-id (field ...) struct-option ...)
         (provide/contract [struct struct-id ([field contract] ...)]) )] ))

  (define-syntax define/contract-parameter
    (syntax-rules()
      [(_ name contract init)
       (define/contract name
         (() (contract) . ->* . (or/c contract void?))
         (make-parameter init))]))
  )
