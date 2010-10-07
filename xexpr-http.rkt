(module xexpr-http racket

  (require test-engine/scheme-tests)
  (require xml)
  (require net/url)
  (require net/head)
  (require net/uri-codec)
  (require (prefix-in h: html))
  (require "define-contract-provide.rkt")
  (require "uri-string.rkt")
  
  (define/provide (xexpr/procedure? x)
    ; We want to allow an xexpr where the body consists of
    ; a procedure? -- but xexpr? doesn't allow this.
    (and (list? x)
         (= (length x) 3)
         (symbol? (first x))
         (attributes? (second x))
         (procedure? (third x))))
  
  (define/provide xexpr/procedure/c (make-contract #:name 'xexpr/procedure/c
                                                   #:first-order xexpr/procedure?))
  
  (define/provide request?
    (and/c (or/c xexpr?
                 xexpr/procedure?)
           (λ (x) (or (symbol=? (car x) 'head)
                      (symbol=? (car x) 'get)
                      (symbol=? (car x) 'post)
                      (symbol=? (car x) 'put)
                      (symbol=? (car x) 'delete)
                      (symbol=? (car x) 'options)
                      ))))
  
  (define/provide request/c (make-contract #:name 'request/c
                                           #:first-order request?))
  
  (define/provide response?
    (and/c (or/c xexpr?
                 xexpr/procedure?)
           (λ (x) (symbol=? (car x) 'response))))
  
  (define/provide response/c (make-contract #:name 'response/c
                                            #:first-order response?))
    
  (define/provide attributes?
    (listof (or/c (list/c symbol? string?)
                  (list/c symbol? string? string?))))
  
  (define/provide attributes/c (make-contract #:name 'attributes/c
                                              #:first-order attributes?))
  
  (define/contract/provide (attributes x)
    ((or/c request/c response/c)
     . -> . attributes?)
    (if (and (>= (length x) 2)
             (list? (second x)))
        (second x)
        '()))
  
  (define/contract/provide (body x)
    ((or/c request/c response/c) . -> . (or/c xexpr? procedure?))
    (if (attributes x)
        (if (>= (length x) 3)
            (third x)
            '())
        (if (>= (length x) 2)
            (second x)
            '())))

  (define/contract/provide (request-method r)
    (request/c . -> . symbol?)
    (car r))
  

  (define/contract/provide (response-status-code r)
    (response/c . -> . exact-positive-integer?)
    (let ([p (assoc 'Status (attributes r))])
      (if p (string->number (second p)) 0)))
  
  (define/contract/provide (response-status-text r)
    (response/c . -> . string?)
    (let ([p (assoc 'Status (attributes r))])
      (if p (third p) "")))
  
  (define/contract/provide (header-value name r [not-found ""])
    ((symbol? (or/c request/c response/c)) (string?) . ->* . string?)
    (let ([p (assoc name (attributes r))])
      (if p (second p) not-found)))
  
  (define/contract/provide (header-value/number name r [not-found 0])
    ((symbol? (or/c request/c response/c)) (number?) . ->* . number?)
    (let ([str (header-value name r)])
      (if (and str (string->number str))
          (string->number str) not-found)))
  
  (define (attribs->alist a)
    (define (list->cons l)
      (cons (first l) (second l)))
    (map list->cons a))
  
  (define (alist->attribs a)
    (define (cons->list c)
      (list (car c) (cdr c)))
    (map cons->list a))
  
  (check-expect (attribs->alist '([a   "1"][b   "2"])) '([a . "1"][b . "2"]))
  (check-expect (alist->attribs '([a . "1"][b . "2"])) '([a   "1"][b   "2"]))
  
  (define/provide (attribute->header-string a)
    (if (or (equal? #\! (string-ref (symbol->string (first a)) 0))
            (equal? 'Status (first a)))
        #f
        (format "~a: ~a" (first a) (second a))))
    
  (define/provide (seconds->gmt-string [s (current-seconds)])
    ; two-digits : num -> str
    (define (two-digits n)
      (let ([str (number->string n)])
        (if (< n 10) (string-append "0" str) str)))
    (define MONTHS
      #("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))
    (define DAYS
      #("Sun" "Mon" "Tue" "Wed" "Thu" "Fri" "Sat"))
    
    (let* ([local-date (seconds->date s)]
           [date (seconds->date (- s
                                   (date-time-zone-offset local-date)
                                   (if (date-dst? local-date) 3600 0)))])
      (format "~a, ~a ~a ~a ~a:~a:~a GMT"
              (vector-ref DAYS (date-week-day date))
              (two-digits (date-day date))
              (vector-ref MONTHS (sub1 (date-month date)))
              (date-year date)
              (two-digits (date-hour date))
              (two-digits (date-minute date))
              (two-digits (date-second date)))))
  
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;
  ;; send-entity
  ;; Send an entity body for either a request or a response.
  ;;
  (define/contract (send-entity r)
    ((or/c request/c response/c) . -> . (or/c bytes? procedure?))
    (cond
      [(procedure? (body r))
       (body r)]
      [(regexp-match? #rx"^application/x-www-form-urlencoded" (header-value 'Content-Type r))
       (string->bytes/utf-8 (alist->form-urlencoded (attribs->alist (second (body r)))))]
      [(regexp-match? #rx"^application/xml" (header-value 'Content-Type r))
       (string->bytes/utf-8
        (call-with-output-string
         (lambda (out)
           (display-xml/content (xexpr->xml (body r)) out))))]
      [(regexp-match? #rx"^text/html" (header-value 'Content-Type r))
       (string->bytes/utf-8
        (call-with-output-string
         (lambda (out)
           (display-xml/content (xexpr->xml (body r)) out))))]
      [(empty? (body r))
       #""]
      [else
       (string->bytes/utf-8 (format "~a" (body r)))]))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;
  ;; receive-entity
  ;; Read an entity body for either a request or a response.
  ;;
  (define/contract/provide (receive-entity in content-type content-length)
    (input-port? string? exact-nonnegative-integer? . -> . xexpr?)
    
    (define (get-element-from-port port) ; convert a string of HTML to an x-expression
      (let ([x (h:read-html-as-xml port)])
        (if (element? x)
            x
            ; Else not an element, just a list of items?
            ; Fine: put them in a pro forma ("fake") root element.
            (if (list? x)
                (make-element #f #f '***entity*** '() x)
                (error)))))
    
    (let ([by (read-bytes content-length in)])
      (let ([str (if (eof-object? by) "" (bytes->string/utf-8 by #\?))])
        (cond
          [(or (regexp-match? #rx"^text/xml" content-type)
               (regexp-match? #rx"^application/xml" content-type))
           (xml->xexpr (document-element (call-with-input-string str read-xml)))]
          [(regexp-match? #rx"^text/html" content-type)
           (xml->xexpr (call-with-input-string str get-element-from-port))]
          [(regexp-match? #rx"^text/plain" content-type)
           str]
          [(regexp-match? #rx"^application/x-www-form-urlencoded" content-type)
           `(data ,(alist->attribs (form-urlencoded->alist (bytes->string/utf-8 by))))]
          [else `(entity () ,(format "~a" by))]))))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;
  ;; Client point of view
  ;;

  (define/contract/provide (http-request r)
    (request/c . -> . response/c)
    (let-values ([(in out) (tcp-connect (header-value '!host r)
                                        (string->number (header-value '!port r)))])
      (begin0
        (http-request/ports r in out)
        (close-input-port in)
        (close-output-port out))))
  
  (define/contract/provide (http-request/ports r in out)
    (request/c input-port? output-port? . -> . response/c)
    
    (define (to-server fmt . args)
      (let ([s (apply format (string-append fmt "\r\n") args)])
        (display s out)))
    
    (let ([data
           (cond
             [(or (symbol=? 'post (request-method r))
                  (symbol=? 'put (request-method r)))
              (send-entity r)]
             [else #f])])
      
      (to-server "~a ~a HTTP/1.1"
                 (string-upcase (symbol->string (request-method r)))
                 (header-value '!path r))
      (when (string=? "" (header-value 'Date r))
        (to-server "Date: ~a" (seconds->gmt-string (current-seconds))))
      (for-each to-server
                (filter-map attribute->header-string
                            (attributes r)))
      (when (bytes? data)
        (to-server "Content-Length: ~a" (bytes-length data)))
      (to-server "")
      
      (cond
        [(bytes? data) (display data out)]
        [(procedure? data) (data out)] ; callback procedure
        [else #f])
      
      (flush-output out))
    
    (let* ([h (purify-port in)]
           [a (cons (extract-http-code/descr h)
                    (map (lambda (x)
                           (list (string->symbol (car x)) (cdr x)))
                         (extract-all-fields h)))])
      (if (symbol=? 'head (request-method r))
          `(response ,a)
          `(response ,a
                     ,(receive-entity in
                                      (header-value 'Content-Type `(response ,a))
                                      (header-value/number 'Content-Length `(response ,a)))))))
  
  (define (extract-http-code/descr head)
    (string? . -> . (list/c symbol? string? string?))
    (let ([m (regexp-match #px"^HTTP/([1]\\.[01]) ([0-9]{3})(?:[ ]*)([^\r\n]*)[\r\n]" head)])
      (if m
          (let ([ver (second m)]
                [code (third m)]
                [descr (if (fourth m) (fourth m) "")])
            (list 'Status code descr))
          (list 'Status "999" "Bad or no response"))))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;
  ;; Server point of view
  ;;
  (define/contract/provide (receive-request in)
    (input-port? . -> . request/c)
    (let* ([str (read-line in 'any)]
           [m (regexp-match #rx"^(.+) (.+) HTTP/[0-9]+\\.([0-9])+" str)]
           [method (if m (string-downcase (second m)) #f)]
           [path (if m (third m) #f)]
           [ver (if m (string-append "1." (fourth m)) #f)]
           [heads (let loop ([heads '()])
                    (let ([str (read-line in 'any)])
                      (if (or (eof-object? str)
                              (string=? str ""))
                          heads
                          (let ([m (regexp-match #rx"^([^:]+):[ ]+(.+)$" str)])
                            (if m
                                (loop (cons (list (string->symbol (second m)) (third m))
                                            heads))
                                (loop heads))))))]
           [heads (append heads `([!path ,path]
                                  [!http-version ,ver]))]
           [r `(,(string->symbol method) ,heads)]
           [ct (header-value 'Content-Type r)]
           [len (header-value/number 'Content-Length r)])
      (if (> len 0)
          `(,(string->symbol method) ,heads ,(receive-entity in ct len))
          `(,(string->symbol method) ,heads))))
  
  (define/contract/provide (send-response r add-connection-close-header? out)
    (response/c boolean? output-port? . -> . void)

    (define (to-client fmt . args)
      (display (apply format (string-append fmt "\r\n") args) out))
    
    (let ([data (send-entity r)])
      
      (to-client "HTTP/1.1 ~a ~a"
                 (response-status-code r)
                 (response-status-text r))
      (when (string=? "" (header-value 'Date r))
        (to-client "Date: ~a" (seconds->gmt-string (current-seconds))))
      (when (string=? "" (header-value 'Server r))
        (to-client "Server: Greg"))
      (for-each to-client
                (filter-map attribute->header-string
                            (attributes r)))
      (when (bytes? data)
        (to-client "Content-Length: ~a" (bytes-length data)))
      (when add-connection-close-header?
        (to-client "Connection: close"))
      (to-client "")
      
      (cond
        [(bytes? data) (display data out)]
        [(procedure? data) (data out)]  ; callback procedure
        [else #f])
      
      (flush-output out)))

  (define/contract/provide (close-connection? request response)
    (request/c response/c . -> . boolean?)
    (or (string=? "1.0" (header-value '!http-version request))
        (string=? "Close" (header-value 'Connection request))
        (string=? "Close" (header-value 'Connection response))))
  
  ;;;;;;;;;
  ;; test
  
  (define (some-tests)
    (define/contract (xh uri [heads '()])
      ((string?) (attributes?) . ->* . attributes?)
      (let-values ([(scheme host port path query fragment) (split-uri uri)])
        (let* ([ssl? (if (and scheme (string-ci=? scheme "https")) #t #f)]
               [port (if port port (if ssl? "443" "80"))]
               [host (if host host "")]
               [path (regexp-replace* #rx" " (combine-uri #:path path #:query query #:fragment fragment) "%20")])
          (append `([Host ,(if (string=? port "80")
                               host ; some servers might choke on optional port
                               (format "~a:~a" host port))]
                    [!host ,host]
                    [!port ,port]
                    [!path ,path]
                    [Date ,(seconds->gmt-string (current-seconds))])
                  heads))))
  
    ; some tests not connecting to real server, just fake ports
    (define (fake-test x)
      (call-with-input-string
       (format "HTTP/1.1 200 OK\r\nHost: www.google.com\r\nDate: ~a\r\n\r\n"
               (seconds->gmt-string (current-seconds)))
       (lambda (in)
         (http-request/ports x in (current-output-port)))))
    
    (fake-test `(get ,(xh "http://www.google.com:8001/foobar?a=1&b=2"
                          `([X-Fake-Header "value"]))))
    
    (fake-test `(post ,(xh "http://www.google.com/"
                           `([Content-Type "application/x-www-form-urlencoded"]))
                      (data ([first "bob"]
                             [last "jones"]))))
    
    (let ()
      (define (callback out)
        (display "Data from the callback!\n" out))
      (fake-test `(put ,(xh "http://www.google.com/")
                       ,callback)))
    
    ; some real tests, connecting to actual servers
    (http-request 
       `(get ,(xh "http://www.google.com/")))
    (http-request 
     `(head ,(xh "http://www.google.com/")))
    
    
    (let ()
      (define s "first=bob&last=jones")
      (call-with-input-string
       s
       (lambda (in)
         (receive-entity in "application/x-www-form-urlencoded" (string-length s)))))
    
    (call-with-input-string
     "GET / HTTP/1.0\r\n"
     (lambda (in)
       (receive-request in)))
    
    )
  
  
  (test)

  )
