(module uri-string scheme
  (require "define-contract-provide.rkt")

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;
  ;; Misc url helpers
  ;;
  ;; This could probably be replaced by Racket's built-in url functions.
  ;; I've gotten bit by encoding problems with those but maybe it was pilot error.

  ;; URL parsing regexp is copied from net/url
  ;; this is following the regexp in Appendix B of rfc 3986, except for using
  ;; `*' instead of `+' for the scheme part (it is checked later anyway, and
  ;; we don't want to parse it as a path element), and the user@host:port is
  ;; parsed here.
  (define uri-rx
    (regexp (string-append
             "^"
             "(?:"              ; / scheme-colon-opt
             "([^:/?#]*)"       ; | #1 = scheme-opt
             ":)?"              ; \
             "(?://"            ; / slash-slash-authority-opt
             "(?:"              ; | / user-at-opt
             "([^/?#@]*)"       ; | | #2 = user-opt
             "@)?"              ; | \
             "([^/?#:]*)?"      ; | #3 = host-opt
             "(?::"             ; | / colon-port-opt
             "([0-9]*)"         ; | | #4 = port-opt
             ")?"               ; | \
             ")?"               ; \
             "([^?#]*)"         ; #5 = path
             "(?:\\?"           ; / question-query-opt
             "([^#]*)"          ; | #6 = query-opt
             ")?"               ; \
             "(?:#"             ; / hash-fragment-opt
             "(.*)"             ; | #7 = fragment-opt
             ")?"               ; \
             "$")))

  (define/contract/provide (split-uri uri-string)
    (string? . -> . (values (or/c #f string?) (or/c #f string?) (or/c #f string?) (or/c #f string?) (or/c #f string?) (or/c #f string?)))
    (match (regexp-match uri-rx uri-string)
      [(list all scheme user host port path query fragment)
       (values scheme host port path query fragment)]
      [_ (error "bad uri")]))
    
  (define/contract/provide (combine-uri #:scheme [scheme #f] #:host [host #f] #:port [port #f] #:path [path #f] #:query [query #f] #:fragment [fragment #f])
    (() (#:scheme (or/c #f string?) #:host (or/c #f string?) #:port (or/c #f string?) #:path (or/c #f string?) #:query (or/c #f string?) #:fragment (or/c #f string?)) . ->* . string?)
    (string-append (if scheme (string-append scheme ":") "")
                   (if host (string-append "//" host) "")
                   (if port (string-append ":" port) "")
                   (if (and path (not (string=? path ""))) path "/")
                   (if query (string-append "?" query) "")
                   (if fragment (string-append "#" fragment) "")))

  )
