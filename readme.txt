Idea #1: The Racket library is inconsitent for client operations like get-pure-port
vs. web server. Examples:
- The former uses string?
  The latter uses byte?
- The former has headers as one string? with exact-field and extract-all-fields.
  The latter has headers in alists.
- The former handles transmitted data by passing it as bytes?
  The latter's make-response/incremental gives you a data-writer proc (where an
  output-port? would be more natural and convenient.)
  
Basically, much of what you learn on one side, is different (or even precisely
opposite) for the other. Many errors simply forgetting to use  #"OK" vs. "OK".

So, let's come up with a consistent representation for requests and responses, and
used both by servers and clients.

Idea #2: Isomorphism between xexpr for the entity and the entire request or response.

xexprs are commonly used for the entity (HTTP "body"):

`(tag ([attr val] ...)
      (body ...))

`(html (head ())
       (body ()))

Why not wrap that in an x-expression representing the entire HTTP request or response
message. Treat the HTTP headers as the "attributes" and the HTTP entity is the "body".

Examples. All of these return #t for xexpr?

`(get ([method "GET"]
       [header "value"])))

`(post ([method "POST"]
        [Content-Type "application/x-www-form-urlencoded"])
       (data ([first "bob"]
              [last "jones"]))))

`(response ([Status "200" "OK"]
            [Date "Mon Jan 1 1900 adsadsf"]
            [Content-Length "43]
            [Content-Type "text/xml"])
           (html ()
                 (head ())
                 (body ()))))

Some other useful examples aren't strictly xexpr?, we bend the rules a bit.
