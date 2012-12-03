#lang racket

(require net/head
         "util.rkt")

(provide extract-http-ver&code&text
         extract-http-ver
         extract-http-code
         extract-http-text
         extract-field/number
         maybe-insert-field
         coalesce-fields
         validate-tx-or-rx-header
         heads-string->dict
         heads-dict->string
         maybe-dict-set
         maybe-dict-set*
         )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Supplement "missing" functions in net/head
;; Headers represented as string.
(define (extract-http-ver&code&text h)
  (match h
    ;; Remember that status description text is optional. For instance
    ;; "HTTP/1.1 200" is valid (the "OK" part is optional).
    [(pregexp "^HTTP/(1\\.[01])\\s+(\\d{3})\\s*(.*?)\\s*\r\n"
              (list _ ver code text))
     (values ver (string->number code) text)]
    [else
     (log-http-warning (string-append "bad response header: " h))
     (values "???" 999 "Bad Response")]))

(define (extract-http-ver h)
  (define-values (ver code text) (extract-http-ver&code&text h))
  ver)

(define (extract-http-code h)
  (define-values (ver code text) (extract-http-ver&code&text h))
  code)

(define (extract-http-text h)
  (define-values (ver code text) (extract-http-ver&code&text h))
  text)

(define (extract-field/number k h [radix 10])
  (define (trim s)
    (match s
      [(pregexp "^\\s*?(\\S*)\\s*?$" (list _ s)) s]
      [else s]))
  (match (extract-field k h)
    [#f #f]
    [(var x)
     (match (string->number (trim x) radix)
       [#f #f]
       [(var n) n])]))

(define/contract (maybe-insert-field k v h)
  ((or/c string? symbol?) any/c string? . -> . string?)
  ;; Insert the field into the header only if it does not already
  ;; exist in the header.
  (let ([k (if (symbol? k) (symbol->string k) k)])
    (if (extract-field k h)
        h
        (insert-field k (format "~a" v) h))))

(define/contract (coalesce-fields heads)
  (string? . -> . string?)
  ;; Combine header fields with the same name into one
  ;; "header-name:comma-separated-value-list" pair as prescribed by
  ;; RFC 2616, section 4.2, without any white-space between
  ;; values. For example, the two metadata headers
  ;; 'x-amz-meta-username: fred' and 'x-amz-meta-username: barney'
  ;; would be combined into the single header 'x-amz-meta-username:
  ;; fred,barney'.
  ;;
  ;; Implementation is trivial we have heads-string->dict
  ;; which already handles dupes. Simply give it our desired separate
  ;; of "," instead of its default "\n".
  (heads-dict->string (heads-string->dict heads ",")))

(define (validate-tx-or-rx-header s)
  ;; (string? . -> . string?)
  ;; Unlike net/head validate-header, this permits response header
  ;; with intial HTTP line. Also, if the header is OK and no exception
  ;; is thrown, we return the string passed in, making it more
  ;; convenient to call this in an expression.
  (match s
    [(pregexp "^HTTP/1\\.[01].+?\r\n(.+?)$"
              (list _ heads))
     (validate-header heads)]
    [else
     (validate-header s)])
  s)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; headers as a `dict'

;; Convert a header string of the form specified in net/head to a
;; dict.
;;
;; Specifically, handle the case of duplicate headers. A real-world
;; example of such duplicate headers is Set-Cookie.  Other than
;; association lists, most types of dicts don't permit duplicate keys,
;; so we can't store duplicate headers like that. Instead, duplicate
;; headers are stored in the dict under the same key, with the various
;; values separated by \n.  Precedent: How Rack handles this.
(define/contract (heads-string->dict s [dupe-sep "\n"])
  ((string?) (string?) . ->* . dict?)
  (for/fold ([h (hash)])
            ([x (in-list (extract-all-fields s))])
    (match-define (cons k v) x)
    (let ([k (string->symbol k)])
      (if (hash-has-key? h k)
          (hash-set h k (string-append (hash-ref h k) dupe-sep v))
          (hash-set h k v)))))

;; Convert a dict into a string of the form specified in net/head,
;; including the trailing \r\n to end all the headers.
;;
;; Does the complement of the duplicate header handling described
;; above.
(define/contract (heads-dict->string h [dupe-sep "\n"])
  ((dict?) (string?) . ->* . string?)
  (string-append
   (for*/fold ([s ""])
              ([(k v) (in-dict h)]
               [v (in-list (regexp-split dupe-sep (format "~a" v)))])
              (string-append s (format "~a: ~a\r\n" k v)))
   "\r\n"))

;; Like dict-set*, but will set the new value for a key only if the
;; key does not already exist in d.
(define/contract (maybe-dict-set* d . kvs)
  (((and/c dict? dict-can-functional-set?))
   #:rest list?
   . ->* .  (and/c dict? dict-can-functional-set?))
    (let loop ([d d]
               [kvs kvs])
      (if (null? kvs)
          d
          (loop (maybe-dict-set d (car kvs) (cadr kvs))
                (cddr kvs)))))

;; Like dict-set, but will set the new value for a key only if the
;; key does not already exist in d.
(define (maybe-dict-set d k v)
  (if (dict-has-key? d k)
      d
      (dict-set d k v)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module+ test
  (require rackunit)
  (define tx/string (string-append "Date: adsfasd;lkfj\r\n"
                                   "Host: www.fake.com\r\n"
                                   "Content-Length: 999\r\n"
                                   "Base-16: FF\r\n"
                                   "Expect: 100-continue\r\n"
                                   "\r\n"))
  (define rx/string (string-append "HTTP/1.1 200 OK\r\n" tx/string))

  (test-case
   "extract-field/number"
   (check-equal? (extract-field/number "Content-Length" rx/string 10)
                 999)
   (check-equal? (extract-field/number "Base-16" rx/string 16)
                 255)
   (check-equal? (extract-field/number "Not-There" rx/string 10)
                 #f)
   (check-equal? (extract-field/number "Content-Type" rx/string 10)
                 #f))

  (test-case
   "coalesce-fields"
   (check-equal? (coalesce-fields "X: 1\r\nX: 2\r\n\r\n")
                 "X: 1,2\r\n\r\n"))

  (test-case
   "heads string <-> dict"
   (define s "Set-Cookie: A\r\nSet-Cookie: B\r\n\r\n")
   (check-equal? (heads-dict->string (heads-string->dict s))
                 s))

  (test-case
   "maybe-dict-set and maybe-dict-set*"
   (check-equal? (dict-ref (maybe-dict-set '([a . "10"]) 'a "10000") 'a)
                 "10")
   (check-equal? (dict-ref (maybe-dict-set '([a . "10"]) 'a "10000") 'a)
                 "10")))
