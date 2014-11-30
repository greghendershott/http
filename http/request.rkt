#lang racket

(require net/url
         net/uri-codec
         net/head
         xml
         (prefix-in h: html)
         openssl
         racket/tcp
         file/gunzip
         racket/date
         "head.rkt"
         "util.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; URL parsing regexp. See 3986. Like the regexp in net/url, but we
;; allow/ignore leading and trailing space.
(define uri-px
  (pregexp (string-append
            "^"
            "\\s*"
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
            "(.*?)"            ; | #7 = fragment-opt
            ")?"               ; \
            "\\s*"
            "$")))

(define/contract/provide (split-uri uri-string)
  (string? . -> . (values (or/c #f string?)
                          (or/c #f string?)
                          (or/c #f exact-positive-integer?)
                          (or/c #f string?)
                          (or/c #f string?)
                          (or/c #f string?)))
  (match uri-string
    [(pregexp uri-px (list _ scheme user host port path query fragment))
     (cond
      ;; This is a bit of a hack to accomdate URIs in the real world,
      ;; like what you might find as HREF values on a web page, and
      ;; what most users would type in a browser address bar. e.g.
      ;; "www.fake.com/blah/blah".  If no scheme found, prepend
      ;; "http://" and try again.
      [(not scheme)
       (split-uri (string-append "http://" uri-string))]
      [else
       (values
        scheme host (and port (string->number port)) path query fragment)])]
    [else (error 'split-uri "can't parse" uri-string)]))

(define/contract/provide (combine-uri [scheme #f]
                                      [host #f]
                                      [port #f]
                                      [path #f]
                                      [query #f]
                                      [fragment #f])
  (() ((or/c #f string?)
       (or/c #f string?)
       (or/c #f exact-positive-integer?)
       (or/c #f string?)
       (or/c #f string?)
       (or/c #f string?))
   . ->* . string?)
  (string-append (if scheme (string-append scheme ":") "")
                 (if host (string-append "//" host) "")
                 (if port (string-append ":" (number->string port)) "")
                 (if (and path (not (string=? path ""))) path "/")
                 (if query (string-append "?" query) "")
                 (if fragment (string-append "#" fragment) "")))

(define/contract/provide (uri->scheme&host&port uri)
  (string?
   . -> . (values (or/c "http" "https") string? exact-positive-integer?))
  (let*-values
      ([(scheme host port path query frag) (split-uri uri)]
       [(scheme) (or scheme "http")]
       [(host) (or host "")]
       [(port) (or port (if (string=? scheme "https") 443 80))])
    (values scheme host port)))

(define/contract/provide (uri&headers->path&header uri heads)
  (string? dict? . -> . (values string? string?))
  (let*-values
      ([(scheme host port path query frag) (split-uri uri)]
       [(scheme) (or scheme "")]
       [(host) (or host "")]
       [(port) (or port (if (string=? scheme "https") 443 80))]
       [(host-val) (string-append host (if (= port 80)
                                           "" ;servers may choke on :80
                                           (format ":~a" port)))]
       [(query) (if query (string-append "?" query) "")]
       [(frag) (if frag (string-append "#" frag) "")]
       [(path) (match path
                 [(or #f "") "/"]
                 [else path])]
       [(p+q+f) (string-append path query frag)]
       ;; Automatically add some headers unless supplied by caller
       [(header)
        (maybe-insert-field
         'Host
         host-val
         (maybe-insert-field 'Date
                             (seconds->gmt-string)
                             (heads-dict->string heads)))])
    (values p+q+f header)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; "Raw" connections.

(define/contract/provide (connect* scheme host port)
  ((or/c "http" "https") string? exact-positive-integer?
   . -> . (values input-port? output-port?))
  (log-http-debug (format "connect* ~a ~a ~a" scheme host port))
  (match scheme
    ["https" (ssl-connect host port)]
    ["http" (tcp-connect host port)]))

(define/contract/provide (connect*-uri uri)
  (string?  . -> . (values input-port? output-port?))
  (define-values (scheme host port) (uri->scheme&host&port uri))
  (connect* scheme host port))

;; Can be called from reuse-manager thread:
(define/contract/provide (disconnect* in out)
  (input-port? output-port? . -> . any)
  (log-http-debug (format "disconnect* ~a ~a" in out))
  (close-output-port out)
  (close-input-port in))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Thread-safety wrapper: used to wrap a set of functions to make them
;; mutually atomic. The function bodies run in a manager thread, so
;; they should not rely on thread-local or continuation-local state.

(define-syntax-rule (begin-managed
                     (define (proc arg ...) body ...)
                     ...)
  (begin
    (define (proc arg ...)
      (call-as-managed (lambda () body ...)))
    ...))

(define manager-th
  (thread
   (lambda ()
     (let loop () ((thread-receive)) (loop)))))

(define (call-as-managed thunk)
  ;; For kill safety, make sure manager-th has ability to run
  ;; before sending the message:
  (thread-resume manager-th (current-thread))
  (define sema (make-semaphore))
  (define result #f)
  (thread-send manager-th
               (lambda ()
                 (set! result (call-with-values thunk vector))
                 (semaphore-post sema)))
  (semaphore-wait sema)
  (vector->values result))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Reusable ("pooled") connections.
;;
;; `disconnect` moves connections that are in `known` hash into a
;; `avail` hash for possible later reuse.
;;
;; `connect` checks the `avail` table, otherwise uses `connect*`.

(define current-pool-timeout (make-parameter 0))
(provide (contract-out [current-pool-timeout parameter/c]))

;; Keep track of potentially reuable connections in `known`, and
;; available-for-reuse connections in `avail`. Use layers of hash
;; tables and weak references to avoid leaks:
(define known (make-weak-hash)) ; in => (list cust out scheme host port)
(define avail (make-weak-hash)) ; cust => (list scheme host port)
;;                                     => in => thread

(begin-managed
 ;; remember! : custodian input-port output-port scheme host port -> void
 ;;  Records the existence of a potentially reusable connection.
 (define (remember! custodian in out scheme host port)
   (hash-set! known in (list custodian out scheme host port)))

 ;; forget! : input-port boolean -> void
 ;;  Forgets a potentially reusable connection, optionally closing it
 ;;  in case it's not closed already.
 (define (forget! in close?)
   (define v (hash-ref known in #f))
   (when v
     (define custodian (car v))
     (define out (cadr v))
     (define s+h+p (cddr v))
     (define a-ht (hash-ref avail custodian #f))
     (when a-ht
       (define ht (hash-ref a-ht s+h+p #f))
       (when ht
         (define th (hash-ref ht in #f))
         (when th
           (kill-thread th)
           (hash-remove! ht in))))
     (hash-remove! known in)
     (when close?
       (log-http-debug (apply format
                              "~a ~a ~a timeout close & rm from pool"
                              s+h+p))
       (disconnect* in out))))

 ;; available! : input-port output-port number -> void
 ;;  Marks an input port as available, assuming that the
 ;;  given output port matches what we remembered.
 (define (available! in out timeout)
   (define v (hash-ref known in #f))
   (cond
    [(and v (eq? (cadr v) out))
     (define custodian (car v))
     (define s+h+p (cddr v))
     (define a-ht (or (hash-ref avail custodian #f)
                      (let ([ht (make-hash)])
                        (hash-set! avail custodian ht)
                        ht)))
     (define ht (or (hash-ref a-ht s+h+p #f)
                    (let ([ht (make-weak-hash)])
                      (hash-set! a-ht s+h+p ht)
                      ht)))
     (log-http-debug (apply format "~a ~a ~a to pool" s+h+p))
     (define th (thread (lambda ()
                          (sleep timeout)
                          (forget! in #t))))
     ;; Give timeout the manager thread's ability to run:
     (thread-resume th (current-thread))
     (hash-set! ht in th)]
    [else
     ;; Unknown or doesn't match
     (log-http-debug "disconnect: not in `known` hash; calling disconnect*")
     (when v (hash-remove! known in))
     ;; Make sure ports are closed:
     (disconnect* in out)]))

 ;; acquire! : custodian scheme host port -> (values in-or-#f out-or-#f)
 ;;  Finds a reusable connection that is available.
 (define (acquire! custodian scheme host port)
   (define a-ht (hash-ref avail custodian #f))
   (cond
    [a-ht
     (define ht (hash-ref a-ht (list scheme host port) #f))
     (define pos (and ht (hash-iterate-first ht)))
     (cond
      [pos
       (define in (hash-iterate-key ht pos))
       (define th (hash-ref ht in #f))
       (hash-remove! ht in)
       (when th (kill-thread th))
       (define v (hash-ref known in #f))
       (cond
        [v (values in (cadr v))]
        [else
         (log-http-error "missing information for supposedly available connection")
         (values #f #f)])]
      [else (values #f #f)])]
    [else (values #f #f)])))

;; Tests for the table-management functions:
(module+ test
  (require rackunit)

  (test-case
   "managed tables"

   (define c1 (current-custodian))
   (define c2 (make-custodian))

   (define (check-no-acquire c
                             #:scheme [scheme "http"]
                             #:host [host "server1"]
                             #:port [port 80])
     (check-equal? (let-values ([(i o) (acquire! c scheme host port)])
                     (vector i o))
                   (vector #f #f)))
   (check-no-acquire c1)

   ;; Ok to try to forget non-remembered:
   (define-values (i1 o1) (make-pipe))
   (forget! i1 #f)
   (check-equal? (port-closed? i1) #f)
   (check-equal? (port-closed? o1) #f)

   ;; Remember and available => acquire (when right context)
   (define-values (i2 o2) (make-pipe))
   (remember! c1 i2 o2 "http" "server1" 80)
   (check-no-acquire c1)
   (available! i2 o2 1000)
   (check-no-acquire c2)
   (check-no-acquire c1 #:scheme "https")
   (check-no-acquire c1 #:host "server2")
   (check-no-acquire c1 #:port 90)
   (check-equal? (let-values ([(i o) (acquire! c1 "http" "server1" 80)])
                   (vector i o))
                 (vector i2 o2))
   (check-no-acquire c1)

   ;; Timeout => no acquire
   (available! i2 o2 0.1)
   (sleep 0.1)
   (sync (system-idle-evt))
   (check-no-acquire c1)
   (check-equal? (port-closed? i2) #t)
   (check-equal? (port-closed? o2) #t)

   ;; Forget => no acquire
   (define-values (i3 o3) (make-pipe))
   (remember! c1 i3 o3 "http" "server1" 80)
   (forget! i3 #f)
   (available! i3 o3 1000)
   (check-no-acquire c1)

   ;; Two available => get them in some order
   (define-values (i4 o4) (make-pipe))
   (define-values (i5 o5) (make-pipe))
   (remember! c2 i4 o4 "http" "server1" 80)
   (remember! c2 i5 o5 "http" "server1" 80)
   (available! i4 o4 1000)
   (available! i5 o5 1000)
   (check-equal? (let-values ([(ia oa) (acquire! c2 "http" "server1" 80)]
                              [(ib ob) (acquire! c2 "http" "server1" 80)])
                   (if (eq? ia i4)
                       (vector ia oa ib ob)
                       (vector ib ob ia oa)))
                 (vector i4 o4 i5 o5))
   (check-no-acquire c2)

   ;; Input and output port mismatch => not made available:
   (define-values (i6 o6) (make-pipe))
   (remember! c2 i6 o6 "http" "server1" 80)
   (available! i6 o4 1000)
   (check-no-acquire c2)
   (check-equal? (port-closed? i6) #t)
   (check-equal? (port-closed? o6) #f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define/contract/provide (connect scheme host port)
  ((or/c "http" "https") string? exact-positive-integer?
   . -> . (values input-port? output-port?))
  (log-http-debug (format "connect ~a ~a ~a" scheme host port))
  ;; If open connection on `free` list, pull it off and use it.
  ;; Else call connect*
  (define-values (in out) (acquire! (current-custodian) scheme host port))
  (cond
   [(not in)
    (define-values (in out) (connect* scheme host port))
    ;; When pool timeout is positive -- i.e. pooling is desired --
    ;; remember the connection. Else don't, which ensures disconnect
    ;; will simply do a raw disconnect*.
    (when (positive? (current-pool-timeout))
      (remember! (current-custodian) in out scheme host port))
    (values in out)]
   [(or (port-closed? in)
        (port-closed? out))
    (log-http-warning (format "pooled port(s) closed ~a ~a ~a"
                              scheme host port))
    (disconnect* in out) ;ensure both closed
    (connect scheme host port)] ;try again
   [else
    (log-http-debug (format "use pool connection for ~a ~a ~a"
                            scheme host port))
    (values in out)]))

(define/contract/provide (connect-uri uri)
  (string?  . -> . (values input-port? output-port?))
  (define-values (scheme host port) (uri->scheme&host&port uri))
  (connect scheme host port))

;; This says that when `disconnect` is called for this connection, it
;; should NOT be left open and pooled for possible reuse. This should
;; be called when close-connection? would return #t, or when there is
;; some exn:fail:network? for either of the ports.
(define/contract/provide (unpool in out)
  (input-port? output-port? . -> . any)
  (log-http-debug (format "unpool ~a ~a" in out))
  (forget! in #f))

(define/contract/provide (disconnect in out)
  (input-port? output-port? . -> . any)
  (log-http-debug (format "disconnect ~a ~a" in out))
  (available! in out (current-pool-timeout)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; When method is "put" or "post", this returns a boolean?. If #t, you
;; should go ahead and transmit the entity (the put or post data),
;; after which you should call purify-port to read the response
;; headers. If #f, you should not transmit the entity data and instead
;; go ahead and call purify-port to read the response headers. (The
;; reason for this is to support Expect 100 Continue.)
;;
;; For other methods, this will always return #t and you should always
;; just go ahead and call purify-port to read the response headers.
(define/contract/provide (start-request in out ver method path heads)
  (input-port? output-port? (or/c "1.0" "1.1") string? string? string?
   . -> . boolean?)
  (log-http-debug (format "-> ~a ~a HTTP/~a" (string-upcase method) path ver))
  (when (log-level? http-logger 'debug)
    (for ([(k v) (in-dict (heads-string->dict heads))])
        (log-http-debug (format "-> ~a: ~a" k v))))

  (display (format "~a ~a HTTP/~a\r\n" (string-upcase method) path ver) out)
  (display (format "~a" heads) out)
  (100-continue? heads in out))

(define/contract (100-continue? heads in out)
  (string? input-port? output-port? . -> . boolean?)
  ;; If "Expect: 100-continue" is in the req headers, we should read
  ;; from the server here. If it gives "HTTP/1.1 100
  ;; Continue\r\n\r\n" then give it the data, else if response
  ;; header (e.g. redirect) do not actually put any data and instead
  ;; just return the headers as the response.
  (flush-output out)
  (match (extract-field "Expect" heads)
    ["100-continue"
     (log-http-debug "Request header 'Expect: 100-continue'. Waiting/peeking...")
     (unless (sync/timeout 30.0 in)
       (raise (exn:fail:network "Timeout waiting for 100 continue"
                                (current-continuation-marks))))
     (match (regexp-match-peek-positions
             #rx"^HTTP/1\\.1 100 Continue(?:\r\n\r\n|\n\n|\r\r)"
             in)
       [(list (cons _ end))
        (read-string end in)
        (log-http-debug "Got 100 continue.")
        #t]
       [_
        (log-http-debug "Did not get 100 continue.")
        ;; Note: Due to using regexp-match-peek-positions, the
        ;; response will remain to read later.
        #f])]
    [_ #t]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Examines the Content-Encoding header if any in `h`, and returns a
;; procedure (input-port? output-port? -> void?) which will copy the
;; bytes and decode them. If there no content encoding specified, or
;; an unsupported encoding, this simply returns `copy-port`.
(define/contract/provide (entity-content-decoder/ports h)
  ((or/c string? bytes?) . -> . (input-port? output-port? . -> . any))
  (define f
    (match (extract-field "Content-Encoding" h)
      [(or "" #f) copy-port]
      ["gzip" gunzip-through-ports]
      ["deflate" inflate]
      [ce (log-http-warning (format "can't handle Content-Encoding \"~a\"" ce))
          copy-port]))
  ;; Ensure this works with Transfer-Encoding: chunked. A function like
  ;; `gunzip-through-ports` knows that its done before it pulls an EOF
  ;; from the input stream. Meanwhile, chunk decoding needs to read
  ;; more bytes to decide that it is done. So, the gunzip completes,
  ;; and the calling context continues on, even though a thread is
  ;; still trying to read from the input stream for un-chunking.
  (lambda (in out)
    (f in out)
    (unless (eof-object? (read-byte in))
      (error "expected EOF after decoded stream"))))

;; If you already have read the entity and have it in `bytes?`, you
;; may use this to decode it based on the Content-Encoding header if
;; any in `h`.
(define/contract/provide (entity-content-decode-bytes b h)
  (bytes? (or/c string? bytes?) . -> . bytes?)
  (define/contract (decode func in-bytes)
    ((input-port? output-port? . -> . void?) bytes? . -> . bytes?)
    (let ([in (open-input-bytes in-bytes)]
          [out (open-output-bytes)])
      (func in out)
      (begin0
          (get-output-bytes out)
        (close-input-port in)
        (close-output-port out))))
  (define func (entity-content-decoder/ports h))
  (if (equal? copy-port func)
      b ; avoid pointless copy
      (decode func b)))

;; This is the core procedure to read an HTTP entity in accordance
;; with the `Transfer-Encooding` header. It returns an `input-port?`
;; from which you can read the entity until the port returns `eof`. By
;; giving you the port and letting you do the reads, you can read a
;; very large entity in small pieces (to provide a progress indicator,
;; or simply to avoid consuming memory on one large `bytes?` object).
(define/contract/provide (read-entity/transfer-decoding-port in h)
  (input-port? (or/c string? bytes?) . -> . input-port?)
  (define te (or (extract-field "Transfer-Encoding" h) ""))
  (define cl (extract-field/number "Content-Length" h 10))
  (cond
   ;; If Transfer-Encoding is "chunked", read chunked (duh).
   [(string-ci=? te "chunked") (make-codec-input-port in un-chunk 'chunked)]
   ;; If Content-Length supplied, limit to exactly that amount.
   [cl (make-limited-input-port in cl)]
   ;; If connection will be closed, whatever.
   [(close-connection? h) in]
   ;; Else nothing good we can do. With HTTP 1.1 persistent connection
   ;; there is no meaningful "eof".
   [else (log-http-warning "can't read until eof using HTTP 1.1")
         (make-limited-input-port in 0)]))

;; `codec` is (input-port? output-port? -> any). For example can be
;; deflate, inflate, gzip-through-ports, gunzip-through-ports.
(define codec-buffer-size (make-parameter 1024))
(define (make-codec-input-port in codec [name #f])
  (define-values (pin pout) (make-pipe (codec-buffer-size) name name))
  (thread (lambda ()
            (codec in pout)
            (close-output-port pout)))
  pin)

(define (un-chunk in out)
  (let loop ()
    (define s (read-line in 'any))
    (define chunk-size (string->number (string-trim s) 16))
    (cond [(not chunk-size) (error 'un-chunk "bad chunk size: ~s" s)]
          [else (write-bytes (read-bytes chunk-size in) out)
                (read-bytes-line in 'any) ;read trailing \r\n
                (unless (zero? chunk-size)
                  (loop))])))

(define/contract/provide (read-entity/port in h out)
  (input-port? (or/c string? bytes?) output-port? . -> . any)
  (let ([in (read-entity/transfer-decoding-port in h)])
    ((entity-content-decoder/ports h) in out)))

;; When you don't care about reading the entire entity into memory, or
;; don't care about providing a progress indicator, you can simply use
;; read-entity/bytes which returns the decoded entity as `bytes?`.
(define/contract/provide (read-entity/bytes in h)
  (input-port? (or/c string? bytes?) . -> . bytes?)
  (define out (open-output-bytes))
  (read-entity/port in h out)
  (get-output-bytes out))

;; Rather than return raw bytes?, this uses Content-Type header if any
;; to pick a more suitable representation as an xexpr?.
(define/contract/provide (read-entity/xexpr in h)
  (input-port? (or/c string? bytes?) . -> . xexpr?)

  (define (html-bytes->xexpr by)
    (define (get-element-from-port in)
      (define x (h:read-html-as-xml in))
      (make-element #f #f '***entity*** '() x))
    (third (xml->xexpr (call-with-input-bytes by get-element-from-port))))

  (define by (read-entity/bytes in h))
  (define ct (or (extract-field "Content-Type" h) ""))
  ;; TO-DO: Handle charset= part of Content-Type correctly, here, or
  ;; up to caller to handle?
  (cond
   [(or (regexp-match? #rx"^text/xml" ct)
        (regexp-match? #rx"^application/xml" ct)
        (and (string=? "" ct)           ;no Content-Type specified
             (regexp-match? #px"^\\s*<\\?xml" by))) ;starts like XML
    (xml->xexpr (document-element (read-xml (open-input-bytes by))))]
   [(regexp-match? #rx"^text/html" ct)
    (html-bytes->xexpr by)]
   [(regexp-match? #rx"^text/plain" ct)
    (format "~a" by)]
   [(regexp-match? #rx"^application/x-www-form-urlencoded" ct)
    `(data ,(form-urlencoded->alist (bytes->string/utf-8 by)))]
   [else
    `(entity () ,(format "~a" by))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (string-trim s)
  (match s
    [(pregexp "^\\s*?(\\S*)\\s*?$" (list _ s)) s]
    [else s]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define MONTHS
  #("Jan" "Feb" "Mar" "Apr" "May" "Jun"
    "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))
(define DAYS
  #("Sun" "Mon" "Tue" "Wed" "Thu" "Fri" "Sat"))

;; two-digits : num -> str
(define (two-digits n)
  (let ([str (number->string n)])
    (if (< n 10) (string-append "0" str) str)))

(define/provide (seconds->gmt-string [s (current-seconds)])
  (define date (seconds->date s #f))
  (format "~a, ~a ~a ~a ~a:~a:~a GMT"
          (vector-ref DAYS (date-week-day date))
          (two-digits (date-day date))
          (vector-ref MONTHS (sub1 (date-month date)))
          (date-year date)
          (two-digits (date-hour date))
          (two-digits (date-minute date))
          (two-digits (date-second date))))

(define/contract/provide (gmt-string->seconds s)
  (string? . -> . exact-integer?)
  (match s
    [(pregexp
      (string-append
       "^"
       "([A-Z]{1,1}[a-z]{2,2}), "          ;weekday, e.g. "Mon"
       "(\\d{1,2}) "                       ;day, e.g. "01"
       "([A-Z]{1,1}[a-z]{2,2}) "           ;month, e.g. "Jan"
       "(\\d{4,4}) "                       ;year, e.g. "2012"
       "(\\d{2,2}):(\\d{2,2}):(\\d{2,2}) " ;hr:mn:sec
       "(.{3,4}?)"                         ;timezone, e.g. GMT or 0:00
       "$"
       )
      (list _ week-day day month year hr mn sc tz))
     (find-seconds (string->number sc)
                   (string->number mn)
                   (string->number hr)
                   (string->number day)
                   (for/or ([i (in-range (vector-length MONTHS))])
                       (if (string=? (vector-ref MONTHS i) month)
                           (add1 i)
                           #f))
                   (string->number year)
                   #f)]
    [else
     (error 'gmt-string->seconds "expcted date string, got ~a" s)]))

;; seconds->gmt-8601-string
;;
;; YYYY-MM-DDThh:mm:ss.000Z, as specified in the ISO 8601 standard.
(define/contract/provide (seconds->gmt-8601-string [style 'T/Z]
                                                   [s (current-seconds)])
  (() ((or/c 'basic 'plain 'T/Z 'T/.000Z) exact-integer?) . ->* . string?)
  (define date (seconds->date s #f))
  (match style
    ['basic
       (format "~a~a~aT~a~a~aZ"
          (date-year date)
          (two-digits (date-month date))
          (two-digits (date-day date))
          (two-digits (date-hour date))
          (two-digits (date-minute date))
          (two-digits (date-second date)))]
    [else
     (format "~a-~a-~a~a~a:~a:~a~a"
             (date-year date)
             (two-digits (date-month date))
             (two-digits (date-day date))
             (match style
               [(or 'T/Z 'T/.000Z) "T"]
               ['plain " "])
             (two-digits (date-hour date))
             (two-digits (date-minute date))
             (two-digits (date-second date))
             (match style
               ['T/Z "Z"]
               ['T/.000Z ".000Z"]
               ['plain ""]))]))

(define/contract/provide (gmt-8601-string->seconds s)
  (string? . -> . exact-integer?)
  (match s
    [(pregexp
      (string-append
       "^"
       "(\\d{4})-(\\d{2})-(\\d{2})"     ;YYYY-MM-DD
       "(T| ){1}"                       ;separator
       "(\\d{2}):(\\d{2}):(\\d{2})"     ;hr:mn:sc
       "(\\.\\d{3})*"                   ;subseconds
       "(Z)*"                           ;timezone
       "$")
       (list _ year month day T hr mn sc _ _))
     (find-seconds (string->number sc)
                   (string->number mn)
                   (string->number hr)
                   (string->number day)
                   (string->number month)
                   (string->number year)
                   #f)]
    [_ (error 'gmt-8601-string->seconds
              "expected RFC 8601 date string, got ~a" s)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define/contract/provide (purify-port/log-debug in)
  (input-port? . -> . string?)
  (define h (purify-port in))
  (when (log-level? http-logger 'debug)
    (log-http-debug (format "<- HTTP/~a ~a ~a"
                            (extract-http-ver h)
                            (extract-http-code h)
                            (extract-http-text h)))
    (for ([(k v) (in-dict (heads-string->dict h))])
        (log-http-debug (format "<- ~a: ~a" k v))))
  h)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define/contract/provide (redirect-uri h)
  (string? . -> . (or/c #f string?))
  (case (extract-http-code h)
    [(301 302 303 305 307) (extract-field "Location" h)]
    [else #f]))

;; Do the two URLs respresent the same connection, i.e. the exact same
;; scheme, host, and port?
(define/contract/provide (same-connection? old new)
  (url? url? . -> . boolean?)
  (and (equal? (url-scheme old) (url-scheme new))
       (equal? (url-host old) (url-host new))
       (equal? (url-port old) (url-port new))))

;; Based on response headers, should the connection be closed?
(define/contract/provide (close-connection? h)
  (string? . -> . boolean?)
  (or (equal? "1.0" (extract-http-ver h))
      (let ([s (extract-field "Connection" h)])
        (and s (string-ci=? s "close")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; call/requests provides a way to make one or more requests to the
;; same host connection, and ensure the ports are closed even if there
;; is an exception. `proc' is responsible for doing everything else,
;; such as calling `start-request', `purify-port/log-debug',
;; `read-entity/bytes', and so on.
(define/contract/provide (call/requests scheme host port proc)
  ((or/c "http" "https")
   string?
   exact-positive-integer?
   (input-port? output-port? . -> . any/c)
   . -> . any/c)
  (define-values (in out) (connect scheme host port))
  (dynamic-wind (lambda () (void))
                (lambda () (proc in out))
                (lambda () (disconnect in out))))

;; Knows how to handle redirects.
;; Expects heads to already contain a Content-Length header.
(define/contract (request/redirect/uri ver method uri data heads
                                       proc redirects)
  ((or/c "1.0" "1.1")
   string?
   string?
   (or/c #f bytes? (output-port? . -> . void?))
   dict?
   (input-port? string? . -> . any/c)
   exact-nonnegative-integer?
   . -> . any/c)
  (define-values (scheme host port) (uri->scheme&host&port uri))
  (define-values (in out) (connect scheme host port))
  (define-values (path rh) (uri&headers->path&header uri heads))
  (define tx-data? (start-request in out ver method path rh))
  (when (and tx-data? data)
    (cond [(bytes? data) (display data out)]
          [(procedure? data) (data out)])
    (flush-output out))
  (define h (purify-port/log-debug in))
  (when (close-connection? h)
    (unpool in out))
  (define location (redirect-uri h))
  (cond [(and location (> redirects 0))
         (unless (string-ci=? method "HEAD")
           (read-entity/bytes in h))
         (disconnect in out)
         (define old-url (string->url uri))
         (define new-url (combine-url/relative old-url location))
         (log-http-debug
          (format (tr "<>" redirects location (url->string new-url))))
         (request/redirect/uri ver method (url->string new-url)
                               data heads proc (sub1 redirects))]
        [else (begin0 (proc in h)
                (disconnect in out))]))

;; call/input-request is a simpler version of `call/requests` for the
;; case where you want to make just one request and it is not a put or
;; post request (there is no data to send). Like `call/requests` it
;; gurantees the ports will be closed.
(define/provide (call/input-request ver method uri heads proc
                                    #:redirects [redirects 10])
  (request/redirect/uri ver method uri #f heads proc redirects))

;; call/output-request is a simpler version of `call/requests` for the
;; case where you want to make just one request and it is a put or
;; post request. The data may be passed as bytes? or as a procedure
;; that will write to an output-port?. If the latter, you must pass
;; `len' unless you have already supplied a Content-Length header
;; yourself.
(define/contract/provide (call/output-request ver method uri data len heads proc
                                              #:redirects [redirects 10])
  (((or/c "1.0" "1.1")
    string?
    string?
    (or/c bytes? (output-port? . -> . void?))
    (or/c #f exact-nonnegative-integer?)
    dict?
    (input-port? string? . -> . any/c))
   (#:redirects exact-nonnegative-integer?)
   . ->* . any/c)
  (request/redirect/uri ver method uri data (maybe-add-cl heads data len)
                        proc redirects))

(define (maybe-add-cl dict data len)
  (define cl (cond [len len]
                   [data (bytes-length data)]
                   [else #f]))
  (cond [cl (heads-string->dict
             (maybe-insert-field "Content-Length"
                                 cl
                                 (heads-dict->string dict)))]
        [else dict]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Equivalents for net/url
;; Why use these instead? They support:
;; 1. SSL (only added in Racket 5.?.?
;; 2. "Expect: Continue 100" header, for big put/post requests.
;; Note that these do NOT support connection pooling.

(define http-ver (make-parameter "1.0"))
(provide http-ver)

(define/contract/provide (xxx-impure-port method url heads data)
  (string? url? dict? (or/c #f bytes?) . -> . input-port?)
  (define uri (url->string url))
  (define-values (in out) (connect*-uri uri)) ;NOT resable connection
  (define new-heads
    (if data
        (heads-string->dict
         (maybe-insert-field "Content-Length"
                             (bytes-length data)
                             (heads-dict->string heads)))
        heads))
  (define-values (path rh) (uri&headers->path&header uri new-heads))
  (define tx-data? (start-request in out (http-ver) method path rh))
  (when (and data tx-data?)
    (display data out))
  (cond
   [(tcp-port? out) (tcp-abandon-port out)]
   [(ssl-port? out) (ssl-abandon-port out)]
   [(output-port? out) (close-output-port out)])
  in)

(define/contract/provide (head-impure-port* url [heads #hash()])
  ((url?) (dict?) . ->* . input-port?)
  (xxx-impure-port "head" url heads #f))

(define/contract/provide (get-impure-port* url [heads #hash()])
  ((url?) (dict?) . ->* . input-port?)
  (xxx-impure-port "get" url heads #f))

(define/contract/provide (delete-impure-port* url [heads #hash()])
  ((url?) (dict?) . ->* . input-port?)
  (xxx-impure-port "delete" url heads #f))

(define/contract/provide (put-impure-port* url data [heads #hash()])
  ((url? bytes?) (dict?) . ->* . input-port?)
  (xxx-impure-port "put" url heads data))

(define/contract/provide (post-impure-port* url data [heads #hash()])
  ((url? bytes?) (dict?) . ->* . input-port?)
  (xxx-impure-port "post" url heads data))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; test

(module+ test
  (require rackunit)

  ;; Non I/O tests
  (test-case
   "close-connection?"
   (check-true (close-connection? (string-append "HTTP/1.1 200 OK\r\n"
                                                 "Connection: close\r\n"
                                                 "\r\n")))
   (check-true (close-connection? "HTTP/1.0 200 OK\r\n\r\n"))
   (check-false (close-connection? "HTTP/1.1 200 OK\r\n\r\n")))

  (test-case
   "split-uri"
   (define-values (s h pt pth q f)
     (split-uri "http://www.fake.com:80/path/to/res?a=1&b=2#fragment"))
   (check-equal? s "http")
   (check-equal? h "www.fake.com")
   (check-equal? pt 80)
   (check-equal? pth "/path/to/res")
   (check-equal? q "a=1&b=2")
   (check-equal? f "fragment"))

  (test-case
   "combine-uri"
   (check-equal? (combine-uri "http"
                              "www.fake.com"
                              80
                              "/path/to/res"
                              "a=1&b=2"
                              "fragment")
                 "http://www.fake.com:80/path/to/res?a=1&b=2#fragment"))

  (test-case
   "redirect-uri"
   (check-equal? (redirect-uri "HTTP/1.0 200 OK\r\n\r\n")
                 #f)
   (check-equal? (redirect-uri (string-append "HTTP/1.0 301\r\n"
                                              "Location: foobar\r\n"
                                              "\r\n"))
                 "foobar"))
  
  (test-case
   "seconds->gmt-string"
   (check-equal? 0
                 (gmt-string->seconds (seconds->gmt-string 0)))
   (define s (current-seconds))
   (check-equal? s
                 (gmt-string->seconds (seconds->gmt-string s))))

  (test-case
   "seconds->gmt-8601-string"
   (check-equal? (seconds->gmt-8601-string 'plain 0)
                 "1970-01-01 00:00:00")
   (check-equal? (seconds->gmt-8601-string 'T/Z 0)
                 "1970-01-01T00:00:00Z")
   (check-equal? (seconds->gmt-8601-string 'T/.000Z 0)
                 "1970-01-01T00:00:00.000Z"))

  (test-case
   "gmt-8601-string->seconds"
   (define s (current-seconds))
   (check-equal? (gmt-8601-string->seconds (seconds->gmt-8601-string 'plain s))
                 s)
   (check-equal? (gmt-8601-string->seconds (seconds->gmt-8601-string 'T/Z s))
                 s)
   (check-equal? (gmt-8601-string->seconds (seconds->gmt-8601-string 'T/.000Z s))
                 s))

  (test-case
   "uri->scheme&host&port"
   (check-equal? (call-with-values
                     (lambda ()
                       (uri->scheme&host&port "http://www.fake.com:8080"))
                   list)
                 (list "http" "www.fake.com" 8080))
   (check-equal? (call-with-values
                     (lambda ()
                       (uri->scheme&host&port "https://www.fake.com"))
                   list)
                 (list "https" "www.fake.com" 443)))

  (test-case
   "uri&headers->path&header"
   (define heads (hash 'Date (format "Date: ~a" (seconds->gmt-string))
                       'Content-Length "123"))
   (define uri "http://www.fake.com/path?query#frag" )
   (define-values (p h) (uri&headers->path&header uri heads))
   (check-equal? p "/path?query#frag")
   ;; did it append the Host header automatically?
   ;; (check-equal? (sort (heads-string->list h) string<?)
   ;;               (sort (append xs `("Host: www.fake.com")) string<?))
   )

  ;; ----------------------------------------------------------------------
  ;; Tests that do I/O

  ;; Confirm HTTP 1.1. processing working correctly by doing two
  ;; requests over the same persistent connection.
  (define (get-twice uri)
    (define heads (hash 'Accept "text/html,text/plain"
                        'Accept-Encoding "gzip,deflate"))
    (define-values (in out) (connect-uri uri))
    (define-values (path rh) (uri&headers->path&header uri heads))
    (define/contract (get)
      (-> (or/c #f 'ok/open 'ok/close 'ok/redirect))
      (start-request in out "1.1" "get" path rh)
      (define h (purify-port/log-debug in))
      (define code (extract-http-code h))
      (case code
        [(999) #f]
        [(301 302) 'ok/redirect]
        [else (define e (read-entity/bytes in h))
              (log-http-debug
               (format "<- ~a bytes entity transfer and content decoded"
                       (bytes-length e)))
              (cond [(close-connection? h)
                     (unpool in out)
                     'ok/close]
                    [else 'ok/open])]))
    (begin0
        (match (get)
          ['ok/open (not (not (get)))] ;try again on same connection
          ['ok/close (log-http-debug "can't try again, due to 'Connection: close'") #t]
          ['ok/redirect (log-http-debug "can't try again, due to redirect") #t]
          [_ #f])
      (disconnect in out)))

  (define xs-uri-to-test
    (remove-duplicates
     `(
       "http://www.racket-lang.org"
       "http://www.httpwatch.com/httpgallery/chunked/"
       "http://www.google.com/"
       "https://www.google.com/"
       "http://www.wikipedia.org"
       "http://www.audiotechnica.com" ;will do multiple redirects
       "http://www.yahoo.com"
       "https://www.yahoo.com"
       "http://www.microsoft.com/"
       "http://www.amazon.com/"
       )))
  (define (test)
    (log-http-info "=== Testing with current-pool-timeout = ~a"
                   (current-pool-timeout))
    (for ([x (in-list xs-uri-to-test)])
      (log-http-info x)
      (log-http-info "  get-twice")
      (test-case (string-append "Actual I/O test, get-twice " x)
                 (check-true (get-twice x)))
      (log-http-info "  call/input-request, no encoding")
      (test-case (string-append "call/input-request, no encoding " x)
                 (check-true
                  (call/input-request "1.1" "GET" x
                                      #:redirects 10
                                      (hash 'Accept "text/html,text/plain")
                                      (lambda (in h)
                                        (read-entity/bytes in h)
                                        #t))))
      (log-http-info "  call/input-request, gzip,deflate")
      (test-case (string-append "call/input-request, gzip,deflate " x)
                 (check-true
                  (call/input-request "1.1" "GET" x
                                      #:redirects 10
                                      (hash
                                       'Accept "text/html,text/plain"
                                       'Accept-Encoding "gzip,deflate")
                                      (lambda (in h)
                                        (read-entity/bytes in h)
                                        #t))))))
  (for ([i '(0 10)])
    (parameterize ([current-pool-timeout i])
      (test))))
