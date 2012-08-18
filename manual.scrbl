#lang scribble/manual

@(require planet/scribble
          (for-label racket)
          (for-label net/url)
          (for-label net/head)
          (for-label (this-package-in request))
          (for-label (this-package-in head))
          )

@title{HTTP}

This libary provides support for HTTP 1.0 and 1.1 connections, as well as
for headers used in HTTP requests and responses.

@table-of-contents{}

@section{Request}


@defmodule/this-package[request]

@subsection{Why not just use @racket[net/url]?}

Racket's @racket[net/url] functions are very convenient for doing simple
requests using HTTP 1.0. Use them when you can.

Sometimes you need to use HTTP 1.1. For example you may need the speed
of making multiple requests to a server over one persistent
connection. Or you may want to use the @tt{Expect: 100-continue}
request header---so that if a server is going to reject or redirect
your @tt{put} or @tt{post} request, it can inform you @italic{before}
you send it a potentially large amount of data for the entity.

This library is intended to help you with those kinds of
applications. It provides basic building blocks to let you do any kind
of HTTP request flow, with help for some of the gory details.

Also it supplies some higher-level support. The @racket[call/requests]
functions are in the style of @racket[call/input-url].  These take
care of some standard setup and teardown, including ensuring the ports
are closed even if an exception is raised.

It even provides variations of @racket[net/url]'s functions like
@racket[get-impure-port]. You can use those when you want to use
@tt{Expect: 100-continue} but do not need to make multiple requests
over one connection.

@subsection{Connections and requests}

@deftogether[(

@defproc[(connect
[scheme (or/c "http" "https")]
[host string?]
[port exact-positive-integer?]
) (values input-port? output-port?)]

@defproc[(disconnect [in input-port?][out output-port?]) any]

)]{

Begin and end an HTTP connection.

Examples:
@racket[
(define-values (in out) (connect "http" "www.google.com" 80))
(disconnect in out)]

}


@defproc[(connect-uri [uri string?])
(values input-port? output-port?)]{

Given @racket[uri], uses @racket[uri->scheme&host&port] and @racket[connect] to
connect.

Example:
@racket[
(define-values (in out) (connect-uri "http://www.google.com/"))
(disconnect in out)
]

}


@defproc[(uri->scheme&host&port
[uri string?]
) (values (or/c "http" "https") string? exact-positive-integer?)]{

Splits @racket[uri] into three elements. If the URI does not specify an
element, then reasonable defaults are provided.

@itemize[

@item{scheme, @racket["http"] (default if unspecified) or @racket["https"]}

@item{host, such as @racket["www.foo.com"]}

@item{port as specified by @racket[uri]. Defaults to @racket[80] if scheme is
@racket["http"] or @racket[443] if @racket["https"].}
]

Examples:
@racket[
> (uri->scheme&host&port "www.foo.com")
"http"
"www.foo.com"
80
> (uri->scheme&host&port "http://www.foo.com")
"http"
"www.foo.com"
80
> (uri->scheme&host&port "https://www.foo.com")
"https"
"www.foo.com"
443
> (uri->scheme&host&port "http://www.foo.com:8080")
"http"
"www.foo.com"
8080
]

}


@defproc[(uri&headers->path&header
[uri string?]
[heads dict?]
) (values string? string?)]{

Splits @racket[uri] into the path portion required for a request, as well as
some headers. If @racket[heads] doesn't already contain @tt{Host} or @tt{Date}
headers they will be added automatically.

Examples:
@racket[
> (uri&headers->path&header "http://www.foo.com" '())
"/"
"Host: www.foo.com\r\nDate: Sat, 18 Aug 2012 16:40:58 GMT\r\n\r\n"
> (uri&headers->path&header
    "http://www.foo.com"
    (hash 'Date "Fri, 04 Nov 2011 22:16:34 GMT"))
"/"
"Host: www.foo.com\r\nDate: Fri, 04 Nov 2011 22:16:34 GMT\r\n\r\n"
> (uri&headers->path&header
     "http://www.foo.com/path/to/res#fragment?query=val"
     '())
"/path/to/res#fragment?query=val"
"Host: www.foo.com\r\nDate: Sat, 18 Aug 2012 16:40:58 GMT\r\n\r\n"
]

}


@defproc[(start-request
[in input-port?]
[out output-port?]
[http-version (or/c "1.0" "1.1")]
[method string?]
[path string?]
[heads string?]
) boolean?]{

Starts an HTTP request. Returns a @racket[boolean?]:

@itemize[

@item{

When @racket[method] is @racket["PUT"] or @racket["POST"], you should pay
attention to the returned @racket[boolean?] value:

@itemize[

@item{@racket[#t]: You should go ahead and transmit the entity (the data for
the put or post request), after which you should call @racket[flush-output] and
call @racket[purify-port/log-debug] to read the response headers.}

@item{@racket[#f]: You should not transmit the entity data. Instead just call
@racket[purify-port/log-debug] to read the response headers. (The reason for
this is to support the @tt{Expect: 100-continue} request header, and the case
where the server says not to continue because it will fail or redirect the
request. This lets you avoid sending a large entity body that won't be used.)}

]
}

@item{For other methods (such as @racket["GET"] or @racket["HEAD"]), this will
always return @racket[#t]. You should always just go ahead and call
@racket[purify-port/log-debug] to read the response headers.}

]

}

@subsection{Reading response headers}

@defproc[(purify-port/log-debug [in input-port?]) string?]{

This is exactly like @racket[purify-port] from @racket[net/url] but this logs
the headers using @racket[log-debug] when @racket[log-level?] is
@racket['debug].

The other functions in this library use @racket[log-debug], so using
@racket[purify-port/log-debug] will give a complete/consistent log
output.

}


@defproc[(redirect-uri [h string?]) (or/c #f string?)]{

Given response headers, if the code is a 30x value for a redirect and if a
@tt{Location} header exists, return the value of the @tt{Location}
header. Otherwise return @racket[#f].

}


@defproc[(same-connection? [old url?][new url?]) boolean?]{

Do @racket[old] and @racket[new] represent the same connection, i.e. the same
scheme, host and port?

}


@defproc[(close-connection? [h string?]) boolean?]{

Do the response headers require the connection to be closed?  For example is
the HTTP version 1.0, or is there a @tt{Connection: close} header?

}



@; ----------------------------------------------------------------------

@subsection{Reading response entities}

@subsubsection{Building blocks}

@defproc[(read-entity/transfer-decoding-port
[in input-port?]
[h string?]
) input-port?]{

Transforms @racket[in] to an @racket[input-port?] that reads an HTTP entity in
accordance with the @tt{Transfer-Encooding} header, specifically the
@tt{chunked} transfer encoding. You can read from the port as if the entity
were not transfer-encoded.

Used by @racket[read-entity/bytes].

}


@defproc[(entity-content-decoder/ports
[heads (or/c string? bytes?)]
) (input-port? output-port? -> any)]{

@margin-note{Note: I wanted this to return an @racket[input-port?] like
@racket[read-entity/transfer-decoding-port] and to be named
@racket[read-entity/content-decoding-port]. Unfortunately, the gzip and deflate
functions provided by Racket don't seem to make that possible, as they use a
@racket[copy-port] pattern.}

Examines the @tt{Content-Encoding} header, if any, and returns a function
@racket[(input-port? output-port? -> void?)] which will copy the bytes and
decode them. If there no content encoding specified, or an unsupported
encoding, this simply returns @racket[copy-port].

}


@defproc[(entity-content-decode-bytes
[bytes bytes?]
[heads (or/c string? bytes?)]
) bytes?]{

If you have already read the entity into @racket[bytes?], you may use this to
decode it based on the @tt{Content-Encoding} header if any in @racket[heads].

}


@subsubsection{Conveniences}

@defproc[(read-entity/bytes [in input-port?][h string?]) bytes?]{

Read the entity from @racket[in] and return it as @racket[bytes?]. This is a
convenience wrapper for @racket[read-entity/transfer-decoding-port] when you
don't mind keeping the entire entity in memory.

}


@defproc[(read-entity/xexpr [in input-port?][h string?]) xexpr?]{

Read the entity from @racket[in] to an @racket[xexpr?]. This is a convenience
wrapper for @racket[read-entity/bytes] when you would like the entity parsed
into an @racket[xexpr].

How the xexpr is created depends on the @tt{Content-Type} response header
present in @racket[h]:

@itemize[

@item{The xexpr is created using @racket[read-xml] and @racket[xml->xexpr] if
the @tt{Content-Type} is @tt{text/xml} or @tt{application/xml}, or if there is
no @tt{Content-Type} specified @italic{and} the first bytes of the entity are
@racket["<?xml"].}

@item{The xexpr is created using @racket[read-html-as-xml] and
@racket[xml->xexpr] if the @tt{Content-Type} is @tt{text/html}.}

@item{If the @tt{Content-Type} is @tt{text/plain} then the xexpr is simply
@racket[bytes?].}

@item{If the @tt{Content-Type} is @tt{application/x-www-form-urlencoded} then
the xexpr is @racket[`(data ,(form-url-encoded->alist (bytes->string/utf-8
bytes)))].}

@item{Otherwise the xexpr is @racket[`(entity () ,(format "~a" by))].}

]

}


@; ----------------------------------------------------------------------

@subsection{@racket[call/input-url] style}

Like @racket[call/input-url], these procedures help you make HTTP requests and
ensure that the ports are closed even if there is an exception.

@defproc[(call/requests
[scheme (or/c "http" "https")]
[host string?]
[port exact-positive-integer?]
[func (input-port? output-port? . -> . any/c)])
any/c]{

Provides a way to make one or more requests to the same host connection, and
ensure the ports are closed even if there is an exception.

Your @racket[func] function is called with two arguments, the input and output
ports. The value you return from @racket[func] is returned as the value of
@racket[call/requests].

@margin-note{Tip: Remember to call @racket[flush-output] after writing data to
@racket[out], otherwise the server may not get the data and respond, while you
wait to read from @racket[in].}

@codeblock{
(define-values (scheme host port) (uri->scheme&host&port uri))
(call/requests
  scheme host port
  (lambda (in out)
    (define-values (path rh)
      (uri&headers->path&header uri '("Expect: 100-continue")))
    (define tx-data? (start-request in out method path rh "1.0"))
    (when tx-data?
      (display data out)
      (flush-output out)) ;Important!
    (define h (purify-port/log-debug in))
    (read-entity/bytes in h)))
}

For a single request, you may prefer to use @racket[call/input-request] or
@racket[call/output-request].

}


@defproc[(call/input-request
[http-version (or/c "1.0" "1.1")]
[method string?]
[uri string?]
[heads dict?]
[entity-reader (input-port? string? . -> . any/c)]
[#:redirects redirects 10]
) any/c]{

A wrapper around @racket[call/requests] for the case where you want to make
just one request, and there is no data to send (e.g. it is a @tt{HEAD},
@tt{GET}, or @tt{DELETE} request).

Like @racket[call/requests], this guarantees the ports will be closed.

Your @racket[entity-reader] function is called after the response header has
been read (the header is passed to you as a @racket[string]), giving you the
ability to read the response entity from the supplied @racket[input-port?]. For
example you may read the response using @racket[read-entity/bytes] or
@racket[read-entity/xexpr].

If @racket[redirects] is non-zero, up to that number of redirects will be
followed automatically. Whether the response is a redirect is determined using
@racket[redirect-uri]. If the redirected location can be reached on the
existing HTTP connection (as determined by @racket[close-connection?] and
@racket[same-connection?]), then the existing connection will be used (which is
faster than disconnecting and reconnecting.)

}


@defproc[(call/output-request
[http-version (or/c "1.0" "1.1")]
[method string?]
[uri string?]
[data (or/c bytes? (output-port? . -> . void?))]
[data-length (or/c #f exact-nonnegative-integer?)]
[heads dict?]
[entity-reader (input-port? string? . -> . any/c)]
[#:redirects redirects 10]
) any/c]{

A wrapper around @racket[call/requests] for the case where you want to make
just one request, and there is data to send (it is a @tt{PUT} or @tt{POST}
request).

Like @racket[call/requests], this guarantees the ports will be closed.

The @racket[data] and @racket[data-length] arguments depend on whether you want
to provide the data as @racket[bytes?] or as a function that will write the
data:

@itemize[

@item{ If @racket[data] is @racket[bytes?], you may set @racket[data-length] to
@racket[#f].  A @tt{Content-Length} header will be created for you using
@racket[(bytes-length data)]. }

@item{ If you pass a writer function for @racket[data], then
@racket[data-length] should be the number of bytes that you will write. This
allows a @tt{Content-Length} header to be created for you. Otherwise, if you
supply @racket[#f] for @racket[data-length], there will be no
@tt{Content-Length} header (unless you supply your own in @racket[heads]). }

]

Your @racket[entity-reader] function is called after the response header has
been read (the header is passed to you as a @racket[string]), giving you the
ability to read the response entity from the supplied @racket[input-port?]. For
example you may read the response using @racket[read-entity/bytes] or
@racket[read-entity/xexpr].

If @racket[redirects] is non-zero, up to that number of redirects will be
followed automatically. Whether the response is a redirect is determined using
@racket[redirect-uri]. If the redirected location can be reached on the
existing HTTP connection (as determined by @racket[close-connection?] and
@racket[same-connection?]), then the existing connection will be used (which is
faster than disconnecting and reconnecting.)

If you supply @racket["1.1"] for @racket[http-version] and the request header
@tt{Expect: 100-continue}, then redirects or failures can be handled much more
efficiently. If the server supports @tt{Expect: 100-continue}, then it can
respond with the redirect or failure @italic{before} you transmit all of the
entity data.

}


@; ----------------------------------------------------------------------

@subsection{@racket[net/url] impure port style}

@deftogether[(
@defproc[(head-impure-port* [url url?][heads (listof string? '())]) input-port?]
@defproc[(get-impure-port* [url url?][heads (listof string? '())]) input-port?]
@defproc[(delete-impure-port* [url url?][heads (listof string? '())]) input-port?]
@defproc[(put-impure-port* [url url?][data bytes?][heads (listof string? '())]) input-port?]
@defproc[(post-impure-port* [url url?][data bytes?][heads (listof string? '())]) input-port?]
@defparam[http-ver version string?]
)]{

Variations of the @racket[net/url] impure port functions, if you prefer that
style for one-off requests.

These variations add the ability to do HTTP 1.1, and support for you including
an @tt{Expect: 100-continue} request header in @racket[heads]. This allows a
PUT or POST request to fail or redirect, @italic{before} @racket[data] is sent
to the server.

To make these functions do HTTP 1.1 requests, you should set the
@racket[http-ver] parameter to "1.1".

Because HTTP 1.1 defaults to persistent connections, it would be nice for you
to include a @tt{Connection: close} request header in @racket[heads].

}


@; ----------------------------------------------------------------------

@subsection{Date string conversion}

@defproc[(seconds->gmt-string [s exact-integer? (current-seconds)]) string?]{

Examples:
@racket[
> (seconds->gmt-string)
"Sat, 18 Aug 2012 16:40:58 GMT"
> (seconds->gmt-string 0)
"Thu, 01 Jan 1970 00:00:00 GMT"
]

}


@defproc[(seconds->gmt-8601-string
[style (or/c 'plain 'T/Z 'T/.00Z) 'T/Z]
[s exact-integer? (current-seconds)]
) string?]{

Examples:
@racket[
> (define sc (current-seconds))
> (seconds->gmt-8601-string 'plain sc)
"2012-08-18 16:40:58"
> (seconds->gmt-8601-string 'T/Z sc)
"2012-08-18T16:40:58Z"
> (seconds->gmt-8601-string 'T/.000Z sc)
"2012-08-18T16:40:58.000Z"
]

}


@defproc[(gmt-8601-string->seconds [s string?]) exact-integer?]{

Examples:
@racket[
> (gmt-8601-string->seconds "1970-01-01 00:00:00")
0
> (gmt-8601-string->seconds "1970-01-01T00:00:00Z")
0
> (gmt-8601-string->seconds "1970-01-01T00:00:00.000Z")
0
]

}


@; ----------------------------------------------------------------------
@; ----------------------------------------------------------------------

@section{Head}

@defmodule/this-package[head]

@; ----------------------------------------------------------------------

@subsection{Supplements to @racket[net/head]}

These stick with @racket[net/head]'s respresentation of headers as a
@racket[string?], and add a few functions that are "missing" or would
be convenient.

@deftogether[(
@defproc[(extract-http-ver [h string?]) string?]
@defproc[(extract-http-code [h string?]) exact-positive-integer?]
@defproc[(extract-http-text [h string?]) string?]
)]{

From an HTTP response header, get the HTTP version (such as @tt{1.0} or
@tt{1.1}), status code number (such as @tt{200}), and the status text
description (such as @tt{OK}).

}


@defproc[(extract-field/number
[field string?]
[h string?]
[radix exact-positive-integer? 10]
) (or/c #f number?)]{

Convenience wrapper for @racket[net/head]'s @racket[extract-field] for fields
whose values are supposed to be a number, such as @tt{Content-Length}. From
the header @racket[h] extract the value of @racket[field] as a number using
@racket[radix]. If the field does not exist or its value cannot be converted
to a number, return @racket[#f].

}


@defproc[(maybe-insert-field
[field (or/c symbol? string?)]
[value any/c]
[h string?]
) string?]{

Like @racket[insert-field], but inserts only if @racket[field] does not
already exist in @racket[h]. Otherwise the @racket[field] and its
@racket[value] already present in @racket[h] remain unchanged.

Use case: You want to ensure the header is present, but not disrupt any
existing value---for example, @tt{Date} or @tt{Content-Length}.

}


@defproc[(coalesce-fields [h string?]) string?]{

Combine header fields with the same name into one, with the values as a comma
separated list (with no space between values), as prescribed by RFC 2616,
section 4.2. For example, the two headers @tt{x-foo: fred} and @tt{x-foo:
barney} would be combined into the single header @tt{x-foo: fred,barney}.

Some web services (hello Amazon) require this as part of "signing" a request
for authentication, which means it has to be done exactly right.

}


@defproc[(validate-tx-or-rx-header [h string?]) string?]{

Similar to @racket[net/head]'s @racket[validate-header], but permits
response headers (with their initial status line, such as @tt{HTTP/1.1 200
OK}) as well as request headers. Also, if the header is valid and no
exception is thrown, returns @racket[h] (making it more convenient to use
this in an expression that both validates and returns a header).

}


@; ----------------------------------------------------------------------

@subsection{Heads as a @racket[dict]}

@defproc[(heads-string->dict
[s string?]
[dupe-sep string? "\n"]
) dict?]{

Convert headers represented as a @racket[string] of the form
specified in @racket[net/head], to a @racket[dict?] where the keys are
@racket[symbol?] and the values are @racket[any/c].

Specifically, handle the case of duplicate headers. A real-world
example of such duplicate headers is Set-Cookie.  Other than
association lists, most types of dicts don't permit duplicate keys, so
we can't store duplicate headers using those. Instead, duplicate
headers are stored in the dict under the same key, with the various
values separated by @racket[dupe-sep].

Examples:
@racket[
> (heads-string->dict "Host: Foo\r\nKey: Value\r\n\r\n")
'#hash((Host . "Foo") (Key . "Value"))
> (heads-string->dict "Key: Value 1\r\nKey: Value 2\r\n\r\n")
'#hash((Key . "Value 1\nValue 2"))
]

}


@defproc[(heads-dict->string
[d dict?]
) string?]{

Convert headers represented as a @racket[dict?] into a
@racket[string?] of the form specified in @racket[net/head], including
the terminating \r\n to end all the headers.  This is the reverse of
@racket[heads-string->dict] including its handling of duplicate
headers.

Examples:
@racket[
> (heads-dict->string '#hash((Host . "Foo") (Key . "Value")))
"Host: Foo\r\nKey: Value\r\n\r\n"
> (heads-dict->string '((Host . "Foo") (Key . "Value")))
"Host: Foo\r\nKey: Value\r\n\r\n"
]

}

@defproc[(maybe-dict-set
[d (and/c dict? dict-can-functional-set?)]
[k symbol?]
[v any/c]
) (and/c dict? dict-can-functional-set?)]{

Like @racket[dict-set], but will set the new value @racket[v] for the
key @racket[k] only if the key does not already exist in the dict.

Examples:
@racket[
> (maybe-dict-set '() 'a "New")
'((a . "New"))
> (maybe-dict-set '([a . "Old"]) 'a "New")
'((a . "Old"))
]

}

@defproc[(maybe-dict-set*
[d (and/c dict? dict-can-functional-set?)]
[kvs list?]
) (and/c dict? dict-can-functional-set?)]{

Like @racket[dict-set*], but will set the new value for a key only if
the key does not already exist in the dict.

}



@; ----------------------------------------------------------------------

@section{License}

Copyright (c) 2012, Greg Hendershott.
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are
met:

@itemize[

@item{ Redistributions of source code must retain the above copyright notice,
this list of conditions and the following disclaimer. }

@item{ Redistributions in binary form must reproduce the above copyright
notice, this list of conditions and the following disclaimer in the
documentation and/or other materials provided with the distribution. }

] @;itemize

@tt{
THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
``AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
}

@; ----------------------------------------------------------------------
