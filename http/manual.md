This libary provides support for HTTP 1.0 and 1.1 connections, as well
as for headers used in HTTP requests and responses.

# 1. Request

```racket
 (require request)
```

## 1.1. Why not just use `net/url`?

Racket’s `net/url` functions are very convenient for doing simple
requests using HTTP 1.0. Use them when you can.

Sometimes you need to use HTTP 1.1. For example you may need the speed
of making multiple requests to a server over one persistent connection.
Or you may want to use the `Expect: 100-continue` request header—so that
if a server is going to reject or redirect your `put` or `post` request,
it can inform you _before_ you send it a potentially large amount of
data for the entity.

This library is intended to help you with those kinds of applications.
It provides basic building blocks to let you do any kind of HTTP request
flow, with help for some of the gory details.

Also it supplies some higher-level support. The `call/requests`
functions are in the style of `call/input-url`.  These take care of some
standard setup and teardown, including ensuring the ports are closed
even if an exception is raised.

It even provides variations of `net/url`’s functions like
`get-impure-port`. You can use those when you want to use `Expect:
100-continue` but do not need to make multiple requests over one
connection.

## 1.2. Connections and requests

```racket
(connect scheme host port) -> input-port? output-port?
  scheme : (or/c "http" "https")                      
  host : string?                                      
  port : exact-positive-integer?                      
(disconnect in out) -> any                            
  in : input-port?                                    
  out : output-port?                                  
(connection-pool-timeout) -> nonnegative-integer?     
(connection-pool-timeout seconds) -> void?            
  seconds : nonnegative-integer?                      
```

Begin and end an HTTP connection.

When `connection-pool-timeout` is positive, `disconnect` will keep the
connection open for that number of seconds, and `connect` may reuse it.
Connections that timeout are automatically closed.

When `connection-pool-timeout` is positive, `connect` behaves like "raw"
`connect*` and `disconnect` behaves like "raw" `disconnect*`.

Examples:

```racket
(define-values (in out) (connect "http" "www.google.com" 80))
(disconnect in out)                                          
```

```racket
(connect-uri uri) -> input-port? output-port?
  uri : string?                              
```

Given `uri`, uses `uri->scheme&host&port` and `connect` to connect.

Example:

```racket
(define-values (in out) (connect-uri "http://www.google.com/"))
(disconnect in out)                                            
```

```racket
(connect* scheme host port) -> input-port? output-port?
  scheme : (or/c "http" "https")                       
  host : string?                                       
  port : exact-positive-integer?                       
(disconnect* in out) -> any                            
  in : input-port?                                     
  out : output-port?                                   
```

Begin and end an HTTP connection that is "raw", in the sense that
connections are not reused even when `connection-pool-timeout` is
positive.

```racket
(uri->scheme&host&port uri) -> (or/c "http" "https")  
                               string?                
                               exact-positive-integer?
  uri : string?                                       
```

Splits `uri` into three elements. If the URI does not specify an
element, then reasonable defaults are provided.

* scheme, `"http"` (default if unspecified) or `"https"`

* host, such as `"www.foo.com"`

* port as specified by `uri`. Defaults to `80` if scheme is `"http"` or
  `443` if `"https"`.

Examples:

```racket
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
```

```racket
(uri&headers->path&header uri heads) -> string? string?
  uri : string?                                        
  heads : dict?                                        
```

Splits `uri` into the path portion required for a request, as well as
some headers. If `heads` doesn’t already contain `Host` or `Date`
headers they will be added automatically.

Examples:

```racket
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
```

```racket
(start-request in                      
               out                     
               http-version            
               method                  
               path                    
               heads)       -> boolean?
  in : input-port?                     
  out : output-port?                   
  http-version : (or/c "1.0" "1.1")    
  method : string?                     
  path : string?                       
  heads : string?                      
```

Starts an HTTP request. Returns a `boolean?`:

* When `method` is `"PUT"` or `"POST"`, you should pay attention to the
  returned `boolean?` value:

  * `#t`: You should go ahead and transmit the entity (the data for the
    put or post request), after which you should call `flush-output` and
    call `purify-port/log-debug` to read the response headers.

  * `#f`: You should not transmit the entity data. Instead just call
    `purify-port/log-debug` to read the response headers. (The reason
    for this is to support the `Expect: 100-continue` request header,
    and the case where the server says not to continue because it will
    fail or redirect the request. This lets you avoid sending a large
    entity body that won’t be used.)

* For other methods (such as `"GET"` or `"HEAD"`), this will always
  return `#t`. You should always just go ahead and call
  `purify-port/log-debug` to read the response headers.

## 1.3. Reading response headers

```racket
(purify-port/log-debug in) -> string?
  in : input-port?                   
```

This is exactly like `purify-port` from `net/url` but this logs the
headers using `log-debug` when `log-level?` is `'debug`.

The other functions in this library use `log-debug`, so using
`purify-port/log-debug` will give a complete/consistent log output.

```racket
(redirect-uri h) -> (or/c #f string?)
  h : string?                        
```

Given response headers, if the code is a 30x value for a redirect and if
a `Location` header exists, return the value of the `Location` header.
Otherwise return `#f`.

```racket
(same-connection? old new) -> boolean?
  old : url?                          
  new : url?                          
```

Do `old` and `new` represent the same connection, i.e. the same scheme,
host and port?

```racket
(close-connection? h) -> boolean?
  h : string?                    
```

Do the response headers require the connection to be closed?  For
example is the HTTP version 1.0, or is there a `Connection: close`
header?

## 1.4. Reading response entities

### 1.4.1. Building blocks

```racket
(read-entity/transfer-decoding-port in h) -> input-port?
  in : input-port?                                      
  h : string?                                           
```

Transforms `in` to an `input-port?` that reads an HTTP entity in
accordance with the `Transfer-Encooding` header, specifically the
`chunked` transfer encoding. You can read from the port as if the entity
were not transfer-encoded.

Used by `read-entity/bytes`.

```racket
(entity-content-decoder/ports heads) 
 -> (input-port? output-port? -> any)
  heads : (or/c string? bytes?)      
```

> Note: I wanted this to return an `input-port?` like
> `read-entity/transfer-decoding-port` and to be named
> `read-entity/content-decoding-port`. Unfortunately, the gzip and deflate
> functions provided by Racket don’t seem to make that possible, as they
> use a `copy-port` pattern.

Examines the `Content-Encoding` header, if any, and returns a function
`(input-port? output-port? -> void?)` which will copy the bytes and
decode them. If there no content encoding specified, or an unsupported
encoding, this simply returns `copy-port`.

```racket
(entity-content-decode-bytes bytes heads) -> bytes?
  bytes : bytes?                                   
  heads : (or/c string? bytes?)                    
```

If you have already read the entity into `bytes?`, you may use this to
decode it based on the `Content-Encoding` header if any in `heads`.

### 1.4.2. Conveniences

```racket
(read-entity/bytes in h) -> bytes?
  in : input-port?                
  h : string?                     
```

Read the entity from `in` and return it as `bytes?`. This is a
convenience wrapper for `read-entity/transfer-decoding-port` when you
don’t mind keeping the entire entity in memory.

```racket
(read-entity/xexpr in h) -> xexpr?
  in : input-port?                
  h : string?                     
```

Read the entity from `in` to an `xexpr?`. This is a convenience wrapper
for `read-entity/bytes` when you would like the entity parsed into an
`xexpr`.

How the xexpr is created depends on the `Content-Type` response header
present in `h`:

* The xexpr is created using `read-xml` and `xml->xexpr` if the
  `Content-Type` is `text/xml` or `application/xml`, or if there is no
  `Content-Type` specified _and_ the first bytes of the entity are
  `"<?xml"`.

* The xexpr is created using `read-html-as-xml` and `xml->xexpr` if the
  `Content-Type` is `text/html`.

* If the `Content-Type` is `text/plain` then the xexpr is simply
  `bytes?`.

* If the `Content-Type` is `application/x-www-form-urlencoded` then the
  xexpr is (data ,(form-url-encoded->alist (bytes->string/utf-8
  bytes)))`.

* Otherwise the xexpr is (entity () ,(format "~a" by))`.

## 1.5. `call/input-url` style

Like `call/input-url`, these procedures help you make HTTP requests and
ensure that the ports are closed even if there is an exception.

```racket
(call/requests scheme host port func) -> any/c  
  scheme : (or/c "http" "https")                
  host : string?                                
  port : exact-positive-integer?                
  func : (input-port? output-port? . -> . any/c)
```

Provides a way to make one or more requests to the same host connection,
and ensure the ports are closed even if there is an exception.

Your `func` function is called with two arguments, the input and output
ports. The value you return from `func` is returned as the value of
`call/requests`.

> Tip: Remember to call `flush-output` after writing data to `out`,
> otherwise the server may not get the data and respond, while you wait to
> read from `in`.

```racket
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
```

For a single request, you may prefer to use `call/input-request` or
`call/output-request`.

```racket
(call/input-request http-version                    
                    method                          
                    uri                             
                    heads                           
                    entity-reader                   
                    #:redirects redirects) -> any/c 
  http-version : (or/c "1.0" "1.1")                 
  method : string?                                  
  uri : string?                                     
  heads : dict?                                     
  entity-reader : (input-port? string? . -> . any/c)
  redirects : 10                                    
```

A wrapper around `call/requests` for the case where you want to make
just one request, and there is no data to send (e.g. it is a `HEAD`,
`GET`, or `DELETE` request).

Like `call/requests`, this guarantees the ports will be closed.

Your `entity-reader` function is called after the response header has
been read (the header is passed to you as a `string`), giving you the
ability to read the response entity from the supplied `input-port?`. For
example you may read the response using `read-entity/bytes` or
`read-entity/xexpr`.

If `redirects` is non-zero, up to that number of redirects will be
followed automatically. Whether the response is a redirect is determined
using `redirect-uri`. If the redirected location can be reached on the
existing HTTP connection (as determined by `close-connection?` and
`same-connection?`), then the existing connection will be used (which is
faster than disconnecting and reconnecting.)

```racket
(call/output-request http-version                   
                     method                         
                     uri                            
                     data                           
                     data-length                    
                     heads                          
                     entity-reader                  
                     #:redirects redirects) -> any/c
  http-version : (or/c "1.0" "1.1")                 
  method : string?                                  
  uri : string?                                     
  data : (or/c bytes? (output-port? . -> . void?))  
  data-length : (or/c #f exact-nonnegative-integer?)
  heads : dict?                                     
  entity-reader : (input-port? string? . -> . any/c)
  redirects : 10                                    
```

A wrapper around `call/requests` for the case where you want to make
just one request, and there is data to send (it is a `PUT` or `POST`
request).

Like `call/requests`, this guarantees the ports will be closed.

The `data` and `data-length` arguments depend on whether you want to
provide the data as `bytes?` or as a function that will write the data:

* If `data` is `bytes?`, you may set `data-length` to `#f`.  A
  `Content-Length` header will be created for you using `(bytes-length
  data)`.

* If you pass a writer function for `data`, then `data-length` should be
  the number of bytes that you will write. This allows a
  `Content-Length` header to be created for you. Otherwise, if you
  supply `#f` for `data-length`, there will be no `Content-Length`
  header (unless you supply your own in `heads`).

Your `entity-reader` function is called after the response header has
been read (the header is passed to you as a `string`), giving you the
ability to read the response entity from the supplied `input-port?`. For
example you may read the response using `read-entity/bytes` or
`read-entity/xexpr`.

If `redirects` is non-zero, up to that number of redirects will be
followed automatically. Whether the response is a redirect is determined
using `redirect-uri`. If the redirected location can be reached on the
existing HTTP connection (as determined by `close-connection?` and
`same-connection?`), then the existing connection will be used (which is
faster than disconnecting and reconnecting.)

If you supply `"1.1"` for `http-version` and the request header `Expect:
100-continue`, then redirects or failures can be handled much more
efficiently. If the server supports `Expect: 100-continue`, then it can
respond with the redirect or failure _before_ you transmit all of the
entity data.

## 1.6. `net/url` impure port style

```racket
(head-impure-port* url heads) -> input-port?     
  url : url?                                     
  heads : (listof string? '())                   
(get-impure-port* url heads) -> input-port?      
  url : url?                                     
  heads : (listof string? '())                   
(delete-impure-port* url heads) -> input-port?   
  url : url?                                     
  heads : (listof string? '())                   
(put-impure-port* url data heads) -> input-port? 
  url : url?                                     
  data : bytes?                                  
  heads : (listof string? '())                   
(post-impure-port* url data heads) -> input-port?
  url : url?                                     
  data : bytes?                                  
  heads : (listof string? '())                   
(http-ver) -> string?                            
(http-ver version) -> void?                      
  version : string?                              
```

Variations of the `net/url` impure port functions, if you prefer that
style for one-off requests.

These variations add the ability to do HTTP 1.1, and support for you
including an `Expect: 100-continue` request header in `heads`. This
allows a PUT or POST request to fail or redirect, _before_ `data` is
sent to the server.

To make these functions do HTTP 1.1 requests, you should set the
`http-ver` parameter to "1.1".

Because HTTP 1.1 defaults to persistent connections, it would be nice
for you to include a `Connection: close` request header in `heads`.

## 1.7. Date string conversion

```racket
(seconds->gmt-string [s]) -> string?    
  s : exact-integer? = (current-seconds)
```

Examples:

```racket
> (seconds->gmt-string)        
"Sat, 18 Aug 2012 16:40:58 GMT"
> (seconds->gmt-string 0)      
"Thu, 01 Jan 1970 00:00:00 GMT"
```

```racket
(seconds->gmt-8601-string [style s]) -> string?
  style : (or/c 'plain 'T/Z 'T/.00Z) = 'T/Z    
  s : exact-integer? = (current-seconds)       
```

Examples:

```racket
> (define sc (current-seconds))         
> (seconds->gmt-8601-string 'plain sc)  
"2012-08-18 16:40:58"                   
> (seconds->gmt-8601-string 'T/Z sc)    
"2012-08-18T16:40:58Z"                  
> (seconds->gmt-8601-string 'T/.000Z sc)
"2012-08-18T16:40:58.000Z"              
```

```racket
(gmt-8601-string->seconds s) -> exact-integer?
  s : string?                                 
```

Examples:

```racket
> (gmt-8601-string->seconds "1970-01-01 00:00:00")     
0                                                      
> (gmt-8601-string->seconds "1970-01-01T00:00:00Z")    
0                                                      
> (gmt-8601-string->seconds "1970-01-01T00:00:00.000Z")
0                                                      
```

# 2. Head

```racket
 (require head)
```

## 2.1. Supplements to `net/head`

These stick with `net/head`’s respresentation of headers as a `string?`,
and add a few functions that are "missing" or would be convenient.

```racket
(extract-http-ver h) -> string?                 
  h : string?                                   
(extract-http-code h) -> exact-positive-integer?
  h : string?                                   
(extract-http-text h) -> string?                
  h : string?                                   
```

From an HTTP response header, get the HTTP version (such as `1.0` or
`1.1`), status code number (such as `200`), and the status text
description (such as `OK`).

```racket
(extract-field/number field h [radix]) -> (or/c #f number?)
  field : string?                                          
  h : string?                                              
  radix : exact-positive-integer? = 10                     
```

Convenience wrapper for `net/head`’s `extract-field` for fields whose
values are supposed to be a number, such as `Content-Length`. From the
header `h` extract the value of `field` as a number using `radix`. If
the field does not exist or its value cannot be converted to a number,
return `#f`.

```racket
(maybe-insert-field field value h) -> string?
  field : (or/c symbol? string?)             
  value : any/c                              
  h : string?                                
```

Like `insert-field`, but inserts only if `field` does not already exist
in `h`. Otherwise the `field` and its `value` already present in `h`
remain unchanged.

Use case: You want to ensure the header is present, but not disrupt any
existing value—for example, `Date` or `Content-Length`.

```racket
(coalesce-fields h) -> string?
  h : string?                 
```

Combine header fields with the same name into one, with the values as a
comma separated list (with no space between values), as prescribed by
RFC 2616, section 4.2. For example, the two headers `x-foo: fred` and
`x-foo: barney` would be combined into the single header `x-foo:
fred,barney`.

Some web services (hello Amazon) require this as part of "signing" a
request for authentication, which means it has to be done exactly right.

```racket
(validate-tx-or-rx-header h) -> string?
  h : string?                          
```

Similar to `net/head`’s `validate-header`, but permits response headers
(with their initial status line, such as `HTTP/1.1 200 OK`) as well as
request headers. Also, if the header is valid and no exception is
thrown, returns `h` (making it more convenient to use this in an
expression that both validates and returns a header).

## 2.2. Heads as a `dict`

```racket
(heads-string->dict s [dupe-sep]) -> dict?
  s : string?                             
  dupe-sep : string? = "\n"               
```

Convert headers represented as a `string` of the form specified in
`net/head`, to a `dict?` where the keys are `symbol?` and the values are
`any/c`.

Specifically, handle the case of duplicate headers. A real-world example
of such duplicate headers is Set-Cookie.  Other than association lists,
most types of dicts don’t permit duplicate keys, so we can’t store
duplicate headers using those. Instead, duplicate headers are stored in
the dict under the same key, with the various values separated by
`dupe-sep`.

Examples:

```racket
> (heads-string->dict "Host: Foo\r\nKey: Value\r\n\r\n")     
'#hash((Host . "Foo") (Key . "Value"))                       
> (heads-string->dict "Key: Value 1\r\nKey: Value 2\r\n\r\n")
'#hash((Key . "Value 1\nValue 2"))                           
```

```racket
(heads-dict->string d) -> string?
  d : dict?                      
```

Convert headers represented as a `dict?` into a `string?` of the form
specified in `net/head`, including the terminating \r\n to end all the
headers.  This is the reverse of `heads-string->dict` including its
handling of duplicate headers.

Examples:

```racket
> (heads-dict->string '#hash((Host . "Foo") (Key . "Value")))
"Host: Foo\r\nKey: Value\r\n\r\n"                            
> (heads-dict->string '((Host . "Foo") (Key . "Value")))     
"Host: Foo\r\nKey: Value\r\n\r\n"                            
```

```racket
(maybe-dict-set d k v) -> (and/c dict? dict-can-functional-set?)
  d : (and/c dict? dict-can-functional-set?)                    
  k : symbol?                                                   
  v : any/c                                                     
```

Like `dict-set`, but will set the new value `v` for the key `k` only if
the key does not already exist in the dict.

Examples:

```racket
> (maybe-dict-set '() 'a "New")           
'((a . "New"))                            
> (maybe-dict-set '([a . "Old"]) 'a "New")
'((a . "Old"))                            
```

```racket
(maybe-dict-set* d kvs) -> (and/c dict? dict-can-functional-set?)
  d : (and/c dict? dict-can-functional-set?)                     
  kvs : list?                                                    
```

Like `dict-set*`, but will set the new value for a key only if the key
does not already exist in the dict.

# 3. License

Copyright (c) 2012, Greg Hendershott. All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are
met:

* Redistributions of source code must retain the above copyright notice,
  this list of conditions and the following disclaimer.

* Redistributions in binary form must reproduce the above copyright
  notice, this list of conditions and the following disclaimer in the
  documentation and/or other materials provided with the distribution.

`THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS “AS
IS” AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.`
