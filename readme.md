Documentation
=============

Documentation is written using Racket's Scribble. The raw source is in
manual.scrbl.

The HTML output that you'd actually want to read can be found on
[PLaneT](http://planet.plt-scheme.org/display.ss?package=http.plt&owner=gh).

You can also generate it using `./make-doc.sh`, then point your browser to
[file:///tmp/http-doc/manual.html](file:///tmp/http-doc/manual.html).

Unit tests
==========

The `rackunit` tests use the submodule feature new in Racket 5.3. Tests are in
`(module+ test ..)`. You can run the tests for a `foo.rkt` file with `raco
test foo.rkt`. You can run tests for all files using `raco test ./`.
