#lang setup/infotab

(define name "http")
(define categories '(net))
(define blurb
  '(p "Provides support for HTTP 1.0 and 1.1 connections,"
      "as well as for headers used in HTTP requests and responses."))
(define release-notes
 '((p "Initial release.")))
(define homepage "https://github.com/greghendershott/http")

(define version "2012-08-18")
(define can-be-loaded-with 'all)

(define primary-file '("main.rkt" "request.rkt" "head.rkt"))
(define scribblings '(("manual.scrbl" (multi-page))))

(define required-core-version "5.3")
(define repositories '("4.x"))
