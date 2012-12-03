#lang setup/infotab

(define name "http")
(define categories '(net))
(define blurb
  '("Provides support for HTTP 1.0 and 1.1 connections,"
    "as well as for headers used in HTTP requests and responses."))
(define homepage "https://github.com/greghendershott/http")

(define release-notes
  '((p "Fix formatting of code examples.")))
(define version "2012-08-18")
(define can-be-loaded-with 'all)

(define primary-file '("main.rkt"))
(define scribblings '(("manual.scrbl" (multi-page))))

(define required-core-version "5.3.1")
(define repositories '("4.x"))
