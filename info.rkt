#lang setup/infotab

(define name "http")
(define categories '(net))
(define blurb
  '("Provides support for HTTP 1.0 and 1.1 connections as well as for headers used in HTTP requests and responses."))
(define homepage "https://github.com/greghendershott/http")

(define release-notes
  '((p "Use Racket's new define-logger.")
    (p "Add a 'basic style to seconds->gmt-8601-string.")))
(define version "2013-04-11")
(define can-be-loaded-with 'all)

(define primary-file '("main.rkt"))
(define scribblings '(("manual.scrbl" (multi-page))))

(define required-core-version "5.3.1.9")
(define repositories '("4.x"))
