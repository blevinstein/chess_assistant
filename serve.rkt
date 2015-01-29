#lang racket

(require web-server/servlet-env)

(require "servlet.rkt")

(define (main)
  (serve/servlet start
    #:launch-browser? #f
    #:port 8000
    #:server-root-path (current-directory)
    #:servlet-regexp #rx""))

(main)
