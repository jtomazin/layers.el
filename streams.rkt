#lang racket

(define/contract (stream-take num strm)
  (-> exact-nonnegative-integer? stream? stream?)
  (if (= num 0)
      empty-stream
      (stream-cons (stream-first strm) (stream-take (- num 1)
                                                    (stream-rest strm)))))
(define (stream-interleave s1 s2)
  (cond [(and (stream-empty? s1)
              (stream-empty? s2)) empty-stream]
        [(stream-empty? s1) s2]
        [(stream-empty? s2) s1]
        [else
         (stream-cons (stream-first s1)
                      (stream-cons (stream-first s2)
                                   (stream-interleave (stream-rest s1)
                                                      (stream-rest s2))))]))

(define (stream-interpose stream el)
  (if (stream-empty? stream)
      empty-stream
      (stream-cons (stream-first stream)
                   (stream-cons el
                                (stream-interpose (stream-rest stream) el)))))
