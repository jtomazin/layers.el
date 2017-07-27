#lang racket

(provide read-file)

;; kludge
(define (strip-lang-decl port)
  (letrec ([read-stream (lambda () (stream-cons (read-line port 'any)
                                                (read-stream)))])
    (open-input-string
     (apply string-append
            (sequence->list (sequence-tail
                             (sequence-add-between
                              (stop-before (read-stream) eof-object?)
                              (string #\newline))
                             1 ;; get rid of #lang
                             ))))))

(define (read-file filename)
  (call-with-input-file filename
    (lambda (in) 
      (letrec ([stripped-lang-in (strip-lang-decl in)]
               [read-stream
                (lambda () (stream-cons (read stripped-lang-in)
                                        (read-stream)))]
               [all-exprs
                (sequence->list
                 (stop-before (read-stream) eof-object?))]
               ;; [layers (filter layer? all-exprs)]
               ;; [exprs (filter-not layer? all-exprs)]
               )
        all-exprs
        ;; (map process exprs)
        ))))
