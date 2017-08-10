;;; <graph> := {<vertex>}*
;; <vertex> := <content> + {<edge>}*
;; <content> := <expr> | <graph>

(define-record-type <vertex>
  (%make-vertex content edges)
  vertex?
  (content vertex-content)
  (edges vertex-edges vertex-add-edge!))
(define (make-vertex content)
  (%make-vertex content '()))
(define (vertex-add-edge v e)
  (let ((new (record-copy v)))
    (vertex-add-edge! new e)
    new))

(define-record-type <edge>
  (%make-edge from to label)
  edge?
  (from edge-from)
  (to edge-to)
  (label edge-label))
(define (make-edge from to #!optional label)
  (%make-edge from to label))

;; API
(define (graph? o)
  (and (list? o)
       (every vertex? o)))

(define empty-graph '())

;; (define (get-vertex name g)
;;   (cond ((null? g) #f)
;;         ((equal? (vertex-name (car g)) name) (car g))
;;         (else (get-vertex name (cdr g)))))

(define (add-vertex v g)
  (cons v g))

;; (define (remove-vertex name g)
;;   (remove (lambda (v) (equal? (vertex-name v) name)) g))

;; (define (add-edge-to-vertex edge vertex)
;;   ;; fixme - prevent duplicates
;;   (make-vertex (vertex-name vertex) (cons edge (vertex-edges vertex))))

;; (define (add-edge from to g #!optional label)
;;   (let ((from-vertex (get-vertex from g))
;;         (new-edge (make-edge from to label))
;;         (other-vertices (remove-vertex from g)))
;;     (cons                               ; implementation leak
;;      (add-edge-to-vertex new-edge (if from-vertex
;;                                       from-vertex
;;                                       (make-vertex from)))
;;      (add-vertex to other-vertices))))

;; (map edge-to (vertex-edges (make-vertex 'a '(b c (d e)))))
;;   -> (b c d)
;; edge-label: -> (() () e)



;; (define g empty-graph)
;; (define g (add-vertex 'a g))
;; (define g (add-edge 'b 'c g 'label1))
;; (map vertex-name g)
;; -> (b c a)
;; (map vertex-edges g)
;; -> ((#[edge 5]) () ())
;; (define g (add-edge 'b 'd g 'label2))
;; (map edge-to (vertex-edges (get-vertex 'b g)))
;; -> (d c)
;; (map edge-label (vertex-edges (get-vertex 'b g)))
;; -> (label2 label1)

;; (define (merge-vertices v1 v2)
;;   (if (not (equal? (vertex-name v1) (vertex-name v2)))
;;       (error "Cannot merge vertices with different names:" v1 v2)
;;       (fold-left (lambda (v e)
;;                    (add-edge-to-vertex e v))
;;                  v1
;;                  (vertex-edges v2))))

;; (define (merge-graphs . graphs)
;;   ;; merge all graphs into the first one and return it
;;   (if (null? (cdr graphs))
;;       (car graphs)
;;       (let ((g1 (car graphs))
;;             (g2 (cadr graphs)))
;;         (pp g1)
;;         (pp g2)
;;         (if (null? g2)
;;             (apply merge-graphs (cons g1 (cddr graphs)))
;;             (let* ((vertex (car g2))    ; first vertex in other graph
;;                    (new-graph 
;;                     (if (get-vertex (vertex-name vertex) g1)
;;                         (cons 
;;                          (merge-vertices (get-vertex (vertex-name vertex) g1)
;;                                          vertex)
;;                          (remove-vertex (vertex-name vertex) g1))
;;                         (cons vertex g1))))
;;               (apply merge-graphs (append (list new-graph (cdr g2))
;;                                           (cddr graphs))))))))


;; (draw-graph (merge-graphs (add-edge 'a 'b empty-graph)
;;                           (add-edge 'a 'c empty-graph))
;;             "./out.dot")

(define (graph->dot g)
  (map
   (lambda (p)
     (let ((v  (car p))
           (id (cdr p)))
       (string-append
        ;; content
        (if (graph? (content v))
            (string-append
             "subgraph" id "{\n"
             (dot->graph (content v))
             "}")
            (string-append
             (symbol->string (vertex-content v)) ";\n"))
        ;; edges
        (apply string-append
               (fold-left ;; flatten
                append '()
                (map (lambda (v)
                       (map (lambda (e)
                              (string-append
                               (number->string id)
                               " -> "
                               (symbol->string (edge-to e))
                               ";\n"))
                            (vertex-edges v)))
                     g)))
        ))))
  )

(define (enumerate l)
  (map cons l (iota (length l) 0 1)))

(define (draw-graph g target-file)
  (with-output-to-file target-file (lambda ()
                                     (display (graph->dot g)))))

;; (draw-graph
;;  (add-edge 'b 'd
;;            (add-edge 'b 'c
;;                      (add-vertex 'a empty-graph)
;;                      'label1)
;;            'label2)
;;  "./out.dot")
