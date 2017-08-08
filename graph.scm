(define-record-type <vertex>
  (%make-vertex name edges)
  vertex?
  (name vertex-name)
  (edges vertex-edges))
(define (make-vertex name #!optional edges)
  (%make-vertex name (if (default-object? edges)
                         '()
                         (map (lambda (e)
                                (cond ((pair? e) ; '(to label)
                                       (let ((to (car to))
                                             (label (cadr to)))
                                         (make-edge name to label)))
                                      ((symbol? e) ; 'to
                                       (make-edge name to))
                                      ((edge? e) ; already an edge
                                       e)
                                      (else
                                       (error "unrecognized edge" e))))
                              edges))))

(define-record-type <edge>
  (%make-edge from to label)
  edge?
  (from edge-from)                      ; redundant but will make it easier later
  (to edge-to)
  (label edge-label))
(define (make-edge from to #!optional label)
  (%make-edge from to label))

;; API
(define (graph? o)
  (and (list? o)
       (vertex? (car o))))

(define empty-graph '())

(define (get-vertex name g)
  (cond ((null? g) #f)
        ((equal? (vertex-name (car g)) name) (car g))
        (else (get-vertex name (cdr g)))))

(define (add-vertex name g)
  (if (get-vertex name g)
      g
      (cons (make-vertex name) g)))

(define (remove-vertex name g)
  (remove (lambda (v) (equal? (vertex-name v) name)) g))

(define (add-edge-to-vertex edge vertex)
  ;; fixme - prevent duplicates
  (make-vertex (vertex-name vertex) (cons edge (vertex-edges vertex))))

(define (add-edge from to g #!optional label) 
  (let ((from-vertex (get-vertex from g)) 
        (new-edge (make-edge from to label))
        (other-vertices (remove-vertex from g)))
    (cons                               ; implementation leak
     (add-edge-to-vertex new-edge (if from-vertex
                                      from-vertex
                                      (make-vertex from)))
     (add-vertex to other-vertices))))

;; (map edge-to (vertex-edges (make-vertex 'a '(b c (d e)))))
;;   -> (b c d)
;; edge-label: -> (() () e)



(define g empty-graph)
(define g (add-vertex 'a g))
(define g (add-edge 'b 'c g 'label1))
(map vertex-name g)
;; -> (b c a)
(map vertex-edges g)
;; -> ((#[edge 5]) () ())
(define g (add-edge 'b 'd g 'label2))
(map edge-to (vertex-edges (get-vertex 'b g)))
;; -> (d c)
(map edge-label (vertex-edges (get-vertex 'b g)))
;; -> (label2 label1)

(define (merge-vertices v1 v2)
  (if (not (equal? (vertex-name v1) (vertex-name v2)))
      (error "Cannot merge vertices with different names:" v1 v2)
      (fold-left (lambda (v e)
                   (add-edge-to-vertex e v))
                 v1
                 (vertex-edges v2))))

(define (merge-graphs . graphs)
  ;; merge all graphs into the first one and return it
  (if (null? (cdr graphs))
      (car graphs)
      (let ((g1 (car graphs))
            (g2 (cadr graphs)))
        (pp g1)
        (pp g2)
        (if (null? g2)
            (apply merge-graphs (cons g1 (cddr graphs)))
            (let* ((vertex (car g2))    ; first vertex in other graph
                   (new-graph 
                    (if (get-vertex (vertex-name vertex) g1)
                        (cons 
                         (merge-vertices (get-vertex (vertex-name vertex) g1)
                                         vertex)
                         (remove-vertex (vertex-name vertex) g1))
                        (cons vertex g1))))
              (apply merge-graphs (append (list new-graph (cdr g2))
                                          (cddr graphs))))))))


;; (draw-graph (merge-graphs (add-edge 'a 'b empty-graph)
;;                           (add-edge 'a 'c empty-graph))
;;             "./out.dot")

(define (graph->dot g)
  (string-append
   "strict digraph {\n"
   ;; vertices
   (apply string-append
          (map (lambda (v)
                 (string-append (symbol->string (vertex-name v)) ";\n"))
               g))
   ;; edges
   (apply string-append
          (fold-left
           append '()
           (map (lambda (v)
                  (map (lambda (e)
                         (string-append
                          (symbol->string (vertex-name v))
                          " -> "
                          (symbol->string (edge-to e))
                          ";\n"))
                       (vertex-edges v)))
                g)))
   "}"))

(define (draw-graph g target-file)
  (with-output-to-file target-file (lambda ()
                                     (display (graph->dot g)))))

(draw-graph
 (add-edge 'b 'd
           (add-edge 'b 'c
                     (add-vertex 'a empty-graph)
                     'label1)
           'label2)
 "./out.dot")
