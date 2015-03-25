#lang typed/racket

(provide sort-by-distance)

(: distance (-> (Vector Real Real) (Vector Real Real) Real))
(define (distance p1 p2)
  (sqrt (+ (sqr (- (vector-ref p2 0)
                   (vector-ref p1 0)))
           (sqr (- (vector-ref p2 1)
                   (vector-ref p1 1))))))

(: make-distance<? (-> (Vector Real Real) (-> (Vector Real Real) (Vector Real Real) Boolean)))
(define (make-distance<? p)
  (λ (p1 p2)
     (< (distance p1 p)
        (distance p2 p))))

(: nearest (-> (Vector Real Real) (Listof (Vector Real Real)) (Vector Real Real)))
(define (nearest p ps)
  (cond [(empty? ps) p]
        [else (first (sort ps
                           (make-distance<? p)))]))

(: sort-by-distance-helper (-> (Vector Real Real) (Listof (Vector Real Real)) (Listof (Vector Real Real))))
(define (sort-by-distance-helper p ps)
  (cond [(empty? ps) ps]
        [else
          (let ([nearest-p (nearest p ps)])
                (cons nearest-p
                      (sort-by-distance-helper
                        nearest-p
                        (remove nearest-p ps))))]))

(: sort-by-distance (-> (Listof (Vector Real Real)) (Listof (Vector Real Real))))
;; Example:
;; > (sort-by-distance (list (vector 1 2) (vector 5 5) (vector 1 1)))
;; '(#(1 1) #(1 2) #(5 5))
(define (sort-by-distance ps)
  (sort-by-distance-helper (vector 0.0 0.0) ps))
