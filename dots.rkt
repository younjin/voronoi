#lang racket

(require plot)

;; Given a finite set of points in a closed and bounded R2 region,
;; this function should find the Voronoi diagram of the area.

;; I will start with trying to remember Racket and induct on number of
;; points, starting with the base case of two.

;; Disclaimer: this is the most inefficient code I have (probably?)
;; ever written. Apologies for the rusty Racket!

;; dots
;; IN: list of coordinates in R2
;; OUT: plot of input coordinates with axes from 0 to appropriate max

(define (dots list)
  (define xs (map (lambda x (first (first x))) list))
  (define ys (map (lambda y (second (first y))) list))
  (define mx -inf.0)
  
  (define (maxlist L)
    (cond
      [(empty? L) mx]
      [(> (first L) mx) (set! mx (first L)) (maxlist (rest L))]
      [else (maxlist (rest L))]))
  
  (define xmax (maxlist xs))
  (define ymax (maxlist ys))
  
  (parameterize ([plot-width 200]
                 [plot-height 200]
                 [plot-x-label #f]
                 [plot-y-label #f])
    (plot (points (map vector xs ys)
                  #:x-min 0 #:x-max (* 1.2 xmax)
                  #:y-min 0 #:y-max (* 1.2 ymax)))))

;; sample: try running with (dots (list (list 1 2) (list 3 4) (list 5 6)))