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

;; Okay, it took about three hours to write a function
;; that would plot the input coordinates. I am super
;; duper rusty. Time to step up my game: I need to figure
;; out how to determine the midpoints between the given
;; coordinates, and plot them (different shapes?).

;; midpoint
;; IN: two coordinates in R2
;; OUT: midpoint between the coordinates

(define (midpt c1 c2)
  (define (mid a b pos)
    (/ (+ (pos a) (pos b)) 2))
  (define midx (mid c1 c2 first))
  (printf "~a\n" midx)
  (define midy (mid c1 c2 second))
  (printf "~a\n" midy))

;; orbs
;; IN: list of coordinates in R2
;; OUT: plot of input coordinates, but with orbs surrounding
;; each coordinate. radius to be calculated somehow.

(define (orbs list)
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
    ;(list 
    (plot (points (map vector xs ys)
                  #:x-min 0 #:x-max (* 1.2 xmax)
                  #:y-min 0 #:y-max (* 1.2 ymax)
    ;(plot (points (map vector xs ys)
                  #:size 125
                  #:color 'blue
                  #:alpha 0.8))))

;; sample:
;; (dots (list (list 1 2) (list 3 5) (list 4 1)))
;; shows the input coordinates by themselves
;; (orbs (list (list 1 2) (list 3 5) (list 4 1)))
;; shows the regions surrounding the coordinates

;; Next steps:
;; Encompass the entire given region.
;; Make the overlaps duke it out (i.e. split the regions in half).
;; Show both the original points and the orbs.
;; Pretty colours.