;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname recognize) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; ***************************************************
;; Yarin Litner (Student ID: 20909301)
;; CS 135 Fall 2020
;; Assignment 04, Problem 3
;; ***************************************************

(require "templates.rkt")

;; "templates.rkt" provides templates, a TemplateLibrary (see data definition)
;; It also provides the following test gestures for your recognizer: 
;;    testd testk tests testy testa testt

;; A Point is a (list Num Num)

;; A Gesture is a (listof (list Num Num))

;; A BoundingBox (BB) is a (list Point Point)
;; requires: the coordinate values in the first point are less than the
;;             respective values in the second point

;; A TemplateLibrary (TL) is a (listof (list Sym Gesture))
;; requires: the list is non-empty
;;           each Sym key is unqiue
;;           each Gesture value is not both vertical and horizontal

;; An AL (association list) is a `(listof (list Point Int))` 
;;   Requires: each key (Int) is unique

;; A ML (match list) is a '(listof (list Sym Num))'
;;   Requires: each key (Sym) is unique

;; A Line is a (list Sym Num)
       
;; 3a)

;; i)

;; (get-x p) consumes a Point p and produces its x-coordinate.
;; Examples:
(check-expect (get-x (list 7 3)) 7)

;; get-x: Point -> Num
(define (get-x p)
  (first p))

;; (get-y p) consumes a Point p and produces its y-coordinate.
;; Examples:
(check-expect (get-y (list 7 3)) 3)

;; get-y: Point -> Num
(define (get-y p)
  (second p))

;; ii)

(define test (list (list 100 0)
                   (list 200 100)
                   (list 100 200)
                   (list 0 100)
                   (list 100 0)))
(define translated-test (list (list 150 50)
                              (list 250 150)
                              (list 150 250)
                              (list 50 150)
                              (list 150 50)))
(define scaled-test (list (list 5000 0)
                          (list 10000 5000)
                          (list 5000 10000)
                          (list 0 5000)
                          (list 5000 0)))

;; (translate-gesture g x-offset y-offset) consumes a Gesture g and two
;;      numbers, an x-offset and a y-offset, and produces a translated gesture.
;; Examples:
(check-expect (translate-gesture test 50 50) translated-test)

;; translate-gesture: Gesture Num Num -> Gesture
(define (translate-gesture g x-offset y-offset)
  (cond[(empty? g) empty]
       [else (cons (list (+ (get-x (first g)) x-offset)
                         (+ (get-y (first g)) y-offset))
                   (translate-gesture (rest g) x-offset y-offset))]))

;; iii)

;; (scale-gesture g x-scale y-scale) consumes a Gesture g and two
;;      numbers, an x-scale and a y-scale, and produces a scaled gesture.
;; Examples:
(check-expect (scale-gesture test 50 50) scaled-test)

;; scale-gesture: Gesture Num Num -> Gesture
(define (scale-gesture g x-scale y-scale)
  (cond[(empty? g) empty]
       [else (cons (list (* (get-x (first g)) x-scale)
                         (* (get-y (first g)) y-scale))
                   (scale-gesture (rest g) x-scale y-scale))]))

;; iv)

;; (get-b-box gesture) consumes consumes a non-empty
;;          Gesture and produces the gesture’s BoundingBox, as defined above.
;; Examples:
(check-expect (get-b-box (list (list 1 1))) (list (list 1 1) (list 1 1)))
(check-expect (get-b-box test) (list (list 0 0) (list 200 200)))

;; get-b-box: Gesture -> BB
;; Requires: Gesture is non-empty
(define (get-b-box gesture)
  (list (list (smallest (gesture->lox gesture))
              (smallest (gesture->loy gesture)))
        (list (largest (gesture->lox gesture))
              (largest (gesture->loy gesture)))))

;; (gesture->lox gesture) consumes a gesture and produces a list of all
;;      x-coordinates of its points.
;; Examples:
(check-expect (gesture->lox test) (list 100 200 100 0 100))

;; gesture->lox: Gesture -> (listof Num)
(define (gesture->lox gesture)
  (cond[(empty? (rest gesture)) (cons (first (first gesture)) empty)]
       [else (cons (first (first gesture)) (gesture->lox (rest gesture)))]))

;; (gesture->loy gesture) consumes a gesture and produces a list of all
;;      y-coordinates of its points.
;; Examples:
(check-expect (gesture->loy test) (list 0 100 200 100 0))

;; gesture->loy: Gesture -> (listof Num)
(define (gesture->loy gesture)
  (cond[(empty? (rest gesture)) (cons (second (first gesture)) empty)]
       [else (cons (second (first gesture)) (gesture->loy (rest gesture)))]))

;; (largest nums) produces the largest number in the list nums.
;; Examples:
(check-expect (largest (cons 7 (cons 9 empty))) 9)

;; largest: (ne-listof Num) -> Num
(define (largest nums)
  (cond[(empty? (rest nums)) (first nums)]
       [(> (first nums) (largest (rest nums))) (first nums)]
       [else (largest (rest nums))]))

;; (smallest nums) produces the smallest number in the list nums.
;; Examples:
(check-expect (smallest (cons 7 (cons 9 empty))) 7)

;; smallest: (ne-listof Num) -> Num
(define (smallest nums)
  (cond[(empty? (rest nums)) (first nums)]
       [(< (first nums) (smallest (rest nums))) (first nums)]
       [else (smallest (rest nums))]))

;; 3bi)

(define mygest (list (list 100 0) (list 200 100) (list 100 200)
(list 0 100) (list 100 50)))
(define test2 (list (list 5 6) (list -3 -4) (list 0 0)))

;; (gesture-length gesture) consumes a Gesture and calculates its length.
;; Examples:
(check-within (gesture-length test) (sqrt 320000) 0.01)
(check-within (gesture-length test2) (+ (sqrt 164) 5) 0.01)

;; gesture-length: Gesture -> Num
(define (gesture-length gesture)
  (cond [(empty? gesture) 0]
        [(empty? (rest gesture)) 0]
        [else (+ (euclidean (first gesture) (second gesture))
                 (gesture-length (rest gesture)))]))

;; Tests
(check-within (gesture-length (list )) 0 0.01)
(check-within (gesture-length (list (list 0 0))) 0 0.01)

;; (euclidean p1 p2) calculates the distance between two Points, p1 and p2.
;; Examples:
(check-within (euclidean (list 3 4) (list 7 7)) 5 0.01)

;; euclidean: Point Point -> Num
(define (euclidean p1 p2)
  (sqrt (+ (expt (- (get-x p2) (get-x p1)) 2)
           (expt (- (get-y p2) (get-y p1)) 2))))

;; 3bii)

;; (get-points g lon) consumes a Gesture, g, and a list of Nat, lon, and
;;         produces a Gesture where each Point in the produced Gesture is
;;         indexed by one element of the list of Nat consumed.
;; Examples:
(check-expect (get-points mygest (list 0 0 2 4 4))
              (list (list 100 0) (list 100 0) (list 100 200) (list 100 50)
                    (list 100 50)))
(check-expect (get-points mygest (list 0 4))
              (list (list 100 0) (list 100 50)))

;; get-points: Gesture (listof Nat) -> Gesture
;; Requires: lon must be a non-decreasing list of Nat in the range [0...(n−1)],
;;           where n is the number of Points in g.       
(define (get-points g lon)
  (cond[(empty? lon) empty]
       [(empty? (rest lon)) (cons (find (indexer g 0) (first lon)) empty)]
       [else (cons (find (indexer g 0) (first lon))
                   (get-points g (rest lon)))]))

;; Tests:
(check-expect (get-points mygest (list )) empty)
(check-expect (get-points mygest (list 0)) (list (list 100 0)))
(check-expect (get-points mygest (list 4)) (list (list 100 50)))
(check-expect (get-points mygest (list 0 1 2 3 4)) mygest)
(check-expect (get-points mygest (list 0 1 2 2 3 3 4))
              (list (list 100 0) (list 200 100) (list 100 200)
                    (list 100 200) (list 0 100) (list 0 100) (list 100 50)))
(check-expect (get-points (list (list 0 0)) (list 0)) (list (list 0 0)))

;; (indexer g i) consumes a Gesture, g, and an Int, i, and produces a
;;       an AL that pairs each point from g with an integer from
;;       [i...(n-1)] where n is the number of points in g. 
;; Examples:
(check-expect (indexer test2 0) (list (list (list 5 6) 0)
                                      (list (list -3 -4) 1)
                                      (list (list 0 0) 2)))

;; indexer: Gesture Int -> AL           
(define (indexer g i)
  (cond[(empty? (rest g))
        (cons (list (first g) i) empty)]
       [else (cons (list (first g) i) (indexer (rest g) (add1 i)))]))

;; (find al i) consumes an AL, al, and an integer, i, and produces the
;;             corresponding value for i in the association list. 
;; Examples:
(check-expect (find (indexer mygest 0) 3) (list 0 100))

;; find: AL Int -> Point
;; Requires: i must be a valid key in al.
(define (find al i)
  (cond[(empty? (rest al)) (first (first al))]
       [(= (second (first al)) i) (first (first al))]
       [else (find (rest al) i)]))             

;; 3c)

;; 3ci)
;; (five-sample gesture) produces a sampling of gesture 5 points
;;  the first, n/4th, n/2th, 3n/4th, and last point.
;; Examples:
(check-expect (five-sample (list (list 1 1) (list 2 2)))
              (list (list 1 1) (list 1 1) (list 2 2) (list 2 2) (list 2 2)))
(check-expect (five-sample (list (list 1 1) (list 2 2) (list 3 3) (list 4 4)
                                (list 5 5) (list 6 6) (list 7 7) (list 8 8)))
              (list (list 1 1) (list 3 3) (list 5 5) (list 7 7) (list 8 8)))

;; five-sample: Gesture -> Gesture
;; requires: gesture is non-empty
(define (five-sample g)
  (get-points g (list 0
                      (floor (* 0.25 (length g)))
                      (floor (* 0.5 (length g)))
                      (floor (* 0.75 (length g)))
                      (length g))))

;; Tests:
(check-expect (five-sample (list (list 1 1) (list 2 2) (list 3 3) (list 4 4)))
              (list (list 1 1) (list 2 2) (list 3 3) (list 4 4) (list 4 4)))
(check-expect (five-sample (list (list 1 1)))
              (list (list 1 1) (list 1 1) (list 1 1) (list 1 1) (list 1 1)))
(check-expect (five-sample (list (list 1 1) (list 2 2) (list 3 3) (list 4 4)
                                 (list 5 5)))
              (list (list 1 1) (list 2 2) (list 3 3) (list 4 4) (list 5 5)))

;; 3cii)

;;(move-and-scale gesture x-scale y-scale) moves gesture to (0, 0) and
;;  scales it by (x-scale)x(y-scale)
;; Examples:
(check-expect (move-and-scale (list (list 1 1)) 1 1) (list (list 0 0)))
(check-expect (move-and-scale (list (list 1 5) (list 3 4)) 1 2)
              (list (list 0 2) (list 2 0)))

;; move-and-scale: Gesture Num Num -> Gesture
;; requires: gesture is non-empty
;;           x-scale > 0
;;           y-scale > 0
(define (move-and-scale g x-scale y-scale)
  (scale-gesture (translate-gesture g (* -1 (first (first (get-b-box g))))
                      (* -1 (second (first (get-b-box g)))))
         x-scale y-scale))

;; Test:
(check-expect (move-and-scale (list (list 5 5) (list 2 2)) 3 0.5)
              (list (list 9 1.5) (list 0 0)))



;; 3ciii)

(define min-width 30)
(define min-height 30)
(define norm-size 200)

;;(normalize-gesture gesture) normalizes gesture to (0,0) and a standard size
;; Examples:
(check-within (normalize-gesture (list (list 0 0) (list 100 100)))
              (list (list 0 0) (list 200 200)) 0.01)
(check-within (normalize-gesture (list (list 100 0) (list 100 50)
                                       (list 200 50)))
              (list (list 0 0) (list 0 200) (list 200 200)) 0.01)

;; normalize-gesture: Gesture -> Gesture
;; requires: gesture is not both vertical and horizontal
;;           gesture is non-empty
(define (normalize-gesture g)
  (cond[(and (< (dx g) min-width) (< (dy g) min-height))
        (move-and-scale g 1 1)]
       [(< (dx g) min-width)
        (move-and-scale g 1 (/ norm-size (dy g)))]
       [(< (dy g) min-height)
        (move-and-scale g (/ norm-size (dx g)) 1)]
       [else
        (move-and-scale g (/ norm-size (dx g)) (/ norm-size (dy g)))]))

;; Tests:
(check-within (normalize-gesture (list (list 0 10) (list 12 24)))
                                 (list (list 0 0) (list 12 14)) 0.01)
(check-within (normalize-gesture (list (list 0 0) (list 100 30)))
              (list (list 0 0) (list 200 200)) 0.01)
(check-within (normalize-gesture (list (list 0 0) (list 100 29)))
              (list (list 0 0) (list 200 29)) 0.01)
(check-within (normalize-gesture (list (list 0 0) (list 30 100)))
              (list (list 0 0) (list 200 200)) 0.01)
(check-within (normalize-gesture (list (list 0 0) (list 29 100)))
              (list (list 0 0) (list 29 200)) 0.01)
(check-within (normalize-gesture (list (list 0 0) (list 400 400)))
              (list (list 0 0) (list 200 200)) 0.01)

;; (dx g) consumes a Gesture, g, and produces the width of its Bounding Box.
;; Examples:
(check-expect (dx test) 200)

;; dx: Gesture -> Num
(define (dx g)
  (abs (- (first (first (get-b-box g)))
           (first (second (get-b-box g))))))

;; (dy g) consumes a Gesture, g, and produces the height of its Bounding Box.
;; Examples:
(check-expect (dy test) 200)

;; dy: Gesture -> Num
(define (dy g)
  (abs (- (second (first (get-b-box g)))
          (second (second (get-b-box g))))))

;; 3civ)

;;(geometric-5match gesture1 gesture2) produces the average distance between
;;  points in sub-sampled gesture1 and gesture2 after sub-sampling them with
;;  k points
;; Examples:
(check-within (geometric-5match
               (list (list 10 10) (list 30 30) (list 50 50)
                     (list 70 70) (list 80 80))
               (list (list 10 10) (list 20 20) (list 30 30)
                     (list 40 40) (list 40 40)))
               16.16 0.01)

;; geometric-5match: Gesture Gesture -> Num
;; requires: gesture1 and gesture2 are each not both vertical and horizontal
(define (geometric-5match g1 g2)
  (/ (d-sum (normalize-gesture (five-sample g1))
            (normalize-gesture (five-sample g2))) 5))

;; Tests:
(check-within (geometric-5match (second (fourth templates))
                                (second (fourth templates))) 0 0.1)

;; (d-sum g1 g2) calculates the sum of distances between corresponding points
;;       in Gestures g1 and g2.
;; Examples:
(check-within (d-sum (list (list 3 4) (list 5 7))
                     (list (list 3 6) (list 5 6))) 3 0.01)

;; d-sum: Gesture Gesture -> Num
;; Requires: g1 and g2 are non-empty and have the same amount of Points.
(define (d-sum g1 g2)
  (cond[(empty? (rest g1)) (euclidean (first g1) (first g2))]
       [else (+ (euclidean (first g1) (first g2))
                (d-sum (rest g1) (rest g2)))])) 

;; 3cv)

;; (five-point-rec candidate template-library) produces the symbol in
;;  template-library closest to candidate
;; Examples:
(check-expect (five-point-rec testd templates) 'd)
(check-expect (five-point-rec testk templates) 'k)


;; five-point-rec Gesture TL -> Sym
;; requires: candidate is not both vertical and horizontal
(define (five-point-rec g temp)
  (first (find-smallest (first (match5-list g temp)) (match5-list g temp))))

;; Tests
(check-expect (five-point-rec tests templates) 's)
(check-expect (five-point-rec testy templates) 'y)

;; (match5-list g temp) consumes a Gesture g, and a TL temp, and produces
;;      a ML with a symbol from the TL corresponding to
;;      the value for their geometric-5match.
;; Examples:
(check-within (first (matchk-list tests templates 5)) (list 'a 130.728) 0.01) 

;; match5-list: Gesture TL -> ML
;; Requires: g and temp are non-empty
(define (match5-list g temp)
  (cond[(empty? (rest temp))
        (cons (list (first (first temp))
                    (geometric-5match g (second (first temp)))) empty)]
       [else (cons (list (first (first temp))
                         (geometric-5match g (second (first temp))))
                   (match5-list g (rest temp)))]))

;; (smaller-line line1 line2) consumes two Lines, line1 and line2, and produces
;;      false if line1 has a greater number and true otherwise.
;; Examples:
(check-expect (smaller-line (list 'c 0.56) (list 'd 0.76)) true)

;; smaller-line: Line Line -> Bool
(define (smaller-line line1 line2)
  (cond[(<= (second line1) (second line2)) true]
       [else false]))

;; (find-smallest line ml) consumes a Line, line, and a ML, ml, and produces
;;     the smallest Line in ml.
;; Examples:
(check-within (find-smallest (first (match5-list tests templates))
                             (match5-list tests templates)) (list 's 0) 0.01) 

;; find-smallest: Line ML -> Line
(define (find-smallest line ml)
  (cond[(empty? ml) line]
       [(smaller-line line (first ml)) (find-smallest line (rest ml))]
       [else (find-smallest (first ml) (rest ml))]))
              
;; 3d)
(define test3 (list (list 1 1) (list 2 2) (list 3 3)))
(define test4 (list (list 40 4) (list 50 5) (list 60 6)))

;; (sub-sample g k)  consumes a Gesture of an arbitrary number of Points and
;;      produces a new Gesture that has exactly k points.
;; Examples:
(check-expect (sub-sample (list (list 1 1) (list 2 2)) 5)
              (list (list 1 1) (list 1 1) (list 2 2) (list 2 2) (list 2 2)))
(check-expect (sub-sample test3 7)
              (list (list 1 1) (list 1 1) (list 2 2)
                    (list 2 2) (list 3 3) (list 3 3) (list 3 3))) 

;; sub-sample: Gesture Nat -> Gesture
;; Requires:
;;    k > 2
;;    g is non-empty
(define (sub-sample g k)
  (get-points g (sub-sample2 g k 0)))

;; Tests:
(check-expect (sub-sample test3 3) test3)
(check-expect (sub-sample test3 5)
              (list (list 1 1) (list 1 1) (list 2 2) (list 3 3) (list 3 3)))
(check-expect (sub-sample (list (list 1 1) (list 2 2) (list 3 3)
                                (list 4 4) (list 5 5)) 3)
              (list (list 1 1) (list 3 3) (list 5 5)))
(check-expect (sub-sample (list (list 1 1)) 3)
              (list (list 1 1) (list 1 1) (list 1 1)))

;; (sub-sample2 g k i)  consumes a Gesture of an arbitrary number of Points and
;;      produces a new Gesture that has exactly k points.
;; Examples and tests: see wrapper function sub-sample

;; sub-sample2: Gesture Nat Nat -> (listof Nat)
;; Requires:
;;    k > 2
;;    g is non-empty
(define (sub-sample2 g k i)
  (cond[(= (sub1 k) i) (cons (sub1 (length g)) empty)]
       [(= i 0) (cons 0 (sub-sample2 g k (add1 i)))]
       [else (cons (floor (* (* (/ 1 (sub1 k)) i) (length g)))
                   (sub-sample2 g k (add1 i)))])) 

;; (geometric-match g1 g2 k) produces the average distance between
;; points in sub-sampled gesture1 and gesture2 after sub-sampling them with
;; k points
;; Examples:
(check-within (geometric-match test3 test4 7) 10.286 0.01)
(check-within (geometric-match test3
              (list (list 4 4) (list 5 5) (list 6 6)) 4) 0 0.01)

;; geometric-match: Gesture Gesture Nat -> Num
;; Requires:
;;    k > 2
;;    g1 and g2 are non-empty
(define (geometric-match g1 g2 k)
  (/ (d-sum (normalize-gesture (sub-sample g1 k))
            (normalize-gesture (sub-sample g2 k))) k))

;; Tests:
(check-within (geometric-match (list (list 170 170) (list 120 120))
                               (list (list 42 154) (list 93 94)) 3)
              200 0.01)

;; (k-point-rec g temp k) produces the symbol in
;;  template-library closest to candidate
;; Examples:
(check-expect (k-point-rec testk templates 6) 'k)
(check-expect (k-point-rec tests templates 10) 's)

;; k-point-rec: Gesture TL Nat -> Sym
;; Requires:
;;   k > 2
;;   g and temp are non-empty
(define (k-point-rec g temp k)
  (first (find-smallest (first (matchk-list g temp k)) (matchk-list g temp k))))

;; Tests
(check-expect (k-point-rec testa templates 3) 'a)
(check-expect (k-point-rec testt templates 7) 't)
(check-expect (k-point-rec testy templates 7) 'y)

;; (matchk-list g temp k) consumes a Gesture g, a TL temp, and a Nat, k,
;;      and produces a ML with a symbol from the TL corresponding to
;;      the value for their geometric-match.
;; Examples:
(check-within (first (matchk-list tests templates 5)) (list 'a 130.728) 0.01)

;; matchk-list: Gesture TL Nat -> ML
;; Requires:
;;   k > 2
;;   g and temp are non-empty
(define (matchk-list g temp k)
  (cond[(empty? (rest temp))
        (cons (list (first (first temp))
                    (geometric-match g (second (first temp)) k)) empty)]
       [else (cons (list (first (first temp))
                         (geometric-match g (second (first temp)) k))
                   (matchk-list g (rest temp) k))]))


