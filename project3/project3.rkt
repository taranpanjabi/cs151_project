#lang typed/racket
(require "../include/cs151-core.rkt")
(require "../include/cs151-image.rkt")
(require "../include/cs151-big-bang.rkt")
(require "../include/cs151-universe.rkt")
(require (only-in typed/racket/gui/base put-file get-file))
(require typed/test-engine/racket-tests) 

;; class done under Adam Shaw

(define-type Player (U 'Black 'White))

(define-struct OccupiedPoint
  ([color : Player]
   [count : Integer]))

(define-type Point (U OccupiedPoint 'EmptyPoint))

(define-struct Game
  ([board : Board]
   [turn : Player]
   [moves : (Listof Integer)]))

(define-struct Board
  ([points : (Listof Point)]
   [black-bar : Integer]
   [white-bar : Integer]
   [black-off : Integer]
   [white-off : Integer]))


(define-struct Style
  ([checker-radius : Integer]
   [spacing : Integer]
   [black-checker : (Integer -> Image)]
   [white-checker : (Integer -> Image)]
   [dark-point : (Integer Boolean -> Image)]
   [light-point : (Integer Boolean -> Image)]
   [background : (Integer Integer -> Image)]
   [label : (String -> Image)]
   [black-die : (Integer Integer -> Image)]
   [white-die : (Integer Integer -> Image)]))


(define-struct PointNum
  ([num : Integer]))

(define-type ClickLoc (U PointNum 'BlackBar 'WhiteBar 'BlackOff 'WhiteOff
                         'BlackDice 'WhiteDice 'Nowhere))

(define-type BoardLoc (U PointNum 'BlackBar 'WhiteBar 'BlackOff 'WhiteOff
                         'Nowhere))


(define-struct World
  ([game : Game]
   [style : Style]
   [whited1 : Integer]
   [whited2 : Integer]
   [blackd1 : Integer]
   [blackd2 : Integer]
   [last-click-1 : ClickLoc]
   [last-click-2 : BoardLoc]
   [last-click-3 : BoardLoc]
   [history : (Listof Game)]))
   

(define point-test (OccupiedPoint 'Black  3))

(: black-checker (Integer -> Image))
(define (black-checker rad)
  (circle rad 'solid 'sandybrown))
;;Eyeball test passed
;;(black-checker 10)
(: white-checker (Integer -> Image))
(define (white-checker rad)
  (circle rad 'solid 'navajowhite))
;;Eyeball test passed
;;(white-checker 10)

;;draws dark triangles
;;takes in radius and boolean and outputs Image
;;accounts for direction of point
(: dark-point (Integer Boolean -> Image))
(define (dark-point rad upward)
  (match upward
    [#t  (beside (flip-horizontal
                  (right-triangle rad (* rad 10) 'solid 'royalblue))
                 (right-triangle rad (* rad 10) 'solid 'royalblue))]
    [else (rotate 180
                  (beside (flip-horizontal
                           (right-triangle rad (* rad 10) 'solid 'royalblue))
                          (right-triangle rad (* rad 10) 'solid 'royalblue)))]))
;;Eyeball test passed
;;(dark-point 12 #t)



;;draws light triangles
;;takes in radius and boolean and outputs Image
;;accounts for direction of point
(: light-point (Integer Boolean -> Image))
(define (light-point rad upward)
  (match upward
    [#t (beside (flip-horizontal
                 (right-triangle rad (* rad 10) 'solid 'lightblue))
                (right-triangle rad (* rad 10) 'solid 'lightblue))]
    [else (rotate 180
                  (beside (flip-horizontal
                           (right-triangle rad (* rad 10) 'solid 'lightblue))
                          (right-triangle rad (* rad 10) 'solid 'lightblue)))]))
;;Eyeball Test passed
;;(light-point 12 #t)



;;draws background for backgammon board
;;takes in padding and radius and outputs image
(: background (Integer Integer -> Image))
(define (background rad pad)
  (rectangle (+ (* 4 rad) (* 16 pad) (* 24 rad)) (* 28 rad)
             'solid 'whitesmoke))

;Eyeball Test Passed

;; creates label for side bar and mid bar
;;takes in string and outputs an Image
(: label (String -> Image))
(define (label string)
  (text string
        (guarantee-byte (exact-ceiling 20))'black))

;; example board struct. starting struct
(define test-board (Board
                    (list (OccupiedPoint 'Black 2) 'EmptyPoint 'EmptyPoint
                          'EmptyPoint 'EmptyPoint (OccupiedPoint 'White 5)
                          'EmptyPoint (OccupiedPoint 'White 3) 'EmptyPoint
                          'EmptyPoint 'EmptyPoint (OccupiedPoint 'Black 5)
                          (OccupiedPoint 'White 5) 'EmptyPoint 'EmptyPoint
                          'EmptyPoint (OccupiedPoint 'Black 3) 'EmptyPoint
                          (OccupiedPoint 'Black 5) 'EmptyPoint 'EmptyPoint
                          'EmptyPoint 'EmptyPoint (OccupiedPoint 'White 2))
                    2 3 2 5))
(define initial-board (Board
                       (list (OccupiedPoint 'Black 2) 'EmptyPoint 'EmptyPoint
                             'EmptyPoint 'EmptyPoint (OccupiedPoint 'White 5)
                             'EmptyPoint (OccupiedPoint 'White 3) 'EmptyPoint
                             'EmptyPoint 'EmptyPoint (OccupiedPoint 'Black 5)
                             (OccupiedPoint 'White 5) 'EmptyPoint 'EmptyPoint
                             'EmptyPoint (OccupiedPoint 'Black 3) 'EmptyPoint
                             (OccupiedPoint 'Black 5) 'EmptyPoint 'EmptyPoint
                             'EmptyPoint 'EmptyPoint (OccupiedPoint 'White 2))
                       0 0 0 0))



;;to garauntee number->string size is byte
;;given by prof
(: guarantee-byte (Integer -> Byte))
(define (guarantee-byte n)
  (if (byte? n) n (error "not a byte")))



;; Helper
;;upward facing or not
;; gives out boolean, if #t upwards facing triangle, else #f
;; inputs -> Integer. output, boolean
(: upward-facing (Integer -> Boolean))
(define (upward-facing pos)
  (cond
    [(<= pos 11) #t]
    [else #f]))

(check-expect (upward-facing 1) #t)
(check-expect (upward-facing 17) #f)
;; Helper
;;lighter color or not
;;gives out boolean, if should be lighter color then #t, else #f
;; inputs -> Integer. Ouptut -> Boolean

(: light-color (Integer -> Boolean))
(define (light-color pos)
  (cond
    [(= (remainder pos 2) 0) #f]
    [else #t]))

(check-expect( light-color 9) #t)
(check-expect( light-color 10) #f)


(: take (All (A) (Integer (Listof A) -> (Listof A))))
;; take n items from the front of the list
;; ex: (take 3 '(a b c d e f)) -> '(a b c)
;; no error if too many items are requested
;;taken from class
(define (take n xs)
  (cond
    [(<= n 0) '()]
    [(empty? xs) '()]
    [else (cons (first xs)
                (take (sub1 n) (rest xs)))]))

(check-expect (take 3 '(a b c d e)) '(a b c))
(check-expect (take 0 '(a b c)) '())
(check-expect (take 99 '(x y z)) '(x y z))

(: drop (All (A) (Integer (Listof A) -> (Listof A))))
;; drop n items from the front of the list
;; ex : (drop 3 '(a b c d e f)) -> '(d e f)
;; no error if too many items are requested
;;taken from class
(define (drop n xs)
  (cond
    [(<= n 0) xs]
    [(empty? xs) '()]
    [else (drop (sub1 n) (rest xs))]))

(check-expect (drop 3 '(a b c d e f)) '(d e f))
(check-expect (drop 99 '(a b c)) '())
(check-expect (drop 0 '(a b c)) '(a b c))

(: chop (All (A) (Integer (Listof A) -> (Listof (Listof A)))))
;; cut list into size-n sublists
;; ex: (chop 2 '(a b c d e f)) -> '((a b) (c d) (e f))
;; ex: (chop 4 '(a b c d e f)) -> '((a b c d) (e f))
;; no error if the number of items is not multiple of n
;;taken from class
(define (chop n xs)
  (cond
    [(empty? xs) '()]
    [else (cons (take n xs)
                (chop n (drop n xs)))]))

(check-expect (chop 2 '(a b c d e f)) '((a b) (c d) (e f)))
(check-expect (chop 2 '(a b c d e)) '((a b) (c d) (e)))

;;draws-triangle
;; Inputs white-index of triangle - to know direction of point
;; Inputs radisu of circle to know size of triangle
;; Takes into account direction and color
;; Outputs a triangle (:O SURPRISED)
(: draw-triangle (Integer Integer -> Image))
(define (draw-triangle rad index)
  (cond
    [(and (< index 12) (light-color index))
     (light-point rad #t)]
    [(and (< index 12) (not (light-color index)))
     (dark-point rad #t)]
    [(and (> index 11) (light-color index))
     (light-point rad #f)]
    [else 
     (dark-point rad #f)]))

;; eyeball test passed
;;(draw-triangle 15 10)
;; (draw-triangle 15 11)
;; (draw-triangle 15 14)

;;Helper to convert player to color
;;Inputs player and outputs color
(: play-to-color ( Point -> Image-Color))
(define (play-to-color point)
  (match point
    [(OccupiedPoint color count)
     (match color
       ['Black 'sandybrown]
       [else 'beige])]))

(check-expect (play-to-color (OccupiedPoint 'Black '4))'sandybrown)
(check-expect (play-to-color (OccupiedPoint 'White '4))'beige)             

;; Helper to draw stacked circles
;; Inputs - Integer for number of circles
;; Inputs - Integer for radius
;;Outputs - Image
(: draw-circles ( Integer Point -> Image))
(define (draw-circles rad point)
  (match point
    ['EmptyPoint empty-image]
    [(OccupiedPoint color count)
     (cond
       [(= count 0) empty-image]
       [(< count 6)
        (match color
          ['Black (above
                   (black-checker rad)
                   (draw-circles rad (OccupiedPoint color (- count 1))))]
          [else (above
                 (white-checker rad)
                 (draw-circles rad (OccupiedPoint color (- count 1))))])]
       [else (overlay/align "middle" "top" (label (number->string count))                                  
                            (draw-circles rad (OccupiedPoint color 5)))])]))
                                                                          

;;Eyeball test passed
;;(draw-circles 19 (OccupiedPoint 'Black 4))
;;(draw-circles 9 (OccupiedPoint 'White 3))
;;(draw-circles 19 (OccupiedPoint 'Black 9))


;;function to draw circle-on-point
;; Inputs - Point rad index
;; Outputs- Image
(: circle-on-point (Integer Point Integer -> Image))
(define (circle-on-point rad point index)
  (cond
    [(upward-facing index) (overlay/align "middle" "bottom"
                                          (draw-circles rad point)
                                          (draw-triangle rad index))]
    [else (overlay/align "middle" "top"
                         (draw-circles rad point)
                         (draw-triangle rad index))]))

;;Eyeball Test passed
;;(circle-on-point 17 point-test 14)
;;(circle-on-point 15 point-test 19)                        
;;(circle-on-point 15 point-test 1)


;;HELPER - draws black checkers off board
;;inputs Int and Board and Outputs Image
(: side-black (Integer Board -> Image))
(define (side-black rad board)
  (match board
    [(Board _ _ _ black-off _)
     (cond
       [(< 0 black-off)(overlay (label (number->string black-off))
                                (black-checker rad))]
       [else empty-image])]))
;;Visual Test Passed


;;Similiar HELPER - draws white checkers off board
;;inputs Int and Board and Outputs Image

(: side-white (Integer Board -> Image))
(define (side-white rad board)
  (match board
    [(Board _ _ _ _ white-off)
     (cond
       [(< 0 white-off)(overlay (label (number->string white-off))
                                (white-checker rad))]
       [else empty-image])]))

;;Draws side-bar depicting pieces off.
;; Uses two helpers right above.
;;Inputs Integer and Board and outputs Image
(: side-bar (Integer Board -> Image))
(define ( side-bar rad board)
  (overlay/align "middle" "middle"
                 (above (side-black rad board)
                        (side-white rad board))
                 (rectangle (* 4 rad) (* 28 rad) 'solid 'pink)))

;;Eyeball Test
;(side-bar 15 (Board '() 1 1 3 2))



;;draws top left points and checkers on them
;; inputs radius, padding, Board and Integer
;;Outputs - Image

(: top-left( Integer Integer Board Integer -> Image))
(define (top-left rad pad board num-calls)
  (cond
    [(= 0 num-calls) empty-image]
    [else
     (match board
       [(Board l _ _ _ _)
        (beside (circle-on-point rad (list-ref l (+ (- 6 num-calls) 12))
                                 (+ (- 6 num-calls) 12))
                (line pad 0 'whitesmoke)
                (top-left rad pad board (sub1 num-calls)))])]))
;;Eyeball Test Passed
;(top-left 15 3 test-board 6)


;;similiar to top left but for top right
(: top-right( Integer Integer Board Integer -> Image))
(define (top-right rad pad board num-calls)
  (cond
    [(= 0 num-calls) empty-image]
    [else
     (match board
       [(Board l _ _ _ _)
        (beside (circle-on-point rad (list-ref l (+ (- 6 num-calls) 18))
                                 (+ (- 6 num-calls) 18))
                (line pad 0 'whitesmoke)
                (top-right rad pad board (sub1 num-calls)))])]))

;;Eyeball Test Passed
;;(beside (top-left 15 2 test-board 6) (top-right 15 2 test-board 6))


;;Helper - Bottom Left
;;similiar to helpers above. draws bottom left 6 triangles
;; Inputs - radius, padding, Board and Integer
;; Outputs - Image

(: bottom-left (Integer Integer Board Integer -> Image))
(define (bottom-left rad pad board num-calls)
  (cond
    [(= 0 num-calls) empty-image]
    [else
     (match board
       [(Board l _ _ _ _)
        (beside (circle-on-point rad (list-ref l (+ (- num-calls 6) 11))
                                 (+ (- num-calls 6) 11))
                (line pad 0 'whitesmoke)
                (bottom-left rad pad board (sub1 num-calls)))])]))
;;Similiar to helper above
;;draws bottom right 6 points with their respective checkers
;;Inputs - radius, padding, Board, Integer
;;Outputs - Image

(: bottom-right (Integer Integer Board Integer -> Image))
(define (bottom-right rad pad board num-calls)
  (cond
    [(= 0 num-calls) empty-image]
    [else
     (match board
       [(Board l _ _ _ _)
        (beside (circle-on-point rad (list-ref l (+ (- num-calls 6) 5))
                                 (+ (- num-calls 6) 5))
                (line pad 0 'whitesmoke)
                (bottom-right rad pad board (sub1 num-calls)))])]))

;;Visual Test Passed
;;(beside (bottom-left 15 2 test-board 6) (bottom-right 15 2 test-board 6))




;;HELPER - draws black checkers in middle
;;inputs Int and Board and Outputs Image
(: mid-black (Integer Board -> Image))
(define (mid-black rad board)
  (match board
    [(Board _ black-bar _ _ _)
     (cond
       [(< 0 black-bar)(overlay (label (number->string black-bar))
                                (black-checker rad))]
       [else empty-image])]))
;;HELPER - draws white checkers in the middle.
;;inputs Int and Board and Outputs Image

(: mid-white (Integer Board -> Image))
(define (mid-white rad board)
  (match board
    [(Board _ _ white-bar _ _)
     (cond
       [(< 0 white-bar)(overlay (label (number->string white-bar))
                                (white-checker rad))]
       [else empty-image])]))

;;Draws middle-bar depicting pieces off.
;; Uses two helpers right above.
;;Inputs Integer and Board and outputs Image
(: mid-bar (Integer Board -> Image))
(define ( mid-bar rad board)
  (overlay/align "middle" "middle"
                 (above (mid-black rad board)
                        (mid-white rad board))
                 (rectangle (* 4 rad) (* 28 rad) 'solid 'lightgray)))

;(mid-bar 15 (Board '() 1 1 3 2))






;;Start of Project 2

(: draw-black-dot ( Integer Integer -> Image))
;; draws a dot for the black dice
;; Inputs two intergers and outputs an image
(define (draw-black-dot feed checker-radius)
  (match feed
    [1 (circle (* 0.18 checker-radius) 'solid 'white)]
    [0 (circle (* 0.18 checker-radius) 'solid 'black)]))

;;Visual Test passed
;;(draw-black-dot 0 15)

(: draw-row ((Listof Integer) Integer -> Image))
;;draws 3 dots above each other
;;Inputs a list of 3 elements and an Integer and outputs an Image
(define (draw-row l checker-radius)
  (match l
    ['() empty-image]
    [(cons f r) (above (draw-black-dot f checker-radius)
                       (draw-row r checker-radius))]))

;;Visual Test passed
;;(draw-row '(1 0 1) 15)
;;(draw-row '(1 1 1) 15)

(: combine-rows (Integer Integer -> Image))
;;combines the 3 rows and takes in the dice input to give out the
;; dice arrangmenet
;; Inputs 2 Integers and outputs an Image
(define (combine-rows c-r number)
  (match number
    [1 (beside (draw-row '(0 0 0) c-r)
               (draw-row '(0 1 0) c-r)
               (draw-row '(0 0 0) c-r))]
    [2 (beside (draw-row '(1 0 0) c-r)
               (draw-row '(0 0 0) c-r)
               (draw-row '(0 0 1) c-r))]
    [3 (beside (draw-row '(1 0 0) c-r)
               (draw-row '(0 1 0) c-r)
               (draw-row '(0 0 1) c-r))]
    [4 (beside (draw-row '(1 0 1) c-r)
               (draw-row '(0 0 0) c-r)
               (draw-row '(1 0 1) c-r))]
    [5 (beside (draw-row '(1 0 1) c-r)
               (draw-row '(0 1 0) c-r)
               (draw-row '(1 0 1) c-r))]
    [6 (beside (draw-row '(1 1 1) c-r)
               (draw-row '(0 0 0) c-r)
               (draw-row '(1 1 1) c-r))]))

;Visual Test passed
;;(combine-rows 10 4)
;; (combine-rows 15 2)

(: black-die ( Integer Integer -> Image))
;;draws black-die
;;Inputs 2 Integers and outputs an Image
(define (black-die c-r number)
  (if (= number 0) empty-image
      (overlay (combine-rows c-r number)
               (square (exact-ceiling (* 1.5 c-r)) 'solid 'black)
               (square (exact-ceiling (* 1.5 c-r)) 'outline 'black))))
               
;;Visual Test Passed
;;(black-die 15 3)
;;(black-die 20 2)

(: draw-white-dot ( Integer Integer -> Image))
;; draws a dot for the white dice
;; Inputs two intergers and outputs an image
(define (draw-white-dot feed checker-radius)
  (match feed
    [1 (circle (* 0.18 checker-radius) 'solid 'black)]
    [0 (circle (* 0.18 checker-radius) 'solid 'white)]))

;;Visual Test Passed
;;Also exact same code as above with change in name and color


(: draw-white-row ((Listof Integer) Integer -> Image))
;; same as draw-row except for other die
(define (draw-white-row l checker-radius)
  (match l
    ['() empty-image]
    [(cons f r) (above (draw-white-dot f checker-radius)
                       (draw-white-row r checker-radius))]))
      
(: combine-row (Integer Integer -> Image))
;;same as combine-rows except for other die
;; Inputs Integer and Integer and outputs Image
;;helper for draw-white die
(define (combine-row c-r number)
  (match number
    [1 (beside (draw-white-row '(0 0 0) c-r)
               (draw-white-row '(0 1 0) c-r)
               (draw-white-row '(0 0 0) c-r))]
    [2 (beside (draw-white-row '(1 0 0) c-r)
               (draw-white-row '(0 0 0) c-r)
               (draw-white-row '(0 0 1) c-r))]
    [3 (beside (draw-white-row '(1 0 0) c-r)
               (draw-white-row '(0 1 0) c-r)
               (draw-white-row '(0 0 1) c-r))]
    [4 (beside (draw-white-row '(1 0 1) c-r)
               (draw-white-row '(0 0 0) c-r)
               (draw-white-row '(1 0 1) c-r))]
    [5 (beside (draw-white-row '(1 0 1) c-r)
               (draw-white-row '(0 1 0) c-r)
               (draw-white-row '(1 0 1) c-r))]
    [6 (beside (draw-white-row '(1 1 1) c-r)
               (draw-white-row '(0 0 0) c-r)
               (draw-white-row '(1 1 1) c-r))]))
;;Visual Test Passed
;; Same as tested above
;;(combine-row 15 3)
;;(combine-row 25 6)

(: white-die ( Integer Integer -> Image))
;; draws white die by overlapying dot pattern on square
;;Inputs 2 Integers and outputs an image
(define (white-die c-r number)
  (if (= number 0) empty-image
      (overlay (combine-row c-r number)
               (square (exact-ceiling (* 1.5 c-r)) 'outline 'black)
               (square (exact-ceiling (* 1.5 c-r)) 'solid 'white))))

;;Visual Test Passed
;;(white-die 25 5)
;;(white-die 15 3)

(: top-triangles : Style Integer -> Integer)
;; helps us locate which triangle of a set of 6 top triangles
;; inputs style and integer and outputs Integer
(define (top-triangles style x)
  (match style
    [(Style rad pad bc wc dp lp background lbl black-die white-die)
     (if (<= (remainder x (+ (* 2 rad) pad)) (* 2 rad))
         (quotient x (+ (* 2 rad) pad)) -1 )]))

(: bottom-triangles : Style Integer -> Integer)
;; helps us locate which triangle of a set of 6 bottom triangles
;; inputs style and integer and outputs Integer
(define (bottom-triangles style x)
  (match style
    [(Style rad pad bc wc dp lp background lbl black-die white-die)
     (if (<= (remainder x (+ (* 2 rad) pad)) (* 2 rad))
         (- 6 (quotient x (+ (* 2 rad) pad))) -1)]))   
     

(: click-where : Style Integer Integer -> ClickLoc)
;; figures out where we are clicking on the board
;; takes in style, integer and integer and outputs a clickloc
(define (click-where style x y)
  (match style
    [(Style rad pad bc wc dp lp background lbl black-die white-die)
     (cond
       [(<= y (* 10 rad))
        (cond
          [(<= x (+ (* 12 rad) (* 6 pad)))
           (if (= (top-triangles style x) -1)
               'Nowhere
               (PointNum (+ 13 (top-triangles style x))))]
          [(< x (+ (* 16 rad) (* 6 pad))) 'Nowhere]
          [(< x (+ (* 28 rad) (* 12 pad)))
           (if
            ( = (top-triangles
                 style
                 (- x (+ (* 16 rad) (* 6 pad)))) -1)
            'Nowhere
            (PointNum (+ 19
                         (top-triangles style (- x (+ (* 16 rad)
                                                      (* 6 pad)))))))]
          [else 'Nowhere])]
       [(>= y (* 18 rad))
        (cond
          [(<= x (+ (* 12 rad) (* 6 pad)))
           (if (= (bottom-triangles style x) -1)
               'Nowhere
               (PointNum (+ 6 (bottom-triangles style x))))]
          [(< x (+ (* 16 rad) (* 6 pad))) 'Nowhere]
          [(< x (+ (* 28 rad) (* 12 pad)))
           (if
            ( = (top-triangles
                 style
                 (- x (+ (* 16 rad) (* 6 pad)))) -1)
            'Nowhere 
            (PointNum
             (bottom-triangles style (- x (+ (* 16 rad)
                                             (* 6 pad))))))]
          [else 'Nowhere])]
       [(and (>= x (+ (* 4.5 rad) (* 2.5 pad)))
             (<= x (+ (* 7.5 rad) (* 3.5 pad)))
             (>= y (* 13.25 rad))
             (<= y (* 14.75 rad))) 'WhiteDice]
       [(and (>= (- x (+ (* 16 rad) (* 6 pad))) (* 4.5 rad) (* 2.5 pad))
             (<= (- x (+ (* 16 rad) (* 6 pad))) (+ (* 7.5 rad) (* 3.5 pad)))
             (>= y (* 13.25 rad))
             (<= y (* 14.75 rad))) 'BlackDice]
       [(and (>= x (+ (* 13 rad) (* 6 pad)))
             (<= x (+ (* 15 rad) (* 6 pad))))
        (cond
          [(and (>= y (* 12 rad))
                (<= y (* 14 rad))) 'BlackBar]
          [(and (>= y (* 14 rad))
                (<= y (* 16 rad))) 'WhiteBar]
          [else 'Nowhere])]
       [(and (>= x (+ (* 29 rad) (* 12 pad)))
             (<= x (+ (* 31 rad) (* 12 pad))))
        (cond
          [(and (>= y (* 12 rad))
                (<= y (* 14 rad))) 'BlackOff]
          [(and (>= y (* 14 rad))
                (<= y (* 16 rad))) 'WhiteOff]
          [else 'Nowhere])]
       [else 'Nowhere])]))
         
      

;;check-expects
;;(check-expect (click-where test-style 140 10) (PointNum 16))
;;(check-expect (click-where test-style 140 380) (PointNum 9))
;;(check-expect (click-where test-style 1 1) (PointNum 13))
;;(check-expect (click-where test-style 120 270) 'BlackDice)
;;(check-expect (click-where test-style 460 270) 'WhiteDice)


(: drop-point ((Listof Point) Integer -> (Listof Point)))
;;Helper function to drop a point from a PointsNum
;; Inputs listof Points and Integer of PointNum and outputs new listof Points
(define (drop-point l num)
  (match l
    ['() '()]
    [(cons f r)
     (match f
       ['() '()]
       [else 
        (cond
          [(= num 1)
           (match f
             [(OccupiedPoint j 1)
              (cons 'EmptyPoint (drop-point r (- num 1)))]
             [(OccupiedPoint j p)
              (cons (OccupiedPoint j (- p 1)) (drop-point r (- num 1)))]
             ['EmptyPoint (error "invalid move")])]
     
          [else (cons f (drop-point r (- num 1)))])])
     ]))
         
(check-expect(drop-point (list (OccupiedPoint 'Black 2) 'EmptyPoint 'EmptyPoint
                               'EmptyPoint 'EmptyPoint (OccupiedPoint 'White 5)
                               'EmptyPoint (OccupiedPoint 'White 3) 'EmptyPoint
                               'EmptyPoint 'EmptyPoint (OccupiedPoint 'Black 5)
                               (OccupiedPoint 'White 5) 'EmptyPoint 'EmptyPoint
                               'EmptyPoint (OccupiedPoint 'Black 3) 'EmptyPoint
                               (OccupiedPoint 'Black 5) 'EmptyPoint 'EmptyPoint
                               'EmptyPoint 'EmptyPoint (OccupiedPoint 'White 2)) 13)
             (list (OccupiedPoint 'Black 2) 'EmptyPoint 'EmptyPoint
                   'EmptyPoint 'EmptyPoint (OccupiedPoint 'White 5)
                   'EmptyPoint (OccupiedPoint 'White 3) 'EmptyPoint
                   'EmptyPoint 'EmptyPoint (OccupiedPoint 'Black 5)
                   (OccupiedPoint 'White 4) 'EmptyPoint 'EmptyPoint
                   'EmptyPoint (OccupiedPoint 'Black 3) 'EmptyPoint
                   (OccupiedPoint 'Black 5) 'EmptyPoint 'EmptyPoint
                   'EmptyPoint 'EmptyPoint (OccupiedPoint 'White 2)))

(: move-by (BoardLoc (Listof Point) -> Player))
;;checks who is making the move being done
;; takes in origin and outputs 'Black or 'White
;; Inputs BoardLoc and Listof Point and outputs a player
(define (move-by bl lp)
  (match bl
    ; ['BlackBar 'Black]
;     ['WhiteBar 'White]
;     ['BlackOff 'Black]
;     ['WhiteOff 'White]

    [(PointNum j)
     (match lp
       ['() (error "no one there")]
       [(cons f r)
        (cond
          [(= j 1)
           (match f
             [(OccupiedPoint color _) color])]
          [else (move-by (PointNum (- j 1)) r)])])]))
        
(: numchecker (BoardLoc (Listof Point) -> Integer))
;;checks number of checkers at a PointNum
;; takes in origin and outputs 'Black or 'White
;; Inputs BoardLoc and Listof Point and outputs a player
(define (numchecker bl lp)
  (match bl
    [(PointNum j)
     (match lp
       ['() 0]
       [(cons f r)
        (cond
          [(= j 1)
           (match f
             [(OccupiedPoint _ n) n]
             ['EmptyPoint 0])]
         
          [else (numchecker (PointNum (- j 1)) r)])])]))
        



(check-expect (move-by (PointNum 13)
                       (list (OccupiedPoint 'Black 2) 'EmptyPoint 'EmptyPoint
                             'EmptyPoint 'EmptyPoint (OccupiedPoint 'White 5)
                             'EmptyPoint (OccupiedPoint 'White 3) 'EmptyPoint
                             'EmptyPoint 'EmptyPoint (OccupiedPoint 'Black 5)
                             (OccupiedPoint 'White 5) 'EmptyPoint 'EmptyPoint
                             'EmptyPoint (OccupiedPoint 'Black 3) 'EmptyPoint
                             (OccupiedPoint 'Black 5) 'EmptyPoint 'EmptyPoint
                             'EmptyPoint 'EmptyPoint (OccupiedPoint 'White 2)))
              'White)

(: player=? (Player Player -> Boolean))
;; checks if two players are the same
;;Inputs two players and outputs a Boolean
(define (player=? p1 p2)
  (match* (p1 p2)
    [('White 'White) #t]
    [('Black 'Black) #t]
    [(_ _) #f]))

(check-expect (player=? 'Black 'Black) #t)
(check-expect (player=? 'Black 'White) #f)


(: add-pointnum ( (Listof Point) BoardLoc Player -> (Listof Point)))
;; adds PointsNum to a Board outputting the list since that is where it goes
;; Inputs a listof Point, BoardLoc (PointNum x), Player ('Black or 'White)
(define (add-pointnum lp destination player)
  (match destination
    [(PointNum x)
     (match (list-ref lp (- x 1))
       ['EmptyPoint (replace-value lp (OccupiedPoint player 1) (- x 1))]
       [(OccupiedPoint player1 n)
        (cond
          [(player=? player player1)
           (replace-value lp (OccupiedPoint player (+ n 1))  (- x 1))]
          [(= n 1) (replace-value lp (OccupiedPoint player 1)  (- x 1))]
          [else (error "Invalid Move")])]
       [_ (error "Invalid Move")])]))

(: replace-value (All (A) ((Listof A) A Integer -> (Listof A))))
;;replaces a value in a given position of a list
;; used help from Niralee for logic
(define (replace-value l b n)
  (match l
    ['() '()]
    [(cons f r)
     (if (= 0 n) (cons b r) (cons f (replace-value r b (- n 1))))]))

(check-expect(replace-value '(1 2 3 4) 5 1) '(1 5 3 4))
(check-expect(replace-value '(1 2 3 4) 5 2) '(1 2 5 4))

(: who? (Board BoardLoc -> (U Player 'None)))
(define (who? board bl)
  (match* (board bl)
    [((Board lp _ _ _ _) (PointNum n))
     (match (list-ref lp (- n 1))
       [(OccupiedPoint color count) color]
       [_ 'None])]
    [((Board _ _ _ _ _ ) 'BlackBar) 'Black]
    [((Board _ _ _ _ _ ) 'WhiteBar) 'White]
    [((Board _ _ _ _ _ ) 'BlackOff) 'Black]
    [((Board _ _ _ _ _ ) 'WhiteOff) 'White]))    

(: same?-who ( Board BoardLoc BoardLoc -> Boolean))
(define (same?-who board bl1 bl2)
  (cond
    [(symbol=? (who? board bl1) (who? board bl2)) #t]
    [(or (symbol=? (who? board bl1) 'None)
         (symbol=? (who? board bl2) 'None)) #t]
    [else #f]))


(: edit-moves : (Listof Integer) BoardLoc BoardLoc -> (Listof Integer))
;;takes in the move being played and outputs the new list
(define (edit-moves l from to)
  (match l
    ['() '()]
    [(cons f r)
     (if (= (abs (distance from to)) f) r
         (cons f (edit-moves r from to)))]))

(check-expect(edit-moves (list 1 4 3 0) (PointNum 5) (PointNum 8)) (list 1 4 0))
(check-expect(edit-moves (list 1 4 3 0) (PointNum 5) (PointNum 9)) (list 1 3 0))
(check-expect (edit-moves (list 1 4 3 0) 'BlackBar (PointNum 1)) (list 4 3 0))
(check-expect (edit-moves (list 1 4 3 0) (PointNum 1) 'WhiteOff) (list 4 3 0))
(check-expect (edit-moves (list 1 1 1 1) (PointNum 1) 'WhiteOff) (list 1 1 1))
(check-expect (edit-moves (list 1 1 1) (PointNum 1) 'WhiteOff) (list 1 1))
(check-expect (edit-moves (list 1 1 1) (PointNum 2) (PointNum 1)) (list 1 1))





(: apply-move (Game BoardLoc BoardLoc -> Game))
;; takes in origin destination and initial board
;;outputs final board
;; worksss
(define (apply-move game origin destination)
  (match game
    [(Game board player moves)
     (match board
       [(Board lp bb wb bo wo)
        (match origin
          ['BlackBar
           (match destination
             [(PointNum n)
              (cond
                [(and (not (same?-who board origin destination))
                      (= (numchecker destination lp) 1))
                 (if (> bb 0)
                     (Game (Board (add-pointnum lp destination 'Black)
                                  (- bb 1) (+ 1 wb) bo wo)
                           player
                           (edit-moves moves origin destination)) game )]
                [else (if (> bb 0)
                          (Game (Board (add-pointnum lp destination 'Black)
                                       (- bb 1) wb bo wo)
                                player
                                (edit-moves moves origin destination)) game)])]
             [ _ (error "invalid move")])]      
          ['WhiteBar
           (match destination
             [(PointNum n)
              (cond
                [(and (not (same?-who board origin destination))
                      (= (numchecker destination lp) 1))
                 (if (> wb 0)
                     (Game (Board
                            (add-pointnum lp destination 'White)
                            (+ bb 1) (- 1 wb) bo wo)
                           player
                           (edit-moves moves origin destination)) game)]
                [else (if (> wb 0)
                          (Game (Board (add-pointnum lp destination 'White)
                                       bb (- wb 1) bo wo)
                                player
                                (edit-moves moves origin destination)) game)])]
             [ _ (error "invalid move")])]
          [(PointNum g)
           (match destination
             ['BlackBar
              (Game
               (Board (drop-point lp g)
                      (+ 1 bb) wb bo wo) player
                                         (edit-moves moves origin destination))]
             ['WhiteBar
              (Game
               (Board (drop-point lp g) bb
                      (+ 1 wb) bo wo) player
                                      (edit-moves moves origin destination))]
             ['BlackOff
              (Game
               (Board (drop-point lp g)
                      bb wb (+ 1 bo) wo) player
                                         (edit-moves moves origin destination))]
             ['WhiteOff
              (Game
               (Board (drop-point lp g)
                      bb wb bo (+ 1 wo)) player
                                         (edit-moves moves origin destination))]
             ['Nowhere  game]
             [(PointNum h)
              (match (list-ref lp (- h 1))
                ['EmptyPoint
                 (Game (Board
                        (add-pointnum
                         (drop-point lp g) destination (move-by origin lp))
                        bb wb bo wo)  player (edit-moves moves origin destination))]
                [(OccupiedPoint player1 n)
                 (match* (player1 (move-by origin lp))
                   [('White 'White)
                    (Game (Board
                           (add-pointnum (drop-point lp g) destination
                                         (move-by origin lp))
                           bb wb bo wo) player
                                        (edit-moves moves origin destination))]
                   [('Black 'Black)
                    (Game (Board
                           (add-pointnum (drop-point lp g)
                                         destination (move-by origin lp))
                           bb wb bo wo) player
                                        (edit-moves moves origin destination))]
                   [('White 'Black)
                    (Game (Board
                           (add-pointnum (drop-point lp g)
                                         destination (move-by origin lp))
                           bb (+ 1 wb) bo wo)
                          player
                          (edit-moves moves origin destination))]
                   [('Black 'White)
                    (Game (Board
                           (add-pointnum
                            (drop-point lp g)
                            destination (move-by origin lp))
                           (+ 1 bb) wb bo wo)
                          player (edit-moves moves origin destination))])])])]
          [else game])])]))


(check-expect (apply-move
               (Game (Board
                      (list (OccupiedPoint 'Black 2) 'EmptyPoint 'EmptyPoint
                            'EmptyPoint 'EmptyPoint (OccupiedPoint 'White 5)
                            'EmptyPoint (OccupiedPoint 'White 3) 'EmptyPoint
                            'EmptyPoint 'EmptyPoint (OccupiedPoint 'Black 5)
                            (OccupiedPoint 'White 5) 'EmptyPoint 'EmptyPoint
                            'EmptyPoint (OccupiedPoint 'Black 3) 'EmptyPoint
                            (OccupiedPoint 'Black 5) 'EmptyPoint 'EmptyPoint
                            'EmptyPoint 'EmptyPoint (OccupiedPoint 'White 2))
                      2 3 2 5) 'Black (list 4 2)) (PointNum 12)
                                                  (PointNum 14))
              (Game (Board
                     (list (OccupiedPoint 'Black 2) 'EmptyPoint 'EmptyPoint
                           'EmptyPoint 'EmptyPoint (OccupiedPoint 'White 5)
                           'EmptyPoint (OccupiedPoint 'White 3) 'EmptyPoint
                           'EmptyPoint 'EmptyPoint (OccupiedPoint 'Black 4)
                           (OccupiedPoint 'White 5) (OccupiedPoint 'Black 1)
                           'EmptyPoint
                           'EmptyPoint (OccupiedPoint 'Black 3) 'EmptyPoint
                           (OccupiedPoint 'Black 5) 'EmptyPoint 'EmptyPoint
                           'EmptyPoint 'EmptyPoint (OccupiedPoint 'White 2))
                     2 3 2 5) 'Black (list 4)))
(check-expect
 (apply-move (Game (Board
                    (list (OccupiedPoint 'Black 2) 'EmptyPoint 'EmptyPoint
                          'EmptyPoint 'EmptyPoint (OccupiedPoint 'White 5)
                          'EmptyPoint (OccupiedPoint 'White 3) 'EmptyPoint
                          'EmptyPoint 'EmptyPoint (OccupiedPoint 'Black 5)
                          (OccupiedPoint 'White 5) 'EmptyPoint 'EmptyPoint
                          'EmptyPoint (OccupiedPoint 'Black 3) 'EmptyPoint
                          (OccupiedPoint 'Black 5) 'EmptyPoint 'EmptyPoint
                          'EmptyPoint 'EmptyPoint (OccupiedPoint 'White 2))
                    2 3 2 5) 'Black (list 4 2)) 'BlackBar (PointNum 4))
 (Game (Board
        (list (OccupiedPoint 'Black 2) 'EmptyPoint 'EmptyPoint
              (OccupiedPoint 'Black 1) 'EmptyPoint
              (OccupiedPoint 'White 5)
              'EmptyPoint (OccupiedPoint 'White 3) 'EmptyPoint
              'EmptyPoint 'EmptyPoint (OccupiedPoint 'Black 5)
              (OccupiedPoint 'White 5) 'EmptyPoint 'EmptyPoint
              'EmptyPoint (OccupiedPoint 'Black 3) 'EmptyPoint
              (OccupiedPoint 'Black 5) 'EmptyPoint 'EmptyPoint
              'EmptyPoint 'EmptyPoint (OccupiedPoint 'White 2))
        1 3 2 5) 'Black (list 2)))





(: distance : BoardLoc BoardLoc -> Integer)
;;  computes distance from first move to second move
;; only computes distance of possible movements
;; Inputs two BoardLoc and outputs an Integer
(define (distance from to)
  (match* (from to)
    [('BlackBar (PointNum num)) num]
    [('WhiteBar (PointNum num)) (- 25 num)]
    [((PointNum num) (PointNum num2)) (- num2 num)]
    [((PointNum num) 'BlackOff) (- 25 num)]
    [((PointNum num) 'WhiteOff) num]
    [(_ _) 500]))

(check-expect (distance 'BlackBar (PointNum 4)) 4)
(check-expect (distance (PointNum 4) (PointNum 11)) 7)
(check-expect (distance (PointNum 19) (PointNum 11)) -8)


(: legal-move? : Game BoardLoc BoardLoc -> Boolean)
;;take in the current game situation
;;checks if a move being made is legal
;;takes in the current Game and the origin and destination
;;and outputs a boolean
(define (legal-move? game start end)
  (match game
    [(Game board turn moves)
     (match board
       [(Board lp bb wb bo wo)
        (local {(define l1 : Integer (if (> (length moves) 0)
                                         (list-ref moves 0) 0))
                (define l2 : Integer (if (> (length moves) 1)
                                         (list-ref moves 1) 0))
                (define l3 : Integer (if (> (length moves) 2)
                                         (list-ref moves 2) 0))
                (define l4 : Integer (if (> (length moves) 3)
                                         (list-ref moves 3) 0))}
          (match board
            [(Board lp bb wb bo wo)
             (if (or (and (symbol=? turn 'Black) (= bb 0))
                     (and (symbol=? turn 'White) (= wb 0)))       
                 (match* (start end)
                
                   [((or 'BlackOff 'WhiteOff) _) #f]
                   [(_ (or 'BlackBar 'WhiteBar)) #f]
                   [((PointNum num1) (PointNum num2))
                    (match turn
                      ['Black
                       (if
                        (or (= l1 (distance (PointNum num1) (PointNum num2)))
                            (= l2 (distance (PointNum num1) (PointNum num2)))
                            (= l3 (distance (PointNum num1) (PointNum num2)))
                            (= l4 (distance (PointNum num1) (PointNum num2))))
                        #t #f)]
                      [ _
                        (if (or (= l1 (- 0 (distance (PointNum num1)
                                                     (PointNum num2))))
                                (= l2 (- 0 (distance (PointNum num1)
                                                     (PointNum num2))))
                                (= l3 (- 0 (distance (PointNum num1)
                                                     (PointNum num2))))
                                (= l4 (- 0 (distance (PointNum num1)
                                                     (PointNum num2)))))
                            #t #f)])]
                   [((PointNum num) 'BlackOff)
                    (if (and (or (= l1 (distance (PointNum num) 'BlackOff))
                                 (= l2 (distance (PointNum num) 'BlackOff))
                                 (= l3 (distance (PointNum num) 'BlackOff))
                                 (= l4 (distance (PointNum num) 'BlackOff)))
                             (= 15
                                (+ (if (symbol=? (which-player game 19) 'Black)
                                       (numchecker (PointNum 19) lp) 0)
                                   (if (symbol=? (which-player game 20) 'Black)
                                       (numchecker (PointNum 20) lp) 0)
                                   (if (symbol=? (which-player game 21) 'Black)
                                       (numchecker (PointNum 21) lp) 0)
                                   (if (symbol=? (which-player game 22) 'Black)
                                       (numchecker (PointNum 22) lp) 0)
                                   (if (symbol=? (which-player game 23) 'Black)
                                       (numchecker (PointNum 23) lp) 0)
                                   (if (symbol=? (which-player game 24) 'Black)
                                       (numchecker (PointNum 24) lp) 0)
                                   bo)))
                        #t #f)]
                   [((PointNum num) 'WhiteOff)
                    (if (and
                         (or
                          (= l1 (distance (PointNum num) 'WhiteOff))
                          (= l2 (distance (PointNum num) 'WhiteOff))
                          (= l3 (distance (PointNum num) 'WhiteOff))
                          (= l4 (distance (PointNum num) 'WhiteOff)))
                         (= 15
                            (+ (if (symbol=? (which-player game 1) 'White)
                                   (numchecker (PointNum 1) lp) 0)
                               (if (symbol=? (which-player game 2) 'White)
                                   (numchecker (PointNum 2) lp) 0)
                               (if (symbol=? (which-player game 3) 'White)
                                   (numchecker (PointNum 3) lp) 0)
                               (if (symbol=? (which-player game 4) 'White)
                                   (numchecker (PointNum 4) lp) 0)
                               (if (symbol=? (which-player game 5) 'White)
                                   (numchecker (PointNum 5) lp) 0)
                               (if (symbol=? (which-player game 6) 'White)
                                   (numchecker (PointNum 6) lp) 0)
                               wo)))
                        #t #f)]
                   [(_ _) #f])
                 (match* (start end)
                   [('BlackBar (PointNum num))
                    (if (or (= l1 (distance 'BlackBar (PointNum num)))
                            (= l2 (distance 'BlackBar (PointNum num)))
                            (= l3 (distance 'BlackBar (PointNum num)))
                            (= l4 (distance 'BlackBar (PointNum num))))
                        #t #f)]
                   [('WhiteBar (PointNum num))
                    (if (or (= l1 (distance 'WhiteBar (PointNum num)))
                            (= l2 (distance 'WhiteBar (PointNum num)))
                            (= l3 (distance 'WhiteBar (PointNum num)))
                            (= l4 (distance 'WhiteBar (PointNum num))))
                        #t #f)]
                   [(_ _) #f]))]))])]))  


(check-expect (legal-move? (Game test-board 'Black (list 4 5 1))
                           (PointNum 5) (PointNum 9)) #f)
(check-expect (legal-move? (Game test-board 'White (list 5 4 1))
                           (PointNum 9) (PointNum 5)) #f)
(check-expect (legal-move? (Game test-board 'White (list 4 5 1))
                           (PointNum 5) (PointNum 9)) #f)
(check-expect (legal-move? (Game test-board 'Black (list 4 4 4 4))
                           (PointNum 1) (PointNum 5)) #f)
(check-expect (legal-move? (Game test-board 'White (list 1 1 1))
                           (PointNum 24) (PointNum 23)) #f)
(check-expect (legal-move? (Game test-board 'White (list 1 1 1))
                           (PointNum 24) (PointNum 20)) #f)

(: valid? : Integer (Listof Integer) -> Boolean)
;;check if an Integer is in a list of integers
;; used in available moves
;;inputs an integer and a listof Integer and outputs
;; a boolean
(define (valid? x l)
  (match l
    ['() #f]
    [(cons f r) (if (= x f) #t
                    (valid? x r))]))

(check-expect (valid? 15 (list 15 1 1 1 1 1)) #t)
(check-expect (valid? 13 (list 15 1 1 1 1 1)) #f)
        
(: distance-to-move-w : Board -> (Listof Integer))
;; given a board
;;outputs a list of distances that white checkers could potentially move to 
(define (distance-to-move-w board)
  (match board
    [(Board lp bb wb bo wo)
     (local
       {(: loop : Integer Integer -> (Listof Integer))
        ;; i is the start point of a move, j is the end point of a move
        (define (loop i j)
          (cond
            [(= i 1) '()]
            [(= j 1) (loop (- i 1) (- i 2))]
            [else 
             (if (symbol=? (who? board (PointNum i)) 'White)
                 (if (or (symbol=? (who? board (PointNum j)) 'White)
                         (>= 1 (numchecker (PointNum  j) lp)))
                     (cons (abs (distance (PointNum i) (PointNum j)))
                           (loop i (- j 1)))
                     (loop i (- j 1)))
                 (loop (- i 1) (- i 2)))]))}
       (loop 24 23)
       )]))

(check-expect (distance-to-move-w test-board)
              '(1 2 3 4 6 8 9 10 11 13 14 15 16 17 18
                  19 20 21 22 2 3 4 5 6 7 8 9 10 11 1 2
                  3 4 5 6 1 2 3 4))


(: num-checkers : Game Integer -> Integer)
;; gives the number of checkers at a paticular pointnum
;;takes in an integer anda  game and outputs
;; an integer
(define (num-checkers game n)
  (match game
    [(Game board player moves)
     (match board
       [(Board lp bb wb bo wo)
        (match lp
          ['() 0]
          [(cons f r)
           (if (= n 1) (match f
                         ['EmptyPoint 0]
                         [(OccupiedPoint player count) count])
               (num-checkers (Game (Board r bb wb bo wo) player moves)
                             (- n 1)))])])]))
(check-expect (num-checkers (Game test-board 'White (list 1 2 3)) 19) 5)
(check-expect (num-checkers (Game test-board 'White (list 1 2 3)) 1) 2)
(check-expect (num-checkers (Game test-board 'White (list 1 2 3)) 6) 5)


(: distance-to-move-b : Board -> (Listof Integer))
;; given a board,
;;outputs a list of distances that black checkers could potentially move to
(define (distance-to-move-b board)
  (match board
    [(Board lp bb wb bo wo)
     (local
       {(: loop : Integer Integer -> (Listof Integer))
        ;; i is the start point of a move, j is the end point of a move
        (define (loop i j)
          (cond
            [(= i 24) '()]
            [(= j 24) (loop (+ i 1) (+ i 2))]
            [else
             (if (symbol=? (who? board (PointNum i)) 'Black)
                 (if (or (symbol=? (who? board (PointNum j)) 'Black)
                         (>= 1 (numchecker (PointNum  j) lp)))
                     (cons (abs (distance (PointNum i) (PointNum j)))
                           (loop i (+ j 1)))
                     (loop i (+ j 1)))
                 (loop (+ i 1) (+ i 2)))]))}
       (loop 1 2))]))

(check-expect (distance-to-move-b test-board)
              (list 1 2 3 4 6 8 9 10 11
                    13 14 15 16 17 18 19 20 21
                    22 2 3 4 5 6 7 8 9 10 11 1
                    2 3 4 5 6 1 2 3 4))
(check-expect (distance-to-move-b initial-board)
              '(1 2 3 4 6 8 9 10 11 13 14
                  15 16 17 18 19 20 21 22
                  2 3 4 5 6 7 8 9 10 11 1 2
                  3 4 5 6 1 2 3 4) )

(: which-player : Game Integer -> (U Player 'None))
;; takes in a game and the num of a (PointNum num)
;;someone please help, this project is longgggggg
;;outputs the player or none depending on who is there
(define (which-player game x)
  (match game
    [(Game board player moves)
     (match board
       [(Board points bb wb bo wo)
        (match points
          ['() 'None]
          [(cons f r)
           (if (= x 1)
               (match f
                 ['EmptyPoint 'None]
                 [(OccupiedPoint player count) player])
               (which-player (Game (Board r bb wb bo wo) player moves)
                             (- x 1)))])])]))

(: available-moves? : Game -> Boolean)
;; determine if the player has any remaining moves
;;produces a list of all remaining moves and checks if the movement we have
;;is in that list
(define (available-moves? game)
  (match game
    [(Game board player moves)
     (match board
       [(Board points bb wb bo wo)
        (local {(define d1 : Integer (if (> (length moves) 0)
                                         (list-ref moves 0) 0))
                (define d2 : Integer (if (> (length moves) 1)
                                         (list-ref moves 1) 0))
                (define d3 : Integer (if (> (length moves) 2)
                                         (list-ref moves 2) 0)) 
                (define d4 : Integer (if (= (length moves) 4)
                                         (list-ref moves 3) 0))}
          (match moves
            ['() #f]
            [_ 
             (match player
               ['Black
                (cond
                  [(> bb 0) (if 
                             (or 
                              (and (<= (num-checkers game d1) 1)
                                   (> (length moves) 0))
                              (and (<= (num-checkers game d2) 1)
                                   (> (length moves) 1))
                              (and (<= (num-checkers game d3) 1)
                                   (> (length moves) 2))
                              (and (<= (num-checkers game d4) 1)
                                   (> (length moves) 3))
                              (and (symbol=? (which-player game d1) 'Black)
                                   (> (length moves) 0))
                              (and (symbol=? (which-player game d1) 'Black)
                                   (> (length moves) 1))
                              (and (symbol=? (which-player game d1) 'Black)
                                   (> (length moves) 2))
                              (and (symbol=? (which-player game d1) 'Black)
                                   (> (length moves) 3)))
                             #t #f)]
                  [(= bb 0)
                   (if (or (valid? d1 (distance-to-move-b board))
                           (valid? d2 (distance-to-move-b board))
                           (valid? d3 (distance-to-move-b board))
                           (valid? d4 (distance-to-move-b board))
                           )
                       #t #f)]        
                  [else #f])]          
               ['White
                (cond
                  [(> wb 0)
                   (if 
                    (or 
                     (and (<= (num-checkers game(- 25 d1)) 1)
                          (> (length moves) 0))
                     (and (<= (num-checkers game (- 25 d2)) 1)
                          (> (length moves) 1))
                     (and (<= (num-checkers game (- 25 d3)) 1)
                          (> (length moves) 2))
                     (and (<= (num-checkers game (- 25 d4)) 1)
                          (= (length moves) 4))
                     (and (symbol=? (which-player game (- 25 d1)) 'White)
                          (> (length moves) 0))
                     (and (symbol=? (which-player game (- 25 d2)) 'White)
                          (> (length moves) 1))
                     (and (symbol=? (which-player game (- 25 d3)) 'White)
                          (> (length moves) 2))
                     (and (symbol=? (which-player game (- 25 d4)) 'White)
                          (= (length moves) 4)))
                    #t #f)]
                  [(= wb 0)
                   (if (or
                        (valid? d1 (distance-to-move-w board))
                        (valid? d2 (distance-to-move-w board))
                        (valid? d3 (distance-to-move-w board))
                        (valid? d4 (distance-to-move-w board)))
                       
                       #t #f)]
                  [else #f])]
               )])
          )])]
    ))


(check-expect ( available-moves? (Game test-board 'Black (list 1 1 1 1))) #t)
(check-expect ( available-moves? (Game test-board 'White (list 6 6))) #f)
(check-expect ( available-moves? (Game initial-board 'White '())) #f)


(: game-over? : Game -> Boolean)
;;checks if a game is over
;;inputs a game and outputs a boolean
(define (game-over? game)
  (match game
    [(Game board turn move)
     (match board
       [(Board lp bb wb bo wo)
        (if (or (= 15 bo) (= 15 wo)) #t #f)])]))

(check-expect (game-over? (Game (Board '() 0 0 15 0) 'White '())) #t)

(: winner : Game -> Player)
;;checks who the winner is if a game is over
(define (winner game)
  (match game
    [(Game board turn move)
     (match board
       [(Board lp bb wb bo wo)
        (cond
          [(= bo 15) 'Black]
          [else 'White])])]))

(check-expect (winner (Game (Board '() 0 0 15 0) 'White '())) 'Black)
(check-expect (winner (Game (Board '() 0 0 0 15) 'White '())) 'White)






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;WORLD ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(: draw-board : Style Board -> Image)
;; draws board from first assignment
;;Inputs style and board and outputs an image
;;checked last time
(define (draw-board style board)
  (if (game-over? (Game board 'White '()))
      (text (string-append "Winner: "  (symbol->string(winner
                                                       (Game board 'White '()))))
            (guarantee-byte 30) 'black)
      (match style
        [(Style rad pad bc wc dp lp background lbl black-die white-die)
         (frame (beside (overlay/align "left" "top"
                                       (beside
                                        (above
                                         (top-left rad pad board 6)
                                         (rectangle 2 (* 8 rad)
                                                    'solid 'whitesmoke )
                                                      
                                         (bottom-left rad pad board 6))
                                        (mid-bar rad board)
                                        (above (top-right rad pad board 6)
                                               (rectangle 2 (* 8 rad)
                                                          'solid 'whitesmoke )
                                               (bottom-right rad pad board 6)))
                                       
                                       (background rad pad))
                        (side-bar rad board)))])))

(define test-style
  (Style 20 5 black-checker white-checker
         dark-point light-point background label black-die white-die)) 
;;Visual Test Passed
;;(draw-board test-style test-board)


(: click-to-board : ClickLoc -> BoardLoc)
;;turns clickloc to boardloc
;;very straightforward
;;never fed dice inputs
(define (click-to-board click)
  (match click
    [(PointNum num) (PointNum num)]
    ['BlackBar 'BlackBar]
    ['WhiteBar 'WhiteBar]
    ['WhiteOff 'WhiteOff]
    ['BlackOff 'BlackOff]
    ['Nowhere 'Nowhere]))

(: click-handler : World Integer Integer Mouse-Event -> World)
;; assigns values to three clickers
;;first is the dice being rolled
;;second is the origin
;; thirs is the destination
;; three levels of matches and last level applies function
;; every invalid input just returns the world
(define (click-handler w x y mouse-event)
  (match mouse-event
    ["button-down"
     (local
       { (define blackd1 : Integer (random 1 7))
         (define blackd2 : Integer (random 1 7))
         (define whited1 : Integer (random 1 7))
         (define whited2 : Integer (random 1 7))}
       (match w
         [(World
           (Game board player moves)
           style whitedice1 whitedice2 blackdice1
           blackdice2 last-click-1 last-click-2 last-click-3 history)
          (match (click-where style x y)
            ['BlackDice
             (if (or (and (available-moves? (Game board 'White moves))
                          (symbol=? player 'White))
                     (symbol=? player 'Black))
                 w
                 (World (Game board 'Black (if (= blackd1 blackd2)
                                               (make-list 4 blackd1)
                                               (list blackd1 blackd2)))
                        style whitedice1 whitedice2
                        blackd1 blackd2
                        'BlackDice 'Nowhere 'Nowhere
                        (cons (Game board player moves)
                              history)))]
            ['WhiteDice
             (if (or (and (available-moves? (Game board 'Black moves))
                          (symbol=? player 'Black))
                     (symbol=? player 'White))
                 w
                 (World (Game board 'White (if (= whited1 whited2)
                                               (make-list 4 whited1)
                                               (list whited1 whited2)))
                        style
                        whited1 whited2 blackdice1 blackdice2
                        'WhiteDice 'Nowhere 'Nowhere
                        (cons (Game board player moves) history)))]
            ;; second level of inputs
            [_
             (match last-click-1
               ['BlackDice
                (match last-click-2
                  ['Nowhere
                   (match (click-where style x y )
                     ['BlackBar 
                      (World (Game board 'Black
                                   moves)
                             style
                             whitedice1 whitedice2
                             blackdice1 blackdice2
                             'BlackDice 'BlackBar 'Nowhere history)]
                     [(PointNum num)
                      (if (symbol=?
                           (who? board (click-to-board
                                        (click-where style x y))) 'Black) 
                          (World (Game board 'Black 
                                       moves)
                                 style whitedice1 whitedice2
                                 blackdice1 blackdice2
                                 'BlackDice (PointNum num)
                                 'Nowhere history) w)]
                     [_ w])]
                  ;; third level of inputs                  
                  [_
                   (match (click-where style x y)
                     [(PointNum num)
                      (match board
                        [(Board lp _ _ _ _)
                         (if
                          (and (symbol=? player 'Black)
                               (or 
                                (symbol=? (who? board
                                                (click-to-board
                                                 (click-where style x y)))
                                          'Black)
                                (> 2 (numchecker (PointNum num)  lp))))
                          (if (legal-move? (Game board player moves)
                                           last-click-2 (PointNum num))
                              (World 
                               (apply-move (Game board player moves) last-click-2
                                           (PointNum num))
                               style whitedice1 whitedice2
                               blackdice1 blackdice2
                               'BlackDice 'Nowhere 'Nowhere
                               (cons (Game board player moves) history))
                              (World 
                               (Game board player moves)
                               style whitedice1 whitedice2
                               blackdice1 blackdice2
                               'BlackDice 'Nowhere 'Nowhere history)) w)])]
                   
                     ['BlackOff
                      (if
                       (legal-move? (Game board player moves)
                                    last-click-2 'BlackOff)
                       (World(apply-move (Game board player moves) last-click-2
                                         'BlackOff)
                             style whitedice1 whitedice2
                             blackdice1 blackdice2
                             'BlackDice 'Nowhere 'Nowhere
                             (cons (Game board player moves) history)) w)]
                     [_ w])])]
             
               ;; second level of inputs       
               ['WhiteDice
                (match last-click-2
                  ['Nowhere
                   (match (click-where style x y )             
                     ['WhiteBar
                      (World
                       (Game board 'White
                             moves)
                       style
                       whitedice1 whitedice2
                       blackdice1 blackdice2
                       'WhiteDice 'WhiteBar 'Nowhere history)]
                     [(PointNum num)
                      (if
                       (symbol=?
                        (who? board
                              (click-to-board
                               (click-where style x y))) 'White) 
                       (World (Game board 'White moves)
                              style whitedice1 whitedice2
                              blackdice1 blackdice2
                              'WhiteDice (PointNum num)
                              'Nowhere history) w)]
                     [_ w])]
                  ;; third level of inputs
                  [_ (match (click-where style x y)
                       [(PointNum num)
                        (match board
                          [(Board lp _ _ _ _)
                           (if
                            (and (symbol=? player 'White)
                                 (or (symbol=?
                                      (who? board
                                            (click-to-board
                                             (click-where style x y))) 'White)
                                     (> 2 (numchecker (PointNum num)  lp))))
                            (if
                             (legal-move?
                              (Game board player moves) last-click-2 (PointNum num))
                             (World
                              (apply-move (Game board player moves) last-click-2
                                          (PointNum num))
                              style whitedice1 whitedice2
                              blackdice1 blackdice2
                              'WhiteDice 'Nowhere 'Nowhere
                              (cons (Game board player moves) history))
                             (World 
                              (Game board player moves)
                              style whitedice1 whitedice2
                              blackdice1 blackdice2
                              'WhiteDice 'Nowhere 'Nowhere history)) w)])]
                       ['WhiteOff
                        (if (legal-move? (Game board player moves)
                                         last-click-2 'WhiteOff)
                            (World (apply-move (Game board player moves)
                                               last-click-2
                                               'WhiteOff)
                                   style whitedice1 whitedice2
                                   blackdice1 blackdice2
                                   'WhiteDice 'Nowhere 'Nowhere
                                   (cons (Game board player moves) history)) w)]
                       [_ w])])]
               [_ w])])]))]

    [_ w]))


(check-expect (click-handler
               (World (Game test-board 'Black
                            (list 1 4)) test-style
                                        4 1 2 3
                                        'BlackDice 'BlackBar 'Nowhere '())
               595 400 "button-down")
              (World
               (Game
                (Board
                 (list (OccupiedPoint 'Black 3) 'EmptyPoint 'EmptyPoint
                       'EmptyPoint 'EmptyPoint (OccupiedPoint 'White 5)
                       'EmptyPoint (OccupiedPoint 'White 3) 'EmptyPoint
                       'EmptyPoint 'EmptyPoint (OccupiedPoint 'Black 5)
                       (OccupiedPoint 'White 5) 'EmptyPoint 'EmptyPoint
                       'EmptyPoint (OccupiedPoint 'Black 3) 'EmptyPoint
                       (OccupiedPoint 'Black 5) 'EmptyPoint 'EmptyPoint
                       'EmptyPoint 'EmptyPoint (OccupiedPoint 'White 2))
                 1 3 2 5) 'Black (list 4)) test-style
                                           4 1 2 3
                                           'BlackDice 'Nowhere 'Nowhere
                                           (list (Game test-board 'Black
                                                       (list 1 4)))))

(check-expect (click-handler
               (World (Game test-board 'Black (list 2 4)) test-style
                      4 1 2 3
                      'BlackDice 'Nowhere 'Nowhere '()) 595 400 "button-down")
              (World (Game test-board 'Black (list 2 4)) test-style
                     4 1 2 3
                     'BlackDice (PointNum 1) 'Nowhere '()))

(: x-location : BoardLoc Style -> Real)
;;tells us the x-location of a BoardLoc
;;inputs boardloc and style
;;outputs a real
(define (x-location bl style)
  (match style
    [(Style rad pad bc wc dp lp bg label bd wd)
     (match bl
       ['BlackOff (+ (* 30 rad) (* 12 pad))]
       ['WhiteOff (+ (* 30 rad) (* 12 pad))]
       ['BlackBar (+ (* 14 rad) (* 7 pad))]
       ['WhiteBar (+ (* 14 rad) (* 7 pad))]
       [(PointNum num)
        (cond
          [(< 0 num 7) (+ (- (* (- 7 num) (+ (* 2 rad) pad)) (+ pad rad))
                          (+ (* 16 rad) (* 7 pad)))]                       
          [(< 6 num 13) (- (* (- 13 num) (+ (* 2 rad) pad)) (+ pad rad))]
          [(< 12 num 19) (- (* (- num 12) (+ (* 2 rad) pad)) (+ pad rad))]
          [else (+ (- (* (- num 18) (+ (* 2 rad) pad)) (+ pad rad))
                   (+ (* 16 rad) (* 7 pad)))])]
       [else -50])]))


(: y-location : BoardLoc Style -> Real)
;;tells us the y-location of a BoardLoc
;;inputs boardloc and style
;;outputs a real
(define (y-location bl style)
  (match style
    [(Style rad pad bc wc dp lp bg label bd wd)
     (match bl
       ['BlackOff (* 13 rad)]
       ['WhiteOff (* 15 rad)]
       ['BlackBar (* 13 rad)]
       ['WhiteBar (* 15 rad)]
       [(PointNum num)
        (cond
          [(< 0 num 13) (* 23 rad)]
          [else (* 5 rad)])]
       [else -50])]))

(: nowhere : ClickLoc -> Symbol)
(define (nowhere cl)
  (match cl
    ['Nowhere 'Nowhere]
    [_ 'some]))
          
         
( : draw-world : World -> Image)
(define (draw-world w)
  (match w
    [(World (Game board player moves) style wd1 wd2 bd1 bd2 lc1 lc2 lc3 history)
     (match style
       [(Style rad pad _ _ _ _ _ _ _ _ )
        (if (symbol=? (nowhere lc1) 'Nowhere)
            (place-image/align
             (match* (lc2 lc3)
               [(lc2 'Nowhere)
                (match lc2
                  [(PointNum num)
                   (rectangle (* 2 rad) (* 10 rad) "outline" 'black)]
                  [_ (square (* 2 rad) "outline" 'black)])]
               [(_ _) empty-image])
             (x-location lc2 style )
             (y-location lc2 style ) "center" "center" 
             (place-image/align (black-die rad bd1 )
                                (+ (* 16 rad) (* 8 rad))
                                (* 14 rad) "center" "center"
                                (place-image/align
                                 (white-die rad  wd1)
                                 (+ (* 2.5 pad) (* 6 rad))
                                 (* 14 rad) "center" "center"
                                 (draw-board style board)))
             )
            
            (place-image/align
             (match* (lc2 lc3)
               [(lc2 'Nowhere)
                (match lc2
                  [(PointNum num)
                   (rectangle (* 2 rad) (* 10 rad) "outline" 'black)]
                  [_ (square (* 2 rad) "outline" 'black)])]
               [(_ _) empty-image])
             (x-location lc2 style )
             (y-location lc2 style ) "center" "center" 
             (place-image/align (beside (black-die rad bd1 )
                                        (square pad 0 "whitesmoke")
                                        (black-die rad bd2))
                                (+ (* 16 rad) (* 8 rad))
                                (* 14 rad) "center" "center"
                                (place-image/align
                                 (beside (white-die rad  wd1)
                                         (square pad 0 "whitesmoke")
                                         (white-die rad wd2))
                                 (+ (* 2.5 pad) (* 6 rad))
                                 (* 14 rad) "center" "center"
                                 (draw-board style board)))
             ))
        ])
     ]))

(: last-start-turn : (Listof Game) -> Game)
(define (last-start-turn games)
  ;; Get the start of the previous turn
  ;; the last turn started
  (match games
    ['() (error "can't undo until you start the game mate")]
    [(cons f r)
     (cond
       [(empty? r) f]
       [(player=? (Game-turn f)
                  (Game-turn (first r)))
        (last-start-turn r)]
       [else f])]))
;;tested in dice test
;;works
;;eyeball test passed
(: 2last-start-turn : (Listof Game) -> Game)
(define (2last-start-turn games)
  ;; Get the start of the turn before the previous turn, in order
  ;; to get the dice value
  (match games
    ['() (error "can't undo until you start the game mate")]
    [(cons f r)
     (cond
       [(empty? r) f]
       [(player=? (Game-turn f)
                  (Game-turn (first r)))
        (2last-start-turn r)]
       [else (last-start-turn r)])]))
;;tested in dice test
;;works
;;eyeball test passed

(: gametomoves : Game -> (Listof Integer))
;;inputs a game and outputs the list of moves
(define (gametomoves game)
  (match game
    [(Game board player moves) moves]))
(check-expect (gametomoves
               (Game (Board '() 0 0 0 0)
                     'White '(1 2 3))) '(1 2 3))


(: notfirst : (Listof Game) -> (Listof Game))
;; drops the first iten from the list
;; Inputs a listof Gaem and outputs a listof Game
(define (notfirst l)
  (match l
    ['() '()]
    [(cons f '()) (list f)]
    [(cons f r) r]))

(check-expect (list (Game (Board '() 0 0 0 0)
                          'White '(1 2 3)) 
                    (Game (Board '() 0 0 0 0)
                          'White '(1 2 3)))
              (Game (Board '() 0 0 0 0)
                    'White '(1 2 3)))

(: changed-moves? : (Listof Game) -> Boolean)
;;checks if you changed-moves in the previous move
;; used in undo to know which dice required
(define (changed-moves? l)
  (match l
    ['() #f]
    [(cons f '()) #f]
    [(cons f r)
     (if (available-moves? f) #f #t)]))

;;works
;;checked in later function

(: initial-world : Integer Integer -> World)
;; sets up initial dice value in order to to get different values
;;compares em and sees which is greater
;;used in run
(define (initial-world w1 b1)
  (if (= w1 b1)
      (initial-world (random 1 7) (random 1 7))
      (World (Game initial-board (if (> w1 b1) 'White 'Black) (list w1 b1))
             test-style w1 0 b1 0 (if (> w1 b1) 'WhiteDice 'BlackDice) 'Nowhere
             'Nowhere '())))

(: point-string : Point -> String)
;;converts a point to a string based on
;;instructions given
(define (point-string point)
  (match point
    ['EmptyPoint "_"]
    [(OccupiedPoint player count)
     (match player
       ['Black (string-append "B" (number->string count))]
       ['White (string-append "W" (number->string count))])]))

(: string-point : String -> Point)
;;converts a string to a point based on
;; instructions given
(define (string-point str)
  (match str
    ["_" 'EmptyPoint]
    [t (if (string=?
            (substring t 0 1)
            "B")
           (OccupiedPoint 'Black
                          (string->integer
                           (substring t 1)))
           (OccupiedPoint 'White
                          (string->integer
                           (substring t 1))))]))

(check-expect (string-point "W3") (OccupiedPoint 'White 3))

(: listp-string : (Listof Point) -> String)
;;convers a list of points to string
;;based on instructions given
(define (listp-string lp)
  (match lp
    ['() ""]
    [(cons f '()) (point-string f)]
    [(cons f r) (string-append (point-string f) " " (listp-string r))]))

(: split : Char String -> (Listof String))
;; divide the string around the given character
;; ex: (split "a&bb&c") -> (list "a" "bb" "c")
;;taken from class
(define (split delim s)
  (local
    {(: lp : (Listof Char) (Listof Char) -> (Listof String))
     ;; traverse list of characters, building up "current-string" along the way
     ;; when "delim" is encountered, cons current-string onto result
     (define (lp chars current-string)
       (match chars 
         ['() (list (list->string (reverse current-string)))]
         [(cons f r)
          (if (char=? f delim)
              (cons (list->string (reverse current-string)) (lp r '()))
              (lp r (cons f current-string)))]))}
    (lp (string->list s) '())))

(: string-lp : String -> (Listof Point))
;;converts a string to a listof points
;;inputs a string and outputs a listof point

(define (string-lp s)
  (local
    {(: actual : (Listof String) -> (Listof Point))
     ;;recursion requires an input of listof string
     
     (define (actual strings)
       (match strings
         ['() '()]
         [(cons f r)
          (cons (string-point f)
                (actual r))]))}
    (actual (string-split s " "))))
  

(: board-string : Board -> String)
(define (board-string board)
  (match board
    [(Board lp bb wb bo wo)
     (string-append (listp-string lp) "|" (number->string bb) "|"
                    (number->string wb) "|" (number->string bo) "|"
                    (number->string wo))]))

(: string->integer : String -> Integer)
;;taken from the profffssss
;;not my fault
(define (string->integer s)
  (local
    {(define conv : (U Complex False) (string->number s))}
    (if (complex? conv) (exact-round (real-part conv))
        (error "string->integer: invalid integer"))))

(: string-board : String -> Board)
(define (string-board str)
  (local
    {(: list : (Listof String))
     (define list (string-split str "|"))}
    (if (= 24 (length (string-lp (first list))))
        (Board (string-lp (first list))
               (string->integer (second list))
               (string->integer (third list))
               (string->integer (fourth list))
               (string->integer (fifth list)))
        (error "incorrect number of points"))))
(check-expect (string-board "_ _ _ _ _ _ _ W3 _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _|0|3|2|5")
              (Board (list 'EmptyPoint 'EmptyPoint 'EmptyPoint 'EmptyPoint 'EmptyPoint
                           'EmptyPoint 'EmptyPoint (OccupiedPoint 'White 3)
                           'EmptyPoint 'EmptyPoint 'EmptyPoint 'EmptyPoint
                           'EmptyPoint 'EmptyPoint 'EmptyPoint 'EmptyPoint
                           'EmptyPoint 'EmptyPoint 'EmptyPoint 'EmptyPoint
                           'EmptyPoint 'EmptyPoint 'EmptyPoint 'EmptyPoint)
                     0 3 2 5))
                                  
  

(: list-string : (Listof Integer) -> String)
;; splits moves
(define (list-string l)
  (match l
    ['() ""]
    [(cons f '()) (number->string f)]
    [(cons f r) (string-append (number->string f) " ")]))

(: string-list : String -> (Listof Integer))
(define (string-list str)
  (if (string=? str "")
      '()
      (local
        {(: helper : (Listof String) -> (Listof Integer))
         (define (helper s)
           (match s
             ['() '()]
             [(cons f r)
              (cons (string->integer f)
                    (helper r))]))}
        (helper (string-split str " ")))))
 


(: game-string : Game -> String)
(define (game-string game)
  (match game
    [(Game board player moves)
     (string-append (board-string board) "@"
                    (if (player=? 'Black player)
                        "B" "W") "@" (list-string moves))]))
    

(: string-game : String -> Game)
(define (string-game str)
  (Game (string-board (first (split #\@ str)))
        (if (string=? (second (split #\@ str)) "B") 'Black 'White)
        (string-list (third (split #\@ str)))))
  
       
(: history-string : (Listof Game) -> String)
(define (history-string l)
  (match l
    ['() ""]
    [(cons f '()) (game-string f)]
    [(cons f r) (string-append (game-string f) "!" (history-string r))]))

(: string-history : String -> (Listof Game))
(define (string-history str)
  (local { (: helper : (Listof String) -> (Listof Game))
           (define (helper l)
             (match l
               ['() '()]
               [(cons f r) (cons (string-game f) (helper r))]))}
    (helper (split #\! str))))
 

(: world->string : World -> String)
;;converts a world to a string to save the game

(define (world->string world)
  (match world
    [(World (Game board player moves) style wd1 wd2 bd1 bd2 lc1 lc2 lc3 history)
     (history-string (append (list (Game board player moves)) history))]))

(: string->world : Style String -> World)
;;takes in a style and string and outputs a world
;; converts the string to a world for a load game
(define (string->world sty str)
  (match (string-history str)
    ['() (error "no saved game")]
    [(cons f r)
     (match f
       [(Game board player moves)
        (match player
          ['White (World (Game board player moves) sty
                         (list-ref (gametomoves
                                    (last-start-turn (string-history str))) 0)
                         (list-ref (gametomoves
                                    (last-start-turn (string-history str))) 1)
                         (list-ref (gametomoves
                                    (2last-start-turn (string-history str))) 0)
                         (list-ref (gametomoves
                                    (2last-start-turn (string-history str))) 1)
                         'WhiteDice 'Nowhere 'Nowhere r)]
          ['Black (World (Game board player moves) sty
                         (list-ref (gametomoves
                                    (last-start-turn (string-history str))) 0)
                         (list-ref (gametomoves
                                    (last-start-turn (string-history str))) 1)
                         (list-ref (gametomoves
                                    (2last-start-turn (string-history str))) 0)
                         (list-ref (gametomoves
                                    (2last-start-turn (string-history str))) 1)
                         'BlackDice 'Nowhere 'Nowhere r)])])]))
       



;; prompt the user for an output file location
;; then, save the game to that file
;; do nothing if the user cancels
(: save-game! : World -> Void)
(define (save-game! w)
  (local
    {(define path : (U Path False) (put-file))}
    (if (path? path)
        (begin
          (write-string (world->string w)
                        (open-output-file path))
          (void))
        (void))))

;; ask the user to choose a file
;; then load an in-progress game from that file
;; use the provided Style to make a new World
;; raise an error if the user cancels or if something goes wrong
(: load-game : Style -> World)
(define (load-game s)
  (local
    {(define path : (U Path False) (get-file))}
    (if (path? path)
        (string->world s (port->string (open-input-file path)))
        (error "load-game: user cancelled"))))

(: key-handler : World String -> World)
(define (key-handler world input)
  (match world
    [(World (Game board player moves) style wd1 wd2 bd1 bd2 lc1 lc2 lc3 history)
     (match input
       [(or "U" "u")
        (match (first history)
          [(Game board1 player1 moves1)
           (if (changed-moves? history)
               (match player
                 ['Black
                  (World (first history) style
                         wd1
                         wd2
                         (list-ref (gametomoves
                                    (2last-start-turn history)) 0)
                         (list-ref (gametomoves
                                    (2last-start-turn history)) 1)
                         'BlackDice 'Nowhere 'Nowhere (notfirst history))]
                 ['White
                  (World (first history) style
                         (list-ref (gametomoves
                                    (2last-start-turn history)) 0)
                         (list-ref (gametomoves
                                    (2last-start-turn history)) 1)
                         bd1
                         bd2
                         'WhiteDice 'Nowhere 'Nowhere (notfirst history))])
               (match player
                 ['Black (World (first history) style
                                wd1 wd2 bd1 bd2
                                'BlackDice 'Nowhere 'Nowhere
                                (notfirst history))]
                 ['White (World (first history) style
                                wd1
                                wd2
                                bd1
                                bd2
                                'WhiteDice 'Nowhere 'Nowhere
                                (notfirst history))]))])]
       [(or "S" "s")
        (begin (save-game! world) world)]
       [(or "L" "l")
        (load-game test-style)])]))


(: run : Style -> World)
;;creates universe
;; runs
;;takes in a style and outputs a World
(define (run style)
  (big-bang (initial-world (random 1 7) (random 1 7)) : World
    [to-draw draw-world]
    [on-mouse click-handler]
    [on-key key-handler]))

