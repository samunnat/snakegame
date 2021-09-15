;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |snake game|) (read-case-sensitive #t) (teachpacks ((lib "image.ss" "teachpack" "2htdp") (lib "universe.ss" "teachpack" "2htdp") (lib "batch-io.ss" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.ss" "teachpack" "2htdp") (lib "universe.ss" "teachpack" "2htdp") (lib "batch-io.ss" "teachpack" "2htdp")) #f)))

(define box 30)
; size of everything
(define snakeblock (overlay (square (* box .996) "solid" "black") (square box "solid" "green")))

(define (column n i)
  (cond
    [(>= 1 n) i]
    [(< 1 n) (above i (column (sub1 n) i))]))

(define (row n i)
  (cond
    [(>= 1 n) i]
    [(< 1 n) (beside i (row (sub1 n) i))]))

(define width 25)
;number of "box"es horizontally

(define height 25)
; number of boxes vertically

(define (boxnum n)
  (+ (* n box) (/ box 2)))
; turns number into the number of boxes on the screen (box -> coordinate)

(define snakescreen (overlay (column height (row width snakeblock)) (empty-scene (* width box)(*  height box))))
; image-height and image-width 600zxc 
(define rleyes (above (circle (* .15 box) "solid" "blue") (rectangle (* .05 box) (* .3 box) "solid" "red") (circle (* .15 box) "solid" "blue")))
(define colors (overlay (circle (* box .2) "solid" "black") (overlay (circle (* box .25) "solid" "blue")
                          (overlay (circle (* box .30) "solid" "green")
                                   (overlay (circle (* box .35) "solid" "yellow")
                                            (overlay (circle (* box .40) "solid" "orange")
                                                     (overlay (circle (* box .45) "solid" "red") (circle (* box .5) "solid" "white"))))))))
(define righthead (overlay/align "right" "center" rleyes colors))
(define lefthead (overlay/align "left" "center" rleyes colors))
(define dteyes (beside (circle (* .15 box) "solid" "blue") (rectangle (* .25 box) (* .05 box) "solid" "red") (circle (* .15 box) "solid" "blue")))
(define downhead (overlay/align "center" "bottom" dteyes colors))
(define tophead (overlay/align "center" "top" dteyes colors))
(define bodsnake (overlay (circle (* box .20) "solid" "blue") colors))
(define food (overlay (circle 2.5 "solid" "green") (overlay (circle (/ box 2) "solid" "orange") (rectangle (* box .9) (* box .9) "solid" "red"))))
(define scorebox (overlay (rectangle (/ (boxnum width) 5) 25 "solid" "yellow") (rectangle (/ (boxnum width) 4) 30 "solid" "blue")))
(define fullscreen (empty-scene (- (boxnum width) (boxnum 0)) (+ (boxnum height) (boxnum 0)) "white"))
;(overlay/xy (above food tophead bodsnake bodsnake bodsnake) -300 -300 snakescreen)
; in order for the snake to move slowly but block by block, make the speed fast, but then add a number in the big bang function tock part
(define-struct snake [sposn xvel yvel fdpsn length])

(define (countlist list)
  (cond
    [(empty? list) 0]
    [(empty? (rest list)) 1]
    [(cons? (rest list)) (+ 1 (countlist (rest list)))]
    [(and (cons? (first list)) (empty? (rest list))) 1]))
(countlist (cons 5 empty))
; counts the number of cons in a list

(define (findlist n list)
  (cond
    [(empty? list) empty]
    [(cons? list)
     (cond
       [(zero? n) (first list)]
       [(positive? n) (findlist (sub1 n) (rest list))])]))
; this function outputs the string/number from N part of the list
(check-expect (findlist 3 (cons "red"
      (cons "orange"
            (cons "yellow"
                  (cons "green" empty))))) "green")


(define (lastlist list)
  (findlist (- (countlist list) 1) list))

(define (snakehead snake)
  (cond
    [(and (positive? (snake-xvel (findlist 0 snake)))
          (zero? (snake-yvel (findlist 0 snake))))
     righthead]
    [(and (zero? (snake-xvel (findlist 0 snake)))
          (positive? (snake-yvel (findlist 0 snake))))
     downhead]
    [(and (zero? (snake-xvel (findlist 0 snake)))
          (negative? (snake-yvel (findlist 0 snake))))
     tophead]
    [(and (negative? (snake-xvel (findlist 0 snake)))
          (zero? (snake-yvel (findlist 0 snake))))
     lefthead]))

; determines what snake head to use

(define (snakelist snake n im)
  (cond
    [(zero? n) (place-image (snakehead snake) (posn-x (snake-sposn (findlist 0 snake))) (posn-y (snake-sposn (findlist 0 snake))) im)]
    [(positive? n) (place-image bodsnake (posn-x (snake-sposn (findlist n snake))) (posn-y (snake-sposn (findlist n snake))) (snakelist snake (sub1 n) im))]))
; renders the snake with positions gotten from the lists ---> WORKS

(define (renderfood snake im)
  (place-image food (posn-x (snake-fdpsn (findlist 0 snake))) (posn-y (snake-fdpsn (findlist 0 snake))) im))

(define (renderscore snake gm im)
  (overlay/align "left" "top" gm
                 (overlay/align "right" "bottom" 
                                (overlay (text (string-append "Score: " 
                                                              (number->string (snake-length (findlist 0 snake)))) 
                                               12 "red") 
                                         scorebox)
                                im)))

(define (rendersnake snake)
  (renderscore snake (snakelist snake (- (countlist snake) 1) (renderfood snake snakescreen)) fullscreen))

; (rendersnake startsnake)

(define listtocut (cons 0 (cons 1 (cons 2 (cons 3 (cons 4 (cons 5 (cons 6 (cons 7 empty)))))))))

(define (cutlist list sn en)
  (cond 
    [(= sn en) empty]
    [else (cons (findlist sn list) (cutlist list (add1 sn) en))]))
;(cutlist listtocut 0 1)
; from sn to en

(define (removelist list n)
  (append (cutlist list 0 (- n 1)) (cutlist list n (countlist list))))
(check-expect (removelist listtocut 3) (cons 0 (cons 1 (cons 3 (cons 4 (cons 5 (cons 6 (cons 7 empty))))))))


(define (same posn1 posn2)
  (and (= (posn-x posn1) (posn-x posn2))
       (= (posn-y posn1) (posn-y posn2))))
(check-expect
 (same (make-posn 2 2)
       (make-posn 2 2))
 true)
; finds out if two posns are the same

(define (ctnpsn psn list)
  (cond
    [(empty? list) false]
    [(cons? list) (or (same (first list) psn)
                      (ctnpsn psn (rest list)))]
    [else false]))
(check-expect
 (ctnpsn (make-posn 2 2) (cons (make-posn 1 1) (cons (make-posn 2 2) (cons (make-posn 3 2) empty))))
 true)
; determines if a posn is in any of the cons in the list.


(define (sposnlist list n)
  (cond
    [(= n (countlist list)) empty]
    [else (cons (snake-sposn (findlist n list)) (sposnlist list (add1 n)))]))
;(sposnlist ssnake 0)

(define (removelast list)
  (cutlist list 0 (- (countlist list) 1)))
;(removelast ssnake)

(define (foodsnake list posn)
  (cond
    [(ctnpsn posn (sposnlist list 0))
     (make-posn (boxnum (random width)) (boxnum (random height)))]
    [else posn]))
; create list of sposns and foodposns, and check to see if the foodposn is contained in the list.
; (foodsnake ssnake (make-posn 10 70))

(define (addlist list listoadd n)
  (append (cutlist list 0 n) listoadd (cutlist list n (countlist list))))
;(addlist listtocut (cons "Sam" empty) 0)

(define (replacelist list listoreplace n)
  (cond
    [(empty? list) listoreplace]
    [else (addlist (removelist list (+ 1 n)) listoreplace n)]))
;(replacelist empty (cons "I'mgreat" empty) 0)
; replaces a part of the list with another without disturbing the rest of list

(define (addhead snake n)
  (cond
    [(and (positive? (snake-xvel (findlist n snake)))
          (zero? (snake-yvel (findlist n snake))))
     (make-posn (+ box (posn-x (snake-sposn (findlist n snake))))
                                          (posn-y (snake-sposn (findlist n snake))))]
    [(and (zero? (snake-xvel (findlist n snake)))
          (positive? (snake-yvel (findlist n snake))))
     (make-posn (posn-x (snake-sposn (findlist n snake)))
                            (+ box (posn-y (snake-sposn (findlist n snake)))))]
    [(and (zero? (snake-xvel (findlist n snake)))
          (negative? (snake-yvel (findlist n snake))))
     (make-posn (posn-x (snake-sposn (findlist n snake)))
                            (- (posn-y (snake-sposn (findlist n snake))) box))]
    [(and (negative? (snake-xvel (findlist n snake)))
          (zero? (snake-yvel (findlist n snake))))
     (make-posn (- (posn-x (snake-sposn (findlist n snake))) box)
                            (posn-y (snake-sposn (findlist n snake))))]))
; adds head based on (findlist 0 snake)'s velocity

(define (changeposn snake yn)
  (append (cons (make-snake (addhead snake 0) 
                            (snake-xvel (findlist 0 snake))
                            (snake-yvel (findlist 0 snake))
                            (cond
                              [(string=? "y" yn) (foodsnake snake (make-posn (boxnum (random width)) (boxnum (random height))))]
                              [else (snake-fdpsn (findlist 0 snake))])
                            (cond
                              [(string=? "y" yn) (+ (snake-length (findlist 0 snake)) 1)]
                              [(string=? "n" yn) (snake-length (findlist 0 snake))])) empty)
          (cond
            [(string=? "y" yn) snake]
            [(string=? "n" yn) (removelast snake)])))
; "y"= food has been eaten- grow the snake
; "N"= no food has been eaten- keep adding head and subtracting tail

(define (movesnake snake)
  (cond
    [(same (snake-sposn (findlist 0 snake)) (snake-fdpsn (findlist 0 snake)))
     (changeposn snake "y")]
    [else (changeposn snake "n")]))
; main tock function
; adds head and subtracts tail if no food is eaten, and if it is, it doesn't subtract the tail to lengthen the snake

(define (keysnake snake ke)
  (cond
    [(and (string=? "up" ke)
          (not (positive? (snake-yvel (findlist 0 snake))))) (replacelist snake (cons (make-snake (snake-sposn (findlist 0 snake))
                                                             0
                                                             -1
                                                             (snake-fdpsn (findlist 0 snake))
                                                             (snake-length (findlist 0 snake))) empty) 0)]
    [(and (string=? "down" ke)
          (not (negative? (snake-yvel (findlist 0 snake))))) (replacelist snake (cons (make-snake (snake-sposn (findlist 0 snake))
                                                             0
                                                             1
                                                             (snake-fdpsn (findlist 0 snake))
                                                             (snake-length (findlist 0 snake))) empty) 0)]
    [(and (string=? "right" ke)
          (not (negative? (snake-xvel (findlist 0 snake))))) (replacelist snake (cons (make-snake (snake-sposn (findlist 0 snake))
                                                             1
                                                             0
                                                             (snake-fdpsn (findlist 0 snake))
                                                             (snake-length (findlist 0 snake))) empty) 0)]
    [(and (string=? "left" ke)
          (not (positive? (snake-xvel (findlist 0 snake))))) (replacelist snake (cons (make-snake (snake-sposn (findlist 0 snake))
                                                             -1
                                                             0
                                                             (snake-fdpsn (findlist 0 snake))
                                                             (snake-length (findlist 0 snake))) empty) 0)]
    [else snake]))

(define (stopsnake snake)
  (cond
    [(or (>= (posn-x (snake-sposn (findlist 0 snake)))
             (boxnum width))
         (or (<= (posn-x (snake-sposn (findlist 0 snake)))
                 (boxnum -1))
             (or (>= (posn-y (snake-sposn (findlist 0 snake)))
                     (boxnum height))
                 (<= (posn-y (snake-sposn (findlist 0 snake)))
                     (boxnum -1)))))
     true]
    [(ctnpsn (snake-sposn (findlist 0 snake)) (sposnlist (cutlist snake 1 (countlist snake)) 0)) true]
    [else false]))

(define endbox (overlay (rectangle (* (boxnum width) (/ 3.5 5)) 25 "solid" "yellow") (rectangle (* (boxnum width) (/ 3.65 5)) 30 "solid" "red")))

(define (stopscreen snake)
  (cond
    [(or (>= (posn-x (snake-sposn (findlist 0 snake)))
             (boxnum width))
         (or (<= (posn-x (snake-sposn (findlist 0 snake)))
                 (boxnum -1))
             (or (>= (posn-y (snake-sposn (findlist 0 snake)))
                     (boxnum height))
                 (<= (posn-y (snake-sposn (findlist 0 snake)))
                     (boxnum -1)))))
     (overlay/align "left" "bottom" (overlay (text "OOPS! Your snake veered offscreen!" (- width (* width .1)) "red") endbox)
                    (rendersnake snake))]
    [(ctnpsn (snake-sposn (findlist 0 snake)) (sposnlist (cutlist snake 1 (countlist snake)) 0))
     (overlay/align "left" "bottom" (overlay (text "OH NO! Your snake ate itself!" 18 "red") endbox)
                    (rendersnake snake))]
    [else (rendersnake snake)]))
; screen rendered at stop, at the bottem-left part of fullscreen

(define snakespeed .2)
; .1(fast) to 10(slow)

(define startsnake (cons (make-snake (make-posn (boxnum 1) (boxnum 1)) 0 5 (make-posn (boxnum (random width)) (boxnum (random height))) 1) empty))
; starts with one snake

(define (playsnake snake) (big-bang snake (on-draw rendersnake) (on-tick movesnake snakespeed) (on-key keysnake) (stop-when stopsnake stopscreen)))
(playsnake startsnake)

