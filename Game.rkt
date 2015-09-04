(require 2htdp/image) 
(require 2htdp/universe)

; 
; INSTRUCTIONS: 
;  Fill the grid with Blue and White squares.
; - A 3-In-A-Row of the same colour is not allowed.
; - Each row and column has an equal number of Blue and White squares.
; 
; - Click to change the color of a square
; - Already colored squares at the beginning of the game cannot be changed
; 
; http://www.brainbashers.com/show3inarow.asp?date=1130&diff=1&size=6
; 
; *****(main G0) to solve MTB***** ;; to see solution: (render-bd (solve MTB))
; *****(main G1) to solve BD1***** ;; to see solution: (render-bd (solve BD1))


;; CONSTANTS: 
(define ROWS (list (list 0 1 2 3 4 5)        ;ROWS
                   (list 6 7 8 9 10 11)
                   (list 12 13 14 15 16 17)
                   (list 18 19 20 21 22 23)
                   (list 24 25 26 27 28 29)
                   (list 30 31 32 33 34 35)))
(define COLUMNS (list (list 0 6 12 18 24 30) ;COLUMNS
                      (list 1 7 13 19 25 31)
                      (list 2 8 14 20 26 32)
                      (list 3 9 15 21 27 33)
                      (list 4 10 16 22 28 34)
                      (list 5 11 17 23 29 35)))

(define BOARD-SIZE 500)

(define MTSQR (overlay (square (/ BOARD-SIZE 6) "outline" "black")
                       (square (/ BOARD-SIZE 6) "solid" (make-color 190 190 190))))
(define BSQR (overlay (square (/ BOARD-SIZE 6) "outline" "black")
                      (square (/ BOARD-SIZE 6) "solid" "blue")))
(define WSQR (overlay (square (/ BOARD-SIZE 6) "outline" "black")
                      (square (/ BOARD-SIZE 6) "solid" "white")))

;; Data definitions 

;; Position is Natural[0,26)
;; interp. the position of a specific square on the board 

(define E empty)
(define B "blue")
(define W "white")

;; Value is one of:
;; - E
;; - B
;; - W
;; interp. a square, either empty, or blue or white 

;; Board is one of:
;; - empty
;; - (cons Square Board) 
;; interp. a 3-in-a-row game board 

(define MTB (list E E E E E E 
                  E E E E E E 
                  E E E E E E 
                  E E E E E E 
                  E E E E E E 
                  E E E E E E )) ;an empty board 
(define BD1 (list E E W E E E
                  E E E B E E
                  E E W E W W
                  B E E E E E
                  E B E E E E
                  E E W E E E)) ;a sample starting board 
(define BD2 (list W E W E E E
                  E E E B E E
                  E E W E W W
                  B E E E E E
                  E B E E E E
                  E E W E E E)) ;a possible next board for BD1
(define BD3 (list W W W E E E
                  E E E B E E
                  E E W E W W
                  B E E E E E
                  E B E E E E
                  E E W E E E)) ;an invalid board 
(define BD4 (list W B W W B B
                  W W B B W B
                  B B W B W W
                  B W B W B W
                  W B B W W B
                  B W W B B W)) ;a solution to BD1 
(define BD5 (list E E W E E E
                  W W E B B E
                  E E W E W W
                  B E E E E E
                  E B E E E E
                  E E W E E E)) ;an unsolvable board 
(define BD6 (list W B W W B B
                  W W B B E B
                  B B E B W W
                  B W E W B W
                  W E B W W B
                  B W W B B E)) ;somewhere between BD1 and BD4


(define-struct game (bd lop))
;; Game is (make-game Board (listof Position))
;; interp. the current game board, and a list of the unchangeable positions 
;;         ie. the initial blue and white squares cannot be changed 

(define G1 (make-game BD1 (list 2 9 14 16 17 18 25 32)))
(define G0 (make-game MTB empty))

;; =============================================================================
;; Functions 

;; Board -> Game 
;; produce the game state of a board 
(check-expect (board->game MTB) (make-game MTB empty))
(check-expect (board->game BD1) (make-game BD1 (list 2 9 14 16 17 18 25 32)))

(define (board->game bd)
  (make-game bd 
             (find-fold bd (λ (sqr) (not (empty? sqr))))))


;; Board -> Board 
;; start the game with (main G1)

(define (main g)
  (big-bang g                          ; Game
            (to-draw   render)         ; Game -> Image
            (stop-when game-over?)     ; Game -> Boolean
            (on-mouse  handle-mouse))) ; Game Integer Integer MouseEvent -> Game

;; Game -> Boolean 
;; produce true if the current game board has no empty squares and is valid  
(check-expect (game-over? (board->game BD4)) true)
(check-expect (game-over? (board->game BD1)) false)
(check-expect (game-over? (make-game MTB empty)) false)

(define (game-over? g)
  (and (solved? (game-bd g))
       (valid? (game-bd g))))

;; Game -> Image 
;; produce the image of the game 

(define (render g)
  (render-bd (game-bd g)))

;; Board -> Image 
;; produce the image of the board 

(define (render-bd bd)
  (foldr above empty-image (map (λ (lop) (render-line bd lop)) ROWS)))

;; Board (listof Position) -> Image 
;; produce the image of one line on the board 

(check-expect (render-line MTB (list 0 1 2 3 4 5))
              (beside (overlay (square (/ BOARD-SIZE 6) "outline" "black")
                               MTSQR)
                      (overlay (square (/ BOARD-SIZE 6) "outline" "black")
                               MTSQR)
                      (overlay (square (/ BOARD-SIZE 6) "outline" "black")
                               MTSQR)
                      (overlay (square (/ BOARD-SIZE 6) "outline" "black")
                               MTSQR)
                      (overlay (square (/ BOARD-SIZE 6) "outline" "black")
                               MTSQR)
                      (overlay (square (/ BOARD-SIZE 6) "outline" "black")
                               MTSQR)))

(define (render-line bd lop)
  (foldr beside empty-image (map (λ (p) (render-square bd p)) lop)))

;; Position -> Image 
;; produce the image of one square on the board 
(check-expect (render-square MTB 0)
              (overlay (square (/ BOARD-SIZE 6) "outline" "black")
                       MTSQR))

(define (render-square bd p)
  (local [(define value (find-value bd p))]
    (cond [(empty? value)
           MTSQR]
          [(string=? B value)
           BSQR]
          [else 
           WSQR])))

;; Game Integer Integer MouseEvent -> Game 
;; change the game (board) when the mouse is clicked

(define (handle-mouse g x y me)
  (make-game (handle-mouse-bd (game-bd g) x y me (game-lop g))
             (game-lop g)))

;; Board Integer Integer MouseEvent (listof Position) -> Board
;; change the board when the mouse is clicked 
;; if mouse is clicked on an unchangeable sqr, or outside the board, keep board
(check-expect (handle-mouse-bd MTB 0 0 "button-down" empty)
              (build-list 36 (λ (n) (if (= 0 n)
                                        B 
                                        E))))
(check-expect (handle-mouse-bd BD1 BOARD-SIZE BOARD-SIZE "button-down"
                               (list 2 9 14 16 17 18 25 32))
              (list E E W E E E
                    E E E B E E
                    E E W E W W
                    B E E E E E
                    E B E E E E
                    E E W E E B))
(check-expect (handle-mouse-bd BD1 124235345 -124 "drag"
                               (list 2 9 14 16 17 18 25 32))
              BD1) 
(check-expect (handle-mouse-bd BD1 BOARD-SIZE BOARD-SIZE "drag"
                               (list 2 9 14 16 17 18 25 32))
              BD1)

(define (handle-mouse-bd bd x y me lop)
  (cond [(and 
          (not (member? (find-square x (find-row y)) lop)) ;not unchangeable sqr
          (mouse=? me "button-down"))
         (change-board bd x y)]
        [else bd]))


;; Board Integer Integer -> Board 
;; produce the new board with the square at position x,y changed 

(check-expect (change-board MTB 0 0) ;change sqr 0
              (build-list 36 (λ (n) (if (= 0 n)
                                        B 
                                        E))))
(check-expect (change-board BD6 BOARD-SIZE 0) ;change sqr 5 
              (list W B W W B W   
                    W W B B E B
                    B B E B W W
                    B W E W B W
                    W E B W W B
                    B W W B B E))

(define (change-board bd x y)
  (local [(define sqr-pos (find-square x (find-row y)))]
    (if (not (false? sqr-pos))
        (fill-blank sqr-pos
                    bd
                    (change-square sqr-pos bd))
        bd)))

;; Square Board -> Square  
;; given a square on the board, change it 
;;       empty -> blue -> white 
(check-expect (change-square 0 MTB) ;empty->blue
              B)
(check-expect (change-square 35 BD1) ;empty->blue
              B)
(check-expect (change-square 5 BD6) ;blue->white 
              W)
(check-expect (change-square 0 BD6) ;white->empty
              E)

(define (change-square p bd)
  (local [(define value (find-value bd p))]
    (cond [(empty? value) B]
          [(string=? B value) W]
          [else empty]))) 

;; Integer -> (listof Position) 
;; given a y coord, determine the list of positions of squares in that row 
;; if y coord is outside the board, produce an empty list 

(check-expect (find-row 0) (list 0 1 2 3 4 5))
(check-expect (find-row BOARD-SIZE) (list 30 31 32 33 34 35))
(check-expect (find-row (+ 1 (/ BOARD-SIZE 6))) (list 6 7 8 9 10 11))
(check-expect (find-row 2389037486908902134) empty) ;y coord outside board


(define (find-row y)
  (local [(define ROW-SIZE (/ BOARD-SIZE 6))]
    (cond [(<= y ROW-SIZE) (first ROWS)]
          [(<= y (* 2 ROW-SIZE)) (second ROWS)]
          [(<= y (* 3 ROW-SIZE)) (third ROWS)]
          [(<= y (* 4 ROW-SIZE)) (fourth ROWS)]
          [(<= y (* 5 ROW-SIZE)) (fifth ROWS)]
          [(<= y BOARD-SIZE) (sixth ROWS)]
          [else empty])))

;; Integer (listof Position) -> Position or false
;; given an x coord and its row, produce the position of the corresponding sqr 
;; false if the mouse is clicked outside of board dimensions 
(check-expect (find-square 0 (list 0 1 2 3 4 5)) 0)
(check-expect (find-square (+ 1 (/ BOARD-SIZE 6)) (list 30 31 32 33 34 35)) 31)
(check-expect (find-square BOARD-SIZE (third ROWS)) 17)
(check-expect (find-square 123784903456789 
                           (list 0 1 2 3 4 5)) false) ;x coord outside board
(check-expect (find-square 10 empty) false) ;y coord outside board

(define (find-square x lop)
  (local [(define COLUMN-SIZE (/ BOARD-SIZE 6))]
    (cond [(empty? lop) false] ;if y coord was outside board produce false
          [(<= x COLUMN-SIZE) (first lop)]
          [(<= x (* 2 COLUMN-SIZE)) (second lop)]
          [(<= x (* 3 COLUMN-SIZE)) (third lop)]
          [(<= x (* 4 COLUMN-SIZE)) (fourth lop)]
          [(<= x (* 5 COLUMN-SIZE)) (fifth lop)]
          [(<= x BOARD-SIZE) (sixth lop)]
          [else false]))) 


;; SOLVE FUNCTION ==============================================================


;; Board -> Board or false
;; produce a solution to the given board, or false if can't 
(check-expect (solve BD1) BD4)
(check-expect (solve BD5) false)

(define (solve bd)
  (local [(define (solve bd)
            (if (solved? bd)
                bd 
                (solve--lobd (next-boards bd))))
          (define (solve--lobd lobd)
            (cond [(empty? lobd) false]
                  [else
                   (local [(define try (solve (first lobd)))]
                     (if (not (false? try))
                         try
                         (solve--lobd (rest lobd))))]))]
    (solve bd)))

;; Board -> Boolean
;; produce true if the board is full (no empty squares)
(check-expect (solved? MTB) false)
(check-expect (solved? BD4) true)
(check-expect (solved? BD5) false)

(define (solved? bd)
  (cond [(empty? bd) true]
        [else 
         (and (not (empty? (first bd)))
              (solved? (rest bd)))]))

;; Board -> (listof Board)
;; produce the possible next boards from given board by filling the first empty
;;         square with first B, then W, but only keeping the valid boards 

(check-expect (next-boards (list B W B W B W
                                 W B W B W B
                                 B W B W B W
                                 W B W B W B
                                 B W B W B W
                                 W B W B E E))
              (list (list B W B W B W
                          W B W B W B
                          B W B W B W
                          W B W B W B
                          B W B W B W
                          W B W B W E)))

(define (next-boards bd)
  (filter valid? (list (fill-blank (first-empty bd) bd B)
                       (fill-blank (first-empty bd) bd W))))


;; Board -> Position 
;; produce the position of the first empty square on board 
(check-expect (first-empty MTB) 0)
(check-expect (first-empty BD2) 1)

(define (first-empty bd)
  ;; acc : Natural ; the position of the current square 
  (local [(define (fe bd acc)
            (cond [(empty? bd) false]
                  [else (if (empty? (first bd))
                            acc
                            (fe (rest bd) (add1 acc)))]))]
    (fe bd 0)))


;; Board Predicate -> (listof Position)
;; produce a list of positions on the board that fit the predicate 

(check-expect (find-fold MTB empty?) (build-list 36 identity))
(check-expect (find-fold BD4 empty?) (list))
(check-expect (find-fold (list B W B W B W
                               W B W B W B
                               B W B W B W
                               W B W B W B
                               B W B W B W
                               W B W B E E)
                         empty?)
              (list 34 35))

(define (find-fold bd pred)
  ;; acc : Natural ; the position of the current square 
  (local [(define (find-fold bd acc)
            (cond [(empty? bd) empty]
                  [else 
                   (if (pred (first bd))
                       (cons acc (find-fold (rest bd) (add1 acc)))
                       (find-fold (rest bd) (add1 acc)))]))]
    (find-fold bd 0)))

;; Position Board Value -> Board
;; fill the square at the given position on the board with the given value  

(check-expect (fill-blank 0 MTB B) (list B E E E E E 
                                         E E E E E E 
                                         E E E E E E 
                                         E E E E E E 
                                         E E E E E E 
                                         E E E E E E))
(check-expect (fill-blank 0 MTB W) (list W E E E E E 
                                         E E E E E E 
                                         E E E E E E 
                                         E E E E E E 
                                         E E E E E E 
                                         E E E E E E))
(check-expect (fill-blank 35 MTB B) (list E E E E E E 
                                          E E E E E E 
                                          E E E E E E 
                                          E E E E E E 
                                          E E E E E E 
                                          E E E E E B))
(check-expect (fill-blank 35 MTB W) (list E E E E E E 
                                          E E E E E E 
                                          E E E E E E 
                                          E E E E E E 
                                          E E E E E E 
                                          E E E E E W))

; Board ->             empty                 (cons Value Board)                          
; 
; Position
;    v
; 
; 0                  impossible             (cons v (rest bd))
;                                                 
; 
; 
; 
; (add1 Postion)     impossible            (cons (first bd) (recurse (sub1 p) (rest bd))) 


(define (fill-blank p bd v)
  (cond [(zero? p) (cons v (rest bd))]
        [else
         (cons (first bd) (fill-blank (sub1 p) (rest bd) v))]))


;; Board -> Boolean
;; produce true if the board does not have 3 blues or whites in a row/column 
;;         and has equal numbers of blues and whites in each row/column 

(check-expect (valid? MTB) true)
(check-expect (valid? BD4) true)
(check-expect (valid? (list B W B W B W
                            W B W B W B
                            B W B W B W
                            W B W B W B
                            B W B W B W
                            W B W B B W))
              false)
(check-expect (valid? (list B W B W B W
                            W B W B W B
                            B W B W B W
                            W B W B W B
                            B W B W B W
                            W B W B B B))
              false)
(check-expect (valid? (list B B B W W W
                            W W W B B B
                            B B B W W W
                            W W W B B B
                            B B B W W W
                            W W W B B B))
              false)

;can't have three in a row
;can't have three in a column 
;if the row/column is full, must have equal number of B and W 

(define (valid? bd)  
  (and 
   (not
    (ormap 
     (λ (lop) (three-in-row? bd lop)) 
     (append ROWS COLUMNS))) ;true if any row/column has 3 in a row 
   (andmap 
    (λ (lop) (same-b-and-w? bd lop))
    (append ROWS COLUMNS)))) ;true if in all rows and columns, B = W 

;; (listof Position) -> Boolean 
;; produce true if the number of B equals the number of W
;;         if there are any empty squares, it is valid  

(check-expect (same-b-and-w? MTB (list 0 1 2 3 4 5)) true)
(check-expect (same-b-and-w? (list B B B W W W
                                   W W W B B B
                                   B B B W W W
                                   W W W B B B
                                   B B B W W W
                                   W W W B B B)
                             (list 5 11 17 23 29 35)) true)
(check-expect (same-b-and-w? (list B B B W W W
                                   W W W B B B
                                   B B B W W W
                                   W W W B B B
                                   B B B W W W
                                   W W W B B B)
                             (list 12 13 14 15 16 17)) true)
(check-expect (same-b-and-w? (list B B B W W W   ;last row not ok
                                   W W W B B B
                                   B B B W W W
                                   W W W B B B
                                   B B B W W W
                                   W W W W B B)
                             (list 30 31 32 33 34 35)) false)
(check-expect (same-b-and-w? (list B B B W W W   ;4th column not ok
                                   W W W B B B
                                   B B B W W W
                                   W W W B B B
                                   B B B W W W
                                   W W W W B B)
                             (list 3 9 15 21 27 33)) false)

(define (same-b-and-w? bd lop)
  (local [(define v (map (λ (p) (find-value bd p)) lop))]
    (or (member? E v) ;any empties in the row/column?
        (= (length (filter (λ (p) (string=? B p)) v))
           (length (filter (λ (p) (string=? W p)) v)))))) 

;; Board Position -> Value 
;; produce the value at the given position on the board 

; Board ->                 empty            (cons Value Board)               
; 
; Position
;    v
; 
; 0                     impossible          (first bd)
; 
; 
; (add1 Position)       impossible          (recurse (rest bd) (sub1 p))

(check-expect (find-value MTB 15) E)
(check-expect (find-value BD4 28) W)

(define (find-value bd p)
  (cond [(zero? p) (first bd)]
        [else
         (find-value (rest bd) (sub1 p))]))

;; Board (listof Position) -> Boolean 
;; produce true if the given row/column has 3 Bs or 3 Ws in a row 

(check-expect (three-in-row? MTB (list 0 1 2 3 4 5)) false)
(check-expect (three-in-row? BD4 (list 4 10 16 22 28 34)) false)
(check-expect (three-in-row? BD3 (list 0 1 2 3 4 5)) true)

(define (three-in-row? bd lop)
  (local [(define lov (map (λ (p) (find-value bd p)) lop))] ;lov from lop     
    (valid-line? lov))) ;check the line of values 

;; (listof Value) -> Boolean 
;; produce true if there are 3 Bs or 3 Ws in succession in given list
(check-expect (valid-line? (list E E E E E E)) false)
(check-expect (valid-line? (list B B B E E E)) true)
(check-expect (valid-line? (list B W B W B B)) false)
(check-expect (valid-line? (list W B E E B W)) false)


(define (valid-line? lov)
  (cond [(empty? (rest (rest lov))) false] 
        [else (local [(define a (first lov))
                      (define b (second lov))
                      (define c (third lov))]
                (or (and (string? a)
                         (string? b)
                         (string? c)
                         (or (string=? B a b c)
                             (string=? W a b c)))
                    (valid-line? (rest lov))))]))            
