;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname PipeFantasy_hw11) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Amy Eng and Matthew Chavez Cruz
;; HW11 Pipe Fantasy Due 12/5

(require 2htdp/image)
(require 2htdp/universe)

;; TASK 7 SCENARIO:
#;(pipe-fantasy (make-gamestate 140 (make-gpipe PIPE-R 1 1 #t RIGHT #f)
                              (make-gooflow (list (make-gpipe PIPE-R 1 1 #t RIGHT #f)) RIGHT)
                              (make-grid 6 60 0 (list
                                                 (make-gpipe PIPE-TL 4 2 #f NONE #f)
                                                 (make-gpipe PIPE-LR 3 2 #f NONE #f)
                                                 (make-gpipe PIPE-BR 1 2 #f NONE #f)
                                                 (make-gpipe PIPE-ALL 1 3 #f NONE #f)
                                                 (make-gpipe PIPE-TR 1 4 #f NONE #f)
                                                 (make-gpipe PIPE-TL 2 4 #f NONE #f)
                                                 (make-gpipe PIPE-TB 2 3 #f NONE #f)
                                                 (make-gpipe PIPE-ALL 2 2 #f NONE #f)
                                                 (make-gpipe PIPE-BL 2 1 #f NONE #f)
                                                 (make-gpipe PIPE-R 1 1 #t RIGHT #f)))
                              TASK7-PIPES))

(define-struct pipe [top bot left right])
;; A Pipe is a (make-pipe Boolean Boolean Boolean Boolean)
;; Interpretation: a pipe with openings in the given directions. A  #true for 
;; one of top, bot, left, right indicates an opening in that direction.
;; starting pipes
(define PIPE-T (make-pipe #true #false #false #false))
(define PIPE-B (make-pipe #false #true #false #false))
(define PIPE-L (make-pipe #false #false #true #false))
(define PIPE-R (make-pipe #false #false #false #true))
;; pipes that are not starting pipes
(define PIPE-TB (make-pipe #true #true #false #false))
(define PIPE-TL (make-pipe #true #false #true #false))
(define PIPE-TR (make-pipe #true #false #false #true))
(define PIPE-LR (make-pipe #false #false #true #true))
(define PIPE-BR (make-pipe #false #true #false #true))
(define PIPE-BL (make-pipe #false #true #true #false))
(define PIPE-ALL (make-pipe #true #true #true #true))
;; Template
(define (pipe-templ p)
  (... (pipe-top p) ...
       (pipe-bot p) ...
       (pipe-left p) ...
       (pipe-right p) ...))

;; list of starting pipes
(define STARTING-PIPES (list PIPE-T PIPE-B PIPE-L PIPE-R))
;; list of pipes that are not starting pipes
(define CONTINUOUS-PIPES (list PIPE-TB PIPE-TL PIPE-TR PIPE-LR PIPE-BR PIPE-ALL PIPE-BL '()))
;; generate-random-pipelist : [List-of Pipe] NaturalNumber -> [List-of Pipe]
;; generates a random list of pipes with a length of the given number
;; (tests omitted for random functions) 
(define (generate-random-pipelist lop length)
  (if (>= length 0) ; if you aren't done generating yet
      (local [;; random-number generates a random number that will correspond to a Pipe.
              ;; Helper function for random-pipe
              (define random-number (random 8))
              ;; random-pipe : NaturalNumber [List-of Pipe] -> Pipe
              ;; Produces a random pipe based on the given random number
              (define (random-pipe rn list)
                (if (<= rn 1) ; if you're at the correct index
                    (first list) ; produce the correct Pipe
                    (random-pipe (- rn 1) (rest list))))] ; otherwise keep recursing
        (cons (random-pipe random-number CONTINUOUS-PIPES) (generate-random-pipelist lop (- length 1))))
      '()
      ))

;; A random list of fifteen pipes
(define RANDOM-PIPES (generate-random-pipelist CONTINUOUS-PIPES 15))
 
;; The list of incoming pipes that will be added every time the user clicks.
;; The first click is always a starting pipe.
(define PIPE-QUEUE (append (generate-random-pipelist STARTING-PIPES 1) RANDOM-PIPES))

;; list of pipes for task 7's scenario. after the listed pipes in the pipe queue on task 7, it randomly generates pipes.
(define TASK7-PIPES (append (list PIPE-BR PIPE-TB PIPE-LR PIPE-ALL PIPE-TL) PIPE-QUEUE))
  
;; A Direction is one of
;; - "Up"
;; - "Down"
;; - "Left"
;; - "Right"
;; - "None"
;; A Direction represents the direction goo is flowing in.
;; Before the goo begins flowing, the direction is NONE.
(define UP "Up")
(define DOWN "Down")
(define LEFT "Left")
(define RIGHT "Right")
(define NONE "None")
;; Template
(define (direction-templ d)
  (cond
    [(string=? UP d)...]
    [(string=? DOWN d)...]
    [(string=? LEFT d)...]
    [(string=? RIGHT d) ...]
    [(string=? NONE d) ...]))


(define-struct gpipe [type row col filled? direction can-cross?])
;; A GPipe is a (make-gpipe Pipe NaturalNumber NaturalNumber Boolean Direction Boolean)
;; Interpretation
;; - type represents the type of pipe indicated by the data design Pipe
;; - row represents the row number of the pipe's location
;; - col represents the col number of the pipe's location
;; - filled? represents if the pipe has goo or not
;; - direction represents the direction of the goo when it filled the pipe
;; - can-cross? represents if a gooed pipe has the potential to get cross gooed
;; Template
;; gpipe-templ : GPipe -> ?
(define (gpipe-templ gp)
  (... (gpipe-type gp) ...
       (gpipe-row gp) ...
       (gpipe-col gp) ...
       (gpipe-filled? gp) ...
       (gpipe-direction gp) ...
       (gpipe-can-cross? gp) ...))

;; pipe->image: Pipe Integer Integer Boolean Direction Boolean -> Image
;; Draws the given pipe on a square tile with length tile-side-length. The width
;; of the pipe is pipe-width. Pipe-width should be less than tile-side-length
;; (tests omitted)
(define (pipe->image pipe tile-side-length pipe-width filled? direction can-cross?)
  (cond    
    ;; starting pipes
        [(and (pipe-top pipe) (not (pipe-left pipe)) (not (pipe-right pipe)) (not (pipe-bot pipe))) (pipe-design "T" tile-side-length pipe-width filled? direction)]
        [(and (not (pipe-top pipe)) (pipe-left pipe) (not (pipe-right pipe)) (not (pipe-bot pipe))) (pipe-design "L" tile-side-length pipe-width filled? direction)]
        [(and (not (pipe-top pipe)) (not (pipe-left pipe)) (pipe-right pipe) (not (pipe-bot pipe))) (pipe-design "R" tile-side-length pipe-width filled? direction)]
        [(and (not (pipe-top pipe)) (not (pipe-left pipe)) (not (pipe-right pipe)) (pipe-bot pipe)) (pipe-design "B" tile-side-length pipe-width filled? direction)]
    ;; continuous pipes
        [(and (pipe-top pipe) (pipe-bot pipe) (not(pipe-left pipe)) (not(pipe-right pipe))) (pipe-design "TB" tile-side-length pipe-width filled? direction)] 
        [(and (pipe-top pipe) (not (pipe-bot pipe)) (pipe-left pipe) (not(pipe-right pipe))) (pipe-design "TL" tile-side-length pipe-width filled? direction)]
        [(and (pipe-top pipe) (not (pipe-bot pipe)) (not(pipe-left pipe)) (pipe-right pipe)) (pipe-design "TR" tile-side-length pipe-width filled? direction)]
        [(and (not(pipe-top pipe)) (not(pipe-bot pipe)) (pipe-left pipe) (pipe-right pipe)) (pipe-design "LR" tile-side-length pipe-width filled? direction)]
        [(and (not(pipe-top pipe)) (pipe-bot pipe) (not(pipe-left pipe)) (pipe-right pipe)) (pipe-design "BR" tile-side-length pipe-width filled? direction)]
        [(and (not(pipe-top pipe)) (pipe-bot pipe) (pipe-left pipe) (not(pipe-right pipe))) (pipe-design "BL" tile-side-length pipe-width filled? direction)]
        [(and (pipe-top pipe) (pipe-bot pipe) (pipe-left pipe) (pipe-right pipe) (not can-cross?)) (pipe-design "ALL" tile-side-length pipe-width filled? direction)]
        [can-cross? (pipe-design "CROSS" tile-side-length pipe-width filled? direction)]))
        
   
;; pipe-design : String NaturalNumber NaturalNumber Boolean Direction -> Image
;; Draws pipe based on the given String. Helper function for pipe->image.
(check-expect (pipe-design "T" 60 20 #true LEFT)
              (overlay (square 20 "solid" "green") (overlay/align "middle" "top" (square 20 "solid" "green") (square 60 "solid" "grey"))))
(check-expect (pipe-design "T" 60 20 #false RIGHT)
              (overlay (square 20 "solid" "black") (overlay/align "middle" "top" (square 20 "solid" "black") (square 60 "solid" "grey"))))
(check-expect (pipe-design "ALL" 60 20 #true DOWN)
              (overlay (above (square 20 "solid" "green") (square 20 "solid" "green") (square 20 "solid" "green"))
                           (beside (square 20 "solid" "black") (square 20 "solid" "black") (square 20 "solid" "black")) (square 60 "solid" "grey")))
;; function
(define (pipe-design pipe t-length p-width filled? direction)
  (local [;; filled?->color : Boolean -> String
          ;; determines the color of the pipe based on given filled?
          (define (filled?->color f)
            (if f
              "green"
              "black"))]
     (cond
       [(string=? pipe "T")  (overlay (square p-width "solid" (filled?->color filled?)) (overlay/align "middle" "top" (square p-width "solid" (filled?->color filled?)) (square t-length "solid" "gray")))]

       [(string=? pipe "L")  (overlay (square p-width "solid" (filled?->color filled?)) (overlay/align "left" "middle" (square p-width "solid" (filled?->color filled?)) (square t-length "solid" "gray")))]

       [(string=? pipe "R")  (overlay (square p-width "solid" (filled?->color filled?)) (overlay/align "right" "middle" (square p-width "solid" (filled?->color filled?)) (square t-length "solid" "gray")))]

       [(string=? pipe "B")  (overlay (square p-width "solid" (filled?->color filled?)) (overlay/align "middle" "bottom" (square p-width "solid" (filled?->color filled?)) (square t-length "solid" "gray")))]
                             
       [(string=? pipe "TB") (overlay (above (square p-width "solid" (filled?->color filled?)) (square p-width "solid" (filled?->color filled?)) (square p-width "solid" (filled?->color filled?))) (square t-length "solid" "gray"))]

       [(string=? pipe "TL") (overlay/align "left" "center" (square p-width "solid" (filled?->color filled?)) (overlay/align "middle" "top" (above (square p-width "solid" (filled?->color filled?)) (square p-width "solid" (filled?->color filled?))) (square t-length "solid" "gray")))]

       [(string=? pipe "TR") (overlay/align "right" "center" (square p-width "solid" (filled?->color filled?)) (overlay/align "middle" "top" (above (square p-width "solid" (filled?->color filled?)) (square p-width "solid" (filled?->color filled?))) (square t-length "solid" "gray")))]

       [(string=? pipe "LR") (overlay (beside (square p-width "solid" (filled?->color filled?)) (square p-width "solid" (filled?->color filled?)) (square p-width "solid" (filled?->color filled?))) (square t-length "solid" "gray"))]

       [(string=? pipe "BR") (overlay/align "right" "center" (square p-width "solid" (filled?->color filled?)) (overlay/align "middle" "bottom" (above (square p-width "solid" (filled?->color filled?)) (square p-width "solid" (filled?->color filled?))) (square t-length "solid" "gray")))]

       [(string=? pipe "BL") (overlay/align "left" "center" (square p-width "solid" (filled?->color filled?)) (overlay/align "middle" "bottom" (above (square p-width "solid" (filled?->color filled?)) (square p-width "solid" (filled?->color filled?))) (square t-length "solid" "gray")))]
       
       [(and (string=? pipe "ALL") (or (string=? UP direction) (string=? DOWN direction) (string=? NONE direction)))
        (overlay
         (above (square p-width "solid" (filled?->color filled?)) (square p-width "solid" (filled?->color filled?)) (square p-width "solid" (filled?->color filled?)))
         (beside (square p-width "solid" "black") (square p-width "solid" "black") (square p-width "solid" "black"))
         (square t-length "solid" "gray"))]
       
       [(and (string=? pipe "ALL") (or (string=? LEFT direction) (string=? RIGHT direction)))
        (overlay
         (beside (square p-width "solid" (filled?->color filled?)) (square p-width "solid" (filled?->color filled?)) (square p-width "solid" (filled?->color filled?)))
        (above (square p-width "solid" "black") (square p-width "solid" "black") (square p-width "solid" "black"))
        (square t-length "solid" "gray"))]
       
       [(string=? pipe "CROSS")
        (overlay
         (above (square p-width "solid" "green") (square p-width "solid" "green") (square p-width "solid" "green"))
         (beside (square p-width "solid" "green") (square p-width "solid" "green") (square p-width "solid" "green"))
         (square t-length "solid" "gray"))])))

(define-struct grid [numtiles tile-length num-replaced listgpipes])
;; A Grid is a (make-grid NaturalNumber NaturalNumber NaturalNumber [List-of GPipes])
;; Interpretation
;; - numtiles is the number of tiles along each side of the Grid(there are size x size squares total in the grid)
;; - tile-length is the length of a single side of a tile
;; - num-replaced is the number of pipes that have been replaced (the user clicked on the same row col index more than once)
;; - listgpipes is a list of GPipes and is one of
;;       - '()
;;       - (make-gpipe Pipe NaturalNumber NaturalNumber Boolean)
;; Examples
(define GRID-1 (make-grid 4 5 0 (list (make-gpipe PIPE-TB 2 3 #f NONE #f) (make-gpipe PIPE-BL 4 3 #f NONE #f) (make-gpipe PIPE-LR 7 7 #f NONE #f))))
(define GRID-2 (make-grid 6 10 0 (list (make-gpipe PIPE-LR 3 8 #f NONE #f) (make-gpipe PIPE-BR 2 6 #f NONE #f))))
(define GRID-3 (make-grid 10 15 0 (list (make-gpipe PIPE-TL 13 2 #f NONE #f) (make-gpipe PIPE-LR 1 1 #f NONE #f) (make-gpipe PIPE-BL 1 4 #f NONE #f))))
(define GRID-4 (make-grid 10 15 0 (list (make-gpipe PIPE-TR 2 5 #f NONE #f) (make-gpipe PIPE-LR 1 1 #f NONE #f) (make-gpipe PIPE-BL 1 4 #f NONE #f))))
;; Template
(define (grid-templ g)
  (... (grid-numtiles g)...
       (grid-tile-length g)...
       (grid-listgpipes g)...
       (grid-num-replaced g)...))

;; Empty 7x7 Grid
(define STARTING-GRID (make-grid 7 15 0 empty))

;; place-pipe: Grid Pipe Integer Integer -> Grid
;; Places the pipe on the grid at the given row and column. We assume that the
;; row and column are valid positions on the grid.
;; Tests
(check-expect (place-pipe STARTING-GRID PIPE-LR 2 5)
              (make-grid 7 15 0 (cons (make-gpipe PIPE-LR 2 5 #false NONE #f) '())))
(check-expect (place-pipe (make-grid 10 30 0 (cons (make-gpipe PIPE-TB 1 2 #false NONE #f) '())) PIPE-BR 4 5)
              (make-grid 10 30 0 (cons (make-gpipe PIPE-BR 4 5 #false NONE #f) (cons (make-gpipe PIPE-TB 1 2 #false NONE #f) '()))))
(check-expect (place-pipe (make-grid 5 60 0 (list (make-gpipe PIPE-BL 2 3 #false NONE #f) (make-gpipe PIPE-LR 4 2 #false NONE #f))) PIPE-ALL 2 3)
              (make-grid 5 60 1 (list (make-gpipe PIPE-ALL 2 3 #false NONE #f) (make-gpipe PIPE-LR 4 2 #false NONE #f))))
(check-expect (place-pipe (make-grid 5 60 0 (list (make-gpipe PIPE-BL 2 3 #false NONE #f))) PIPE-TB 2 3)
              (make-grid 5 60 1 (list (make-gpipe PIPE-TB 2 3 #false NONE #f))))
;; function
(define (place-pipe grid pipe row col)
      (local [;; gpipe-at? : GPipe -> Boolean
              ;; #true if there's a GPipe at the given row and col
              ;; #false if there is not a GPipe at the given row and col
              (define (gpipe-at? gp)
               (if (and (= (gpipe-row gp) row) (= (gpipe-col gp) col))
                   #false
                   #true))
              ;; get-gpipe: List-of Gpipes Number Number -> Gpipe
              ;; Returns the gpipe at the given row and col
              (define (get-gpipe list row col)
                (if (= row (gpipe-row (first list)))
                    (if (= col (gpipe-col (first list)))
                        (first list)
                        (get-gpipe (rest list) row col))
                    (get-gpipe (rest list) row col)))]
     ; if there's already a pipe at the row and column and it is not already filled:
     (if (pipe-exists-at (grid-listgpipes grid) row col)
         (make-grid (grid-numtiles grid) (grid-tile-length grid) (+ 1 (grid-num-replaced grid))   ;; add new GPipe to list of GPipes
             (add-pipe pipe row col (filter gpipe-at? (grid-listgpipes grid))))  ;; remove the previously existing GPipe

         (make-grid (grid-numtiles grid) (grid-tile-length grid) (grid-num-replaced grid)
             (add-pipe pipe row col (grid-listgpipes grid))))))


;; coords: NaturalNumber NaturalNumber -> NaturalNumber
;; Converts x/y (mouse) coordinates into row/col indexes
;; Tests
(check-expect (coords 125 30) 4)
(check-expect (coords 57 5) 11)
;; Function
(define (coords coordpoint tile-length)
  (floor(/ coordpoint tile-length)))

;; add-pipe : Pipe NaturalNumber NaturalNumber [List-of GPipe] -> [List-of GPipe]
;; Adds pipe row col as first element in grid-listgpipes
;; Tests
(check-expect (add-pipe PIPE-TB 2 3 '())
              (cons (make-gpipe PIPE-TB 2 3 #f NONE #f) '()))
(check-expect (add-pipe PIPE-TB 2 3 (list (make-gpipe PIPE-LR 5 3 #f NONE #f) '()))
              (list (make-gpipe PIPE-TB 2 3 #f NONE #f) (make-gpipe PIPE-LR 5 3 #f NONE #f)'()))
;; Function
(define (add-pipe pipe row col list)
  (cond
    [(empty? list) (cons (make-gpipe pipe row col #f NONE #f) '())]
    [(cons? list)
     (cons (make-gpipe pipe row col #f NONE #f) list)]
    )
  )
     
;; pipe-at: Grid Integer Integer -> [Optional Pipe]
;; Produces the pipe at the given row and column, or #false if that position is
;; is blank. We assume that the row and column are valid positions on the grid.
;; Tests
(check-expect (pipe-at (make-grid 7 7 0 (cons (make-gpipe PIPE-TB 2 2 #f NONE #f) (cons (make-gpipe PIPE-ALL 6 4 #f NONE #f) '()))) 6 4)
              PIPE-ALL)
(check-expect (pipe-at (make-grid 7 7 0 (cons (make-gpipe PIPE-BR 5 7 #f NONE #f) (cons (make-gpipe PIPE-LR 3 5 #f NONE #f) '()))) 3 2)
              #false)
(check-expect (pipe-at (make-grid 5 5 0 (cons (make-gpipe PIPE-BL 1 1 #f NONE #f) (cons (make-gpipe PIPE-TR 1 2 #f NONE #f) (cons (make-gpipe PIPE-LR 1 3 #f NONE #f) '())))) 1 1)
              PIPE-BL)
;; Function
(define (pipe-at grid row col)
   (cond
     [(not (pipe-exists-at (grid-listgpipes grid) row col)) #false] ; pipe does not exist at row col
     [(pipe-exists-at (grid-listgpipes grid) row col) ; pipe exists at row col
      (get-pipe (grid-listgpipes grid) row col)] ; produce pipe at row col
     )
  )


;; pipe-exists-at : [List-of GPipe] row col -> Boolean
;; Produces #true if there is a pipe at the given row and col
;; and #false otherwise. Helper function for pipe-at.
;; Tests
(check-expect (pipe-exists-at (cons (make-gpipe PIPE-TB 1 1 #f NONE #f) '()) 1 1)
              #true)
(check-expect (pipe-exists-at (cons (make-gpipe PIPE-TB 1 1 #f NONE #f) (cons (make-gpipe PIPE-TR 8 3 #f NONE #f) '())) 4 2)
              #false)
(check-expect (pipe-exists-at '() 3 6)
              #false)
(check-expect (pipe-exists-at (cons (make-gpipe PIPE-TB 2 2 #f NONE #f) (cons (make-gpipe PIPE-ALL 6 4 #f NONE #f) '())) 6 4)
              #true)
(check-expect (pipe-exists-at (cons (make-gpipe PIPE-TB 2 2 #f NONE #f) (cons (make-gpipe PIPE-ALL 2 4 #f NONE #f) '())) 2 4)
              #true)
;; Function
(define (pipe-exists-at list row col) 
  (cond
    [(empty? list) #false]
    [(cons? list)
     (if (= row (gpipe-row (first list)))
         (if (= col (gpipe-col (first list)))
             #true
             (pipe-exists-at (rest list) row col))
         (pipe-exists-at (rest list) row col))] 
    )
  )

;; get-pipe : [List-of GPipe] row col -> Pipe
;; Produces the pipe at the row and col given. Helper function for pipe-at,
;; so assumes there is a pipe at the given row and col
;; Tests
(check-expect (get-pipe (list (make-gpipe PIPE-TB 4 2 #f NONE #f) (make-gpipe PIPE-TR 1 3 #f NONE #f) '()) 1 3)
               PIPE-TR)
(check-expect (get-pipe (list (make-gpipe PIPE-ALL 12 5 #f NONE #f) '()) 12 5)
              PIPE-ALL)
(check-expect (get-pipe (list (make-gpipe PIPE-BL 1 1 #f NONE #f) (make-gpipe PIPE-BR 12 7 #f NONE #f) '()) 1 1)
              PIPE-BL)
(check-expect (get-pipe (list (make-gpipe PIPE-BL 1 1 #f NONE #f) (make-gpipe PIPE-BR 1 4 #f NONE #f) '()) 1 4)
              PIPE-BR)
;; Function
(define (get-pipe list row col)
  (if (= row (gpipe-row (first list)))
    (if (= col (gpipe-col (first list)))
               (gpipe-type (first list))
        (get-pipe (rest list) row col))
  (get-pipe (rest list) row col))
  )


;; blank-tile : NaturalNumber -> Image
;; Draws a square representing a blank tile using the given length/width
;; Helper function for grid->image.
;; Tests
(check-expect (blank-tile 20) (overlay (square 20 "outline" "gray") (square 20 "solid" "white")))
(check-expect (blank-tile 40) (overlay (square 40 "outline" "gray") (square 40 "solid" "white")))
(check-expect (blank-tile 100) (overlay (square 100 "outline" "gray") (square 100 "solid" "white")))
;; Function
(define (blank-tile tile-side-length)
  (overlay (square tile-side-length "outline" "gray") (square tile-side-length "solid" "white")))

;; blank-grid-cols : NaturalNumber NaturalNumber Grid -> Image
;; Generates a row of blank tiles. Helper function for blank-grid.
;; Tests
(check-expect (blank-grid-cols 1 70 (make-grid 5 70 0 '()))
              (beside (blank-tile 70) (blank-tile 70) (blank-tile 70) (blank-tile 70) (blank-tile 70)))
(check-expect (blank-grid-cols 1 70 (make-grid 1 70 0 '()))
              (blank-tile 70))
(check-expect (blank-grid-cols 1 40 (make-grid 9 40 0 '()))
              (beside (blank-tile 40) (blank-tile 40) (blank-tile 40) (blank-tile 40) (blank-tile 40)
                      (blank-tile 40) (blank-tile 40) (blank-tile 40) (blank-tile 40)))
;; Function
(define (blank-grid-cols c tile-side-length grid)
  (if (< c (grid-numtiles grid))
      (beside
       (blank-tile tile-side-length)
       (blank-grid-cols (+ c 1) tile-side-length grid))
       (blank-tile tile-side-length))
  )

;; blank-grid : NaturalNumber NaturalNumber NaturalNumber Grid -> Image
;; Draws a completely empty grid with correct number of tiles that are length tile-side-length.
;; values r and c should both start at 1
;; Helper function for grid->image.
;; Tests
(check-expect (blank-grid 1 1 70 (make-grid 2 70 0 '()))
              (above
               (beside
                (blank-tile 70) (blank-tile 70))
               (beside
                 (blank-tile 70) (blank-tile 70))))
(check-expect (blank-grid 1 1 20 (make-grid 1 20 0 '()))
              (blank-tile 20))
(check-expect (blank-grid 1 1 20 (make-grid 3 20 0 '()))
              (above
               (beside
                (blank-tile 20) (blank-tile 20) (blank-tile 20))
               (beside
                (blank-tile 20) (blank-tile 20) (blank-tile 20))
               (beside
                (blank-tile 20) (blank-tile 20) (blank-tile 20))))
;; Function
(define (blank-grid r c tile-side-length grid)
  (if (< r (grid-numtiles grid))
      (above
       (blank-grid-cols c tile-side-length grid)
       (blank-grid (+ 1 r) c tile-side-length grid))
      (blank-grid-cols c tile-side-length grid)))


;; draw-grid : [List-of GPipes] NaturalNumber NaturalNumber Grid: -> Image
;; draws list of gpipes at appropriate locations
;; tests
(check-expect (draw-grid (list (make-gpipe PIPE-TB 1 1 #f NONE #f) (make-gpipe PIPE-ALL 5 5 #f NONE #f)) 60 20 (make-grid 5 1 0 '()) LEFT)
              (overlay/xy
               (pipe->image PIPE-TB 60 20 #f LEFT #f)
               -60 -60
               (overlay/xy
                (pipe->image PIPE-ALL 60 20 #f LEFT #f)
                -300 -300
                (blank-grid 1 1 60 (make-grid 5 1 0 '())))))
(check-expect (draw-grid (list (make-gpipe PIPE-BR 3 5 #f NONE #f) (make-gpipe PIPE-TR 2 6 #f NONE #f)) 60 20 (make-grid 7 40 0 '()) DOWN)
              (overlay/xy
               (pipe->image PIPE-BR 60 20 #f DOWN #f) 
               -180 -300
               (overlay/xy
                (pipe->image PIPE-TR 60 20 #f DOWN #f)
                -120 -360
                (blank-grid 1 1 60 (make-grid 7 40 0 '())))))
(check-expect (draw-grid empty 60 20 (make-grid 3 5 0 '()) UP)
              (blank-grid 1 1 60 (make-grid 3 5 0 '())))
;; function
(define (draw-grid list tile-side-length pipe-width grid direction)
  (cond
    [(empty? list) (blank-grid 1 1 tile-side-length grid)]
    [(cons? list)
     (overlay/xy
      (pipe->image (gpipe-type (first list)) tile-side-length pipe-width (gpipe-filled? (first list)) (gpipe-direction (first list)) (gpipe-can-cross? (first list)))
      (* -1 (gpipe-row (first list)) tile-side-length) ; x-coord
      (* -1 (gpipe-col (first list)) tile-side-length) ; y-coord
      (draw-grid (rest list) tile-side-length pipe-width grid direction))]
    )
  )


;; grid->image : Grid Integer Integer -> Image
;; Draws the grid of pipes. Every tile should be a square with side length
;; tile-side-length and every pipe should have width pipe-width.
;; (check-expects omitted)
;; Function
(define (grid->image grid tile-side-length pipe-width direction)
  (draw-grid (grid-listgpipes grid) tile-side-length pipe-width grid direction))


(define-struct gooflow [goopath direction])
;; A GooFlow is a (make-gooflow [List-of GPipe] Direction)
;; - goopath is current list of GPipes the goo has flown through
;; - direction is the current direction the goo is flowing
;; Represents the goo's existing path and where it is flowing next
;; Examples
(define GF-1 (make-gooflow (list (make-gpipe PIPE-R 1 1 #true RIGHT #f) (make-gpipe PIPE-ALL 2 1 #true RIGHT #f)) RIGHT))
(define GF-2 (make-gooflow (list (make-gpipe PIPE-B 3 4 #true LEFT #f) (make-gpipe PIPE-LR 4 3 #true RIGHT #f)) RIGHT))
(define GF-3 (make-gooflow (list (make-gpipe PIPE-T 2 5 #true DOWN #f) (make-gpipe PIPE-TB 2 4 #true DOWN #f) (make-gpipe PIPE-BL 2 3 #true LEFT #f)) LEFT))
(define GF-4 (make-gooflow (list (make-gpipe PIPE-T 3 5 #true LEFT #f) (make-gpipe PIPE-TB 3 4 #true RIGHT #f) (make-gpipe PIPE-BL 2 3 #true DOWN #f)) LEFT))
;; Template
(define (gooflow-templ gf)
  (... (gooflow-goopath gf)...
       (gooflow-direction gf)...))

                                 
                                       
;; grid-goo-propagate : GooFlow Grid -> GooFlow
;; Moves goo forward by one tile. If the goo is stuck, produce the same goo. The starting
;; pipe is considered to have goo from the beginning.

(check-expect (grid-goo-propagate GF-2 GRID-4) GF-2)

(check-expect (grid-goo-propagate GF-2 GRID-3)
              (make-gooflow (list (make-gpipe PIPE-B 3 4 #true LEFT #f) (make-gpipe PIPE-LR 4 3 #true RIGHT #f)) RIGHT))

(check-expect (grid-goo-propagate (make-gooflow (list (make-gpipe PIPE-T 3 2 #true UP #f)) UP)
                                  (make-grid 7 60 0 (list (make-gpipe PIPE-ALL 3 1 #false NONE #f) (make-gpipe PIPE-T 3 2 #true UP #f))))
              (make-gooflow (list (make-gpipe PIPE-ALL 3 1 #true UP #f) (make-gpipe PIPE-T 3 2 #true UP #f)) UP))

(check-expect (grid-goo-propagate (make-gooflow (list (make-gpipe PIPE-ALL 1 2 #true RIGHT #f) (make-gpipe PIPE-R 1 1 #true RIGHT #f)) RIGHT)
                                  (make-grid 3 60 0 (list (make-gpipe PIPE-BL 2 2 #false NONE #f) (make-gpipe PIPE-ALL 1 2 #false NONE #f) (make-gpipe PIPE-R 1 1 #true RIGHT #f))))
              (make-gooflow (list (make-gpipe PIPE-BL 2 2 #true RIGHT #f) (make-gpipe PIPE-ALL 1 2 #true RIGHT #f) (make-gpipe PIPE-R 1 1 #true RIGHT #f)) DOWN))

(check-expect (grid-goo-propagate (make-gooflow (list (make-gpipe PIPE-TB 2 4 #true DOWN #f) (make-gpipe PIPE-B 1 4 #true DOWN #f)) DOWN)
                                  (make-grid 5 60 0 (list (make-gpipe PIPE-TL 2 5 #false NONE #f) (make-gpipe PIPE-TB 2 4 #true DOWN #f) (make-gpipe PIPE-B 1 4 #true DOWN #f))))
              (make-gooflow (list (make-gpipe PIPE-TL 2 5 #true DOWN #f) (make-gpipe PIPE-TB 2 4 #true DOWN #f) (make-gpipe PIPE-B 1 4 #true DOWN #f)) LEFT))

(check-expect (grid-goo-propagate (make-gooflow (list (make-gpipe PIPE-TL 3 4 #true LEFT #f) (make-gpipe PIPE-TB 2 4 #true DOWN #f) (make-gpipe PIPE-B 1 4 #true DOWN #f)) LEFT)
                                  (make-grid 5 60 0 (list (make-gpipe PIPE-ALL 2 4 #false NONE #f) (make-gpipe PIPE-TL 3 4 #true LEFT #f) (make-gpipe PIPE-TB 2 4 #true DOWN #f) (make-gpipe PIPE-B 1 4 #true DOWN #f))))
              (make-gooflow (list (make-gpipe PIPE-ALL 2 4 #true LEFT #t) (make-gpipe PIPE-TL 3 4 #true LEFT #f) (make-gpipe PIPE-TB 2 4 #true DOWN #f) (make-gpipe PIPE-B 1 4 #true DOWN #f)) LEFT))


(define (grid-goo-propagate gf gr)
  (local 
    [;; move-one : GooFlow -> [List-of Number]
     ;; Takes in a goo flow and produces a (cons row col) that is moved forward in the
     ;; direction of the goo based on the location of the last gpipe in
     ;; gooflow-goopath ([List-of GPipe]).
     (define (move-one gf)
       (cond
         [(string=? RIGHT (gooflow-direction gf)) (list (+ (gpipe-row (first(gooflow-goopath gf))) 1) (gpipe-col (first(gooflow-goopath gf))) )]
         [(string=? LEFT (gooflow-direction gf)) (list  (- (gpipe-row (first(gooflow-goopath gf))) 1) (gpipe-col (first(gooflow-goopath gf))))]
         [(string=? UP (gooflow-direction gf)) (list (gpipe-row (first(gooflow-goopath gf))) (- (gpipe-col (first(gooflow-goopath gf))) 1))]
         [(string=? DOWN (gooflow-direction gf)) (list (gpipe-row (first(gooflow-goopath gf))) (+ (gpipe-col (first(gooflow-goopath gf))) 1))]))
     
     ;; contains the indexes of the next grid location the goo travels in
     (define c-pair (move-one gf)) 
     ;; contains the indexes of the original row and col
     (define c1-pair (list (gpipe-row (first(gooflow-goopath gf))) (gpipe-col (first(gooflow-goopath gf)))))
     
     ;; open? : Direction Pipe -> Boolean
     ;; Checks if the pipe the goo is going into has an opening that works with the
     ;; direction the goo is in.
     (define (open? direction pipe)
       (cond
          [(and (string=? RIGHT direction) (pipe-left pipe)) #t]
          [(and (string=? LEFT direction) (pipe-right pipe)) #t]
          [(and (string=? UP direction) (pipe-bot pipe)) #t]
          [(and (string=? DOWN direction) (pipe-top pipe)) #t]
          [else #f]))
     
     ;; change-direction : Direction Pipe -> Direction
     ;; Used after open?, so assumes that the goo is already in a pipe.
     ;; After the goo is in the pipe, change the direction of the pipe if needed (and maintain
     ;; the same direction) depending on the type of pipe the goo is currently in and the
     ;; previous direction of the goo flow.
     (define (change-direction prev pipe)
       (cond
         [(string=? RIGHT prev) (if (pipe-right pipe) RIGHT (alternative-direction RIGHT pipe))]
         [(string=? LEFT prev) (if (pipe-left pipe) LEFT (alternative-direction LEFT pipe))]
         [(string=? UP prev) (if (pipe-top pipe) UP (alternative-direction UP pipe))]
         [(string=? DOWN prev) (if (pipe-bot pipe) DOWN (alternative-direction DOWN pipe))]))

     ;; alternative-direction : Direction Pipe -> Direction
     ;; Used when the goo cannot flow in the same Direction as its previous Direction; changes
     ;; the Direction based on the given Pipe.
     (define (alternative-direction prev pipe)
       (cond
          [(string=? RIGHT prev) (if (pipe-top pipe) UP DOWN)]
          [(string=? LEFT prev) (if (pipe-top pipe) UP DOWN)]
          [(string=? UP prev) (if (pipe-left pipe) LEFT RIGHT)]
          [(string=? DOWN prev) (if (pipe-left pipe) LEFT RIGHT)]))
     
     ;; get-gpipe: List-of Gpipes Number Number -> Gpipe
              ;; Returns the gpipe at the given row and col
              (define (get-gpipe list row col)
                (if (= row (gpipe-row (first list)))
                    (if (= col (gpipe-col (first list)))
                        (first list)
                        (get-gpipe (rest list) row col))
                    (get-gpipe (rest list) row col)))

    ;; goo-pipe-exists-at? : List-of Gpipes -> Boolean
    ;; #t when there is a pipe in gooflow-goopath with the same given row and column.
    ;; #f when there is not a pipe in gooflow-goopath with the same given row and column.
    (define (goo-pipe-exists-at? gp)
      (not (and (= (gpipe-row gp) (first c-pair)) (= (gpipe-col gp) (second c-pair)))))]

    ;; end of local definitions

  ;; propagating
  (if (and (pipe-exists-at (grid-listgpipes gr) (first c-pair) (second c-pair))
           (open? (gooflow-direction gf) (get-pipe (grid-listgpipes gr) (first c-pair) (second c-pair))))
      (if (andmap goo-pipe-exists-at? (gooflow-goopath gf))
          (make-gooflow (cons (make-gpipe(get-pipe (grid-listgpipes gr) (first c-pair) (second c-pair)) (first c-pair) (second c-pair) #t (gooflow-direction gf) #f) (gooflow-goopath gf)) ;; gooflow-goopath
                             (change-direction (gooflow-direction gf) (get-pipe (grid-listgpipes gr) (first c-pair) (second c-pair))))
          (make-gooflow (cons (make-gpipe(get-pipe (grid-listgpipes gr) (first c-pair) (second c-pair)) (first c-pair) (second c-pair) #t (gooflow-direction gf) #t) (gooflow-goopath gf)) ;; gooflow-goopath
                             (change-direction (gooflow-direction gf) (get-pipe (grid-listgpipes gr) (first c-pair) (second c-pair)))))
      gf))) ;; gooflow-direction

;; gooify : GooFlow Grid -> Grid
;; updates Grid so that grid-listpipes fills up with goo. (turns the last element's gpipe-filled? from
;; #f to #t when it is filled with goo)

(check-expect (gooify (make-gooflow (list (make-gpipe PIPE-TB 2 4 #true RIGHT #f) (make-gpipe PIPE-B 1 4 #true LEFT #f)) DOWN)
                      (make-grid 5 60 0 (list (make-gpipe PIPE-TL 3 4 #false NONE #f) (make-gpipe PIPE-TB 2 4 #true RIGHT #f) (make-gpipe PIPE-B 1 4 #true LEFT #f))))
              (make-grid 5 60 0 (list (make-gpipe PIPE-TB 2 4 #true DOWN #f) (make-gpipe PIPE-TL 3 4 #false NONE #f) (make-gpipe PIPE-B 1 4 #true LEFT #f))))
(check-expect (gooify (make-gooflow (list (make-gpipe PIPE-ALL 2 2 #true DOWN #f) (make-gpipe PIPE-B  1 2 #true DOWN #f)) DOWN)
                      (make-grid 6 60 0 (list (make-gpipe PIPE-ALL 2 2 #false NONE #f) (make-gpipe PIPE-B 1 2 #true DOWN #f))))
              (make-grid 6 60 0 (list (make-gpipe PIPE-ALL 2 2 #true DOWN #f) (make-gpipe PIPE-B 1 2 #true DOWN #f))))
(check-expect (gooify (make-gooflow (list (make-gpipe PIPE-R 4 0 #true RIGHT #f)) RIGHT)
                      (make-grid 6 60 0 (list (make-gpipe PIPE-LR 4 1 #false NONE #f) (make-gpipe PIPE-R 4 0 #true RIGHT #f))))
              (make-grid 6 60 0 (list (make-gpipe PIPE-R  4 0 #true RIGHT #f) (make-gpipe PIPE-LR 4 1 #false NONE #f))))

(define (gooify gf gr)
  (local [;; gpipe? : GPipe -> Boolean
          ;; #true if there's a GPipe at the given row and col
          ;; #false if there is not a GPipe at the given row and col
          (define (gpipe? gp)
               (if (and (= (gpipe-row gp) (gpipe-row NEW-GOO)) (= (gpipe-col gp) (gpipe-col NEW-GOO)))
                   #false
                   #true))
    
          ;; most recent pipe that has been gooed (the first element in gooflow-goopath)
          (define NEW-GOO (first (gooflow-goopath gf)))

          ;; add-goo-pipe : Pipe Number Number [List-of GPipe] -> [List-of GPipe]
          ;; adds a GPipe with goo in it to the given [List-of GPipe]
          (define (add-goo-pipe pipe row col cross? list)
            (cond
              [(empty? list) (cons (make-gpipe pipe row col #t (gooflow-direction gf) cross?) '())]
              [(cons? list) (cons (make-gpipe pipe row col #t (gooflow-direction gf) cross?) list)]))]
               
      (make-grid (grid-numtiles gr) (grid-tile-length gr) (grid-num-replaced gr)   ;; add new GPipe to list of GPipes
            (add-goo-pipe (gpipe-type NEW-GOO) (gpipe-row NEW-GOO) (gpipe-col NEW-GOO) (gpipe-can-cross? NEW-GOO)
                             (filter gpipe? (grid-listgpipes gr))))))


;; draw-pipequeue : GameState NaturalNumber -> Image
;; draws the pipe queue of n pipes

(define (draw-pipequeue gs n)
  (cond
    ; when there are no more incoming pipes and you are in the last square
    [(and (empty? (gamestate-incpipes gs)) (= n 1)) (square (grid-tile-length (gamestate-grid gs)) "outline" "grey")]
    ; when there are no more incoming pipes and you are not in the last square
    [(and (empty? (gamestate-incpipes gs)) (not (= n 1)))
     (beside
      (square (grid-tile-length (gamestate-grid gs)) "outline" "grey")
      (draw-pipequeue (make-gamestate (gamestate-ticks gs)
                                      (gamestate-startingpipe gs)
                                      (gamestate-gooflow gs)
                                      (gamestate-grid gs)
                                      empty)
                      (- n 1)))]
    ; when there are incoming pipes and you are in the last square
    [(and (cons? (gamestate-incpipes gs)) (= n 1))
     (pipe->image (first (gamestate-incpipes gs))
                   (grid-tile-length (gamestate-grid gs))
                   (/ (grid-tile-length (gamestate-grid gs)) 3)
                   #false (gooflow-direction (gamestate-gooflow gs))#f)]
    ; when there are incoming pipes and you are not in the last square
    [(and (cons? (gamestate-incpipes gs)) (not (= n 1)))
     (beside
      (pipe->image (first (gamestate-incpipes gs))
                   (grid-tile-length (gamestate-grid gs))
                   (/ (grid-tile-length (gamestate-grid gs)) 3)
                   #false (gooflow-direction (gamestate-gooflow gs)) #f)
      (draw-pipequeue (make-gamestate (gamestate-ticks gs)
                                      (gamestate-startingpipe gs)
                                      (gamestate-gooflow gs)
                                      (gamestate-grid gs)
                                      (rest (gamestate-incpipes gs)))
                      (- n 1)))]))
          

;; initial-gooflow : GPipe -> GooFlow
;; creates the intitial gooflow using the given starting pipe

(check-expect (initial-gooflow (make-gpipe PIPE-T 1 1 #true UP #f))
              (make-gooflow (list (make-gpipe PIPE-T 1 1 #true UP #f)) UP))
(check-expect (initial-gooflow (make-gpipe PIPE-B 1 1 #true DOWN #f))
              (make-gooflow (list (make-gpipe PIPE-B 1 1 #true DOWN #f)) DOWN))
(check-expect (initial-gooflow (make-gpipe PIPE-L 1 1 #true LEFT #f))
              (make-gooflow (list (make-gpipe PIPE-L 1 1 #true LEFT #f)) LEFT))
(check-expect (initial-gooflow (make-gpipe PIPE-R 1 1 #true RIGHT #f))
              (make-gooflow (list (make-gpipe PIPE-R 1 1 #true RIGHT #f)) RIGHT))

(define (initial-gooflow sp)
  (cond
    [(pipe-top (gpipe-type sp)) (make-gooflow (list sp) UP)]
    [(pipe-bot (gpipe-type sp)) (make-gooflow (list sp) DOWN)]
    [(pipe-left (gpipe-type sp)) (make-gooflow (list sp) LEFT)]
    [(pipe-right (gpipe-type sp)) (make-gooflow (list sp) RIGHT)]))



(define-struct gamestate [ticks startingpipe gooflow grid incpipes])
;; A GameState is a (make-gamestate GPipe GooFlow Grid [List-of Pipes] Number)
;; - ticks is the number of ticks that have passed
;; - startingpipe is the first GPipe on the Grid that is arbitrarily placed
;;   and considered to have goo from the start.
;; - gooflow is the game's current GooFlow
;; - grid is the game's current grid
;; - incpipes is the list of incoming pipes.
;; Examples
(define GS-1 (make-gamestate 0 (make-gpipe PIPE-T 4 2 #t UP #f) (initial-gooflow (make-gpipe PIPE-T 4 2 #t UP #f))
                             (make-grid 7 30 0 (list (make-gpipe PIPE-ALL 2 2 #f NONE #f) (make-gpipe PIPE-TB 4 2 #f NONE #f) (make-gpipe PIPE-BR 7 2 #f NONE #f)))
                             CONTINUOUS-PIPES))
(define GS-2 (make-gamestate 0 (make-gpipe PIPE-B 1 1 #t DOWN #f) (initial-gooflow (make-gpipe PIPE-B 1 1 #t DOWN #f))
                             (make-grid 2 30 0 (list (make-gpipe PIPE-LR 2 2 #f NONE #f) (make-gpipe PIPE-TB 1 2 #f NONE #f) (make-gpipe PIPE-B 1 1 #t DOWN #f)))
                             CONTINUOUS-PIPES))
(define GS-3 (make-gamestate 0 (make-gpipe PIPE-L 5 5 #t LEFT #f) (initial-gooflow (make-gpipe PIPE-L 5 5 #t LEFT #f))
                             (make-grid 5 30 0 (list (make-gpipe PIPE-TB 4 0 #f NONE #f) (make-gpipe PIPE-TL 4 2 #f NONE #f) (make-gpipe PIPE-TR 5 3 #f NONE #f) (make-gpipe PIPE-LR 5 4 #f NONE #f) (make-gpipe PIPE-L 5 5 #t LEFT #f)))
                             CONTINUOUS-PIPES))
(define GS-4 (make-gamestate 0 (make-gpipe PIPE-L 5 5 #t LEFT #f) (initial-gooflow (make-gpipe PIPE-L 5 5 #t LEFT #f))
                             (make-grid 5 30 0 (list (make-gpipe PIPE-TB 4 0 #t DOWN #f) (make-gpipe PIPE-TL 4 2 #t LEFT #f) (make-gpipe PIPE-TR 5 3 #f NONE #f) (make-gpipe PIPE-LR 5 4 #f NONE #f) (make-gpipe PIPE-L 5 5 #t LEFT #f)))
                             empty))

;; Template
;; gs-templ : GameState -> ?
(define (gs-templ gs)
  (... (gamestate-ticks gs)...
       (gamestate-startingpipe gs)...
       (gamestate-gooflow gs)...
       (gamestate-grid gs) ...
       (gamestate-incpipes gs)...))

;; gamestate-init : GameState -> GameState
;; inititalizes the GameState
(check-expect (gamestate-init 7 0 0 DOWN RANDOM-PIPES)
              (make-gamestate 140
                              (make-gpipe PIPE-B 0 0 #t DOWN #f)
                              (make-gooflow (list (make-gpipe PIPE-B 0 0 #t DOWN #f)) DOWN)
                              (make-grid 7 60 0 (list (make-gpipe PIPE-B 0 0 #t DOWN #f)))
                              RANDOM-PIPES))
(check-expect (gamestate-init 5 3 2 RIGHT RANDOM-PIPES)
              (make-gamestate 140
                              (make-gpipe PIPE-R 3 2 #t RIGHT #f)
                              (make-gooflow (list (make-gpipe PIPE-R 3 2 #t RIGHT #f)) RIGHT)
                              (make-grid 5 60 0 (list (make-gpipe PIPE-R 3 2 #t RIGHT #f)))
                              RANDOM-PIPES))
(check-expect (gamestate-init 6 3 4 LEFT PIPE-QUEUE)
              (make-gamestate 140
                              (make-gpipe PIPE-L 3 4 #t LEFT #f)
                              (make-gooflow (list (make-gpipe PIPE-L 3 4 #t LEFT #f)) LEFT)
                              (make-grid 6 60 0 (list (make-gpipe PIPE-L 3 4 #t LEFT #f)))
                              PIPE-QUEUE))

(define (gamestate-init numtiles initpipe-row initpipe-col direction-initpipe incomingpipes)
  (local [;; Direction->Pipe : Direction -> Gpipe
          ;; produces a (starting) gpipe based on the given Direction
          (define (direction->pipe d)
            (cond
              [(string=? UP d) (make-gpipe PIPE-T initpipe-row initpipe-col #true UP #false)]
              [(string=? DOWN d) (make-gpipe PIPE-B initpipe-row initpipe-col #true DOWN #false)]
              [(string=? LEFT d) (make-gpipe PIPE-L initpipe-row initpipe-col #true LEFT #false)]
              [(string=? RIGHT d) (make-gpipe PIPE-R initpipe-row initpipe-col #true RIGHT #false)]))]
  (make-gamestate 140
                  (direction->pipe direction-initpipe)
                  (initial-gooflow (direction->pipe direction-initpipe))
                  (make-grid numtiles 60 0 (list (direction->pipe direction-initpipe)))
                  incomingpipes)))

;; place-pipe-on-click : GameState Integer Integer MouseEvent -> GameState
;; If the user clicks on a tile and there are incoming pipes available, places
;; the next incoming pipe on that tile. If no pipes are available, does nothing.
;; Tests
(check-expect  (place-pipe-on-click GS-1 187 155 "button-down")
               (make-gamestate 0
                (make-gpipe PIPE-T 4 2 #t UP #f)
                (initial-gooflow (make-gpipe PIPE-T 4 2 #t UP #f))
                (make-grid 7 30 0 (list (make-gpipe PIPE-TB 6 5 #f NONE #f) (make-gpipe PIPE-ALL 2 2 #f NONE #f) (make-gpipe PIPE-TB 4 2 #f NONE #f) (make-gpipe PIPE-BR 7 2 #f "None" #f)))
                (rest CONTINUOUS-PIPES)))
(check-expect (place-pipe-on-click GS-2 15 15 "button-up") GS-2)
(check-expect (place-pipe-on-click GS-3 15 15 "button-down")
              (make-gamestate 0
               (make-gpipe PIPE-L 5 5 #t LEFT #f)
               (initial-gooflow (make-gpipe PIPE-L 5 5 #t LEFT #f))
               (make-grid 5 30 0 (list
                                (make-gpipe PIPE-TB 0 0 #f NONE #f)
                                (make-gpipe PIPE-TB 4 0 #f NONE #f)
                                (make-gpipe PIPE-TL 4 2 #f NONE #f)
                                (make-gpipe PIPE-TR 5 3 #f NONE #f)
                                (make-gpipe PIPE-LR 5 4 #f NONE #f)
                                (make-gpipe PIPE-L 5 5 #t LEFT #f)))
               (rest CONTINUOUS-PIPES)))
;; Function
(define (place-pipe-on-click gs x y me)
  (local [;; get-gpipe : [List-of Gpipe] Number Number
          ;; basically same function as get-pipe, but returns a Gpipe
          ;; instead of a Pipe
          (define (get-gpipe logp row col)
            (if (= row (gpipe-row (first logp)))
                (if (= col (gpipe-col (first logp)))
                    (first logp)
                    (get-gpipe (rest logp) row col))
                (get-gpipe (rest logp) row col)))
            
          ;; gpipe-at : Grid Integer Integer -> [Optional GPipe]
          ;; basically same function as pipe-at, but returns a Gpipe if there
          ;; is a Gpipe at the given row col instead of just a Pipe
          (define (gpipe-at grid row col)
            (cond
              [(not (pipe-exists-at (grid-listgpipes grid) row col)) #false] ; gpipe does not exist at row col
              [(pipe-exists-at (grid-listgpipes grid) row col) ; gpipe exists at row col
               (get-gpipe (grid-listgpipes grid) row col)])) ; produce gpipe at row col
          
          ;; gpipe-exists-in : GPipe [List-of GPipe] -> Boolean
          ;; #t if the given gpipe is in the given gooflow list
          ;; #f is the given gpipe is not in the given gooflow list
          (define (gpipe-exists-in gp logp)
            (ormap (λ (listgp) (and (= (gpipe-row gp) (gpipe-row listgp)) (= (gpipe-col gp) (gpipe-col listgp)))) logp))]

  (if (string=? me "button-down") ; when the button clicks
    
          (if ; if user is out of incoming pipes, clicks on a gooed pipe, outside the grid, or on the starting pipe, do nothing.
           (or (and (gpipe? (gpipe-at (gamestate-grid gs) (coords x (grid-tile-length (gamestate-grid gs))) (coords y (grid-tile-length (gamestate-grid gs)))))
                    (gpipe-exists-in (gpipe-at (gamestate-grid gs) (coords x (grid-tile-length (gamestate-grid gs))) (coords y (grid-tile-length (gamestate-grid gs))))
                                     (gooflow-goopath (gamestate-gooflow gs))))
               (or (<= (grid-numtiles (gamestate-grid gs)) (coords x (grid-tile-length (gamestate-grid gs))))
                    (<= (grid-numtiles (gamestate-grid gs)) (coords y (grid-tile-length (gamestate-grid gs)))))
               (and (= (gpipe-row (gamestate-startingpipe gs)) (coords x (grid-tile-length (gamestate-grid gs))))
                    (= (gpipe-col (gamestate-startingpipe gs)) (coords y (grid-tile-length (gamestate-grid gs)))))
               (empty? (gamestate-incpipes gs)))
           
              gs
              
              ; otherwise, place pipe at the appropriate location
          (make-gamestate (gamestate-ticks gs)
                          (gamestate-startingpipe gs) ; maintain starting pipe
                          (gamestate-gooflow gs) ; maintain gooflow
                          (place-pipe (gamestate-grid gs) ; place the pipe at the appropriate coordinates
                                      (first (gamestate-incpipes gs))
                                      (coords x (grid-tile-length (gamestate-grid gs)))
                                      (coords y (grid-tile-length (gamestate-grid gs))))
                          (rest (gamestate-incpipes gs)))) ; move through the pipe queue
      
      gs))) ; if button does not click, do nothing


;; get-score: Gamestate -> Integer
;; gets the score of the given gamestate. excludes the starting pipe

(check-expect (get-score (make-gamestate 0
                          (make-gpipe PIPE-L 2 0 #true "Left" #false)
                          (make-gooflow
                           (list (make-gpipe PIPE-LR 2 0 #true RIGHT #f)
                                 (make-gpipe PIPE-LR 1 0 #true RIGHT #f)
                                 (make-gpipe PIPE-R 0 0 #true RIGHT #f)) RIGHT)
                          (make-grid 7 60 0
                           (list (make-gpipe PIPE-LR 2 0 #false RIGHT #f)
                                 (make-gpipe PIPE-LR 1 0 #false RIGHT #f)
                                 (make-gpipe PIPE-R 0 0 #false RIGHT #f)))
                          CONTINUOUS-PIPES)) 100)
(check-expect (get-score (make-gamestate 0
                          (make-gpipe PIPE-L 2 0 #true "Left" #false)
                          (make-gooflow
                           (list (make-gpipe PIPE-LR 2 0 #true RIGHT #f)
                                 (make-gpipe PIPE-LR 1 0 #true RIGHT #f)
                                 (make-gpipe PIPE-R 0 0 #true RIGHT #f)) RIGHT)
                          (make-grid 7 60 2
                           (list (make-gpipe PIPE-LR 2 0 #false RIGHT #f)
                                 (make-gpipe PIPE-LR 1 0 #false RIGHT #f)
                                 (make-gpipe PIPE-R 0 0 #false RIGHT #f)))
                          CONTINUOUS-PIPES)) 0)
(check-expect (get-score (make-gamestate 0
                          (make-gpipe PIPE-L 2 0 #true "Left" #false)
                          (make-gooflow
                           (list (make-gpipe PIPE-BL 2 1 #true DOWN #f)
                                 (make-gpipe PIPE-LR 2 0 #true RIGHT #f)
                                 (make-gpipe PIPE-LR 1 0 #true RIGHT #f)
                                 (make-gpipe PIPE-R 0 0 #true RIGHT #f)) DOWN)
                          (make-grid 7 60 5
                           (list (make-gpipe PIPE-BL 2 1 #false DOWN #f)
                                 (make-gpipe PIPE-LR 2 0 #false RIGHT #f)
                                 (make-gpipe PIPE-LR 1 0 #false RIGHT #f)
                                 (make-gpipe PIPE-R 0 0 #false RIGHT #f)))
                          CONTINUOUS-PIPES)) -100)

(define (get-score gs)
  (local [;; cross-count: [List-of Gpipe] -> Integer
          ;; Counts the paths that are goo-ed twice by taking the length of a can-cross? only list
          (define (cross-count lgpipe)
            (length (filter (λ (gp) (gpipe-can-cross? gp)) lgpipe)))]
   (* 50 (- (length (gooflow-goopath (gamestate-gooflow gs)))(grid-num-replaced (gamestate-grid gs)) 1))))

;; score->image : Gamestate -> Image
;; draws the gamestate score
(check-expect (score->image (make-gamestate 0
                                            (make-gpipe PIPE-L 2 0 #true "Left" #false)
                                            (make-gooflow
                                             (list (make-gpipe PIPE-LR 2 0 #true RIGHT #f)
                                                   (make-gpipe PIPE-LR 1 0 #true RIGHT #f)
                                                   (make-gpipe PIPE-R 0 0 #true RIGHT #f)) RIGHT)
                                            (make-grid 7 60 0
                                                       (list (make-gpipe PIPE-LR 2 0 #false RIGHT #f)
                                                             (make-gpipe PIPE-LR 1 0 #false RIGHT #f)
                                                             (make-gpipe PIPE-R 0 0 #false RIGHT #f)))
                                            CONTINUOUS-PIPES))
              (text "Score: 100" 20 "black"))
(check-expect (score->image (make-gamestate 0
                          (make-gpipe PIPE-L 2 0 #true "Left" #false)
                          (make-gooflow
                           (list (make-gpipe PIPE-LR 2 0 #true RIGHT #f)
                                 (make-gpipe PIPE-LR 1 0 #true RIGHT #f)
                                 (make-gpipe PIPE-R 0 0 #true RIGHT #f)) RIGHT)
                          (make-grid 7 60 2
                           (list (make-gpipe PIPE-LR 2 0 #false RIGHT #f)
                                 (make-gpipe PIPE-LR 1 0 #false RIGHT #f)
                                 (make-gpipe PIPE-R 0 0 #false RIGHT #f)))
                          CONTINUOUS-PIPES))
              (text "Score: 0" 20 "black"))
(check-expect (score->image (make-gamestate 0
                                            (make-gpipe PIPE-L 2 0 #true "Left" #false)
                                            (make-gooflow
                                             (list (make-gpipe PIPE-BL 2 1 #true DOWN #f)
                                                   (make-gpipe PIPE-LR 2 0 #true RIGHT #f)
                                                   (make-gpipe PIPE-LR 1 0 #true RIGHT #f)
                                                   (make-gpipe PIPE-R 0 0 #true RIGHT #f)) DOWN)
                                            (make-grid 7 60 5
                                                       (list (make-gpipe PIPE-BL 2 1 #false DOWN #f)
                                                             (make-gpipe PIPE-LR 2 0 #false RIGHT #f)
                                                             (make-gpipe PIPE-LR 1 0 #false RIGHT #f)
                                                             (make-gpipe PIPE-R 0 0 #false RIGHT #f)))
                                            CONTINUOUS-PIPES))
              (text "Score: -100" 20 "black"))

(define (score->image gs)
  (text (string-append "Score: " (number->string (get-score gs))) 20 "black"))

;; render-game : GameState -> Image
;; (check-expects omitted)
(define (render-game gs)
   (overlay/align "middle" "bottom"
       (draw-pipequeue gs 3)
       (overlay/align "middle" "top"
          (above
            (grid->image (gamestate-grid gs) (grid-tile-length (gamestate-grid gs))  (/ (grid-tile-length (gamestate-grid gs)) 3) (gooflow-direction (gamestate-gooflow gs)))
            (score->image gs))
        (empty-scene (* 60 (grid-numtiles (gamestate-grid gs))) (+ 90 (* 60 (grid-numtiles (gamestate-grid gs))))))))

;; stop-game : GameState -> Boolean
;; tells the program to stop once the goo cannot flow any longer
;; tests:
; case where goo flows out of the pipe, so the game stops
(check-expect (stop-game (make-gamestate 0
                          (make-gpipe PIPE-B 1 1 #true DOWN #f)
                          (make-gooflow (list (make-gpipe PIPE-TB 2 1 #t DOWN #f) (make-gpipe PIPE-B 1 1 #t DOWN #f)) DOWN)
                          (make-grid 5 60 0 (list (make-gpipe PIPE-BL 3 1 #false NONE #f)
                                                (make-gpipe PIPE-TB 2 1 #true DOWN #f)
                                                (make-gpipe PIPE-B 1 1 #true DOWN #f)))
                          empty))
              #true)
 ; case where goo may flow through the next pipe, so the game must continue
(check-expect (stop-game (make-gamestate 0
                          (make-gpipe PIPE-B 1 1 #true DOWN #f)
                          (make-gooflow (list (make-gpipe PIPE-TB 1 2 #t DOWN #f) (make-gpipe PIPE-B 1 1 #t DOWN #f)) DOWN)
                          (make-grid 5 60 0 (list (make-gpipe PIPE-TB 1 3 #false DOWN #f)
                                                (make-gpipe PIPE-TB 1 2 #true RIGHT #f)
                                                (make-gpipe PIPE-B 1 1 #true UP #f))) 
                          CONTINUOUS-PIPES))
              #false)

(define (stop-game gs)
  (local [;; goosame? : GooFlow -> Boolean
          ;; checks if the GPipe row and col in gooflow-goopath matches the GPipe row and col in gooflow-goopath after
          ;; grid-goo-propagate; this means that the goo cannot move forward and the game should end
          ;; #true if the goo can't move forward, #false if it can.
          (define (goosame? gf)
            (cond
              [(empty? (gooflow-goopath gf)) #false]
              [(cons? (gooflow-goopath gf))
               (if (and (not (empty? (rest (gooflow-goopath gf)))) ; there's a second element to gooflow-goopath
                        (= (gpipe-row (first (gooflow-goopath gf))) ; row of first element in gooflow-goopath matches
                           (gpipe-row (first (gooflow-goopath (grid-goo-propagate gf (gamestate-grid gs))))))
                        (= (gpipe-col (first (gooflow-goopath gf))) ; col of first element in gooflow-goopath matches 
                           (gpipe-col (first (gooflow-goopath (grid-goo-propagate gf (gamestate-grid gs)))))))
                   #true
                   #false)]))]
                                                                                
  (if (goosame? (gamestate-gooflow gs))
      #true
      #false)))

;; tick-handler : Gamestate -> Gamestate
;; handles the on-tick function in the big-bang function
(check-expect (tick-handler (gamestate-init 7 0 0 DOWN RANDOM-PIPES))
              (make-gamestate 139
                              (make-gpipe PIPE-B 0 0 #t DOWN #f)
                              (make-gooflow (list (make-gpipe PIPE-B 0 0 #t DOWN #f)) DOWN)
                              (make-grid 7 60 0 (list (make-gpipe PIPE-B 0 0 #t DOWN #f)))
                              RANDOM-PIPES))
(check-expect (tick-handler (make-gamestate 0
                              (make-gpipe PIPE-R 3 2 #t RIGHT #f)
                              (make-gooflow (list (make-gpipe PIPE-R 3 2 #t RIGHT #f)) RIGHT)
                              (make-grid 5 60 0 (list (make-gpipe PIPE-R 3 2 #t RIGHT #f)))
                              RANDOM-PIPES))
              (make-gamestate 28
                              (make-gpipe PIPE-R 3 2 #t RIGHT #f)
                              (make-gooflow (list (make-gpipe PIPE-R 3 2 #t RIGHT #f)) RIGHT)
                              (make-grid 5 60 0 (list (make-gpipe PIPE-R 3 2 #t RIGHT #f)))
                              RANDOM-PIPES))
(check-expect (tick-handler (make-gamestate 22
                              (make-gpipe PIPE-R 3 2 #t RIGHT #f)
                              (make-gooflow (list (make-gpipe PIPE-R 3 2 #t RIGHT #f)) RIGHT)
                              (make-grid 5 60 0 (list (make-gpipe PIPE-R 3 2 #t RIGHT #f)))
                              RANDOM-PIPES))
              (make-gamestate 21
                              (make-gpipe PIPE-R 3 2 #t RIGHT #f)
                              (make-gooflow (list (make-gpipe PIPE-R 3 2 #t RIGHT #f)) RIGHT)
                              (make-grid 5 60 0 (list (make-gpipe PIPE-R 3 2 #t RIGHT #f)))
                              RANDOM-PIPES))
(define (tick-handler gs)
  (cond
    [(= (gamestate-ticks gs) 0)
     (make-gamestate 28
                     (gamestate-startingpipe gs)
                     (grid-goo-propagate (gamestate-gooflow gs) (gamestate-grid gs))
                     (gooify (grid-goo-propagate (gamestate-gooflow gs) (gamestate-grid gs)) (gamestate-grid gs))
                     (gamestate-incpipes gs))]
    [else (make-gamestate (- (gamestate-ticks gs) 1)
                     (gamestate-startingpipe gs)
                     (gamestate-gooflow gs)
                     (gamestate-grid gs)
                     (gamestate-incpipes gs))])) 
                                 
  

;; pipe-fantasy : GameState -> GameState
;; runs the pipe fantasy game
;; (check-expects omitted for big-bang functions)
(define (pipe-fantasy initial-game-state)
  (big-bang
      initial-game-state
    [to-draw render-game]
    [on-tick tick-handler]
    [on-mouse place-pipe-on-click]
    [stop-when stop-game render-game]))
