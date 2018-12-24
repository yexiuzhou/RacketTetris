;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |tetris v3|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;(require spd/tags)
(require 2htdp/image)
(require 2htdp/universe)
;;==============================================================================
;; CONSTANTS
;;==============================================================================
(define BLOCKSIZE 40)
(define GRID-COLS 10)
(define GRID-ROWS 20)
(define GRID-WIDTH (* GRID-COLS BLOCKSIZE))
(define GRID-HEIGHT (* GRID-ROWS BLOCKSIZE))
(define EMPTYWORLD (empty-scene GRID-WIDTH
                                GRID-HEIGHT))
(define END-TEXT-SIZE 24)
(define RUNNING-TEXT-SIZE 12)
(define TEXT-COLOUR "black")
;;==============================================================================
;; DATA DEFINITIONS
;;==============================================================================
;(@HtDD Block)
(define-struct block (x y colour))
;; Block is (make-block Number Number Colour)
;; interp. one square that composes a tetra tile
;;         x, y positions (relative to each other) and
;;         colour of the single block
#;
(define (fn-for-block b)
  (... (block-x b)
       (block-y b)
       (block-colour b)))
;;------------------------------------------------------------------------------
;(@HtDD Tile)
(define-struct tile (center blocks))
;; Tile is (make-tile Posn (listof Block))
;(@HtDD Posn)
;; Posn is (make-posn X Y)       ;; it's built in
;; interp. center is the point around which the tetra block rotates,
;;         blocks is a (listof Blocks) that represent the tetra block shape
#;
(define (fn-for-tile t)
  (... (tile-center t)
       (tile-blocks t)))
;;------------------------------------------------------------------------------
;(@HtDD World)
(define-struct world (tile set-blocks score))
;; World is (make-world Tile (listof Blocks) Natural)
;; interp. tile is the tile that is currently moving,
;;         set-blocks are blocks at have reached the bottom of the screen
;;         score is the number of blocks that have been placed on the grid
#;
(define (fn-for-world w)
  (... (world-tile w)
       (world-set-blocks w)
       (world-score w)))
;;==============================================================================
;; TETRIS TILES
;;==============================================================================
(define I-COLOUR "cyan")
(define J-COLOUR "blue")
(define L-COLOUR "orange")
(define O-COLOUR "yellow")
(define S-COLOUR "green")
(define T-COLOUR "purple")
(define Z-COLOUR "red")
;;------------------------------------------------------------------------------
(define I
  (make-tile (make-posn (- (/ (* GRID-COLS BLOCKSIZE) 2)
                           (/ BLOCKSIZE 2))
                        (/ (* -1 BLOCKSIZE) 2))
             (list (make-block -1 0 I-COLOUR) (make-block 0 0 I-COLOUR)
                   (make-block 1 0 I-COLOUR) (make-block 2 0 I-COLOUR))))
(define J
  (make-tile (make-posn (- (/ (* GRID-COLS BLOCKSIZE) 2)
                           (/ BLOCKSIZE 2))
                        (/ (* -3 BLOCKSIZE) 2))
             (list (make-block -1 -1 J-COLOUR) (make-block -1 0 J-COLOUR)
                   (make-block 0 0 J-COLOUR) (make-block 1 0 J-COLOUR))))
(define L
  (make-tile (make-posn (- (/ (* GRID-COLS BLOCKSIZE) 2)
                           (/ BLOCKSIZE 2))
                        (/ (* -3 BLOCKSIZE) 2))
             (list (make-block -1 0 L-COLOUR) (make-block 0 0 L-COLOUR)
                   (make-block 1 -1 L-COLOUR) (make-block 1 0 L-COLOUR))))
(define O
  (make-tile (make-posn (- (/ (* GRID-COLS BLOCKSIZE) 2)
                           (/ BLOCKSIZE 2))
                        (/ (* -3 BLOCKSIZE) 2))
             (list (make-block 0 0 O-COLOUR) (make-block 0 -1 O-COLOUR)
                   (make-block -1 -1 O-COLOUR) (make-block -1 0 O-COLOUR))))
(define S
  (make-tile (make-posn (- (/ (* GRID-COLS BLOCKSIZE) 2)
                           (/ BLOCKSIZE 2))
                        (/ (* -3 BLOCKSIZE) 2))
             (list (make-block -1 0 S-COLOUR) (make-block 0 -1 S-COLOUR)
                   (make-block 0 0 S-COLOUR) (make-block 1 -1 S-COLOUR))))
(define T
  (make-tile (make-posn (- (/ (* GRID-COLS BLOCKSIZE) 2)
                           (/ BLOCKSIZE 2))
                        (/ (* -3 BLOCKSIZE) 2))
             (list (make-block -1 0 T-COLOUR) (make-block 0 0 T-COLOUR)
                   (make-block 0 -1 T-COLOUR) (make-block 1 0 T-COLOUR))))
(define Z
  (make-tile (make-posn (- (/ (* GRID-COLS BLOCKSIZE) 2)
                           (/ BLOCKSIZE 2))
                        (/ (* -3 BLOCKSIZE) 2))
             (list (make-block -1 -1 Z-COLOUR) (make-block 0 -1 Z-COLOUR)
                   (make-block 0 0 Z-COLOUR) (make-block 1 0 Z-COLOUR))))
;;------------------------------------------------------------------------------
;(@HtDF num->tile)
;(@signature Natural -> Tile)
;; produce tile corresponsing to the given random natural number [0,6]
;;(@template Natural)?
(define (num->tile n)
  (cond [(= n 0) I]
        [(= n 1) J]
        [(= n 2) L]
        [(= n 3) O]
        [(= n 4) S]
        [(= n 5) T]
        [else Z]))
(define SW (make-world (num->tile (random 7)) empty 0))
;;==============================================================================
;; WORLD FUNCTIONS
;;==============================================================================
;(@HtDF main)
;(@signature World -> World)
;; starts world with (main STARTWORLD)
;(@template htdw-main)
(define (main w)
  (big-bang w
    (on-tick next-world 0.4)
    (on-key handle-key)
    (to-draw render-world)
    (stop-when check-overflow gameover)))
;;------------------------------------------------------------------------------
;(@HtDF next-world)
;(@signature World -> World)
;; produce next world state from given user input (or lack of input)
;(@template World)
(define (next-world w)
  (local [(define layers-to-check (- GRID-HEIGHT (/ BLOCKSIZE 2)))
          (define tile (world-tile w))
          (define center-tile (tile-center tile))
          (define new-tile (make-world (num->tile (random 7))
                                       (append (world-set-blocks w)
                                               (modify-tile-blocks tile))
                                       (+ 4 (world-score w))))]
    (cond [(down-collision? w) new-tile]
          [(bottom-collision? (world-tile w)) new-tile]
          [(full-row? layers-to-check w) (shift-set-blocks-down layers-to-check
                                                                w)]
          [else
           (make-world (make-tile (make-posn (posn-x center-tile)
                                             (+ BLOCKSIZE (posn-y center-tile)))
                                  (tile-blocks tile))
                       (world-set-blocks w)
                       (world-score w))])))
;(@HtDF modify-tile-blocks)
;(@signature Tile -> (listof Block))
;; add the tile-blocks of World to world-set-blocks
;(@template Tile)
(define (modify-tile-blocks t)
  (correct-position (tile-blocks t)
                    (tile-center t)))

;(@HtDF correct-position)
;(@signature (listof Block) Posn -> (listof Block))
;; correct the position of block to be added to world-set-blocks
;(@template (listof Block) add-param encapsulated)
(define (correct-position lob c)
  (local [(define old-x-pos (posn-x c))
          (define old-y-pos (posn-y c))
          (define (correct-position-lob lob)
            (cond [(empty? lob) empty]
                  [else
                   (cons (make-block (+ old-x-pos  ;; new-x-pos
                                        (* BLOCKSIZE (block-x (first lob))))
                                     (+ old-y-pos  ;; new-y-pos
                                        (* BLOCKSIZE (block-y (first lob))))
                                     (block-colour (first lob)))
                         (correct-position-lob (rest lob)))]))]
    (correct-position-lob lob)))

;(@HtDF full-row?)
;(@signature Number World -> Boolean)
;; produce true if every row is full
;(@template Number add-param)
(define (full-row? n w)
  (cond [(< n 0) false] ;; row being checked is below the floor
        [(check-row-n n w) true]
        [else
         (full-row? (- n BLOCKSIZE) w)]))
;(@HtDF check-row-n)
;(@signature Number Number World -> Boolean)
;; produce true if specific row n is full
;(@template World add-param accumulator)
(define (check-row-n n w)
  (local [(define (check-row-n-count count n lob)
            (cond [(empty? lob)
                   false]
                  [(and (= (block-y (first lob)) n) (= count GRID-COLS))
                   true]
                  [(and (= (block-y (first lob)) n) (< count GRID-COLS))
                   (check-row-n-count (add1 count)
                                      n
                                      (rest lob))]
                  [else
                   (check-row-n-count count
                                      n
                                      (rest lob))]))]
    (check-row-n-count 1 n (world-set-blocks w))))

;(@HtDF shift-set-blocks-down)
;(@signature Number World -> World)
;; remove full rows from world-set-blocks and shift rows above that down
;(@template World add-param)
(define (shift-set-blocks-down n w)
  (cond [(< n 0) w]
        [(check-row-n n w)
         (make-world (world-tile w)
                     (drop-set-blocks n
                                      (remove-row n
                                                  (world-set-blocks w)))
                     (world-score w))]
        [else
         (shift-set-blocks-down (- n BLOCKSIZE) w)]))

;(@HtDF remove-row)
;(@signature Number (listof Block) -> (listof Block))
;; break specific row n from rest of world-set-blocks and removes it
;(@template (listof Block) add-param)
(define (remove-row n lob)
  (cond [(empty? lob) lob]
        [else
         (if (= (block-y (first lob)) n)       ;; if block is in row of interest
             (remove-row n (rest lob))         ;; remove, check other blocks
             (cons (first lob)                 ;; otherwise cons block to the NR
                   (remove-row n (rest lob))))]))

;(@HtDF drop-set-blocks)
;(@signature Number (listof Block) -> (listof Block))
;; drop all rows above the one that was removed (row n) by one
;(@template (listof Block) add-param)
(define (drop-set-blocks n lob)
  (cond [(empty? lob) lob]
        [else
         (if (< (block-y (first lob)) n)      ;; if blocks are above removed row
             (cons (new-dropped-block (first lob)) ;; drop block
                   (drop-set-blocks n (rest lob))) ;; otherwise continue
             (cons (first lob)
                   (drop-set-blocks n (rest lob))))]))

;(@HtDF new-dropped-block)
;(@signature Block -> Block)
;; produce block that has moved down by BLOCKSIZE
;(@template Block)
(define (new-dropped-block b)
  (make-block (block-x b)
              (+ BLOCKSIZE (block-y b))
              (block-colour b)))
;;------------------------------------------------------------------------------
;(@HtDF handle-key)
;(@signature World KeyEvent -> World)
;; moves tile of world based on left, right, s, a, down keys
;(@template KeyEvent add-param)
(define (handle-key w ke)
  (local [(define tile (world-tile w))
          (define blocks-tile (tile-blocks tile))
          (define colour-block (block-colour (first blocks-tile)))]
    (cond [(key=? ke "left")
           (move-l-or-r w (- 0 BLOCKSIZE))]      ;; move tile left
          [(key=? ke "right")
           (move-l-or-r w (+ 0 BLOCKSIZE))]      ;; move tile right
          [(and (key=? ke "up")
                (not (string=? O-COLOUR
                               colour-block)))
           (rotate-tile w)]                      ;; rotate tile clockwise
          ;[(and (key=? ke "a")
          ;      (not (string=? O-COLOUR
          ;                     colour-block)))
          ; (rotate-tile
          ;  (rotate-tile
          ;   (rotate-tile w)))]                 ;; rotate tile counterclockwise
          [(key=? ke "down") (next-world w)]     ;; accelerate movement of tile
          [else w])))
;;-------------------------------------
;; MOVE HELPERS
;;-------------------------------------
;(@HtDF move-l-or-r)
;(@signature World Integer -> World)
;; moves tile based on offset unless a collision is detected
;(@template World add-param)
(define (move-l-or-r w offset)
  (local [(define tile (world-tile w))
          (define center-tile (tile-center tile))
          (define blocks-tile (tile-blocks tile))]
    (cond [(side-collision? w offset) w]
          [else
           (make-world (make-tile (update-posn center-tile offset) blocks-tile)
                       (world-set-blocks w)
                       (world-score w))])))

;(@HtDF update-posn)
;(@signature Posn Integer -> Posn)
;; update the position of the tile based on the offset
;(@template Posn add-param)
(define (update-posn c offset)
  (make-posn (+ (posn-x c) offset)
             (posn-y c)))
;;-------------------------------------
;; ROTATE HELPERS
;;-------------------------------------
;(@HtDF rotate-tile)
;(@signature World -> World)
;; produces a new world if rotating tile does not collide with wall or Block
;(@template World)
(define (rotate-tile w)
  (cond [(rotate-collision? w) w]
        [else
         (local [(define tile (world-tile w))]
           (make-world (make-tile (tile-center tile)
                                  (rotate-tile-blocks (tile-blocks tile)))
                       (world-set-blocks w)
                       (world-score w)))]))

;(@HtDF rotate-tile-blocks)
;(@signature (listof Block) -> (listof Block))
;; produce new (listof Block) for tile after being rotated
;(@template (listof Block) accumulator)
(define (rotate-tile-blocks lob)
  (local [(define (rotate-blocks lob rsf)
            (cond [(empty? lob) rsf]
                  [else
                   (rotate-blocks
                    (rest lob)
                    (append rsf (list (rotate-block-cw (first lob)))))]))]
    (rotate-blocks lob empty)))

;(@HtDF rotate-block-cw)
;(@signature Block -> Block)
;; rotate given block 90 degrees counterclockwise around posn
;(@template Block)
(define (rotate-block-cw b)
  (make-block (- 0 (block-y b))
              (block-x b)
              (block-colour b)))
;;------------------------------------------------------------------------------
;(@HtDF render-world)
;(@signature World -> Image)
;; renders the world with set-blocks, tile, and current score
;(@template World)
(define (render-world w)
  (local [(define score-text
            (string-append "Score: "
                           (number->string (world-score w))))
          (define score-display
            (text score-text RUNNING-TEXT-SIZE TEXT-COLOUR))]
    (place-image score-display
                 (+ RUNNING-TEXT-SIZE             ;; places score-display
                    (image-width score-display))
                 (image-height score-display)     ;; at top left
                 (render-set-blocks (world-set-blocks w)
                                    (world-tile w)))))

;(@HtDF render-set-blocks)
;(@signature (listof Block) Tile -> Image)
;; renders the set-blocks and tile
;(@template (listof Block) add-param)
(define (render-set-blocks lob t)
  (cond [(empty? lob) (render-tile t)]
        [else
         (local [(define first-block (first lob))]
           (place-image (render-block first-block)    ;; render individual block
                        (block-x first-block)
                        (block-y first-block)
                        (render-set-blocks (rest lob) t)))]))

;(@HtDF render-tile)
;(@signature Tile -> Image)
;; renders the current tile on EMPTYWORLD
;(@template Tile)
(define (render-tile t)
  (render-tile-blocks (tile-blocks t) (tile-center t)))

;(@HtDF render-tile-blocks)
;(@signature (listof Block) Posn -> Image)
;; render the blocks that compose tile
;(@template (listof Block) add-param)
(define (render-tile-blocks lob c)
  (cond [(empty? lob) EMPTYWORLD]
        [else
         (place-image
          (render-block (first lob)) ;; render individual block
          (+ (* (block-x (first lob)) BLOCKSIZE)  ;; pos to render block based
             (posn-x c))                          ;; on relation to other blocks
          (+ (* (block-y (first lob)) BLOCKSIZE)  ;; in the tile and the center
             (posn-y c))                          ;; point of the tile
          (render-tile-blocks (rest lob) c))]))

;(@HtDF render-block)
;(@signature Block -> Image)
;; renders individual block (ie solid square of BLOCKSIZE and given colour)
;(@template Block)
(define (render-block b)
  (overlay (square (- BLOCKSIZE 2) "solid" (block-colour b))
           (square BLOCKSIZE "solid" "black")))
;;------------------------------------------------------------------------------
;(@HtDF check-overflow)
;(@signature World -> Boolean)
;; Return true if there are blocks in set-blocks over top of world
;(@template World)
(define (check-overflow w)
  (check-overflow--blocks (world-set-blocks w)))

;(@HtDF check-overflow--blocks)
;(@signature (listof Block) -> Boolean)
;; Return true if there are blocks in set-blocks over top of world
;(@template (listof Block))
(define (check-overflow--blocks lob)
  (cond [(empty? lob) false]
        [else
         (if (< (block-y (first lob)) 0)
             true
             (check-overflow--blocks (rest lob)))]))

;(@HtDF gameover)
;(@signature World -> Image)
;; Renders final gameover screen when set-blocks extends over the top of world
;(@template World)
(define (gameover w)
  (place-image (text (string-append "Score: " (number->string (world-score w)))
                     END-TEXT-SIZE
                     TEXT-COLOUR)
               (/ GRID-WIDTH 2)
               (/ GRID-HEIGHT 2)
               (place-image (text "Gameover" END-TEXT-SIZE TEXT-COLOUR)
                            (/ GRID-WIDTH 2)
                            (- (/ GRID-HEIGHT 2) END-TEXT-SIZE)
                            EMPTYWORLD)))
;;==============================================================================
;; COLLISION HELPERS
;;==============================================================================
;;-------------------------------------
;; LEFT RIGHT MOVEMENT COLLISION?
;;-------------------------------------
;(@HtDF side-collision?)
(define (side-collision? w offset)
  (side-collision-tile? (world-tile w) (world-set-blocks w) offset))

;(@HtDF side-collision-tile?)
(define (side-collision-tile? t lob offset)
  (side-collision-lob? (tile-blocks t) (tile-center t) lob offset))

;(@HtDF side-collision-lob?)
;(@signature (listof Block) Posn (listof Block) Integer -> Boolean)
;; produce true if moving tile by offset has a collision with set-blocks
;(@template (listof Block) add-param)
(define (side-collision-lob? lob c set-lob offset)
  (cond [(empty? lob) false]
        [else
         (local [(define new-x-pos (+ offset (posn-x c)
                                      (* (block-x (first lob)) BLOCKSIZE)))]
           (or (side-collision-check (first lob)
                                     set-lob
                                     (posn-x c)
                                     (posn-y c)
                                     offset)
               (< new-x-pos 0)
               (> new-x-pos GRID-WIDTH)
               (side-collision-lob? (rest lob) c set-lob offset)))]))

;(@HtDF side-collision-check)
;(@signature Block (listof Block) Number Number Integer -> Boolean)
;; produce true blocks in tile would collide with blocks in set-blocks
;(@template (listof Block) add-param)
(define (side-collision-check b lob x-pos y-pos offset)
  (cond [(empty? lob) false]
        [else
         (or (and (= (+ offset
                        x-pos
                        (* BLOCKSIZE (block-x b)))        ;; potential new x-pos
                     (block-x (first lob)))               ;; x-pos of first lob
                  (= (+ y-pos
                        (* BLOCKSIZE (block-y b)))        ;; potential new y-pos
                     (block-y (first lob))))              ;; y-pos of first lob
             (side-collision-check b (rest lob) x-pos y-pos offset))]))
;;-------------------------------------
;; ROTATION MOVEMENT COLLISION?
;;-------------------------------------
;(@HtDF rotate-collision?)
;(@signature World -> Boolean)
;; return true if tile-blocks rotated position collides with world-set-blocks
;(@template World)
(define (rotate-collision? w)
  (local [(define tile (world-tile w))
          (define blocks-tile (tile-blocks tile))
          (define center-tile (tile-center tile))]
    (cond [(empty? blocks-tile) false]
          [else
           (local [(define first-tile-block (first blocks-tile))
                   (define new-x-pos (+ (* BLOCKSIZE
                                           (- 0 (block-y first-tile-block)))
                                        (posn-x center-tile)))
                   (define new-y-pos (+ (* BLOCKSIZE
                                           (block-x first-tile-block))
                                        (posn-y center-tile)))]
             (or (rotate-block-parser first-tile-block
                                      (world-set-blocks w)
                                      (posn-x center-tile)
                                      (posn-y center-tile))
                 (< new-x-pos 0)          ;; new tile doesn't go past left wall
                 (> new-x-pos GRID-WIDTH) ;; new tile doesn't go past right wall
                 (> new-y-pos GRID-HEIGHT);; new tile doesn't go over top
                 (rotate-collision? (make-world (make-tile center-tile
                                                           (rest blocks-tile))
                                                (world-set-blocks w)
                                                (world-score w)))))])))
;(@HtDF rotate-block-parser)
;(@signature Block (listof Block) Number Number -> Boolean)
;; produce true if tile's new rotated position collides with world-set-blocks
;(@template (listof Block) add-param)
(define (rotate-block-parser b lob x-pos y-pos)
  (cond [(empty? lob) false]
        [else
         (or (and (= (+ x-pos (* BLOCKSIZE (- 0 (block-y b))))
                     (block-x (first lob)))                  ;; check x,y pos
                  (= (+ y-pos (* BLOCKSIZE (block-x b)))     ;; of rotated
                     (block-y (first lob))))                 ;; tiles to all
             (rotate-block-parser b                          ;; blocks in
                                  (rest lob)                 ;; world-set-blocks
                                  x-pos
                                  y-pos))]))
;;-------------------------------------
;; DOWNWARD COLLISION?
;;-------------------------------------
;(@HtDF down-collision?)
;(@signature World -> Boolean)
;; check if future tile position down collides with world-set-blocks
;(@template World)
(define (down-collision? w)
  (local [(define tile (world-tile w))
          (define blocks-tile (tile-blocks tile))
          (define center-tile (tile-center tile))]
    (down-collision-tile? blocks-tile (world-set-blocks w) center-tile)))

;(@HtDF down-collision-tile?)
;(@signature (listof Block) (listof Block) Posn -> Boolean)
;; check if blocks of future tile positions collides with world-set-blocks
;(@template (listof Block) add-param)
(define (down-collision-tile? tile lob pos)
  (cond [(empty? tile) false]
        [else
         (or (down-block-parser (first tile)
                                lob
                                (posn-x pos)
                                (posn-y pos))
             (down-collision-tile? (rest tile)
                                   lob
                                   pos))]))

;(@HtDF down-block-parser)
;(@signature Block (listof Block) Number Number -> Boolean)
;; produce true if future tile block position collides with world-set-blocks
;(@template (listof Block) add-param)
(define (down-block-parser b lob pos-x pos-y)
  (cond [(empty? lob) false]
        [else
         (or (and (= (+ pos-x (* BLOCKSIZE (block-x b)))
                     (block-x (first lob)))
                  (= (+ BLOCKSIZE pos-y (* BLOCKSIZE (block-y b)))
                     (block-y (first lob))))
             (down-block-parser b (rest lob) pos-x pos-y))]))
;;-------------------------------------
;; FLOOR COLLISION?
;;-------------------------------------
;(@HtDF bottom-collision?)
;(@signature Tile -> Boolean)
;; produce true if the the tile is in contact with the bottom of the board
;(@template Tile)
(define (bottom-collision? t)
  (local [(define blocks-tile (tile-blocks t))
          (define center-tile (tile-center t))]
    (bottom-collision-tile? blocks-tile center-tile)))

;(@HtDF bottom-collision-tile?)
;(@signature (listof Block) Posn -> Boolean)
;; produce true if tile-blocks are in contact with the bottom of the board
;(@template (listof Block) add-param)
(define (bottom-collision-tile? lob c)
  (cond [(empty? lob) false]
        [else
         (or (> (+ (posn-y c) (* BLOCKSIZE (block-y (first lob))))  ;; new y-pos
                (- GRID-HEIGHT (/ (* 3 BLOCKSIZE) 2)))       ;; limit for center
             (bottom-collision-tile? (rest lob) c))]))
;;==============================================================================
;; TESTS - THAT I DIDN'T DO WHILE WRITING ALL THIS. DERP
;;==============================================================================