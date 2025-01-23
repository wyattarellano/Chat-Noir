#lang htdp/isl+
(require 2htdp/universe)
(require 2htdp/image)

;; constants
(define BOARD-SIZE 7)
(define IMAGE-WIDTH 70)
(define IMAGE-HEIGHT 70)
(define MAX-CHARS-HORIZONTAL 7.5)
(define MAX-CHARS-VERTICAL 7)
(define E-SCENE-COLOR 'white)
(define E-SCENE-W (* MAX-CHARS-HORIZONTAL IMAGE-WIDTH))
(define E-SCENE-H (* MAX-CHARS-VERTICAL IMAGE-HEIGHT))
(define E-SCENE (empty-scene E-SCENE-W E-SCENE-H E-SCENE-COLOR))
(define HALF-IMG-W (/ IMAGE-WIDTH 2))

;hello
(define MAX-IMAGE-X-EVEN 6.5)
(define MIN-IMAGE-X-EVEN 0.5)
(define MAX-IMAGE-X-ODD 6)
(define MIN-IMAGE-X-ODD 0)
(define MIN-IMAGE-Y 0)
(define MAX-IMAGE-Y 6)

(define PIX-OF-IMG-0 35)
(define OFFSET-VALUE .5)
(define OFFSET-RANGE+ 1.5)
(define OFFSET-RANGE 1)
(define ODD-Y-X-VALS '(0 1 2 3 4 5 6))
(define EVEN-Y-X-VALS '(.5 1.5 2.5 3.5 4.5 5.5 6.6))
(define LIST-ROW-1-OR-5 '(1 5))
(define EVEN-ROW-CHECK 2)
(define Y-ROW 4)
(define X-ROW 6)
(define PICK-ODD-ROW 2)
(define PLAYER-LIMIT 2)

;; Data Definitions

#|
A character image (ci) is an image which is at most
IMAGE-WIDTH x IMAGE-HEIGHT pixels

An image-x is an integer in [0 .. MAX-CHARS-HORIZONTAL-1]

An image-y is an integer in [0 .. MAX-CHARS-VERTICAL-1]

A scene is an E-SCENE-W x E-SCENE-H image

A pixel-x is an integer in [0 .. E-SCENE-W - 1]
A pixel-Y is an integer in [0 .. E-SCENE-H - 1]
|#

;; Sample image-x
(define AN-IMG-X (/ MAX-CHARS-HORIZONTAL 2))
(define MIN-IMG-X 0)
(define MAX-IMG-X (sub1 MAX-CHARS-HORIZONTAL))

;; Sample image-y
(define AN-IMG-Y (/ MAX-CHARS-VERTICAL 2))
(define MIN-IMG-Y 0)
(define MAX-IMG-Y (sub1 MAX-CHARS-VERTICAL))

;; a turn can be either:
;;  1. 'cat
;;  2. 'blocker

;; sample instances of turn
;(define INIT-TURN 'blocker)
(define INIT-TURN 'blocker) 
(define CAT-TURN 'cat)
(define BLOCKER-TURN 'blocker)


;; making templates for catposn, a space, and an los

;; A catposn is a posn (make-posn image-x image-y)

;; sample instances of catposn
(define INIT-CATPOSN (make-posn 3 3))
(define CAT-POSN2 (make-posn 2.5 2))

#|
;; TEMPLATE FOR FUNCTION ON A CATPOSN

;; catposn ... -> ...
;; Purpose: ...
(define (f-on-catposn a-catposn ...)
   (... (f-on-image-x (posn-x a-catposn)) ...
    ... (f-on-image-y (posn-y a-catposn)))

;; Sample instances of catposn
  (define CATPOSN1  (make-posn ... ...))
  (define CATPOSN2  (make-posn ... ...))

;; Sample expressions for f-on-catposn
(define CATPOSN1-VAL ... CATPOSN1 ...)
(define CATPOSN2-VAL ... CATPOSN2 ...) ...

;; Tests using sample computations for f-on-catposn
(check-expect (f-on-world CATPOSN1 ...) CATPOSN1-VAL)
(check-expect (f-on-world CATPOSN2 ...) CATPOSN2-VAL) ...

;; Tests using sample values for f-on-catposn
(check-expect (f-on-catposn ... ...) ... )
...    

|#

;; a space is a posn

;; sample instances of space
(define SPACE1 (make-posn 0.5 0))
(define SPACE2 (make-posn 1 2))


#|

TEMPLATE FOR A FUNCTION ON A SPACE

;; space ... -> ...
;; Purpose: ...
(define (f-on-space a-space ...)
   (... (f-on-image-x (posn-x a-space)) ...
    ... (f-on-image-y (posn-y a-space)))

;; Sample instances of space
  (define SPACE1  (make-posn ... ...))
  (define SPACE2  (make-posn ... ...))

;; Sample expressions for f-on-space
(define SPACE1-VAL ... SPACE1 ...)
(define SPACE2-VAL ... SPACE2 ...) ...

;; Tests using sample computations for f-on-space
(check-expect (f-on-world SPACE1 ...) SPACE1-VAL)
(check-expect (f-on-world SPACE2 ...) SPACE2-VAL) ...

;; Tests using sample values for f-on-space
(check-expect (f-on-space ... ...) ... )
...    

|#

;; a list of space (los) is either:
;;  1. '()
;;  2. (cons space los)

;; makes all space posns (its a list of spaces)
(define space-list
  (apply append   ;; the apply function applies append to each element in the list so its not a list of lists (i found it in the help desk)
         (build-list BOARD-SIZE
                     (λ (y)
                       (if (odd? y)
                           (map (λ (x) (make-posn x y)) ODD-Y-X-VALS)
                           (map (λ (x) (make-posn x y)) EVEN-Y-X-VALS))))))

;; sample instances of los
(define EMPT-LOS '())
(define ALL-SPACES space-list) 
(define INIT-BLOCKED-SPACES (list (make-posn (+ OFFSET-VALUE (random X-ROW)) (* EVEN-ROW-CHECK (random Y-ROW)))
                                  (make-posn (+ OFFSET-VALUE (random X-ROW)) (* EVEN-ROW-CHECK (random Y-ROW)))
                                  (make-posn (+ OFFSET-VALUE (random X-ROW)) (* EVEN-ROW-CHECK (random Y-ROW)))
                                  (make-posn (random X-ROW) (list-ref LIST-ROW-1-OR-5 (random PICK-ODD-ROW)))
                                  (make-posn (random X-ROW) (list-ref LIST-ROW-1-OR-5 (random PICK-ODD-ROW)))
                                  (make-posn (random X-ROW) (list-ref LIST-ROW-1-OR-5 (random PICK-ODD-ROW)))
                                  (make-posn (random X-ROW) (list-ref LIST-ROW-1-OR-5 (random PICK-ODD-ROW)))))
(define INIT-BLOCKED-SPACES2 (list (make-posn 2 1) (make-posn 5 7) (make-posn 2 3)))
(define INIT-BLOCKED-SPACES3 (list (make-posn 2 3) (make-posn 4 3) (make-posn 3.5 2) (make-posn 2.5 2) (make-posn 3.5 4) (make-posn 2.5 4)))


#|
TEMPLATE FOR A FUNCTION ON A LOS
 ;; Sample instances of los
     (define LOS1 ...)
     (define LOS2 ...) 

     ;; los ... --> ...
     ;; Purpose: ...
     (define (f-on-los a-los ...)
       (if (empty? a-los)
           ...
           ...(f-on-space (first a-los))...(f-on-los (rest a-los))...)))
    
     ;; Sample expressions for f-on-los
     (define LOS1-VAL ...)
     (define LOS2-VAL ...)
        ...

     ;; Tests using sample computations for f-on-los
     (check-expect (f-on-los LOS1 ...) LOS1-VAL)
     (check-expect (f-on-los LOS2 ...) LOS2-VAL)
        ...

     ;; Tests using sample values for f-on-los
     (check-expect (f-on-los ...) ...)
        ...
|#

;; a world is a structure: (make-world turn posn los)
(define-struct world (turn catposn blockedspaces))

;; sample instances of a world
(define UNINIT-WORLD 'uninitialized)
(define INIT-WORLD (make-world INIT-TURN INIT-CATPOSN INIT-BLOCKED-SPACES))
(define WORLD2 (make-world CAT-TURN CAT-POSN2 INIT-BLOCKED-SPACES2))
(define WORLD3 (make-world BLOCKER-TURN (make-posn 5 5) (list (make-posn 1 1))))
(define WORLD4 (make-world CAT-TURN (make-posn 3 3) INIT-BLOCKED-SPACES3))
(define WORLD5 (make-world BLOCKER-TURN (make-posn 0 3) INIT-BLOCKED-SPACES))
(define WORLD6 (make-world CAT-TURN (make-posn 5 5) (list (make-posn 1 1))))

;; A world is either
;;  1. 'uninitialized
;;  2. a structure: (make-world turn catposn los)


#|
 TEMPLATE FOR FUNCTIONS ON A WORLD
 world ... --> ...
 Purpose:
 (define (f-on-world w ...)
   (if (eq? a-world 'uninitialized)
       ...
       (... (f-on-turn (world-turn w))... (f-on-catposn (world-catposn w))
        ... (world-blockedspaces w)...)))

  ;; Sample instances of world
  (define WORLD1  'uninitialized)
  (define WORLD2  (make-world ... ... ...))

  ;; Sample expressions for f-on-world
  (define WORLD1-VAL ... WORLD1 ...)
  (define WORLD2-VAL ... WORLD2 ...) ...

  ;; Tests using sample computations for f-on-world
  (check-expect (f-on-world WORLD1 ...) WORLD1-VAL)
  (check-expect (f-on-world WORLD2 ...) WORLD2-VAL) ...

  ;; Tests using sample values for f-on-world
  (check-expect (f-on-world ... ...) ... ) ...     

(define INIT-WORLD  (make-world INIT-TURN INIT-CATPOSN INIT-LOS))
(define INIT-WORLD2 (make-world INIT-TURN2 INIT-CATPOSN2 INIT-LOS2))
(define WORLD3 (make-world 'cat (make-posn 3 3)
                                 (list (make-posn 5 5) (make-posn 3 3))
                           ))
(define WORLD4 (make-world 'blocker (make-posn 1 4)
                                 (list (make-posn 3.5 5) (make-posn 4.5 2))
                           ))

(define UNINIT-WORLD 'uninitialized)

|#
;; A universe is a structure: (make-univ (listof iworld) world)
(define-struct univ (iws game))

;; Template for a function on a universe
#| ;; Sample instances of univ
   (define UNIV1 (make-univ ... ...))

   universe ... --> ...
   Purpose:
   (define (f-on-univ a-univ ...)
     (...(univ-iws a-univ)...(univ-world a-univ)...))

   ;; Sample expressions for f-on-univ
   (define UNIV1-VAL ...) ...

   ;; Tests using sample computations for f-on-univ
   (check-expect (f-on-univ UNIV1 ...) UNIV1-VAL) ...

   ;; Tests using sample values for f-on-univ
   (check-expect (f-on-univ ... ...) ...) ...
|#

;; Sample instances of universe
(define INIT-UNIV  (make-univ '() UNINIT-WORLD))
(define OTHR-UNIV  (make-univ (list iworld1 iworld2) WORLD3))
(define OTHR-UNIV2 (make-univ (list iworld3 iworld2) WORLD4))
(define OTHR-UNIV3 (make-univ (list iworld1 iworld2) WORLD6))

;;Marshalling and Unmarshalling

;;a-cat -> list
;;Purpose: Marshal Cat
(define (marshal-cat a-cat)
  (list (posn-x a-cat)
        (posn-y a-cat)))

;; Sample expression for marshal-cat
(define SAMP-MARSHAL-CAT-VAL1 (list (posn-x INIT-CATPOSN)
                                    (posn-y INIT-CATPOSN)))
(define SAMP-MARSHAL-CAT-VAL2 (list (posn-x CAT-POSN2)
                                    (posn-y CAT-POSN2)))

;; tests using sample computations for marshal-cat
(check-expect (marshal-cat INIT-CATPOSN) SAMP-MARSHAL-CAT-VAL1)
(check-expect (marshal-cat CAT-POSN2) SAMP-MARSHAL-CAT-VAL2)

;; tests using sample values for marshal-cat
(check-expect (marshal-cat (make-posn 3 3)) (list 3 3))
(check-expect (marshal-cat (make-posn 2.5 4)) (list 2.5 4))


;; a-space -> list
;; Purpose: Marshal Space
(define (marshal-space a-space)
  (list (posn-x a-space)
        (posn-y a-space)))

;; sample expressions for marshal-space
(define SAMP-MARSHAL-SPACE-VAL1 (list (posn-x SPACE1)
                                      (posn-y SPACE1)))
(define SAMP-MARSHAL-SPACE-VAL2 (list (posn-x SPACE2)
                                      (posn-y SPACE2)))

;; tests using sample computations for marshal-space
(check-expect (marshal-space SPACE1) SAMP-MARSHAL-SPACE-VAL1)
(check-expect (marshal-space SPACE2) SAMP-MARSHAL-SPACE-VAL2)

;; tests using sample values for marshal-space
(check-expect (marshal-space (make-posn 3 3)) (list 3 3))
(check-expect (marshal-space (make-posn 4.5 6)) (list 4.5 6))

;; world -> mw
;; Purpose: To marshal a world
(define (marshal-world a-world)
  (list (world-turn a-world)
        (marshal-cat (world-catposn a-world))
        (map marshal-space (world-blockedspaces a-world))
        ))

;; sample expressions for marshal-world
(define MINIT-WORLD (list (world-turn INIT-WORLD)
                          (marshal-cat (world-catposn INIT-WORLD))
                          (map marshal-space (world-blockedspaces INIT-WORLD))
                          ))
(define MWORLD2 (list (world-turn WORLD2)
                      (marshal-cat (world-catposn WORLD2))
                      (map marshal-space (world-blockedspaces WORLD2))
                      ))

;; tests using sample computations for marshal-world
(check-expect (marshal-world INIT-WORLD) MINIT-WORLD)
(check-expect (marshal-world WORLD2) MWORLD2)

;; tests using sample values for marshal-world
(check-expect (marshal-world (make-world 'blocker (make-posn 3 3) (list (make-posn 3.5 2) (make-posn 2 5) (make-posn 1 5))))
              (list 'blocker (list 3 3) (list (list 3.5 2) (list 2 5) (list 1 5))))
(check-expect (marshal-world (make-world 'cat (make-posn 2.5 2) (list (make-posn 2 1) (make-posn 5 7) (make-posn 2 3))))
              (list 'cat (list 2.5 2) (list (list 2 1) (list 5 7) (list 2 3))))

#|
     A to-player message (tpm) is: (cons 'world mw)

 tpm ... --> ...
 Purpose:
 (define (f-on-tpm a-tpm ...)
    (...(f-on-mw (rest a-tpm) ...)...))

 Sample instances of tpm
 (define A-TPM (cons 'world ...))

 Sample expressions for f-on-tpm
 (define A-TPM-VAL (f-on-mw (rest A-TPM) ...))

 Tests using sample for computations for f-on-mw
 (check-expect  (f-on-tpm A-TPM ...) A-TPM-VAL) ...

 Tests using sample for vaues for f-on-mw
 (check-expect  (f-on-tpm ... ...) ...) ...
|#



#| A to-server message (tsm) is (list 'click mouse-x mouse-y)

  ;; tsm ... --> ...
  ;; Purpose:
  (define (f-on-tsm a-tsm ...)
    (local [(define tag (first a-tsm))]
      (cond [(eq? tag 'click)  ...]
            [else
              (error (format "Unknown to-server message type ~s"
                       a-tsm))])))

  ;; Sample instances of tsm
  (define MV-TSM ...)
  ...

  ;; Sample expressions for f-on-tsm
  (define MV-TSM-VAL ...)

  ;; Tests using sample computations for f-on-tsm
  (check-expect (f-on-tsm MV-TSM ...) MV-TSM-VAL)
   ...

  ;; Tests using sample values for f-on-tsm
  (check-expect (f-on-tsm ... ...) ...) ...
|#

;; Sample instances of tsm
(define BLOCK-LEFT (list 'click 2.5 4))
(define CAT-UP (list 'click 6 5))
(define BLOCK-LEFT2 (list 'click 1 3))


;; univ iworld tsm --> bundle or throws error
;; Purpose: Process the message to create new universe
;; ASSUMPTION: The given univ is not INIT-UNIV
(define (process-message a-univ an-iw a-tsm)
  (local [(define tag (first a-tsm))
          (define name (iworld-name an-iw))
          (define game (univ-game a-univ))

          ;; number number  --> bundle
          ;; Purpose: Process a mouse event to return next world
          (define (process-mouse-click mouse-x mouse-y)
            (local [;; number number -> world
                    ;; Purpose: Make a new world
                    (define (new-world mouse-x mouse-y)
                      (local [;; posn los -> Boolean
                              ;; Purpose: To determine if the given space is not a blocked space
                              (define (not-blocked-space? a-posn a-los)
                                (not (ormap (λ (blocked-space) (equal? a-posn blocked-space)) a-los)))

                               ;; image-x --> pixel-x
                              ;; Purpose: To translate the given image-x to a pixel-x
                              (define (image-x->pix-x ix)
                                (+ (* ix IMAGE-WIDTH) HALF-IMG-W))

                              ;; image-y --> pixel-y
                              ;; Purpose: To translate the given image-y to a pixel-y
                              (define (image-y->pix-y iy)
                                (+ (* iy IMAGE-HEIGHT) HALF-IMG-W))

                              ;; world number number -> posn
                              ;; Purpose: Move the cat
                              (define (move-cat a-world mouse-x mouse-y)
                                (local [;; Functions that determine which tile is clicked

                                        ;; world number number -> Boolean
                                        ;; Purpose: To determine if the mouse click is on the space to the left of the cat
                                        (define (click-is-left? a-world m-x m-y)
                                          (and (<= (image-y->pix-y (- (posn-y (world-catposn a-world)) OFFSET-VALUE))
                                                   m-y
                                                   (image-y->pix-y (+ (posn-y (world-catposn a-world)) OFFSET-VALUE)))
                                               (<= (image-x->pix-x (- (posn-x (world-catposn a-world)) OFFSET-RANGE+))
                                                   m-x
                                                   (image-x->pix-x (- (posn-x (world-catposn a-world)) OFFSET-VALUE)))))

                                        ;; world number number -> Boolean
                                        ;; Purpose: To determine if the mouse click is on the space to the right of the cat
                                        (define (click-is-right? a-world m-x m-y)
                                          (and (<= (image-y->pix-y (- (posn-y (world-catposn a-world)) OFFSET-VALUE))
                                                   m-y
                                                   (image-y->pix-y (+ (posn-y (world-catposn a-world)) OFFSET-VALUE)))
                                               (<= (image-x->pix-x (+ (posn-x (world-catposn a-world)) OFFSET-VALUE))
                                                   m-x
                                                   (image-x->pix-x (+ (posn-x (world-catposn a-world)) OFFSET-RANGE+)))))

                                        ;; world number number -> Boolean
                                        ;; Purpose: To determine if the mouse click is on the space top right of the cat
                                        (define (click-is-top-right? a-world m-x m-y)
                                          (and (<= (image-y->pix-y (- (posn-y (world-catposn a-world)) OFFSET-RANGE+))
                                                   m-y
                                                   (image-y->pix-y (- (posn-y (world-catposn a-world)) OFFSET-VALUE)))
                                               (<= (image-x->pix-x (posn-x (world-catposn a-world)))
                                                   m-x
                                                   (image-x->pix-x (+ (posn-x (world-catposn a-world)) OFFSET-RANGE)))))

                                        ;; world number number -> Boolean
                                        ;; Purpose: To determine if the mouse click is on the space top left of the cat
                                        (define (click-is-top-left? a-world m-x m-y)
                                          (and (<= (image-y->pix-y (- (posn-y (world-catposn a-world)) OFFSET-RANGE+))
                                                   m-y
                                                   (image-y->pix-y (- (posn-y (world-catposn a-world)) OFFSET-VALUE)))
                                               (<= (image-x->pix-x (- (posn-x (world-catposn a-world)) OFFSET-RANGE))
                                                   m-x
                                                   (image-x->pix-x (posn-x (world-catposn a-world))))))

                                        ;; world number number -> Boolean
                                        ;; Purpose: To determine if the mouse click is on the space bottom left of the cat
                                        (define (click-is-bottom-left? a-world m-x m-y)
                                          (and (<= (image-y->pix-y (+ (posn-y (world-catposn a-world)) OFFSET-VALUE))
                                                   m-y
                                                   (image-y->pix-y (+ (posn-y (world-catposn a-world)) OFFSET-RANGE+)))
                                               (<= (image-x->pix-x (- (posn-x (world-catposn a-world)) OFFSET-RANGE))
                                                   m-x
                                                   (image-x->pix-x (posn-x (world-catposn a-world))))))

                                        ;; world number number -> Boolean
                                        ;; Purpose: To determine if the mouse click is on the space bottom right of the cat
                                        (define (click-is-bottom-right? a-world m-x m-y)
                                          (and (<= (image-y->pix-y (+ (posn-y (world-catposn a-world)) OFFSET-VALUE))
                                                   m-y
                                                   (image-y->pix-y (+ (posn-y (world-catposn a-world)) OFFSET-RANGE+)))
                                               (<= (image-x->pix-x (posn-x (world-catposn a-world)))
                                                   m-x
                                                   (image-x->pix-x (+ (posn-x (world-catposn a-world)) OFFSET-RANGE)))))]
                                  
                                  (cond [(and (click-is-left? a-world mouse-x mouse-y)
                                              (not-blocked-space? (make-posn (sub1 (posn-x (world-catposn a-world))) (posn-y (world-catposn a-world)))
                                                                  (world-blockedspaces a-world)))
                                         (make-posn (sub1 (posn-x (world-catposn a-world))) (posn-y (world-catposn a-world)))]
                                        [(and (click-is-right? a-world mouse-x mouse-y)
                                              (not-blocked-space? (make-posn (add1 (posn-x (world-catposn a-world))) (posn-y (world-catposn a-world)))
                                                                  (world-blockedspaces a-world)))
                                         (make-posn (add1 (posn-x (world-catposn a-world))) (posn-y (world-catposn a-world)))]
                                        [(and (click-is-top-left? a-world mouse-x mouse-y)
                                              (not-blocked-space? (make-posn (- (posn-x (world-catposn a-world)) OFFSET-VALUE) (sub1 (posn-y (world-catposn a-world))))
                                                                  (world-blockedspaces a-world)))
                                         (make-posn (- (posn-x (world-catposn a-world)) OFFSET-VALUE) (sub1 (posn-y (world-catposn a-world))))]
                                        [(and (click-is-top-right? a-world mouse-x mouse-y)
                                              (not-blocked-space? (make-posn (+ (posn-x (world-catposn a-world)) OFFSET-VALUE) (sub1 (posn-y (world-catposn a-world))))
                                                                  (world-blockedspaces a-world)))
                                         (make-posn (+ (posn-x (world-catposn a-world)) OFFSET-VALUE) (sub1 (posn-y (world-catposn a-world))))]
                                        [(and (click-is-bottom-left? a-world mouse-x mouse-y)
                                              (not-blocked-space? (make-posn (- (posn-x (world-catposn a-world)) OFFSET-VALUE) (add1 (posn-y (world-catposn a-world))))
                                                                  (world-blockedspaces a-world)))
                                         (make-posn (- (posn-x (world-catposn a-world)) OFFSET-VALUE) (add1 (posn-y (world-catposn a-world))))]
                                        [(and (click-is-bottom-right? a-world mouse-x mouse-y)
                                              (not-blocked-space? (make-posn (+ (posn-x (world-catposn a-world)) OFFSET-VALUE) (add1 (posn-y (world-catposn a-world))))
                                                                  (world-blockedspaces a-world)))
                                         (make-posn (+ (posn-x (world-catposn a-world)) OFFSET-VALUE) (add1 (posn-y (world-catposn a-world))))]
                                        [else (world-catposn a-world)])
                                  ))


                              ;; number -> boolean
                               ;; Purpose: Determine if row is even
                               (define (even-row? mouse-y)
                                 (even? (round (/ (- mouse-y PIX-OF-IMG-0) IMAGE-HEIGHT))))

                              
                               ;;number number -> posn
                               ;;Purpose: Make a posn based on their Y posn
                               (define (even-posn mouse-x mouse-y)
                                 (make-posn (+ (floor (/ (- mouse-x PIX-OF-IMG-0) IMAGE-WIDTH)) OFFSET-VALUE)
                                            (round (/ (- mouse-y PIX-OF-IMG-0) IMAGE-HEIGHT))))
                               
                               
                               ;;number number -> posn
                               ;;Purpose: Make a posn based on their Y posn
                               (define (odd-posn mouse-x mouse-y)
                                 (make-posn (round (/ (- mouse-x PIX-OF-IMG-0) IMAGE-WIDTH))
                                            (round (/ (- mouse-y PIX-OF-IMG-0) IMAGE-HEIGHT))))


                                ;;number number -> posn
                               ;;Purpose: Make a posn
                               (define (block mouse-x mouse-y)
                                 (if (even-row? mouse-y)
                                     (cons (even-posn mouse-x mouse-y) (world-blockedspaces game))
                                     (cons (odd-posn mouse-x mouse-y) (world-blockedspaces game))))

                               
                               ;; number number world -> boolean
                               ;; Purpose: Determine if a posn made by a mouse is already blocked
                               (define (not-member-of-blocked mouse-x mouse-y a-world)
                                 (and (not (member? (even-posn mouse-x mouse-y) (world-blockedspaces a-world)))
                                     (not (member? (odd-posn mouse-x mouse-y) (world-blockedspaces a-world)))))

                               ;;number number world -> boolean
                               ;;Purpose: Determine if a posn made by a mouse is a catposn
                               (define (not-on-cat mouse-x mouse-y a-world)
                                 (and (not (equal? (even-posn mouse-x mouse-y) (world-catposn a-world)))
                                      (not (equal? (odd-posn mouse-x mouse-y) (world-catposn a-world)))))
                               
                              ;; number number -> posn
                              ;; Purpose: To determine if a click is in the space-list
                              (define (valid-space? mouse-x mouse-y)
                                (or (member?
                                     (even-posn mouse-x mouse-y)
                                     space-list)
                                    (member?
                                     (odd-posn mouse-x mouse-y)
                                     space-list)))
                              
                                               
                               ;; world number number -> boolean
                               ;; Purpose: To determine if a click is valid
                               (define (valid-click? mouse-x mouse-y a-world)
                                 (and (not-blocked-space? (make-posn mouse-x mouse-y) (world-blockedspaces a-world))
                                      (valid-space? mouse-x mouse-y)
                                      (not-member-of-blocked mouse-x mouse-y a-world)
                                      (not-on-cat mouse-x mouse-y a-world)))

                               

                               ;; world number number -> posn
                               ;; Purpose: To block a space based on the the given mouseclick
                               (define (block-space a-world mouse-x mouse-y)
                                 (if (valid-click? mouse-x mouse-y a-world)
                                     (block mouse-x mouse-y)
                                     (world-blockedspaces a-world)))]

                        
                        (cond [(and (equal? name "cat")
                                    (equal? (world-turn game) 'cat)
                                    (valid-click? mouse-x mouse-y game)
                                    (not (equal? (move-cat game mouse-x mouse-y) (world-catposn game))))
                               (make-world 'blocker (move-cat game mouse-x mouse-y) (world-blockedspaces game))]
                              [(and (equal? name "blocker")
                                    (equal? (world-turn game) 'blocker)
                                    (valid-click? mouse-x mouse-y game))
                               (make-world 'cat (world-catposn game) (block-space game mouse-x mouse-y))]
                              [else game])))]
                       
              
                    (make-bundle (make-univ (univ-iws a-univ) (new-world mouse-x mouse-y))
                                 (map (λ (iw)
                                        (make-mail iw (cons 'world (marshal-world (new-world mouse-x mouse-y)))))
                                      (univ-iws a-univ))
                                 '())))]
          (if (eq? tag 'click)
              (process-mouse-click (second a-tsm) (third a-tsm))
              (error (format "Unknown to-server message type ~s"
                             a-tsm)))))



;; Tests using sample values for process-message
(check-expect
 (process-message OTHR-UNIV3 iworld1 CAT-UP)
 (make-bundle
  (make-univ
   (list iworld1 iworld2)
   (make-world
    'cat
    (make-posn 5 5)
    (list (make-posn 1 1))))
  (list
   (make-mail
    iworld1
    (list
     'world
     'cat
     (list 5 5)
     (list (list 1 1))))
   (make-mail
    iworld2
    (list
     'world
     'cat
     (list 5 5)
     (list (list 1 1)))))
  '()))

(check-expect
 (process-message OTHR-UNIV iworld1 BLOCK-LEFT)
 (make-bundle
  (make-univ
   (list
    iworld1
    iworld2)
   (make-world
    'blocker
    (make-posn 5 5)
    (list (make-posn 1 1))))
  (list
   (make-mail
    iworld1
    (list
     'world
     'blocker
     (list 5 5)
     (list (list 1 1))))
   (make-mail
    iworld2
    (list
     'world
     'blocker
     (list 5 5)
     (list (list 1 1)))))
  '()))

(check-error (process-message OTHR-UNIV iworld2 (list 'clicker 2 4))
             (format "Unknown to-server message type ~s"
                     (list 'clicker 2 4)))




;; universe iworld --> bundle
;; Purpose: Add new world to the universe
(define (add-player a-univ an-iw)
  (cond [(member? (iworld-name an-iw) (map iworld-name (univ-iws a-univ)))
         (make-bundle a-univ '() (list an-iw))]
        [(< (length (univ-iws a-univ)) PLAYER-LIMIT)
         (local [(define new-iws (cons an-iw (univ-iws a-univ)))
                 (define game (univ-game a-univ))
                 (define new-game (if (equal? game UNINIT-WORLD)
                                      (make-world
                                       INIT-TURN
                                       INIT-CATPOSN
                                       INIT-BLOCKED-SPACES
                                       )
                                      (make-world
                                       (world-turn game)
                                       (world-catposn game)
                                       (world-blockedspaces game)
                                       )))]
           (make-bundle
            (make-univ new-iws new-game)
            (map
             (λ (iw)
               (make-mail iw
                          (cons 'world (marshal-world new-game))))
             new-iws)
            '()))]
        [else (make-bundle a-univ (list (make-mail an-iw UNINIT-WORLD)) (list an-iw))]))
 
;; Sample expressions for add-player
(define RPT-ADD (make-bundle OTHR-UNIV '() (list iworld1)))
(define <-2-ADD (local [(define new-iws (cons iworld2 (univ-iws INIT-UNIV)))
                        (define game (univ-game INIT-UNIV))
                        (define new-game (if (equal? game UNINIT-WORLD)
                                             (make-world
                                              INIT-TURN
                                              INIT-CATPOSN
                                              INIT-BLOCKED-SPACES
                                              )
                                             (make-world
                                              (world-turn game)
                                              (world-catposn game)
                                              (world-blockedspaces game)
                                              )))]
                  (make-bundle
                   (make-univ new-iws new-game)
                   (map
                    (λ (iw)
                      (make-mail iw
                                 (cons 'world (marshal-world new-game))))
                    new-iws)
                   '())))

(define NEW-ADD (make-bundle
                 (make-univ
                  (list iworld1 iworld2)
                  (make-world
                   'blocker
                   (make-posn 5 5)
                   (list (make-posn 1 1))))
                 (list (make-mail iworld3 'uninitialized)) (list iworld3)))

;; Tests using sample computations for add-player
(check-expect (add-player OTHR-UNIV iworld1) RPT-ADD)
(check-expect (add-player INIT-UNIV iworld2) <-2-ADD)
(check-expect (add-player OTHR-UNIV iworld3) NEW-ADD)


;; univ iworld --> bundle
;; Purpose: Remove given iw from universe and game
;; ASSUMPTION: Given univ is not INIT-UNIV
(define (rm-player a-univ an-iw)
  (local [(define iws  (univ-iws a-univ))
          (define game (univ-game a-univ))
          (define new-iws (filter (λ (iw)
                                    (not (string=? (iworld-name an-iw)
                                                   (iworld-name iw))))
                                  iws))
          (define new-game (make-world
                            (world-turn game)
                            (world-catposn game)
                            (world-blockedspaces game)
                            ))]
    (make-bundle (make-univ new-iws new-game)
                 (map (λ (iw) (make-mail iw (cons 'world (marshal-world new-game))))
                      new-iws)
                 '())))

;; Sample expressions for rm-player
(define RM-IW1 (local [(define iws  (univ-iws OTHR-UNIV))
                       (define game (univ-game OTHR-UNIV))
                       (define new-iws (filter (λ (iw)
                                                 (not (string=? (iworld-name iworld1)
                                                                (iworld-name iw))))
                                               iws))
                       (define new-game (make-world
                                         (world-turn game)
                                         (world-catposn game)
                                         (world-blockedspaces game)
                                         ))]
                 (make-bundle (make-univ new-iws new-game)
                              (map (λ (iw) (make-mail iw (cons 'world (marshal-world new-game))))
                                   new-iws)
                              '())))

(define RM-IW2 (local [(define iws  (univ-iws OTHR-UNIV2))
                       (define game (univ-game OTHR-UNIV2))
                       (define new-iws (filter (λ (iw)
                                                 (not (string=? (iworld-name iworld2)
                                                                (iworld-name iw))))
                                               iws))
                       (define new-game (make-world
                                         (world-turn game)
                                         (world-catposn game)
                                         (world-blockedspaces game)
                                         ))]
                 (make-bundle (make-univ new-iws new-game)
                              (map (λ (iw) (make-mail iw (cons 'world (marshal-world new-game))))
                                   new-iws)
                              '())))

;; Tests using sample computations for rm-player
(check-expect (rm-player OTHR-UNIV  iworld1) RM-IW1)
(check-expect (rm-player OTHR-UNIV2 iworld2) RM-IW2)


;; Tests using sample values for rm-player
(check-expect (rm-player (make-univ (list iworld1) WORLD3) iworld1)
              (make-bundle (make-univ '() (make-world 'blocker (make-posn 5 5) (list (make-posn 1 1)))) '() '()))


;; Z --> univ
;; Purpose: To run the server
(define (run-server a-z)
  (universe
   INIT-UNIV
   (on-msg        process-message)
   (on-new        add-player)
   (on-disconnect rm-player)))


(run-server "Marco")