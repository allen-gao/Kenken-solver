;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname kenken) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(define-struct puzzle (size board constraints))
;; A Puzzle = (make-puzzle 
;;               Nat 
;;               (listof (listof (union Symbol Nat Guess))
;;               (listof (list Symbol Nat Symbol)))
(define-struct guess (symbol number))
;; A Guess = (make-guess Symbol Nat)

;;==============================================================================
;; Part (a)
;; find-blank: Puzzle -> (union Posn false 'guess)
;; find a blank space in the puzzle, or return false if the puzzle is complete
;; Example: 
(check-expect (find-blank puzzle1) (make-posn 0 0))

(define (find-blank puzz)
  (local
    [(define original-board (puzzle-board puzz))
     (define original-constraints (puzzle-constraints puzz))
     (define size (puzzle-size puzz))
     (define (find-blank-helper size board constraints posn-acc guess-bool)
       (cond
         [(empty? constraints) false]
         [(empty? board)
          (cond
            [guess-bool 'guess]
            [else (find-blank-helper size 
                                     original-board
                                     (rest constraints)
                                     (make-posn 0 0)
                                     false)])]
         [(empty? (first board))
          (find-blank-helper size
                             (rest board)
                             constraints 
                             (make-posn 0
                                        (add1 (posn-y posn-acc)))
                             guess-bool)]
         [(and (guess? (first (first board)))
               (symbol=? (first (first constraints))
                         (guess-symbol (first (first board)))))
          (find-blank-helper size
                             (cons (rest (first board))
                                   (rest board))
                             constraints
                             (make-posn (add1 (posn-x posn-acc))
                                        (posn-y posn-acc))
                             true)]
         [(equal? (first (first constraints)) (first (first board)))
          posn-acc]
         [else (find-blank-helper size
                                  (cons (rest (first board))
                                        (rest board))
                                  constraints
                                  (make-posn (add1 (posn-x posn-acc))
                                             (posn-y posn-acc))
                                  guess-bool)]))]
    (find-blank-helper (puzzle-size puzz)
                       original-board
                       original-constraints
                       (make-posn 0 0)
                       false)))

;;==============================================================================
;; Part (b)
;; used-in-row: Puzzle Posn -> (listof Nat)
;; produce the list of numbers used in the same row as the (x,y) location
;; in the puzzle
;; Example:
(check-expect (used-in-row puzzle1 (make-posn 1 1)) empty)

(define (used-in-row puz pos)
  (quicksort 
   (filter number? 
           (map (lambda (x)
                  (cond
                    [(guess? x)
                     (guess-number x)]
                    [else x]))
                (list-ref (puzzle-board puz) (posn-y pos)))) <))



;; used-in-column: puzzle posn -> (listof nat)
;; produce the list of numbers used in the same column as the (x,y) location
;; in the puzzle
;; Examples:
(check-expect (used-in-column puzzle1 (make-posn 1 1)) empty)
(check-expect (used-in-column puzzle1partial (make-posn 2 2)) (list 1))
(check-expect (used-in-column puzzle1partial2 (make-posn 0 1)) (list 2))

(define (used-in-column puz pos)
  (quicksort (filter number? (map (lambda (y)
                                    (cond
                                      [(guess? y) (guess-number y)]
                                      [else y]))
                                  (map (lambda (x)
                                         (list-ref x (posn-x pos)))
                                       (puzzle-board puz)))) <))




;;==============================================================================
;; Part (c)

(define (allvals n) (build-list n (lambda (x) (add1 x))))

;; available-vals: Puzzle Posn -> (listof Nat)
;; produce the list of valid entries for the (x,y) location of the puzzle
(check-expect (available-vals puzzle1 (make-posn 2 3)) '(1 2 3 4))

(define (available-vals puz pos)
  (local
    [(define row-nums (used-in-row puz pos))
     (define column-nums (used-in-column puz pos))
     (define all-vals (allvals (puzzle-size puz)))]
    (quicksort (filter (lambda (x)
                         (and (not (member? x row-nums))
                              (not (member? x column-nums))))
                       all-vals) <)))


;;==============================================================================
;; Part (d)

;; place-guess: (listof (listof (union Symbol Nat Guess))) Posn Nat 
;;              -> (listof (listof (union Symbol Nat Guess)))
;; fill in the (x,y) location of the board puz with val
;; Examples:
(check-expect (place-guess (puzzle-board puzzle1) (make-posn 3 2) 5)
              (list
               (list 'a 'b 'b 'c)
               (list 'a 'd 'e 'e)
               (list 'f 'd 'g (make-guess 'g 5))
               (list 'f 'h 'i 'i)))
(check-expect (place-guess (puzzle-board puzzle1) (make-posn 0 0) 1)
              (list
               (list (make-guess 'a 1) 'b 'b 'c)
               (list 'a 'd 'e 'e)
               (list 'f 'd 'g  'g )
               (list 'f 'h 'i 'i)))


(define (place-guess brd pos val)
  (local
    [(define (column-fn column x-pos val)
       (cond
         [(= 0 x-pos)
          (cons (make-guess (first column) val) (rest column))]
         [else (cons (first column) 
                     (column-fn (rest column) (sub1 x-pos) val))]))]
    (cond
      [(= 0 (posn-y pos))
       (cons (column-fn (first brd) (posn-x pos) val)
             (rest brd))]
      [else 
       (cons (first brd)
             (place-guess (rest brd)
                          (make-posn (posn-x pos) (sub1 (posn-y pos)))
                          val))])))


;; fill-in-guess: Puzzle Posn Nat -> Puzzle
;; fill in the (x,y) location of puz with val
;; Example:
(check-expect (fill-in-guess puzzle1 (make-posn 3 2) 5)
              (make-puzzle
               4
               (list
                (list 'a 'b 'b 'c)
                (list 'a 'd 'e 'e)
                (list 'f 'd 'g (make-guess 'g 5))
                (list 'f 'h 'i 'i))
               (puzzle-constraints puzzle1)))


(define (fill-in-guess puz pos val)
  (make-puzzle (puzzle-size puz) 
               (place-guess (puzzle-board puz) pos val) 
               (puzzle-constraints puz)))

;;==============================================================================
;; Part (e)

;; guess-valid?:  verify that the guesses made on the board are valid
(define (guess-valid? puz)
  (local
    [(define board (puzzle-board puz))
     (define constraint-symbol (first (first (puzzle-constraints puz))))
     (define constraint-operator (third (first (puzzle-constraints puz))))
     (define constraint-result (second (first (puzzle-constraints puz))))
     (define val-list (map guess-number 
                           (filter (lambda (x)
                                     (and (guess? x)
                                          (symbol=? constraint-symbol
                                                    (guess-symbol x))))
                                   (foldr append empty board))))]
    (cond
      [(symbol=? '+ constraint-operator)
       (= constraint-result (foldr + 0 val-list))]
      [(symbol=? '- constraint-operator)
       (= constraint-result (abs (- (first val-list) (second val-list))))]
      [(symbol=? '* constraint-operator)
       (= constraint-result (foldr * 1 val-list))]
      [(symbol=? '/ constraint-operator)
       (or (= constraint-result (/ (first val-list) (second val-list)))
           (= constraint-result (/ (second val-list) (first val-list))))]
      [(symbol=? '= constraint-operator)
       (= constraint-result (first val-list))])))


;;==============================================================================
;; Part (f)
;; 
;; apply-guess:  Puzzle -> Puzzle
;; apply the guesses by converting them into numbers and removing the first element
;; from the constraints

(define (apply-guess puz)
  (make-puzzle (puzzle-size puz)
               (map (lambda (column)
                      (map (lambda (x)
                             (cond
                               [(guess? x)
                                (guess-number x)]
                               [else x]))
                           column))
                    (puzzle-board puz))
               (rest (puzzle-constraints puz))))


;;==============================================================================
;; Part (g)
;; neighbours: Puzzle -> (listof Puzzle)
;; produce a list of next puzzles in the implicit graph
;; Example:
(check-expect (neighbours puzzle3)
              (list 
               (make-puzzle 
                2 
                (list 
                 (list 'a (make-guess 'b 1)) 
                 (list 'c 'b)) 
                '((b 3 +) 
                  (c 2 =)
                  (a 1 =)))
               (make-puzzle 
                2 
                (list 
                 (list 'a (make-guess 'b 2)) 
                 (list 'c 'b)) 
                '((b 3 +) 
                  (c 2 =)
                  (a 1 =)))))


(define (neighbours puz)
  (cond
    [(false? (find-blank puz)) empty]
    [(and (equal? 'guess (find-blank puz))
          (guess-valid? puz))
     (list (apply-guess puz))]
    [(and (equal? 'guess (find-blank puz))
          (not (guess-valid? puz))) empty]
    [else (map (lambda (val)
                 (make-puzzle (puzzle-size puz)
                              (place-guess (puzzle-board puz)
                                           (find-blank puz)
                                           val)
                              (puzzle-constraints puz)))
               (available-vals puz (find-blank puz)))]))


;; This is just the find-route function from Module 12, slides
;; 30-36.  (Read a bit ahead if you want the deatils.) The explicit
;; graph G has been removed, and the termination condition (the desired
;; destination) is when the puzzle is complete (that is, find-blank
;; returns false).

;; solve-kenken: Puzzle -> (union Puzzle false)
;; find a solution to a KenKen puzzle, or return false if there is no
;; solution
(check-expect (time (solve-kenken puzzle1)) puzzle1soln)
(define (solve-kenken orig)
  (local
    [(define (solve-kenken-helper to-visit visited)
       (cond
         [(empty? to-visit) false]
         [(boolean? (find-blank (first to-visit))) (first to-visit)]
         [(member (first to-visit) visited)
          (solve-kenken-helper (rest to-visit) visited)]
         [else
          (local [(define nbrs (neighbours (first to-visit)))
                  (define new (filter (lambda (x) (not (member x visited))) nbrs))
                  (define new-to-visit (append new (rest to-visit)))
                  (define new-visited (cons (first to-visit) visited))]
            (solve-kenken-helper new-to-visit new-visited))]))]
    (solve-kenken-helper (list orig) empty)))
;;; The time special form shows you the number of milliseconds spent
;;; evaluating the given expression.  The first number (cpu time) is
;;; the important one.
(check-expect (time (solve-kenken puzzle2)) puzzle2soln)
(check-expect (solve-kenken puzzle3partial) false)


;; ======================== TESTING ===========================

;; Some useful constants
;; from assignment specification
(define puzzle1
  (make-puzzle 
   4
   (list
    (list 'a 'b 'b 'c)
    (list 'a 'd 'e 'e)
    (list 'f 'd 'g 'g)
    (list 'f 'h 'i 'i))
   (list
    (list 'a 6 '*)
    (list 'b 3 '-)
    (list 'c 3 '=)
    (list 'd 5 '+)
    (list 'e 3 '-)
    (list 'f 3 '-)
    (list 'g 2 '/)
    (list 'h 4 '=)
    (list 'i 1 '-))))

;; a partial solution to puzzle1
(define puzzle1partial
  (make-puzzle 
   4
   (list
    (list 'a 'b 'b 'c)
    (list 'a 2 1 4)
    (list 'f 3 'g 'g)
    (list 'f 'h 'i 'i))
   (list
    (list 'a 6 '*)
    (list 'b 3 '-)
    (list 'c 3 '=)
    (list 'f 3 '-)
    (list 'g 2 '/)
    (list 'h 4 '=)
    (list 'i 1 '-))))

;; a partial solution to puzzle1 with a cage partially filled in
(define puzzle1partial2
  (make-puzzle 
   4
   (list
    (list (make-guess 'a 2) 'b 'b 'c)
    (list 'a 2 1 4)
    (list 'f 3 'g 'g)
    (list 'f 'h 'i 'i))
   (list
    (list 'a 6 '*)
    (list 'b 3 '-)
    (list 'c 3 '=)
    (list 'f 3 '-)
    (list 'g 2 '/)
    (list 'h 4 '=)
    (list 'i 1 '-))))

;; a partial solution to puzzle1 with a cage partially filled in
;; but not yet verified 
(define puzzle1partial3
  (make-puzzle 
   4
   (list
    (list (make-guess 'a 2) 'b 'b 'c)
    (list (make-guess 'a 3) 2 1 4)
    (list 'f 3 'g 'g)
    (list 'f 'h 'i 'i))
   (list
    (list 'a 6 '*)
    (list 'b 3 '-)
    (list 'c 3 '=)
    (list 'f 3 '-)
    (list 'g 2 '/)
    (list 'h 4 '=)
    (list 'i 1 '-))))

;; The solution to puzzle 1
(define puzzle1soln
  (make-puzzle
   4
   '((2 1 4 3)
     (3 2 1 4)
     (4 3 2 1)
     (1 4 3 2))
   empty))

;; wikipedia KenKen example
(define puzzle2
  (make-puzzle
   6
   '((a b b c d d)
     (a e e c f d)
     (h h i i f d)
     (h h j k l l)
     (m m j k k g)
     (o o o p p g))
   '((a 11 +)
     (b 2 /)
     (c 20 *)
     (d 6 *)
     (e 3 -)
     (f 3 /)
     (g 9 +)
     (h 240 *)
     (i 6 *)
     (j 6 *)
     (k 7 +)
     (l 30 *)
     (m 6 *)
     (o 8 +)
     (p 2 /))))

;; The solution to puzzle 2
(define puzzle2soln
  (make-puzzle
   6
   '((5 6 3 4 1 2)
     (6 1 4 5 2 3)
     (4 5 2 3 6 1)
     (3 4 1 2 5 6)
     (2 3 6 1 4 5)
     (1 2 5 6 3 4))
   empty))

;; Tiny board
(define puzzle3
  (make-puzzle 
   2 
   '((a b) 
     (c b)) 
   '((b 3 +) 
     (c 2 =)
     (a 1 =))))

(define puzzle3partial
  (make-puzzle
   2 
   (list
    (list 'a (make-guess 'b 1))
    (list 'c (make-guess 'b 2)))
   '((b 3 +) 
     (c 2 =)
     (a 1 =))))  

;; a big board:  will take a *long* time without trying the bonus
(define puzzle4
  (make-puzzle
   9
   '((a a b c d e e f f)
     (g h b c d i j k l)
     (g h m m i i j k l)
     (n o m p p q q r s)
     (n o t u p v v r s)
     (n w t u x x y z z)
     (A w B C C C y D D)
     (A B B E E F G H I)
     (J J K K F F G H I))
   '((a 2 /)
     (b 11 +)
     (c 1 -)
     (d 7 *)
     (e 4 -)
     (f 9 +)
     (g 1 -)
     (h 4 /)
     (i 108 *)
     (j 13 +)
     (k 2 -)
     (l 5 -)
     (m 84 *)
     (n 24 *)
     (o 40 *)
     (p 18 *)
     (q 2 /)
     (r 2 -)
     (s 13 +)
     (t 10 +)
     (u 13 +)
     (v 2 -)
     (w 63 *)
     (x 1 -)
     (y 3 /)
     (z 2 /)
     (A 7 +)
     (B 13 +)
     (C 336 *)
     (D 1 -)
     (E 15 +)
     (F 12 *)
     (G 9 +)
     (H 5 -)
     (I 18 *)
     (J 3 /)
     (K 40 *))))

;; Self defined tests

;; test1-guess is not confirmed to be solvable.
(define test1-guess
  (make-puzzle 
   4
   (list
    (list (make-guess 'a 1) 'b 'b 'c)
    (list (make-guess 'a 2) 'd 'e 'e)
    (list 'f 'd 'g 'g)
    (list 'f 'h 'i 'i))
   (list
    (list 'a 6 '*)
    (list 'b 3 '-)
    (list 'c 3 '=)
    (list 'd 5 '+)
    (list 'e 3 '-)
    (list 'f 3 '-)
    (list 'g 2 '/)
    (list 'h 4 '=)
    (list 'i 1 '-))))

;; test2 is not confirmed to be solvable.
(define test2
  (make-puzzle 
   4
   (list
    (list 1 'b 'b 'c)
    (list 2 'd 'e 'e)
    (list 'f 'd 'g 'g)
    (list 'f 'h 'i 'i))
   (list
    (list 'a 6 '*)
    (list 'b 3 '-)
    (list 'c 3 '=)
    (list 'd 5 '+)
    (list 'e 3 '-)
    (list 'f 3 '-)
    (list 'g 2 '/)
    (list 'h 4 '=)
    (list 'i 1 '-))))

;; -----------------------------------------------------------------------------

(define test1
  (make-puzzle
   6
   '(((make-guess 'a 5) b b c d d)
     (a e e c f d)
     (h h i i f d)
     (h h j k l l)
     (m m j k k g)
     (o o o p p g))
   '((a 11 +)
     (b 2 /)
     (c 20 *)
     (d 6 *)
     (e 3 -)
     (f 3 /)
     (g 9 +)
     (h 240 *)
     (i 6 *)
     (j 6 *)
     (k 7 +)
     (l 30 *)
     (m 6 *)
     (o 8 +)
     (p 2 /))))

(check-expect (find-blank (make-puzzle 0 empty empty)) false)
(check-expect (find-blank puzzle1) (make-posn 0 0))
(check-expect (find-blank puzzle1soln) false)
(check-expect (find-blank puzzle1partial2) (make-posn 0 1))
(check-expect (find-blank test1-guess) 'guess)
(check-expect (find-blank test2) (make-posn 1 0))
(check-expect (find-blank puzzle3partial) 'guess)
(check-expect (find-blank test1) (make-posn 0 1))

(check-expect (used-in-row (make-puzzle 1 '((1)) empty) (make-posn 0 0)) '(1))
(check-expect (used-in-row (make-puzzle 1 '((a)) '((a 1 =))) (make-posn 0 0)) empty)
(check-expect (used-in-row puzzle1partial (make-posn 2 2)) (list 3))
(check-expect (used-in-row puzzle1partial2 (make-posn 0 1)) (list 1 2 4))
(check-expect (used-in-row puzzle1partial3 (make-posn 3 0)) (list 2))
(check-expect (used-in-row puzzle1soln (make-posn 1 2)) (list 1 2 3 4))
(check-expect (used-in-row puzzle4 (make-posn 2 2)) empty)

(check-expect (used-in-column (make-puzzle 1 '((1)) empty) (make-posn 0 0)) '(1))
(check-expect (used-in-column (make-puzzle 1 '((a)) '((a 1 =))) (make-posn 0 0)) empty)
(check-expect (used-in-column puzzle1 (make-posn 1 1)) empty)
(check-expect (used-in-column puzzle1partial (make-posn 2 2)) (list 1))
(check-expect (used-in-column puzzle1partial2 (make-posn 0 1)) (list 2))
(check-expect (used-in-column puzzle2soln (make-posn 1 2)) '(1 2 3 4 5 6))

(check-expect (available-vals puzzle1partial (make-posn 2 2)) '(2 4))
(check-expect (available-vals puzzle1partial2 (make-posn 0 1)) '(3))
(check-expect (available-vals puzzle2soln (make-posn 2 2)) empty)
(check-expect (available-vals (make-puzzle 1 '((1)) empty) (make-posn 0 0)) empty)
(check-expect (available-vals (make-puzzle 1 '((a)) '((a 1 =))) (make-posn 0 0)) '(1))

(check-expect (place-guess (puzzle-board puzzle1) (make-posn 3 2) 5)
              (list
               (list 'a 'b 'b 'c)
               (list 'a 'd 'e 'e)
               (list 'f 'd 'g (make-guess 'g 5))
               (list 'f 'h 'i 'i)))

(check-expect (place-guess (puzzle-board puzzle1) (make-posn 0 0) 1)
              (list
               (list (make-guess 'a 1) 'b 'b 'c)
               (list 'a 'd 'e 'e)
               (list 'f 'd 'g  'g )
               (list 'f 'h 'i 'i)))

(check-expect (place-guess (puzzle-board puzzle1) (make-posn 1 0) 1)
              (list
               (list 'a (make-guess 'b 1) 'b 'c)
               (list 'a 'd 'e 'e)
               (list 'f 'd 'g  'g )
               (list 'f 'h 'i 'i)))

(check-expect (place-guess (puzzle-board puzzle1) (make-posn 0 1) 1)
              (list
               (list 'a 'b  'b 'c)
               (list (make-guess 'a 1) 'd  'e 'e)
               (list 'f 'd 'g  'g )
               (list 'f 'h 'i 'i)))

(check-expect (place-guess (puzzle-board puzzle1) (make-posn 1 1) 1)
              (list
               (list 'a 'b  'b 'c)
               (list 'a (make-guess 'd 1) 'e 'e)
               (list 'f 'd 'g  'g )
               (list 'f 'h 'i 'i)))

;; testing constants:
(define test-plus-t
  (make-puzzle
   6
   (list (list (make-guess 'a 5) 'b 'b 'c 'd 'd)
         (list (make-guess 'a 6) 'e 'e 'c 'f 'd)
         (list 'h 'h 'i 'i 'f 'd)
         (list 'h 'h 'j 'k 'l 'l)
         (list 'm 'm 'j 'k 'k 'g)
         (list 'o 'o 'o 'p 'p 'g))
   '((a 11 +)
     (b 2 /)
     (c 20 *)
     (d 6 *)
     (e 3 -)
     (f 3 /)
     (g 9 +)
     (h 240 *)
     (i 6 *)
     (j 6 *)
     (k 7 +)
     (l 30 *)
     (m 6 *)
     (o 8 +)
     (p 2 /))))

(define test-plus-f
  (make-puzzle
   6
   (list (list (make-guess 'a 5) 'b 'b 'c 'd 'd)
         (list (make-guess 'a 4) 'e 'e 'c 'f 'd)
         (list 'h 'h 'i 'i 'f 'd)
         (list 'h 'h 'j 'k 'l 'l)
         (list 'm 'm 'j 'k 'k 'g)
         (list 'o 'o 'o 'p 'p 'g))
   '((a 11 +)
     (b 2 /)
     (c 20 *)
     (d 6 *)
     (e 3 -)
     (f 3 /)
     (g 9 +)
     (h 240 *)
     (i 6 *)
     (j 6 *)
     (k 7 +)
     (l 30 *)
     (m 6 *)
     (o 8 +)
     (p 2 /))))

(define test-minus-f
  (make-puzzle
   6
   (list (list (make-guess 'a 5) 'b 'b 'c 'd 'd)
         (list (make-guess 'a 1) 'e 'e 'c 'f 'd)
         (list 'h 'h 'i 'i 'f 'd)
         (list 'h 'h 'j 'k 'l 'l)
         (list 'm 'm 'j 'k 'k 'g)
         (list 'o 'o 'o 'p 'p 'g))
   '((a 1 -)
     (b 2 /)
     (c 20 *)
     (d 6 *)
     (e 3 -)
     (f 3 /)
     (g 9 +)
     (h 240 *)
     (i 6 *)
     (j 6 *)
     (k 7 +)
     (l 30 *)
     (m 6 *)
     (o 8 +)
     (p 2 /))))

(define test-minus-t
  (make-puzzle
   6
   (list (list (make-guess 'a 5) 'b 'b 'c 'd 'd)
         (list (make-guess 'a 4) 'e 'e 'c 'f 'd)
         (list 'h 'h 'i 'i 'f 'd)
         (list 'h 'h 'j 'k 'l 'l)
         (list 'm 'm 'j 'k 'k 'g)
         (list 'o 'o 'o 'p 'p 'g))
   '((a 1 -)
     (b 2 /)
     (c 20 *)
     (d 6 *)
     (e 3 -)
     (f 3 /)
     (g 9 +)
     (h 240 *)
     (i 6 *)
     (j 6 *)
     (k 7 +)
     (l 30 *)
     (m 6 *)
     (o 8 +)
     (p 2 /))))

(define test-mult-t
  (make-puzzle
   6
   (list (list (make-guess 'a 5) 'b 'b 'c 'd 'd)
         (list (make-guess 'a 4) 'e 'e 'c 'f 'd)
         (list 'h 'h 'i 'i 'f 'd)
         (list 'h 'h 'j 'k 'l 'l)
         (list 'm 'm 'j 'k 'k 'g)
         (list 'o 'o 'o 'p 'p 'g))
   '((a 20 *)
     (b 2 /)
     (c 20 *)
     (d 6 *)
     (e 3 -)
     (f 3 /)
     (g 9 +)
     (h 240 *)
     (i 6 *)
     (j 6 *)
     (k 7 +)
     (l 30 *)
     (m 6 *)
     (o 8 +)
     (p 2 /))))

(define test-mult-f
  (make-puzzle
   6
   (list (list (make-guess 'a 5) 'b 'b 'c 'd 'd)
         (list (make-guess 'a 2) 'e 'e 'c 'f 'd)
         (list 'h 'h 'i 'i 'f 'd)
         (list 'h 'h 'j 'k 'l 'l)
         (list 'm 'm 'j 'k 'k 'g)
         (list 'o 'o 'o 'p 'p 'g))
   '((a 20 *)
     (b 2 /)
     (c 20 *)
     (d 6 *)
     (e 3 -)
     (f 3 /)
     (g 9 +)
     (h 240 *)
     (i 6 *)
     (j 6 *)
     (k 7 +)
     (l 30 *)
     (m 6 *)
     (o 8 +)
     (p 2 /))))

(define test-div-t
  (make-puzzle
   6
   (list (list (make-guess 'a 4) 'b 'b 'c 'd 'd)
         (list (make-guess 'a 2) 'e 'e 'c 'f 'd)
         (list 'h 'h 'i 'i 'f 'd)
         (list 'h 'h 'j 'k 'l 'l)
         (list 'm 'm 'j 'k 'k 'g)
         (list 'o 'o 'o 'p 'p 'g))
   '((a 2 /)
     (b 2 /)
     (c 20 *)
     (d 6 *)
     (e 3 -)
     (f 3 /)
     (g 9 +)
     (h 240 *)
     (i 6 *)
     (j 6 *)
     (k 7 +)
     (l 30 *)
     (m 6 *)
     (o 8 +)
     (p 2 /))))

(define test-div-f
  (make-puzzle
   6
   (list (list (make-guess 'a 1) 'b 'b 'c 'd 'd)
         (list (make-guess 'a 3) 'e 'e 'c 'f 'd)
         (list 'h 'h 'i 'i 'f 'd)
         (list 'h 'h 'j 'k 'l 'l)
         (list 'm 'm 'j 'k 'k 'g)
         (list 'o 'o 'o 'p 'p 'g))
   '((a 2 /)
     (b 2 /)
     (c 20 *)
     (d 6 *)
     (e 3 -)
     (f 3 /)
     (g 9 +)
     (h 240 *)
     (i 6 *)
     (j 6 *)
     (k 7 +)
     (l 30 *)
     (m 6 *)
     (o 8 +)
     (p 2 /))))


(check-expect (guess-valid? puzzle3partial) true)
(check-expect (guess-valid? test-plus-t) true)
(check-expect (guess-valid? test-plus-f) false)
(check-expect (guess-valid? test-minus-t) true)
(check-expect (guess-valid? test-minus-f) false)
(check-expect (guess-valid? test-mult-t) true)
(check-expect (guess-valid? test-mult-f) false)
(check-expect (guess-valid? test-div-f) false)
(check-expect (guess-valid? test-div-t) true)
(check-expect (guess-valid? (make-puzzle 1 (list (list (make-guess 'a 1))) '((a 1 =)))) true)
(check-expect (guess-valid? (make-puzzle 1 (list (list (make-guess 'a 1))) '((a 4 =)))) false)

(check-expect (apply-guess puzzle3partial)
              (make-puzzle
               2 
               (list
                (list 'a 1)
                (list 'c 2))
               '((c 2 =)
                 (a 1 =)))) 

(check-expect (apply-guess test-plus-t)
              (make-puzzle
               6
               (list
                (list 5 'b 'b 'c 'd 'd)
                (list 6 'e 'e 'c 'f 'd)
                (list 'h 'h 'i 'i 'f 'd)
                (list 'h 'h 'j 'k 'l 'l)
                (list 'm 'm 'j 'k 'k 'g)
                (list 'o 'o 'o 'p 'p 'g))
               (list
                (list 'b 2 '/)
                (list 'c 20 '*)
                (list 'd 6 '*)
                (list 'e 3 '-)
                (list 'f 3 '/)
                (list 'g 9 '+)
                (list 'h 240 '*)
                (list 'i 6 '*)
                (list 'j 6 '*)
                (list 'k 7 '+)
                (list 'l 30 '*)
                (list 'm 6 '*)
                (list 'o 8 '+)
                (list 'p 2 '/))))

(check-expect (apply-guess test-div-t)
              (make-puzzle
               6
               (list
                (list 4 'b 'b 'c 'd 'd)
                (list 2 'e 'e 'c 'f 'd)
                (list 'h 'h 'i 'i 'f 'd)
                (list 'h 'h 'j 'k 'l 'l)
                (list 'm 'm 'j 'k 'k 'g)
                (list 'o 'o 'o 'p 'p 'g))
               (list
                (list 'b 2 '/)
                (list 'c 20 '*)
                (list 'd 6 '*)
                (list 'e 3 '-)
                (list 'f 3 '/)
                (list 'g 9 '+)
                (list 'h 240 '*)
                (list 'i 6 '*)
                (list 'j 6 '*)
                (list 'k 7 '+)
                (list 'l 30 '*)
                (list 'm 6 '*)
                (list 'o 8 '+)
                (list 'p 2 '/))))

(check-expect (apply-guess test-minus-t)
              (make-puzzle
               6
               (list
                (list 5 'b 'b 'c 'd 'd)
                (list 4 'e 'e 'c 'f 'd)
                (list 'h 'h 'i 'i 'f 'd)
                (list 'h 'h 'j 'k 'l 'l)
                (list 'm 'm 'j 'k 'k 'g)
                (list 'o 'o 'o 'p 'p 'g))
               (list
                (list 'b 2 '/)
                (list 'c 20 '*)
                (list 'd 6 '*)
                (list 'e 3 '-)
                (list 'f 3 '/)
                (list 'g 9 '+)
                (list 'h 240 '*)
                (list 'i 6 '*)
                (list 'j 6 '*)
                (list 'k 7 '+)
                (list 'l 30 '*)
                (list 'm 6 '*)
                (list 'o 8 '+)
                (list 'p 2 '/))))

(check-expect (apply-guess test-mult-t)
              (make-puzzle
               6
               (list
                (list 5 'b 'b 'c 'd 'd)
                (list 4 'e 'e 'c 'f 'd)
                (list 'h 'h 'i 'i 'f 'd)
                (list 'h 'h 'j 'k 'l 'l)
                (list 'm 'm 'j 'k 'k 'g)
                (list 'o 'o 'o 'p 'p 'g))
               (list
                (list 'b 2 '/)
                (list 'c 20 '*)
                (list 'd 6 '*)
                (list 'e 3 '-)
                (list 'f 3 '/)
                (list 'g 9 '+)
                (list 'h 240 '*)
                (list 'i 6 '*)
                (list 'j 6 '*)
                (list 'k 7 '+)
                (list 'l 30 '*)
                (list 'm 6 '*)
                (list 'o 8 '+)
                (list 'p 2 '/))))

(check-expect (apply-guess (make-puzzle 1 (list (list (make-guess 'a 1))) '((a 1 =))))
              (make-puzzle 1 (list (list 1)) empty))

(check-expect (neighbours puzzle2soln) empty)
(check-expect (neighbours test-mult-f) empty)
(check-expect (neighbours puzzle3partial)
              (list 
               (make-puzzle
                2 
                (list
                 (list 'a 1)
                 (list 'c 2))
                '((c 2 =)
                  (a 1 =)))))  
(check-expect (neighbours test-div-t)
              (list (make-puzzle
                     6
                     (list
                      (list 4 'b 'b 'c 'd 'd)
                      (list 2 'e 'e 'c 'f 'd)
                      (list 'h 'h 'i 'i 'f 'd)
                      (list 'h 'h 'j 'k 'l 'l)
                      (list 'm 'm 'j 'k 'k 'g)
                      (list 'o 'o 'o 'p 'p 'g))
                     (list
                      (list 'b 2 '/)
                      (list 'c 20 '*)
                      (list 'd 6 '*)
                      (list 'e 3 '-)
                      (list 'f 3 '/)
                      (list 'g 9 '+)
                      (list 'h 240 '*)
                      (list 'i 6 '*)
                      (list 'j 6 '*)
                      (list 'k 7 '+)
                      (list 'l 30 '*)
                      (list 'm 6 '*)
                      (list 'o 8 '+)
                      (list 'p 2 '/)))))

(check-expect (neighbours puzzle1partial)
              (list
               (make-puzzle
                4
                (list (list (make-guess 'a 1) 'b 'b 'c) 
                      (list 'a 2 1 4)
                      (list 'f 3 'g 'g) 
                      (list 'f 'h 'i 'i))
                (list (list 'a 6 '*)
                      (list 'b 3 '-) 
                      (list 'c 3 '=) 
                      (list 'f 3 '-) 
                      (list 'g 2 '/)
                      (list 'h 4 '=)
                      (list 'i 1 '-)))
               (make-puzzle
                4
                (list (list (make-guess 'a 2) 'b 'b 'c) 
                      (list 'a 2 1 4) 
                      (list 'f 3 'g 'g)
                      (list 'f 'h 'i 'i))
                (list (list 'a 6 '*)
                      (list 'b 3 '-) 
                      (list 'c 3 '=) 
                      (list 'f 3 '-)
                      (list 'g 2 '/)
                      (list 'h 4 '=) 
                      (list 'i 1 '-)))
               (make-puzzle
                4
                (list (list (make-guess 'a 3) 'b 'b 'c)
                      (list 'a 2 1 4)
                      (list 'f 3 'g 'g) 
                      (list 'f 'h 'i 'i))
                (list (list 'a 6 '*)
                      (list 'b 3 '-)
                      (list 'c 3 '=) 
                      (list 'f 3 '-) 
                      (list 'g 2 '/) 
                      (list 'h 4 '=) 
                      (list 'i 1 '-)))
               (make-puzzle
                4
                (list (list (make-guess 'a 4) 'b 'b 'c)
                      (list 'a 2 1 4)
                      (list 'f 3 'g 'g) 
                      (list 'f 'h 'i 'i))
                (list (list 'a 6 '*)
                      (list 'b 3 '-)
                      (list 'c 3 '=) 
                      (list 'f 3 '-)
                      (list 'g 2 '/) 
                      (list 'h 4 '=)
                      (list 'i 1 '-)))))
