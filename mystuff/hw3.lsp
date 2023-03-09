; HOMEWORK 3 LISP FILE (CONTAINS 1 BIG PROBLEM)
; (C) Nevin Liang Fall 2021


; CS161 Hw3: Sokoban
;
; *********************
;    READ THIS FIRST
; *********************
;
; All functions that you need to modify are marked with 'EXERCISE' in their header comments.
; Do not modify a-star.lsp.
; This file also contains many helper functions. You may call any of them in your functions.
;
; *Warning*: The provided A* code only supports the maximum cost of 4999 for any node.
; That is f(n)=g(n)+h(n) < 5000. So, be careful when you write your heuristic functions.
; Do not make them return anything too large.
;
; For Allegro Common Lisp users: The free version of Allegro puts a limit on memory.
; So, it may crash on some hard sokoban problems and there is no easy fix (unless you buy
; Allegro).
; Of course, other versions of Lisp may also crash if the problem is too hard, but the amount
; of memory available will be relatively more relaxed.
; Improving the quality of the heuristic will mitigate this problem, as it will allow A* to
; solve hard problems with fewer node expansions.
;
; In either case, this limitation should not significantly affect your grade.
;
; Remember that most functions are not graded on efficiency (only correctness).
; Efficiency can only influence your heuristic performance in the competition (which will
; affect your score).
;
;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; General utility functions
; They are not necessary for this homework.
; Use/modify them for your own convenience.
;

;
; For reloading modified code.
; I found this easier than typing (load "filename") every time.
;
(defun reload()
  (load "hw3.lsp"))

;
; For loading a-star.lsp.
;
(defun load-a-star()
  (load "a-star.lsp"))

;
; Reloads hw3.lsp and a-star.lsp
;
(defun reload-all()
  (reload)
  (load-a-star))

;
; A shortcut function.
; goal-test and next-states stay the same throughout the assignment.
; So, you can just call (sokoban <init-state> #'<heuristic-name>).
;
;
(defun sokoban (s h)
  (a* s #'goal-test #'next-states h))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; end general utility functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; We now begin actual Sokoban code
;

; Define some global variables
(setq blank 0)
(setq wall 1)
(setq box 2)
(setq keeper 3)
(setq star 4)
(setq boxstar 5)
(setq keeperstar 6)

; Some helper functions for checking the content of a square
(defun isBlank (v)
  (= v blank))

(defun isWall (v)
  (= v wall))

(defun isBox (v)
  (= v box))

(defun isKeeper (v)
  (= v keeper))

(defun isStar (v)
  (= v star))

(defun isBoxStar (v)
  (= v boxstar))

(defun isKeeperStar (v)
  (= v keeperstar))

;
; Helper function of getKeeperPosition
;
(defun getKeeperColumn (r col)
  (cond ((null r) nil)
	(t (if (or (isKeeper (car r)) (isKeeperStar (car r)))
	       col
	     (getKeeperColumn (cdr r) (+ col 1))
	     );end if
	   );end t
	);end cond
  )

;
; getKeeperPosition (s firstRow)
; Returns a list indicating the position of the keeper (c r).
;
; Assumes that the keeper is in row >= firstRow.
; The top row is the zeroth row.
; The first (right) column is the zeroth column.
;
(defun getKeeperPosition (s row)
  (cond ((null s) nil)
	(t (let ((x (getKeeperColumn (car s) 0)))
	     (if x
		 ;keeper is in this row
		 (list row x)
		 ;otherwise move on
		 (getKeeperPosition (cdr s) (+ row 1))
		 );end if
	       );end let
	 );end t
	);end cond
  );end defun

;
; cleanUpList (l)
; returns l with any NIL element removed.
; For example, if l is '(1 2 NIL 3 NIL), returns '(1 2 3).
;
(defun cleanUpList (L)
  (cond ((null L) nil)
	(t (let ((cur (car L))
		 (res (cleanUpList (cdr L)))
		 )
	     (if cur
		 (cons cur res)
		  res
		 )
	     );end let
	   );end t
	);end cond
  );end

; EXERCISE: Modify this function to return true (t)
; if and only if s is a goal state of a Sokoban game.
; (no box is on a non-goal square)
;
; Currently, it always returns NIL. If A* is called with
; this function as the goal testing function, A* will never
; terminate until the whole search space is exhausted.
;
(defun goal-test (s)
  (cond
    ((> (count box (car s)) 0) nil)  ;make sure no boxes in any row
    ((> (count star (car s)) 0) nil) ;make sure no goals in any row
    ((null (cdr s)) t) ;test cdr after instead of before to minimize depth
    (t (goal-test (cdr s)))
    ) ;end cond
  ) ;end defun

; EXERCISE: Modify this function to return the list of
; sucessor states of s.
;
; This is the top-level next-states (successor) function.
; Some skeleton code is provided below.
; You may delete them totally, depending on your approach.
;
; If you want to use it, you will need to set 'result' to be
; the set of states after moving the keeper in each of the 4 directions.
; A pseudo-code for this is:
;
; ...
; (result (list (try-move s UP) (try-move s DOWN) (try-move s LEFT) (try-move s RIGHT)))
; ...
;
; You will need to define the function try-move and decide how to represent UP,DOWN,LEFT,RIGHT.
; Any NIL result returned from try-move can be removed by cleanUpList.
;
;
; next-states takes in a single state and tries to move in all of the 4 directions
; called try-move which attempts to move the keeper in the specified direction
(defun next-states (s)
  (let* (
      (result (list (try-move s '(-1 0)) 
                    (try-move s '(1 0)) 
                    (try-move s '(0 -1)) 
                    (try-move s '(0 1)))
        )
      )
    (cleanUpList result) ; gets rid of all the nils that try-move can return
    ) ; end let
  ) ; end defun

; try-move attemps to move the keeper in the direction specified
; it either returns a state or nil depending on whether the move was successful
(defun try-move (s dir)
  (let* (
    (pos (getKeeperPosition s 0)) ; original position of the keeper
    (x (car pos)) (y (cadr pos))
    (dx (car dir)) (dy (cadr dir)) ; delta movement
    (nx (+ x dx)) (ny (+ y dy)) ; new positions
    (bx (+ nx dx)) (by (+ ny dy)) ; new box positions (used if new pos is box)
    (og-type (square-type s x y)) ; types of all the positions labelled above
    (new-type (square-type s nx ny))
    (new-btype (square-type s bx by))
    (move-type (can-move og-type new-type new-btype)) ; with box or without or nil
    )
    (if move-type ; if can-move returns anything not nil, move and update all needed elements
      (move s x y nx ny bx by move-type)
      nil
      ) ; end if
    ) ; end let
  ) ; end defun

; returns either box, keeper, or nil
; box if new-type is a box and box can be moved to new-btype pos
; keeper if new-post is not a box and keeper can be moved to new-type pos
; nil if the change cannot happen
(defun can-move (og-type new-type new-btype)
  (cond
    ((and
      (or (= new-type box) (= new-type boxstar)) ; box or boxstar where new-type should be
      (or (= new-btype blank) (= new-btype star)) ; blank or star where new-btype should be
      ) box) ;moving the keeper and the box is valid
    ((or (= new-type blank) (= new-type star)) keeper) ; otherwise move just keeper
    (t nil) ; otherwise can't move at all
    ) ; end cond
  ) ; end defun

; the actual function that returns the new state (and processes all the moves)
; needs to call more than one move since more than one thing can move at once
(defun move (s x y nx ny bx by move-type)
  (cond
    ((= move-type box) ; moving the keeper and the box requires 3 different state changes
      (rs (rs (rs s bx by box) nx ny keeper) x y blank)
      )
    ((= move-type keeper) ; moving just the keeper requires just 2 different state changes
      (rs (rs s nx ny keeper) x y blank)
      )
    (t nil) ; never supposed to happen since move-type should always be non-nil
    ) ; end cond
  ) ; end defun

; replaces state of (x, y) with type where type is box or keeper or blank
; made it separate function to deal with star states
(defun rs (s x y type)
  ; in case the original box was any star state
  (if (or (= (square-type s x y) star)
          (= (square-type s x y) boxstar)
          (= (square-type s x y) keeperstar))
    ; we have to make the new object also starred when it goes on that square
    (cond
      ((= type box) (set-state s x y boxstar))
      ((= type keeper) (set-state s x y keeperstar))
      ((= type blank) (set-state s x y star))
      ) ; end cond
    ; call the mutator to change the states
    (set-state s x y type)
    ) ; end if
  ) ; end defun

; sets element with index x of list l to data
(defun set-elem (l x data)
  (cond
    ; reaches end of recursion, append the new data to the rest of the list
    ((= x 0) (cons data (cdr l)))
    ; otherwise, append the current element to the rest of the list whatever it ends up being
    (t (cons (car l) (set-elem (cdr l) (- x 1) data)))
    ) ; end cond
  ) ; end defun

; sets element with row x and col y in s to type
(defun set-state (s x y type)
  ; setting element in a grid by calling set-elem twice for row/col
  (set-elem s x (set-elem (nth x s) y type))
  ) ; end defun

; EXERCISE: Modify this function to compute the trivial
; admissible heuristic.
;
(defun h0 (s)
  0
  )

; EXERCISE: Modify this function to compute the
; number of misplaced boxes in s.
;
; This heuristic is admissible. The minimum number of steps to place a box
; that is not at a goal is one. There is no sequence of moves that requires
; less than one move to move a box not on a goal to a goal. Thus, to move K
; boxes that are not on the goal to a goal each requires at least K moves.
; Since the number of misplaced boxes, K, is less than or equal to the minimum
; cost of solving the problem, this heuristic is admissible.
;
(defun h1 (s)
  (cond
    ; end state of recursion, no more elements in s
    ((null s) 0)
    ; otherwise, take a look at each row of s and count the number of boxes
    (t (+ (h1 (cdr s)) (count box (car s))))
    ) ; end cond 
  ) ; end defun

; EXERCISE: Change the name of this function to h<UID> where
; <UID> is your actual student ID number. Then, modify this
; function to compute an admissible heuristic value of s.
;
; This function will be entered in the competition.
; Objective: make A* solve problems as fast as possible.
; The Lisp 'time' function can be used to measure the
; running time of a function call.
;
; my heuristic is admissible. when the solution is impossible aka a box is stuck in a corner
; the value of my heuristic is 4000 (which is less than the 5000 limit allowed). otherwise,
; my heuristic is the sum of the manhattan distances from each of the boxes to their nearest
; goal. each box will HAVE to move at least that amount or else it is impossible to solve the
; problem. in addition, I add the distance from the keeper to the nearest box because the
; keeper will HAVE to encounter a box at some point and move to it or one that is farther away
(defun h705575353 (s)
  ; first define all lists of types that we will use later on
  (let* ((boxes (type-pos s box 0))
         (goals (type-pos s star 0))
         (keeper (getKeeperPosition s 0)))
    (max ; take the maximum of all of these heuristic elements
      ; sum of distances from boxes to nearest goals and distance from keeper to nearest box
      (+ (if (> (length boxes) 0) (near-box keeper boxes)
           0) ; end if
         (if (> (length goals) 0) (goal-dists boxes goals)
           0) ; end if
         )
      ; nearest distance from keeper to goal
      (goal-dists (list keeper) goals)
      (if (box-in-corner s boxes) 4000
        0)
      ) ; end max
    ) ; end let
  ) ; end defun

; sum of distances to every single goal. box-list is a list of boxes (I took advantage
; of this and actually used it for the distance from keeper to goal by pretending he's a
; box lol sorry)
(defun goal-dists (box-list goal-list)
  ; for every goal find the nearest box and sum them all together
  (cond
    ((null goal-list) 0)
    (t (+ (near-box (car goal-list) box-list) (goal-dists box-list (cdr goal-list))))
    ) ; end cond
  ) ; end defun

; finds the nearest box given a position and a list of boxes
(defun near-box (pos box-list)
  (cond
    ((null box-list) 1000) ; big number because we want min of everything
    (t (min (manhattan (car box-list) pos) (near-box pos (cdr box-list))))
    ) ; end cond
  ) ; end defun

; tests if any box is in a corner (whether that be a corner made of boxes or walls)
(defun box-in-corner (s box-list)
  (cond
    ; end of recusion if box list is empty no more boxes to check
    ((null box-list) nil)
    ; test if current box is in a corner otherwise move on to next box
    (t (or (in-corner s (first (car box-list)) (second (car box-list))) 
           (box-in-corner s (cdr box-list))))
    ) ; end cond
  ) ; end defun

; tests if box at position (x, y) is in a corner
(defun in-corner (s x y)
  ; it is in a corner if one of the four corners of the box is all obstacles
  (or (and (obstacle s (+ x 1) (+ y 1)) (obstacle s x (+ y 1)) (obstacle s (+ x 1) y))
      (and (obstacle s (+ x 1) (- y 1)) (obstacle s (+ x 1) y) (obstacle s x (- y 1)))
      (and (obstacle s (- x 1) (- y 1)) (obstacle s (- x 1) y) (obstacle s x (- y 1)))
      (and (obstacle s (- x 1) (+ y 1)) (obstacle s (- x 1) y) (obstacle s x (+ y 1)))
    ) ;end or
  ) ;end defun

; retrieves type of the (x, y) square
(defun square-type (s x y)
  ; every square outside the border is also technically a wall (since we cant leave)
  (if (and (> x -1) (> y -1) (< x (length s)) (< y (length (car s))))
    (nth y (nth x s))
    wall
    ) ; end if 
  ) ; end defun

; gets list of coords of that type
; for example, given a type like box, give a list of all coordinates for every box
(defun type-pos (s type row)
  (cond
    ((null (car s)) nil) ; end of recursion all elements in s are gone 
    ; if there is an element in that row of type 'type, search for it using the helper func
    ; and add it to the return list 
    ((> (count type (car s)) 0) (append (type-pos-helper (car s) type row 0) 
                                        (type-pos (cdr s) type (+ 1 row))))
    ; otherwise, just move on to the next row
    (t (type-pos (cdr s) type (+ 1 row)))
    ) ; end cond
  ) ; end defun

; searches through the specified row in the above function to find all coordinates of type
(defun type-pos-helper (l type row col)
  (cond
    ; end of recursive state, there are no more elements to check 
    ((null l) nil)
    ; same as before - check if current element is of that type and add it to the return list
    ((= (car l) type) (cons (list row col) (type-pos-helper (cdr l) type row (+ 1 col))))
    ; otherwise move on to the next element in the row 
    (t (type-pos-helper (cdr l) type row (+ 1 col)))
    ) ; end cond
  ) ; end defun

;tests if wall or box or boxstar
(defun obstacle (s x y)
  ; a square is an obstacle only if it is one of these three things
  (or (= (square-type s x y) boxstar) (= (square-type s x y) box) (= (square-type s x y) wall))
  ) ; end defun

;tests if wall or in bound
(defun barrier (s x y)
  (= (square-type s x y) wall) ; a barrier, unlike an obstacle cannot move ever
  ) ; end defun

; finds manhattan distance
(defun manhattan (a b)
  ; uses absolute value and differences to sum and find manhattan distance
  (+ (abs (- (car a) (car b))) (abs (- (cadr a) (cadr b))))
  ) ; end defun

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | Some predefined problems.
 | Each problem can be visualized by calling (printstate <problem>). For example, (printstate p1).
 | Problems are roughly ordered by their difficulties.
 | For most problems, we also privide 2 additional number per problem:
 |    1) # of nodes expanded by A* using our next-states and h0 heuristic.
 |    2) the depth of the optimal solution.
 | These numbers are located at the comments of the problems. For example, the first problem below
 | was solved by 80 nodes expansion of A* and its optimal solution depth is 7.
 |
 | Your implementation may not result in the same number of nodes expanded, but it should probably
 | give something in the same ballpark. As for the solution depth, any admissible heuristic must
 | make A* return an optimal solution. So, the depths of the optimal solutions provided could be used
 | for checking whether your heuristic is admissible.
 |
 | Warning: some problems toward the end are quite hard and could be impossible to solve without a good heuristic!
 |
 |#

;(80,7)
(setq p1 '((1 1 1 1 1 1)
	   (1 0 3 0 0 1)
	   (1 0 2 0 0 1)
	   (1 1 0 1 1 1)
	   (1 0 0 0 0 1)
	   (1 0 0 0 4 1)
	   (1 1 1 1 1 1)))

;(110,10)
(setq p2 '((1 1 1 1 1 1 1)
	   (1 0 0 0 0 0 1)
	   (1 0 0 0 0 0 1)
	   (1 0 0 2 1 4 1)
	   (1 3 0 0 1 0 1)
	   (1 1 1 1 1 1 1)))

;(211,12)
(setq p3 '((1 1 1 1 1 1 1 1 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 0 0 0 2 0 3 4 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 1 1 1 1 1 1 1 1)))

;(300,13)
(setq p4 '((1 1 1 1 1 1 1)
	   (0 0 0 0 0 1 4)
	   (0 0 0 0 0 0 0)
	   (0 0 1 1 1 0 0)
	   (0 0 1 0 0 0 0)
	   (0 2 1 0 0 0 0)
	   (0 3 1 0 0 0 0)))

;(551,10)
(setq p5 '((1 1 1 1 1 1)
	   (1 1 0 0 1 1)
	   (1 0 0 0 0 1)
	   (1 4 2 2 4 1)
	   (1 0 0 0 0 1)
	   (1 1 3 1 1 1)
	   (1 1 1 1 1 1)))

;(722,12)
(setq p6 '((1 1 1 1 1 1 1 1)
	   (1 0 0 0 0 0 4 1)
	   (1 0 0 0 2 2 3 1)
	   (1 0 0 1 0 0 4 1)
	   (1 1 1 1 1 1 1 1)))

;(1738,50)
(setq p7 '((1 1 1 1 1 1 1 1 1 1)
	   (0 0 1 1 1 1 0 0 0 3)
	   (0 0 0 0 0 1 0 0 0 0)
	   (0 0 0 0 0 1 0 0 1 0)
	   (0 0 1 0 0 1 0 0 1 0)
	   (0 2 1 0 0 0 0 0 1 0)
	   (0 0 1 0 0 0 0 0 1 4)))

;(1763,22)
(setq p8 '((1 1 1 1 1 1)
	   (1 4 0 0 4 1)
	   (1 0 2 2 0 1)
	   (1 2 0 1 0 1)
	   (1 3 0 0 4 1)
	   (1 1 1 1 1 1)))

;(1806,41)
(setq p9 '((1 1 1 1 1 1 1 1 1)
	   (1 1 1 0 0 1 1 1 1)
	   (1 0 0 0 0 0 2 0 1)
	   (1 0 1 0 0 1 2 0 1)
	   (1 0 4 0 4 1 3 0 1)
	   (1 1 1 1 1 1 1 1 1)))

;(10082,51)
(setq p10 '((1 1 1 1 1 0 0)
	    (1 0 0 0 1 1 0)
	    (1 3 2 0 0 1 1)
	    (1 1 0 2 0 0 1)
	    (0 1 1 0 2 0 1)
	    (0 0 1 1 0 0 1)
	    (0 0 0 1 1 4 1)
	    (0 0 0 0 1 4 1)
	    (0 0 0 0 1 4 1)
	    (0 0 0 0 1 1 1)))

;(16517,48)
(setq p11 '((1 1 1 1 1 1 1)
	    (1 4 0 0 0 4 1)
	    (1 0 2 2 1 0 1)
	    (1 0 2 0 1 3 1)
	    (1 1 2 0 1 0 1)
	    (1 4 0 0 4 0 1)
	    (1 1 1 1 1 1 1)))

;(22035,38)
(setq p12 '((0 0 0 0 1 1 1 1 1 0 0 0)
	    (1 1 1 1 1 0 0 0 1 1 1 1)
	    (1 0 0 0 2 0 0 0 0 0 0 1)
	    (1 3 0 0 0 0 0 0 0 0 0 1)
	    (1 0 0 0 2 1 1 1 0 0 0 1)
	    (1 0 0 0 0 1 0 1 4 0 4 1)
	    (1 1 1 1 1 1 0 1 1 1 1 1)))

;(26905,28)
(setq p13 '((1 1 1 1 1 1 1 1 1 1)
	    (1 4 0 0 0 0 0 2 0 1)
	    (1 0 2 0 0 0 0 0 4 1)
	    (1 0 3 0 0 0 0 0 2 1)
	    (1 0 0 0 0 0 0 0 0 1)
	    (1 0 0 0 0 0 0 0 4 1)
	    (1 1 1 1 1 1 1 1 1 1)))

;(41715,53)
(setq p14 '((0 0 1 0 0 0 0)
	    (0 2 1 4 0 0 0)
	    (0 2 0 4 0 0 0)
	    (3 2 1 1 1 0 0)
	    (0 0 1 4 0 0 0)))

;(48695,44)
(setq p15 '((1 1 1 1 1 1 1)
	    (1 0 0 0 0 0 1)
	    (1 0 0 2 2 0 1)
	    (1 0 2 0 2 3 1)
	    (1 4 4 1 1 1 1)
	    (1 4 4 1 0 0 0)
	    (1 1 1 1 0 0 0)
	    ))

;(91344,111)
(setq p16 '((1 1 1 1 1 0 0 0)
	    (1 0 0 0 1 0 0 0)
	    (1 2 1 0 1 1 1 1)
	    (1 4 0 0 0 0 0 1)
	    (1 0 0 5 0 5 0 1)
	    (1 0 5 0 1 0 1 1)
	    (1 1 1 0 3 0 1 0)
	    (0 0 1 1 1 1 1 0)))

;(3301278,76)
(setq p17 '((1 1 1 1 1 1 1 1 1 1)
	    (1 3 0 0 1 0 0 0 4 1)
	    (1 0 2 0 2 0 0 4 4 1)
	    (1 0 2 2 2 1 1 4 4 1)
	    (1 0 0 0 0 1 1 4 4 1)
	    (1 1 1 1 1 1 0 0 0 0)))

;(??,25)
(setq p18 '((0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1)
	    (0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0)
	    (0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0)
	    (0 0 0 0 0 0 0 0 3 0 0 0 0 0 0 0)
	    (0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0)
	    (0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0)
	    (1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 4 1 0 0 0 0)
	    (0 0 0 0 1 0 2 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 2 0 0 0 4 1 0 0 0 0)
	    ))
;(??,21)
(setq p19 '((0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (1 1 1 1 0 0 0 0 1 1 1 1)
	    (0 0 0 0 1 0 0 1 0 0 0 0)
	    (0 0 0 0 0 0 3 0 0 0 2 0)
	    (0 0 0 0 1 0 0 1 0 0 0 4)
	    (1 1 1 1 0 0 0 0 1 1 1 1)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 2 0 4 1 0 0 0)))

;(??,??)
(setq p20 '((0 0 0 1 1 1 1 0 0)
	    (1 1 1 1 0 0 1 1 0)
	    (1 0 0 0 2 0 0 1 0)
	    (1 0 0 5 5 5 0 1 0)
	    (1 0 0 4 0 4 0 1 1)
	    (1 1 0 5 0 5 0 0 1)
	    (0 1 1 5 5 5 0 0 1)
	    (0 0 1 0 2 0 1 1 1)
	    (0 0 1 0 3 0 1 0 0)
	    (0 0 1 1 1 1 1 0 0)))

;(??,??)
(setq p21 '((0 0 1 1 1 1 1 1 1 0)
	    (1 1 1 0 0 1 1 1 1 0)
	    (1 0 0 2 0 0 0 1 1 0)
	    (1 3 2 0 2 0 0 0 1 0)
	    (1 1 0 2 0 2 0 0 1 0)
	    (0 1 1 0 2 0 2 0 1 0)
	    (0 0 1 1 0 2 0 0 1 0)
	    (0 0 0 1 1 1 1 0 1 0)
	    (0 0 0 0 1 4 1 0 0 1)
	    (0 0 0 0 1 4 4 4 0 1)
	    (0 0 0 0 1 0 1 4 0 1)
	    (0 0 0 0 1 4 4 4 0 1)
	    (0 0 0 0 1 1 1 1 1 1)))

;(??,??)
(setq p22 '((0 0 0 0 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0)
	    (0 0 0 0 1 0 0 0 1 0 0 0 0 0 0 0 0 0 0)
	    (0 0 0 0 1 2 0 0 1 0 0 0 0 0 0 0 0 0 0)
	    (0 0 1 1 1 0 0 2 1 1 0 0 0 0 0 0 0 0 0)
	    (0 0 1 0 0 2 0 2 0 1 0 0 0 0 0 0 0 0 0)
	    (1 1 1 0 1 0 1 1 0 1 0 0 0 1 1 1 1 1 1)
	    (1 0 0 0 1 0 1 1 0 1 1 1 1 1 0 0 4 4 1)
	    (1 0 2 0 0 2 0 0 0 0 0 0 0 0 0 0 4 4 1)
	    (1 1 1 1 1 0 1 1 1 0 1 3 1 1 0 0 4 4 1)
	    (0 0 0 0 1 0 0 0 0 0 1 1 1 1 1 1 1 1 1)
	    (0 0 0 0 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | Utility functions for printing states and moves.
 | You do not need to understand any of the functions below this point.
 |#

;
; Helper function of prettyMoves
; from s1 --> s2
;
(defun detectDiff (s1 s2)
  (let* ((k1 (getKeeperPosition s1 0))
	 (k2 (getKeeperPosition s2 0))
	 (deltaX (- (car k2) (car k1)))
	 (deltaY (- (cadr k2) (cadr k1)))
	 )
    (cond ((= deltaX 0) (if (> deltaY 0) 'DOWN 'UP))
	  (t (if (> deltaX 0) 'RIGHT 'LEFT))
	  );end cond
    );end let
  );end defun

;
; Translates a list of states into a list of moves.
; Usage: (prettyMoves (a* <problem> #'goal-test #'next-states #'heuristic))
;
(defun prettyMoves (m)
  (cond ((null m) nil)
	((= 1 (length m)) (list 'END))
	(t (cons (detectDiff (car m) (cadr m)) (prettyMoves (cdr m))))
	);end cond
  );

;
; Print the content of the square to stdout.
;
(defun printSquare (s)
  (cond ((= s blank) (format t " "))
	((= s wall) (format t "#"))
	((= s box) (format t "$"))
	((= s keeper) (format t "@"))
	((= s star) (format t "."))
	((= s boxstar) (format t "*"))
	((= s keeperstar) (format t "+"))
	(t (format t "|"))
	);end cond
  )

;
; Print a row
;
(defun printRow (r)
  (dolist (cur r)
    (printSquare cur)
    )
  );

;
; Print a state
;
(defun printState (s)
  (progn
    (dolist (cur s)
      (printRow cur)
      (format t "~%")
      )
    );end progn
  )

;
; Print a list of states with delay.
;
(defun printStates (sl delay)
  (dolist (cur sl)
    (printState cur)
    (sleep delay)
    );end dolist
  );end defun
