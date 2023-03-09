; HOMEWORK 2 LISP FILE (CONTAINS 2 PROBLEMS)
; (C) Nevin Liang Fall 2021

;;;;;;;;;;;;;;
; Homework 2 ;
;;;;;;;;;;;;;;

;;;;;;;;;;;;;;
; Question 1 ;
;;;;;;;;;;;;;;

; DFSRL calculates a dfs traversal and returns a visited right to left order
; input: FRINGE = list of search trees
; output: list representing traversal order from right to left
(defun DFSRL (FRINGE)
  (cond ((NULL FRINGE) nil)
    ((atom FRINGE) (list FRINGE))
    (t (append (DFSRL (cdr FRINGE)) (DFSRL (car FRINGE))))
  )
)
;(write (DFSRL '((((L E) F) T))))
;(write (DFSRL '((R (I (G (H T)))))))
;(write (DFSRL '(((A (B)) (D) C))))
;(write (DFSRL '((T (H R E) E))))
;(write (DFSRL '((A ((C ((E) D)) B)))))

;;;;;;;;;;;;;;
; Question 2 ;
;;;;;;;;;;;;;;


; These functions implement a depth-first solver for the homer-baby-dog-poison
; problem. In this implementation, a state is represented by a single list
; (homer baby dog poison), where each variable is T if the respective entity is
; on the west side of the river, and NIL if it is on the east side.
; Thus, the initial state for this problem is (NIL NIL NIL NIL) (everybody 
; is on the east side) and the goal state is (T T T T).

; The main entry point for this solver is the function DFS, which is called
; with (a) the state to search from and (b) the path to this state. It returns
; the complete path from the initial state to the goal state: this path is a
; list of intermediate problem states. The first element of the path is the
; initial state and the last element is the goal state. Each intermediate state
; is the state that results from applying the appropriate operator to the
; preceding state. If there is no solution, DFS returns NIL.
; To call DFS to solve the original problem, one would call 
; (DFS '(NIL NIL NIL NIL) NIL) 
; However, it should be possible to call DFS with a different initial
; state or with an initial path.

; First, we define the helper functions of DFS.

; FINAL-STATE takes a single argument S, the current state, and returns T if it
; is the goal state (T T T T) and NIL otherwise.
(defun FINAL-STATE (S)
  ; pretty trivial. only return true when the state is equivalent to (T T T T)
  (equal '(T T T T) S)
)

; NEXT-STATE returns the state that results from applying an operator to the
; current state. It takes three arguments: the current state (S), and which entity
; to move (A, equal to h for homer only, b for homer with baby, d for homer 
; with dog, and p for homer with poison). 
; It returns a list containing the state that results from that move.
; If applying this operator results in an invalid state (because the dog and baby,
; or poisoin and baby are left unsupervised on one side of the river), or when the
; action is impossible (homer is not on the same side as the entity) it returns NIL.
; NOTE that next-state returns a list containing the successor state (which is
; itself a list); the return should look something like ((NIL NIL T T)).
(defun NEXT-STATE (S A)
  ; there are 4 possible values of A, so we have 4 if statements for each possibility
  (cond
    ((equal A 'h) ; when we move only homer
      ; when we move only homer, all of the error conditions can happen so we check all of them
      (if (and (equal (first S) (second S)) (or (equal (third S) (second S)) (equal (second S) (fourth S))))
        NIL
        ; if none of them happen, all we have to do is flip the current value of homer
        (list (cons (not (first S)) (rest S)))
      )
    ) 
    ((equal A 'b) ; when we move homer and the baby
      ; when we move homer and the baby, the baby will be fine. only error is if homer is not with the baby
      (if (not (equal (first S) (second S))) 
        NIL
        ; same as before, but now we flip both homer and the baby
        (list (list (not (first S)) (not (second S)) (third S) (fourth S)))
      )
    )
    ((equal A 'd) ; when we move homer and the dog
      ; when homer and dog move, 2 errors possible: homer not on same side as dog, and baby with poison 
      (if (or (not (equal (first S) (third S))) (equal (second S) (fourth S)))
        NIL
        ; same as before, whichever one moves due to successful end state, we flip the bits
        (list (list (not (first S)) (second S) (not (third S)) (fourth S)))
      )
    )
    ((equal A 'p) ; when we move homer and the poison
      ; when homer and poison move, 2 errors possible: homer not on same side as poison, and baby with dog
      (if (or (not (equal (first S) (fourth S))) (equal (second S) (third S)))
        NIL
        ; homer and poison switch sides together
        (list (list (not (first S)) (second S) (third S) (not (fourth S))))
      )
    )
  )
)
; (write (NEXT-STATE '(T T NIL T) 'p))

; SUCC-FN returns all of the possible legal successor states to the current
; state. It takes a single argument (s), which encodes the current state, and
; returns a list of each state that can be reached by applying legal operators
; to the current state.
(defun SUCC-FN (S)
  ; all of the following 4 moves are possible. so we stick them in one big list
  (append (NEXT-STATE S 'h) (NEXT-STATE S 'b) (NEXT-STATE S 'd) (NEXT-STATE S 'p))
)
; (write (SUCC-FN '(T T NIL T)))

; ON-PATH checks whether the current state is on the stack of states visited by
; this depth-first search. It takes two arguments: the current state (S) and the
; stack of states visited by DFS (STATES). It returns T if s is a member of
; states and NIL otherwise.
(defun ON-PATH (S STATES)
  (cond ((NULL STATES) nil) ; if no states left, return nil
    ((equal (car STATES) S) T) ; otherwise if first element of states matches S, return true found
    (t (ON-PATH S (cdr STATES))) ; recurse with the rest of the elements (aside from the first)
  )
)
; (write (ON-PATH '(T T T T) '((T T T T) (T T T NIL) (T T NIL NIL))))

; MULT-DFS is a helper function for DFS. It takes two arguments: a list of
; states from the initial state to the current state (PATH), and the legal
; successor states to the last, current state in the PATH (STATES). PATH is a
; first-in first-out list of states; that is, the first element is the initial
; state for the current search and the last element is the most recent state
; explored. MULT-DFS does a depth-first search on each element of STATES in
; turn. If any of those searches reaches the final state, MULT-DFS returns the
; complete path from the initial state to the goal state. Otherwise, it returns
; NIL.
(defun MULT-DFS (STATES PATH)
  (cond ((NULL STATES) nil) ; 
    ; try dfs'ing from the first state in the queue with the path
    ; if that doesn't work, there is no path so we check the rest of the states and recurse
    ((NULL (DFS (car STATES) PATH)) (MULT-DFS (cdr STATES) PATH))
    (t (DFS (car STATES) PATH)) ; check if the current element of STATES has reached final state
  )
)

; DFS does a depth first search from a given state to the goal state. It
; takes two arguments: a state (S) and the path from the initial state to S
; (PATH). If S is the initial state in our search, PATH is set to NIL. DFS
; performs a depth-first search starting at the given state. It returns the path
; from the initial state to the goal state, if any, or NIL otherwise. DFS is
; responsible for checking if S is already the goal state, as well as for
; ensuring that the depth-first search does not revisit a node already on the
; search path.
(defun DFS (S PATH)
  (cond ((FINAL-STATE S) (append PATH (list S))) ; end case: S is goal state, so append to path and return
    ((ON-PATH S PATH) nil) ; make sure we don't revisit a state that has already been visited
    (t (MULT-DFS (SUCC-FN S) (append PATH (list S)))) ; call helper function to dfs from current state
  )
)
; (write (DFS '(NIL NIL NIL NIL) NIL))






