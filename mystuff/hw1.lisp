; HOMEWORK 1 LISP FILE (CONTAINS 3 PROBLEMS)
; (C) Nevin Liang Fall 2021
; notes: comment out lines after each function for test cases

; ==========================================================================

; PROBLEM 1
; SEQ calculates the Nth Tribonacci number
; input: integer N
; output: integer representing the Nth Tribonacci number
(defun SEQ (N)
  ; recursion: if base case then return 1
  (if (< n 3)
    1
    ; else, return the sum of the recursive formula
    (+ (SEQ (- N 1)) 
       (SEQ (- N 2)) 
       (SEQ (- N 3))
    )
  )
)
; just a test example
; (write (SEQ 8))

; ==========================================================================

; PROBLEM 2
; SUMS calculates the # of addition ops that SEQ needs 
; input: integer N
; output: integer representing the # of additions that SEQ(N) would compute
(defun SUMS (N)
  ; recursion: if base case (one of 0, 1, 2) no additions needed
  (if (< n 3)
    0
    ; else, return recurse and add 2 to the total # of additions
    (+ 2 
       (SUMS (- N 1))
       (SUMS (- N 2))
       (SUMS (- N 3))
    )
  )
)
; just a test example
; (write (SUMS 8))

; ==========================================================================

; PROBLEM 3
; Replaces all elements of a tree with the value 0
; input: tree list L
; output: exact same tree list structure but all elements are 0
(defun ANON (L)
  ; recursion: base cases are 1 or 0 elements left
  ; if 0 elements left return nil
  (cond ((not L) nil)
        ; if 1 element left then replace it with 0
        ((atom L) '0)
        ; otherwise split it into first element and last elements and recurse
        (t (cons (ANON (car L)) 
                 (ANON (cdr L))))
  )
)
; just a test example
; (write (ANON '((asdf) 1 ((L f) 3))))

; project-specs test cases
; (write (ANON '42))
; (write (ANON 'FOO))
; (write (ANON '(((L E) F) T)))
; (write (ANON '(5 FOO 3.1 -0.2)))
; (write (ANON '(1 (FOO 3.1) -0.2)))
; (write (ANON '(((1 2) (FOO 3.1)) (BAR -0.2))))
; (write (ANON '(R (I (G (H T))))))

