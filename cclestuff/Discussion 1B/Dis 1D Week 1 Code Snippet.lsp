(defun factorial (n)
  (if (< n 0)
      nil
    (if (= n 0)
        1
        (* n (factorial (- n 1))))))


(defun length_list (l)
  (if (not l)
      0
    (+ 1 (length_list (cdr l)))))

(defun find_k (l k)
  (if (= k 0)
      (car l)
    (find_k (cdr l) (- k 1))))

(defun delete_k (l k)
  (if (= k 0)
      (cdr l)
    (cons (car l) (delete_k (cdr l) (- k 1)))))


(defun min_tree (tree)
  (if (not (cadr tree))
      (car tree)
    (min_tree (cadr tree))))

(defun insert_l (l x)
  (if (not l)
      (list x)
    (if (< (car l) x)
        (cons (car l) (insert_l (cdr l) x))
      (cons x l))))

(defun sort (l)
  (if (not l)
      ()
    (insert_l (sort (cdr l)) (car l))))

(defun left_k (l k)
  (if (= k 0)
      ()
    (cons (car l) (left_k (cdr l) (- k 1)))))


(defun right_k (l k)
  (if (= k 0)
      (cdr l)
    (right_k (cdr l) (- k 1))))


(defun build_tree (l)
  (build_tree_help (sort l)))


(defun build_tree_help (l)
  (if (not l)
      ()
    (list (find_k l (floor (length_list l) 2))
          (build_tree_help (left_k l (floor (length_list l) 2)))
          (build_tree_help (right_k l (floor (length_list l) 2))))))
    
