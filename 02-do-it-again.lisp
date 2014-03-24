;
; Chapter 2 of The Little Schemer:
; Do It, Do It Again, and Again, and Again ...
;
;use car cdr cons null atom and eq
; the last can omit, for cond will return nil in common lisp
; in common lisp  t is true  and nil is false
(defun lat? (lst)
  (cond
    ((null lst) t)
    ((atom (car lst)) (lat? (cdr lst)))
    (t nil)))
;
(lat '(bacon and eggs))  ;t
(lat '(bacon (switch or eggs)))  ;nil

(defun member? (a lat)
  (cond
    ((null lat) nil)
    (t (or (eq a (car lat)) (member? a (cdr lat))))))
;
(member?  'meat '(mashed potatoes and meat gravy)) ;t
(member?  'meat '(mashed potatoes and egg gravy)) ;nil
(member?  'meat '(mashed potatoes and (meat egg) gravy)) ;nil




