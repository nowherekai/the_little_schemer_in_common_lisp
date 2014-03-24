;
; Chapter 1 of The Little Schemer:
; Toys
;
; common lisp is little different with secheme
;
;atom  list  S-expression  car cdr  cons
;*The Law of Cdr The primitive cdr is defined only for non-empty lists. The cdr of any nonempty list is always another list.
(set l '((b) (x y) ((c)))
(cdr (cdr l)) ; (((c)))   **

; *cons takes two arguments: the first one is any S-expression; the second one is any list.
; *cons insert the first argument in the second list as its(the new list) first elemetn.



