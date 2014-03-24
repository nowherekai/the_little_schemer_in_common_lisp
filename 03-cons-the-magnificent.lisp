;
; Chapter 3 of The Little Schemer:
; Cons the Magnificent
;

;"Rember" stands for remove a member.
;remove the first member that is equal
(defun rember (a lat)
  (cond
    ((null lat) nil)
    ((eq a (car lat)) (cdr lat))
    (t (cons (car lat) (rember a (cdr lat))))))

(rember 'mint '(lamb chops and mint jelly)) ;(lamb chops and jelly)

;"The function firsts takes one argument, a list, which is either a null list or contains only non-empty lists. It builds another list composed of the first S-expression of each internal list."
;(caar l) is abbr for (car (car l))
(defun firsts (l)
  (cond
    ((null l) nil)
    (t (cons (caar l) (firsts (cdr l))))))

(firsts '(((five plums) four) (eleven green oranges) ((no) more)))

(defun insertR (new old lat)
  (cond
    ((null lat) nil)
    ((eq old (car lat))
     (cons old 
           (cons new 
                 (insertR new old (cdr lat)))))
    (t (cons (car lat) (insertR new old (cdr lat))))))

(insertR 'jalapeno 'and '(tacos tamales and salsa)) ; (tacos tamales and jalapeno salsa)

(defun insertL (new old lat)
  (cond
    ((null lat) nil)
    ((eq old (car lat))
     (cons new 
           (cons old
                 (insertL new old (cdr lat)))))
    (t (cons (car lat) (insertL new old (cdr lat))))))

(insertL 'jalapeno 'and '(tacos tamales and salsa)) ; (tacos tamales jalapeno and salsa)

(defun subst. (new old lat)
  (cond
    ((null lat) nil)
    ((eq old (car lat))
     (cons new 
           (subst. new old (cdr lat))))
    (t (cons (car lat) (subst. new old (cdr lat))))))

(subst. 'jalapeno 'and '(tacos tamales and salsa)) ; (tacos tamales jalapeno salsa)

    
(defun subst2 (new old1 old2 lat)
  (cond
    ((null lat) nil)
    ((eq old1 (car lat))
     (cons new 
           (subst2 new old1 old2 (cdr lat))))
    ((eq old2 (car lat))
     (cons new 
           (subst2 new old1 old2 (cdr lat))))
    (t (cons (car lat) (subst2 new old1 old2 (cdr lat))))))
(subst2 'jalapeno 'and 'or '(tacos or tamales and salsa)) ; (tacos jalapeno tamales jalapeno salsa)

(defun multirember (a lat)
  (cond
    ((null lat) nil)
    ((eq a (car lat))
     (multirember a (cdr lat)))
    (t (cons (car lat) 
             (multirember a (cdr lat))))))
(multirember 'cup '(coffee cup tea cup and hick cup))  ;(coffee tea and hick)

(defun multiinsertR  (new old lat)
  (cond
    ((null lat) nil)
    ((eq old (car lat))
     (cons old (cons new (multiinsertR new old (cdr lat)))))
    (t (cons (car lat) (multiinsertR new old (cdr lat))))))

(defun multiinsertL  (new old lat)
  (cond
    ((null lat) nil)
    ((eq old (car lat))
     (cons new (cons old (multiremberL new old (cdr lat)))))
    (t (cons (car lat) (multiremberL new old (cdr lat))))))

(defun multisubst (new old lat)
  (cond
    ((null lat) nil)
    ((eq old (car lat))
     (cons new (multisubst new old (cdr lat))))
    (t (cons (car lat) (multisubst new old (cdr lat))))))







