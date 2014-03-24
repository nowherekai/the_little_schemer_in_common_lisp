; Chapter 5 of The Little Schemer:

; *Oh My Gawd*: It's Full of Stars
(defun rember* (a l)
  (cond
    ((null l) nil)
    ((atom (car l)) 
     (cond 
       ((eq (car l) a)
        (rember* a (cdr l)))
       (t (cons (car l) (rember* a (cdr l))))))
    (t (cons (rember* a (car l))
             (rember* a (cdr l))))))

  (rember* 'sauce '(((tomato sauce))
                    ((bean) sauce)
                    (and ((flying)) sauce)))

(defun insertR* (new old l)
  (cond
    ((null l) nil)
    ((atom (car l))
     (cond
       ((eq (car l) old)
        (cons old (cons new (insertR* new old (cdr l)))))
       (t (cons (car l) (insertR* new old (cdr l))))))
    (t (cons (insertR* new old (car l))
             (insertR* new old (cdr l))))))
      
(insertR* 'roast 'chuck '((how much (wood))
                          could
                          ((a (wood) chuck)) 
                          (((chuck)))
                          (if (a) ((wood chuck))) 
                          could chuck wood))


(defun occur* (a l)
  (cond
    ((null l) 0)
    ((atom (car l))
     (cond
       ((eq a (car l))
        (1+ (occur* a (cdr l))))
       (t (occur* a (cdr l)))))
    (t (+ (occur* a (car l)) (occur* a (cdr l))))))

(occur* 'banana  '((banana) 
                   (split ((((banana ice)))
                           (cream (banana))
                           sherbet))
                   (banana) 
                   (bread) 
                   (banana brandy)))


(defun subst* (new old l)
  (cond
    ((null l) nil)
    ((atom (car l))
     (cond
       ((eq old (car l))
        (cons new (subst* new old (cdr l))))
       (t (cons (car l) (subst* new old (cdr l))))))
    (t (cons (subst* new old (car l))
             (subst* new old (cdr l))))))

(subst* 'orange 'banana '((banana)
                          (split ((((banana ice)))
                                  (cream (banana))
                                  sherbet))
                          (banana)
                          (bread)
                          (banana brandy)))

(defun insertL* (new old l)
  (cond
    ((null l) nil)
    ((atom (car l))
     (cond
       ((eq (car l) old)
        (cons new (insertL* new old (cdr l))))
       (t (cons (car l) (insertL* new old (cdr l))))))
    (t (cons (insertL* new old (car l)) (insertL* new old (cdr l))))))
(insertL* 'pecker 'chuck '((how much (wood))
                           could
                           ((a (wood) chuck))
                           (( (chuck)))
                           (if (a) ((wood chuck)))
                           could chuck wood))

(defun member* (a l)
  (cond
    ((null l) nil)
    ((atom (car l))
     (cond
       ((eq (car l) a) t)
       (t (member* a (cdr l)))))
     (t (or (member* a (car l)) (member* a (cdr l))))))
(member* 'chips '((potato) (chips ((with) fish) (chips))))

(defun leftmost (l)
  (cond
    ((atom l) l)
    (t (leftmost (car l)))))

(defun eqlist? (l1 l2)
  (cond
    ((and (null l1) (null l2)) t)
    ((and (null l1) (atom (car l2))) nil)
    ((null l1) nil)

    ((and (atom (car l1)) (null l2)) nil)
    ((and (atom (car l1)) (atom (car l2)))
     (and (eq (car l1) (car l2)) (eqlist? (cdr l1) (cdr l2))))
    (((atom (car l1))) nil)

    ((null l2) nil)
    ((atom (car l2)) nil)
    (t (and (eqlist? (car l1) (car l2))
            (eqlist? (cdr l1) (cdr l2))))))

(defun eqlist? (l1 l2)
  (cond
    ((and (null l1) (null l2)) t)
    ((or (null l1) (null l2)) nil)

    ((and (atom (car l1) (atom (car l2))))
     (and (eq (car l1) (car l2)) (eqlist? (cdr l1) (cdr l2))))

    ((or (atom (car l1)) (atom (car l2))) nil)

    (t (and (eqlist? (car l1) (car l2))
            (eqlist? (cdr l1) (cdr l2))))))



(defun equal? (s1 s2)
  (cond
    ((and (atom s1) (atom s2))
     (eq s2 s2))
    ((or (atom s1) (atom s2)) nil)

    (t (eqlist? s1 s2))))

;??
(defun eqlist? (l1 l2)
  (cond
    ((and (null l1) (null l2)) t)
    ((or (null l1) (null l2)) nil)

    (t (and (equal? (car l1) (car l2))
            (equal? (cdr l1) (cdr l2))))))

(defun rember (s l)
  (cond
    ((null l) nil)
    ((atom (car l))
     (cond
       ((equal? (car l) s) (cdr l))
       (t (cons (car l) (rember s (cdr l)))))
     (t (cond
          ((equal? (car l) s) (cdr l))
          (t (cons (car l) (rember s (cdr l)))))))))

(defun rember (s l)
  (cond
    ((null l) nil)
    (t (cond
         ((equal? (car l) s) (cdr l))
         (t (cons (car l) (rember s (cdr l))))))))
(defun rember (s l)
  (cond
    ((null l) nil)
    ((equal? (car l) s) (cdr l))
    (t (cons (car l) (rember s (cdr l))))))
               
