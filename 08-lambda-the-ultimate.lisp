;
; Chapter 8 of The Little Schemer:
; Lambda the Ultimate

(defun rember-f (test? a l)
  (cond
    ((null l) nil)
    ((funcall test? a (car l)) (cdr l))
    (t (cons (car l)
             (rember-f test? a (cdr l))))))

(defun eq?-c (a)
  (lambda (x)
    (eq x a)))

(setf eq?-salad (eq?-c 'salad))


;rewirte rember-f
(defun rember-f (test?)
  (lambda (a l)
    (cond
      ((null l) nil)
      ((funcall test? a (car l)) (cdr l))
      (t (cons (car l)
               (funcall (rember-f test?) a (cdr l)))))))

(setf rember-eq? (rember-f #'eq))
(funcall rember-eq? 1 '(1 2 3))


(defun insertL-f (test?)
  (lambda (new old l)
    (cond
      ((null l) nil)
      ((funcall test? old (car l))
       (cons new (cons old (cdr l))))
      (t (cons (car l)
               (funcall (insertL-f test?) new old (cdr l)))))))

(defun insertR-f (test?)
  (lambda (new old l)
    (cond
      ((null l) nil)
      ((funcall test? old (car l))
       (cons old (cons new (cdr l))))
      (t (cons (car l)
               (funcall (insertR-f test?) new old (cdr l)))))))

(defun seqL (new old l)
  (cons new (cons old l )))

(defun seqR (new old l)
  (cons old (cons new l)))

(defun insert-g (seq)
  (lambda (new old l)
    (cond
      ((null l) nil)
      ((eq (car l) old)
       (funcall seq new old (cdr l)))
      (t (cons (car l)
               (funcall (insert-g seq) new old (cdr l)))))))

(setf insertL (insert-g #'seqL))
(setf insertR (insert-g #'seqR))
(setf insertL (insert-g #'(lambda (new old l)
                            (cons (new (cons old l))))))

(defun seqS (new old l)
  (cons new l))

(setf subst. (insert-g #'seqS))

(defun atom-to-function (x)
  (cond
    ((eq x '+) #'+)
    ((eq x '*) #'*)
    ((eq x ^) #'expt)))

(defun value (nexp)
  (cond
    ((atom nexp) nexp)
    (t (funcall (atom-to-function (operator nexp))
                (value (1st-sub-exp nexp))
                (value (2nd-sub-exp nexp))))))

(defun multirember-f (test?)
  (lambda (a lat)
    ((null lat) nil)
    ((funcall test? a (car lat))
     (funcall (multirember-f test?) a (cdr lat)))
    (t (cons (car lat) (funcall (multirember-f test?) a (cdr lat))))))

(setf multirember-eq? (multirember-f #'eq))
(funcall multirember-eq? '1 '(1 2 1 3))

(setf eq?-tuna (eq?-c 'tuna))
(defun multiremberT (test? lat)
  (cond
    ((null lat) nil)
    ((test? (car lat))
     (multirember-f test? (cdr lat)))
    (t (cons (car lat)
             (multiremberT test? (car lat))))))


(defun multirember&co (a lat col)
  (cond
    ((null lat)
     (funcall col nil nil))
    ((eq a (car lat))
     (let ((new-col #'(lambda (newlat seen)
                        (funcall col newlat (cons (car lat) seen)))))
       (multirember&co a (cdr lat) new-col)))

    (t (let ((new-col #'(lambda (newlat seen)
                          (funcall col (cons (car lat) newlat) seen))))
         (multirember&co a (cdr lat) new-col)))))

(setf a-friend #'(lambda (x y) (print x) (print y) nil))

(multirember&co 1 '(1 2 1 3) a-friend)


(defun multiinsertL (new old lat)
  (cond
    ((null lat) nil)
    ((eq (car lat) old)
     (cons new (cons old (multiinsertL new old (cdr lat)))))
    (t (cons (car lat)
             (multiinsertL new old (cdr lat))))))

(defun multiinsertR (new old lat)
  (cond
    ((null lat) nil)
    ((eq (car lat) old)
     (cons old (cons new (multiinsertL new old (cdr lat)))))
    (t (cons (car lat)
             (multiinsertR new old (cdr lat))))))
(defun multiinsertLR (new oldL oldR lat)
  (cond
    ((null lat) nil)
    ((eq (car lat) oldL)
     (cons new (cons old (multiinsertL new oldL oldR (cdr lat)))))
    ((eq car lat) oldR)
     (cons old (cons new (multiinsertL new oldL oldR (cdr lat)))))
  (t (cons (car lat)
             (multiinsertLR new oldL oldR (cdr lat)))))

;?
(defun multiinsertLR&co (new oldL oldR lat col)
  (cond
    ((null lat) 
     (funcall col '(nil) 0 0))
    ((eq (car lat) oldL)
     (multiinsertLR&co new oldL oldR (cdr lat)
                       #'(lambda (newlat L R)
                           (funcall col (cons new (cons oldL  newlat) (1+ L) R)))
    ((eq (car lat) oldR)
     (multiinsertLR&co new oldL oldR (cdr lat)
                       #'(lambda (newlat L R)
                           (funcall col (cons new (cons oldR newlat) L (1+ R))))
    (t (multiinsertLR&co new oldL oldR (cdr lat)
                         #'(lambda (newlat L R)
                             (funcall col (cons (car lat) newlat) L R)))))

(multiinsertLRfJco 'salty 'fish 'chips '(chips and fish or fish and chips) col)


(defun evens-only* (l)
  (cond
    ((null l) nil)
    ((atom (car l))
     (cond
       ((evenp (car l))
        (cons (car l) (evens-only* (cdr l))))
       (t (evens-only* (cdr l)))))
    (t (cons (evens-only* (car l))
             (evens-only* (cdr l))))))

;?
(defun evens-only&co (l col)
  (cond
    ((null l)
     (funcall col nil 0 1))
    ((atom (car l))
     (cond
       ((evenp (car l))
        (evens-only&co (cdr l) #'(lambda (newlat sum product)
                                   (funcall col (cons (car l) newlat) sum (* (car l) product))))
        (t (evens-only*co (cdr l) #'(lambda (newlat sum product)
                                      (funcall col newlat (+ (car l) sum) product)))))))
    (t (evens-only&co (car l) #'(lambda (al as ap)
                                  (evens-only&co (cdr l)  #'(lambda (dl ds dp)
                                                              (funcall col (cons al dl)
                                                                       (+ as ds)
                                                                       (* ap dp)))))))))

