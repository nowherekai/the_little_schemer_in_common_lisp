;
; Chapter 9 of The Little Schemer:
; ...and Again, and Again, and Again, ...
;
;

(defun looking (a lat)
  (keep-looking a (pick 1 lat) lat))

(looking 'caviar '(6 2 4 caviar 5 7 3))

(defun keep-looking (a ele lat)
  (cond
    ((numberp ele)
     (keep-looking a (pick ele lat) lat))
    (t (eq ele a))))

(keep-looking 'caviar 1 '(6 2 4 caviar 5 7 3))

; (pick 2 '(1 2 ...)) will dead loop 
(defun pick (n lat)
  (cond
    ((null lat) nil)
    ((zerop (1- n)) (car lat))
    (t (pick (1- n) (cdr lat)))))
(pick 1 '(1 2 3))


(defun eternity (x)
  (eternity x))

; 
(defun first. (pair)
  (car pair))

(defun second. (pair)
  (cadr pair))

(defun build (first second)
  (cons first (cons second nil)))

(defun a-pair? (x)
  (cond
    ((null x) nil)
    ((atom x) nil)
    ((null (cdr x)) nil)
    ((null (cddr x)) t)
    (t nil)))

(defun shift (pair)
  (build (first (first pair))
         (build (second (first pair))
                (second pair))))

(defun align (pora)
  (cond
    ((atom pora) pora)
    ((a-pair? (first pora))
     (align (shift pora)))
    (t (build (first pora)
              (align (second pora))))))

(defun length* (pora)
  (cond
    ((atom pora) 1)
    (t (+ (length* (first pora))
          (length* (second pora))))))

(defun weight* (pora)
  (cond
    ((atom pora) 1)
    (t (+ (* (weight* (first pora)) 2)
          (weight* (second pora))))))

(defun revpair (pair)
  (build (second pair) (first pair)))

(defun shuffle (pora)
  (cond
    ((atom pora) pora)
    ((a-pair? (first pora))
     (shuffle (revpair pora)))
    (t (build (first pora)
              (shuffle (second pora))))))

(defun C (n)
  (cond
    ((= n 1) 1)
    (t (cond
         ((evenp n) (C (/ n 2)))
         (t (C (1+ (* 3 n))))))))
; try (A 4 3) ... 时间复杂度是多少?
(defun A (n m)
  (cond
    ((zerop n) (1+ m))
    ((zerop m) (A (1- n) 1))
    (t (A (1- n)
          (A n (1- m))))))

(defun will-sop? (f)
  ...)

(defun last-try (x)
  (and (will-sop? #'last-try)
       (eternity x)))

;length0
(lambda (l)
  (cond
    ((null l) 0)
    (t (1+ (eternity (cdr l))))))
;length<=1
(lambda (l)
  (cond
    ((null l) 0)
    (t (1+ (funcall (lambda (l)
                      (cond
                        ((null l) 0)
                        (t (1+ (eternity (cdr l))))))
                    (cdr l))))))
;length0
((lambda (length)
   (lambda (l)
     (cond
       ((null l) 0)
       (t (1+ (funcall length (cdr l)))))))
 #'eternity)
(funcall .. nil)

;length<=1
((lambda (f)
   (lambda (l)
     (cond
       ((null l) 0)
       (t (1+ (funcall f (cdr l)))))))
 ((lambda (g)
    (lambda (l)
      (cond
        ((null l) 0)
        (t (1+ (funcall g (cdr l)))))))
  #'eternity))

;length<=2
((lambda (length)
   (lambda (l)
     (cond
       ((null l) 0)
       (t (1+ (funcall length (cdr l)))))))
   ((lambda (length)
      (lambda (l)
      (cond
        ((null l) 0)
        (t (1+ (funcall length (cdr l)))))))
   ((lambda (length)
      (lambda (l)
        (cond
          ((null l) 0)
          (t (1+ (funcall length (cdr l)))))))
    #'eternity)))

;mk-length
;length0
((lambda (mk-length)
   (funcall mk-length #'eternity))
 (lambda (length)
   (lambda (l)
     (cond
       ((null l) 0)
       (t (1+ (funcall length (cdr l))))))))

;length<=1
((lambda (mk-length)
   (funcall mk-length
            (funcall mk-length #'eternity)))
 (lambda (length)
   (lambda (l)
     (cond
       ((null l) 0)
       (t (1+ (funcall length (cdr l))))))))

;length<=2
((lambda (mk-length)
   (funcall mk-length
            (funcall mk-length 
                     (funcall mk-length #'eternity))))
 (lambda (length)
   (lambda (l)
     (cond
       ((null l) 0)
       (t (1+ (funcall length (cdr l))))))))


((lambda (mk-length)
   (funcall mk-length mk-length))
 (lambda (length)
   (lambda (l)
     (cond
       ((null l) 0)
       (t (1+ (funcall length (cdr l))))))))

((lambda (mk-length)
   (funcall mk-length mk-length))
 (lambda (length)
   (lambda (l)
     (cond
       ((null l) 0)
       (t (1+ (funcall mk-length (cdr l))))))))






