;
; Chapter 4 of The Little Schemer:
; Numbers Games


;use 1+ 1- to write +.
;we can treat zero? like null? since zerop asks if a number is empty and null  asks if a list is empty.
;cons builds lists and l+ builds numbers.


(defun +. (n m)
  (cond
    ((zerop m) n)
    (t (1+ (+. n (1- m)))))) 
(+. 100 200)  ;300

(defun -. (n m)
  (cond
    ((zerop m) n)
    (t (1- (-. n (1- m))))))
(-. 400 222)  ;178


(defun addup (tup)
  (cond
    ((null tup) 0)
    (t (+. (car tup) (addup (cdr tup))))))
(addup '(1 2 3))  ;6

(defun *. (n m)
  (cond
    ((zerop m) 0)
    (t (+. n (*. n (1- m))))))
(*. 2 4)  ;8

(defun tup+ (tup1 tup2)
  (cond
    ((null tup1) tup2)
    ((null tup2) tup1)
    (t (cons (+ (car tup1) (car tup2))
             (tup+ (cdr tup1) (cdr tup2))))))
(tup+ '(1 2) '(3 4)) ;(4 6)

;write the function zerop and 1-

(defun >. (n m)
  (cond
    ((zerop n) nil)
    ((zerop m) t)
    (t (>. (1- n) (1- m)))))
(>. 3 2);true
(>. 3 3);nil
(>. 2 3);nil

(defun <. (n m)
  (cond
    ((zerop m) nil)
    ((zerop n) t)
    (t (<. (1- n) (1- m)))))

(defun =. (n m)
  (cond
    ((zerop n) (zerop m))
    ((zerop m) nil)
    (t (=. (1- n) (1- m)))))
(defun =. (n m)
  (cond
    ((or (<. n m) (>. n m)) nil)
    (t t)))


(defun ^ (n m)
  (cond
    ((zerop m) 1)
    (t (* n (^ n (1- m))))))

(defun o/ (n m)
  (cond
    ((< n m) 0)
    (t (1+ (o/ (- n m) m)))))

(defun len (lat)
  (cond
    ((null lat) 0)
    (t (1+ (len (cdr lat))))))

(defun pick (n lat)
  (cond
    ((= 1 n) (car lat))
    (t (pick (1- n) (cdr lat)))))

(defun rempick (n lat)
  (cond
    ((= 1 n) (cdr lat))
    (t (cons (car lat) (rempick (1- n) (cdr lat))))))

(defun no-nums (lat)
  (cond
    ((null lat) nil)
    ((numberp (car lat)
              (numberp (cdr lat))))
    (t (cons (car lat) (numberp (cdr lat))))))

(defun all-nums (lat)
  (cond
    ((null lat) nil)
    ((numberp (car lat)
              (cons (car lat) (all-nums (cdr lat)))))
    (t (all-nums (cdr lat)))))

(defun occur (a lat)
  (cond
    ((null lat) 0)
    ((eq a (car lat))
     (1+ (occur a (cdr lat))))
    (t (occur a (cdr lat)))))

(defun one? (n)
  (= 1 n))


(defun rempick (n lat)
  (cond
    ((one? n) (cdr lat))
    (t (cons (car lat)
             (rempick (cdr lat))))))



