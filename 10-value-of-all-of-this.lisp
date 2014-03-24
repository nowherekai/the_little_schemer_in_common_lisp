; Chapter 10 of The Little Schemer:
; What Is the Value of All This?
;
(defun build (first second)
  (cons first (cons second nil)))
(setf new-entry  #'build)

(funcall new-entry 1 2)

(defun lookup-in-entry (name entry entry-f)
  (lookup-in-entry-help name
                        (first entry)
                        (second entry)
                        entry-f))

(defun lookup-in-entry-help (name names values entry-f)
  (cond
    ((null names) (funcall entry-f name))
    ((eq (car names) name)
     (car values))
    (t (lookup-in-entry-help name 
                             (cdr names)
                             (cdr values)
                             entry-f))))

(lookup-in-entry 'entree '((appetizer entree beverage) 
                           (food tastes good)) #'print)


(setf extend-table #'cons)

(funcall extend-table '((1 2) (a b)) '(((appetizer entree beverage)
                                        (pate boeuf vin))
                                       ((beverage dessert)
                                        ((food is) (number one with us)))))


(defun lookup-in-table (name table table-f)
  (cond
    ((null table) (funcall table-f name))
    (t (lookup-in-entry name
                        (car table)
                        (lambda (name)
                          (lookup-in-table name
                                           (cdr table)
                                           table-f))))))


(cons 'car
      (cons (cons 'quote
                  (cons 
                    (cons 'a
                          (cons 'b
                                (cons 'c
                                      (quote ())))) 
                    (quote ())))
            (quote ()))) 


(defun atom-to-action (e)
  (cond
    ((numberp e) '*cosnt)
    ((eq e 't) '*cosnt)
    ((eq e 'nil) '*cosnt)
    ((eq e 'cons) '*cosnt)
    ((eq e 'car) '*cosnt)
    ((eq e 'cdr) '*cosnt)
    ((eq e 'null) '*cosnt)
    ((eq e 'eq) '*cosnt)
    ((eq e 'atom) '*cosnt)
    ((eq e 'zerop) '*cosnt)
    ((eq e '1+) '*cosnt)
    ((eq e '1-) '*const)
    ((eq e 'numberp) '*const)
    ((eq e 'function) '*const)
    ((eq e 'funcall) '*const)
    (t '*identifier)))

(atom-to-action 'car)

(defun list-to-action (e)
  (cond
    ((atom (car e))
     (cond
       ((eq (car e) 'quote) '*quote)
       ((eq (car e) 'lambda) '*lambda)
       ((eq (car e) 'cond) '*cond)
       (t '*application)))
    (t '*application)))

(list-to-action '(quote car))

(defun expression-to-action (e)
  (cond
    ((atom e) (atom-to-action e))
    (t (list-to-action e))))

(defun value (e)
  (meaning e (quote ())))

(defun meaning (e table)
  (funcall (expression-to-action e) e table))

(defun *const (e table)
  (cond
    ((numberp e) e)
    ((eq e t) t)
    ((eq e nil) nil)
    (t (build (quote primitive) e))))

(defun text-of (e)
  (second e))
(defun *quote (e table)
  (text-of e))

(defun *identifier (e table)
  (lookup-in-table e table initial-table))

(defun initial-table (name)
  (car (quote ())))

(defun *lambda (e table)
  (build (quote non-primitive) (cons table (cdr e))))

(meaning '(lambda (x) (cons x y)) '(((y z) ((8) 9))))

(defun table-of (l)
  (first l))
(defun formals-of (l)
  (second l))
(defun body-of (l)
  (third l))

(defun evcon (lines table)
  (cond
    ((meaning (question-of (car lines)) table)
     (meaning (answer-of (car lines)) table))
    (t (evcon (cdr lines) table))))

(defun question-of (l)
  (first l))
(defun answer-of (l)
  (second l))

(defun *cond (e table)
  (evcon (cond-lines-of e) table))
(defun cond-lines-of (e)
  (cdr e))

;not ok
(*cond '(cond (coffee klatsch) (else party)) '(((coffee) (t)) ((klatsch party) (5 (6)))))


(defun evlis (args table)
  (cond
    ((null args) nil)
    (t (cons (meaning (car args) table)
             (evlis (cdr args) table)))))

(defun *application (e table)
  (apply.
    (meaning (function-of e) table)
    (meaning (arguments-of e) table)))
(defun function-of (e)
  (car e))
(defun arguments-of (e)
  (cdr e))
(defun primitive? (l)
  (eq (first l) 'primitive))
(defun non-primitive? (l)
  (eq (first l) 'non-primitive))

(defun apply. (fun vals)
  (cond
    ((primitive? fun)
     (apply-primitive (second fun) vals))
    ((no-primitive? fun)
     (apply-closure (second fun) vals))))

(defun  apply-primitive (name vals)
  (cond
    ((eq name 'cons)
     (cons (first vals) (second vals)))
    ((eq name 'car)
     (car (first vals)))
    ((eq name 'cdr)
     (cdr (first vals)))
    ((eq name 'null)
     (null (first vals)))
    ((eq name 'eq)
     (eq (first vals) (second vals)))
    ((eq name ':atom?)
     (:atom? (first vals)))
    ((eq name 'zerop)
     (zerop (first vals)))
    ((eq name '1+)
     (1+ (first vals)))
    ((eq name '1-)
     (1- (first vals)))
    ((eq name numberp)
     (numberp (first vals)))))
(defun :atom? (x)
  (cond
    ((atom x) t)
    ((null x) nil)
    ((eq (car x) 'primitive) t)
    ((eq (car x) 'non-primitive t))))




