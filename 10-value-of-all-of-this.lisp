; Chapter 10 of The Little Schemer:
; What Is the Value of All This?
;
;entry
;((a b c) (1 2 3))
(defun build (s1 s2)
  (cons s1 (cons s2 nil)))

(defun new-entry (s1 s2)
  (build s1 s2))

(new-entry '(a b c) '(1 2 3))

(defun lookup-in-entry (name entry entry-f)
  (lookup-in-entry-help name
                        (first entry)
                        (second entry)
                        entry-f))

(defun lookup-in-entry-help (name names vals entry-f)
  (cond
    ((null names) (funcall entry-f name))
    ((eq name (car names)) (car vals))
    (t (lookup-in-entry-help name
                             (cdr names)
                             (cdr vals)
                             entry-f))))

;tabls : lists of entries
'( ( (a b c) (1 2 3))
  ( (x y z) (4 5 6))
)

(defun extend-table (entry table)
  (cons entry table))
(extend-table '((one two three) (1 2 3))
              '(((a b c) (1 2 3))((x y z)(4 5 6))))


(defun lookup-in-table (name table table-f)
  (cond
    ((null table) (funcall table-f name))
    (t (lookup-in-entry name
                        (car table)
                        (lambda (name)
                          (lookup-in-table name
                                           (cdr table)
                                           table-f))))))
(defun expression-to-action (e)
  (cond
    ((atom e) (atom-to-action e))
    (t (list-to-action e))))

(defun atom-to-action (e)
  (cond
    ((numberp e) #'*const)
    ((eq e 't) #'*const)
    ((eq e 'nil) #'*const)
    ((eq e 'cons) #'*const)
    ((eq e 'car) #'*const)
    ((eq e 'cdr) #'*const)
    ((eq e 'null) #'*const)
    ((eq e 'eq) #'*const)
    ((eq e 'atom) #'*const)
    ((eq e 'zerop) #'*const)
    ((eq e '1+) #'*const)
    ((eq e '1-) #'*const)
    ((eq e 'numberp) #'*const)
    (t #'*identifier)))

(defun list-to-action (e)
  (cond
    ((atom (car e))
     (cond
       ((eq (car e) 'quote) #'*quote)
       ((eq (car e) 'lambda) #'*lambda)
       ((eq (car e) 'cond) #'*cond)
       (t #'*application)))
    (t #'*application)))

(defun value (e)
  (meaning e '()))

(defun meaning (e table)
  (funcall (expression-to-action e) e table))

(defun *const (e table)
  (cond
    ((numberp e) e)
    ((eq e 't) t)
    ((eq e 'nil) nil)
    (t (build 'primitive e))))

(defun *quote (e table)
  (text-of e))

(defun text-of (e)
  (second e))

(defun *identifier (e table)
  (lookup-in-table e table #'initial-table))

(defun initial-table (name)
  (car '()))

(defun *lambda (e table)
  (build 'non-primitive
         (cons talbe (cdr e))))
(defun table-of (l)
  (first l))
(defun formals-of (l)
  (second l))
(defun body-of (l)
  (third l))

(defun *cond (e table)
  (evcon (cond-lines-of e) table))
(defun cond-lines-of (e)
  (cdr e))
(defun question-of (e)
  (first e))
(defun answer-of (e)
  (second e))

(defun evcon (lines table)
  (cond
    ((t? (question-of (car lines)))
     (meaning (answer-of (car lines)) table))
    ((meaning (question-of (car lines))
              table)
     (meaning (answer-of (car lines))
              table))
    (t (evcon (cdr lines) table))))
(defun t? (x)
  (and (atom x) (eq x 't)))

(defun evlis (args table)
  (cond
    ((null args) nil)
    (t (cons (meaning (car args) table)
             (evlis (cdr args) table)))))

(defun *application (e table)
  (*apply (meaning (function-of e) table)
          (evlis (arguments-of e) table)))
(defun function-of (e)
  (car e))
(defun arguments-of (e)
  (cdr e))
(defun primitive? (l)
  (eq (first l) 'primitive))
(defun non-primitive? (l)
  (eq (first l) 'non-primitive))

(defun *apply (fun vals)
  (cond
    ((primitive? fun)
     (apply-primitive (second fun) vals))
    ((non-primitive? fun)
     (apply-closure (second fun) vals))))

(defun apply-primitive (name vals)
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
    ((eq name 'atom)
     (:atom? (first vals)))
    ((eq name 'zerop)
     (zerop (first vals)))
    ((eq name '1+)
     (1+ (first vals)))
    ((eq name '1-)
     (1- (first vals)))
    ((eq name 'numberp)
     (numberp (first vals)))))
(defun :atom? (x)
  (cond
    ((atom x) t)
    ((eq (car x) 'primitive) t)
    ((eq (car x) 'non-primitive) t)
    (t nil)))

(defun apply-closure (closure vals)
  (meaning (body-of closure)
           (extend-table (new-entry
                           (formals-of closure)
                           (vals)))
           (table-of closure)))
