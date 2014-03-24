; Chapter 6 of The Little Schemer:
; Shadows
; For the purpose of this chapter, an arithmetic expression is either an atom (including numbers), or two arithmetic expressions combined by +, x, or ^.

(defun ^ (x n)
  (expt x n))

(defun numbered? (aexp)
  (cond
    ((numberp aexp) t)
    ((eq (operator aexp) '+ )
     (and (numbered? (car aexp)) (numbered? (caddr (aexp)))))
    ((eq (operator aexp) '* )
     (and (numbered? (car aexp)) (numbered? (caddr (aexp)))))
    ((eq (operator aexp) '^ )
     (and (numbered? (car aexp)) (numbered? (caddr (aexp)))))))

(defun operator (aexp)
  (cadr aexp))

(defun numbered? (aexp)
  (cond
    ((atom aexp) (numberp aexp))
    (t (and (numbered? (car aexp)) (numbered? (caddr aexp))))))

(defun value (nexp)
  (cond
    ((atom nexp) nexp)
    ((eq (operator nexp) '+) (+ (value (car nexp)) (value (caddr nexp))))
    ((eq (operator nexp) '*) (* (value (car nexp)) (value (caddr nexp))))
    ((eq (operator nexp) '*) (expt (value (car nexp)) (value (caddr nexp))))
    ))

;前缀表达式
(defun 1st-sub-exp (aexp)
  (cadr aexp))

(defun 2st-sub-exp (aexp)
  (caddr aexp))
(defun operator (aexp)
  (car aexp))

(defun value (nexp)
  (cond
    ((atom nexp) nexp)
    ((eq (operator nexp) '+) (+ (value (1st-sub-exp nexp)) (value (2st-sub-exp nexp))))
    ((eq (operator nexp) '*) (* (value (1st-sub-exp nexp)) (value (2st-sub-exp nexp))))
    ((eq (operator nexp) '^) (expt (value (1st-sub-exp nexp)) (value (2st-sub-exp nexp))))
    ))

;改成中缀形式，只需要改变help method就行
; vlaue不用变动
(defun 1st-sub-exp (aexp)
  (car aexp))
(defun operator (aexp)
  (cadr aexp))








