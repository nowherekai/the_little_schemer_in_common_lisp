;
; Chapter 7 of The Little Schemer:
; Friends and Relations
;
;member? function from chapter 2
(defun member? (a lat)
  (cond
    ((null lat) nil)
    (t (or (eq a (car lat)) (member? a (cdr lat))))))
;
;

(defun set? (lat)
  (cond
    ((null lat) t)
    ((member? (car lat) (cdr lat)) nil)
    (t (set? (cdr lat)))))

(defun makeset (lat)
  (cond
    ((null lat) nil)
    ((member? (car lat) (cdr lat)) (makeset (cdr lat)))
    (t (cons (car lat) (makeset (cdr lat))))))
;rewrite makeset use multirember
(defun makeset (lat)
  (cond
    ((null lat) nil)
    (t (cons (car lat) (makeset (multirember (car lat) (cdr lat)))))))


(defun subset? (set1 set2)
  (cond
    ((null set1) t)
    ((member? (car set1) set2) (subset? (cdr set1) set2))))

(defun eqset? (set1 set2)
  (and (subset? set1 set2) (subset? set2 set1)))

;有相同的element
(defun intersect? (set1 set2)
  (cond 
    ((null set1) nil)
    ((member? (car set2) set2) t)
    (t (intersect? (cdr set1) set2))))

;return all same ele
(defun intersect (set1 set2)
  (cond
    ((null set1) nil)
    ((member? (car set1) set2)
     (cons (car set1) (intersect (cdr set1) set2)))
    (t (intersect (cdr set1) set2))))

; 合并 去掉重复的
(defun union (set1 set2)
  (cond
    ((null set1) set2)
    ((member? (car set1) set2)
     (union (cdr set1) set2))
    (t (cons (car set1) (union (cdr set1) set2)))))

; 
(defun set- (set1 set2)
  (cond
    ((null set1) nil)
    ((member? (car set1) set2)
     (set- (cdr set1) set2))
    (t (cons (car set1) (set- (cdr set1) set2)))))


(defun intersectall (l-set)
  (cond
    ((null (cdr l-set)) (car l-set))
    (t (intersect (car l-set)
                  (intersectall (cdr l-set))))))

; '(()) is not a empty list, it has one element '()
(defun a-pair? (x)
  (cond
    ((null x) nil)
    ((atom x) nil)
    ((null (cdr x)) nil)
    ((null (cddr x)) t)
    (t nil)))

(defun first (p)
  (car p))
(defun second (p)
  (cadr p))

(defun build (s1 s2)
  (cons s1 (cons s2 nil)))

(defun third (l)
  (caddr l))

;We use rel to stand for relation.
;firsts define in chapter 3
;"The function firsts takes one argument, a list, which is either a null list or contains only non-empty lists. It builds another list composed of the first S-expression of each internal list."
;(caar l) is abbr for (car (car l))
(defun firsts (l)
  (cond
    ((null l) nil)
    (t (cons (caar l) (firsts (cdr l))))))

(defun fun? (rel)
  (set? (firsts rel)))

(defun revrel (rel)
  (cond
    ((null rel) nil)
    ((cons (build (second (car rel)) (first (car rel)))
           (revrel (cdr rel))))))

(defun repair (p)
  (build (second p) (first p)))

(defun revrel (rel)
  (cond
    ((null rel) nil)
    ((cons (repair (car rel))
           (revrel (cdr rel))))))

;?
(defun fullfun? (fun)
  (and (fun? fun) (fun? (revrel fun))))









