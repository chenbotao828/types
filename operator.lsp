(defun % (x y)
  (check "%" (list x num? y num?)) 
  (rem x y)
  )

(defun ** (x y)
  (check "**" (list x num? y num?)) 
  (expt x y)
  )

(setq pow **)

;; normal division
(defun // (x y)
  (check "//" (list x num? y num?)) 
(/ (+ x 0.0) y)
  )

(defun == (x y / list==)
  (defun list== (x y / ret)
    (setq ret t)
    (cond ((/= (length x) (length y)) (setq ret nil))
          (while (and x ret)
                 (setq ret (and ret (== (car x) (car y)))
                       x (cdr x) y (cdr y))
                 ))
    ret
    )
  (defun al== (x y / ret fst)
    (setq ret t)
    (cond ((/= (length x) (length y)) (setq ret nil))
          (while (and x ret)
                 (setq fst (car x)
                       ret (and ret (== fst (assoc (caar x) y)))
                       x (cdr x))
                 ))
    ret
    )  
  (cond 
    ((= x y) t)
    ((eq x y) t)
    ((equal x y) t)
    ((if (and (num? x) (num? y))
         (equal x y 0.0000000000000001)
         (equal x y))
     T)
    ((and (list? x) (list? y))
     (if (and (al? x) (al? y))
       (al== x y)
       (list== x y)
       )
     )
    (t nil))
  )

(defun != (x y)
  (not (== x y))
  )

(defun ?= (operator x y)
  ;; x sym
  (if (not (sym? x))
      (*error* "?=, Not sym")
      )
  (if (not (and (num? (eval x)) (num? y)))
      (*error* "?=, Not number")
      )
  (set x (operator (eval x) y))
  )

(defun += (x y) (check "+=" (list x sym? y num?)) (?= + x y))

(defun -= (x y) (check "-=" (list x sym? y num?)) (?= - x y))

(defun *= (x y) (check "*=" (list x sym? y num?)) (?= * x y))

(defun //= (x y) (check "//=" (list x sym? y num?)) (?= // x y))

(defun %= (x y) (check "%=" (list x sym? y num?)) (?= % x y))

(defun **= (x y) (check "**=" (list x sym? y num?)) (?= ** x y))

(defun _/= (x y) (check "_/=" (list x sym? y num?)) (?= / x y))

(set '& logand)

(set '| logior)

(set '^ (lambda (x y)  (Boole 6 x y)))

;; ~ -> builtin

(set '<< (lambda (num dig) (lsh num dig)))

(set '>> (lambda (num dig) (lsh num (- 0 dig))))

;; and or not -> builtin

;; in in mix
;; not_in in mix

;; comparisons of all type
(defun <<< (x1 x2 / i2t t1 t2)
  (defun i2t (i)
    (dot
      (list (cons nil 0) (cons 'INT 1) (cons 'REAL 1) (cons 'STR 2) 
            (cons 'SYM 3) (cons 'SUBR 4) (cons 'USUBR 5) (cons 'ENAME 6)
            (cons 'PICKSET 7) (cons 'FILE 8) (cons 'PAGETB 9)
            (cons 'VARIANT 10) (cons 'SAFEARRAY 11) (cons 'EXRXSUBR 12)
            (cons 'VLA-object 13) (cons 'LIST 14)) 
      (type i))
    )
  (setq t1 (i2t x1) t2 (i2t x2))
  (cond ((/= t1 t2) (< t1 t2))
        ((member t1 (list 0 1 2)) (< x1 x2))
        ((= t1 14)
         (if (== (car x1) (car x2))
             (<<< (cdr x1) (cdr x2))
             (<<< (car x1) (car x2))))
        (t (< (str x1) (str x2)))
        )
  )

(defun >>> (x1 x2 / i2t t1 t2)
  (defun i2t (i)
    (dot
      (list (cons nil 0) (cons 'INT 1) (cons 'REAL 1) (cons 'STR 2) 
            (cons 'SYM 3) (cons 'SUBR 4) (cons 'USUBR 5) (cons 'ENAME 6)
            (cons 'PICKSET 7) (cons 'FILE 8) (cons 'PAGETB 9)
            (cons 'VARIANT 10) (cons 'SAFEARRAY 11) (cons 'EXRXSUBR 12)
            (cons 'VLA-object 13) (cons 'LIST 14)) 
      (type i))
    )
  (setq t1 (i2t x1) t2 (i2t x2))
  (cond ((/= t1 t2) (> t1 t2))
        ((member t1 (list 0 1 2)) (> x1 x2))
        ((= t1 14)
         (if (== (car x1) (car x2))
             (>>> (cdr x1) (cdr x2))
             (>>> (car x1) (car x2))))
        (t (> (str x1) (str x2)))
        )
  )
