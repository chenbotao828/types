(defun % (x y)
  (check "%" (list x num? y num?)) 
  (rem x y)
  )

(defun ** (x y)
  (check "**" (list x num? y num?)) 
  (expt x y)
  )

;; normal division
(defun // (x y)
  (check "//" (list x num? y num?)) 
  (/ (+ x 0.0) y)
  )

(defun == (x y)
  (cond 
    ((= x y) T)
    ((eq x y) T)
    ((if (and (numberp x) (numberp y))
         (equal x y 0.0000000000000001)
         (equal x y))
     T)
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

(defun += (x y) (check "+=" (list x num? y num?)) (?= + x y))

(defun -= (x y) (check "-=" (list x num? y num?)) (?= - x y))

(defun *= (x y) (check "*=" (list x num? y num?)) (?= * x y))

(defun //= (x y) (check "//=" (list x num? y num?)) (?= // x y))

(defun %= (x y) (check "%=" (list x num? y num?)) (?= % x y))

(defun **= (x y) (check "**=" (list x num? y num?)) (?= ** x y))

(defun _/= (x y) (check "_/=" (list x num? y num?)) (?= / x y))

(set '& logand)

(set '| logior)

(set '^ (lambda (x y)  (Boole 6 x y)))

;; ~ -> builtin

(set '<< (lambda (num dig) (lsh num dig)))

(set '>> (lambda (num dig) (lsh num (- 0 dig))))

;; and or not -> builtin

(defun in (x y)
  (if (str? y)
      (progn
        (check "in" (list x str? y str?))
        (if (vl-string-search x y) t nil))
      (progn
        (check "in" (list y (list str? list?)))
        (if (member x y) t nil))
      )
  )

(defun not_in (x y)
  (if (str? y)
      (progn
        (check "not_in" (list x str? y str?))
        (not (if (vl-string-search x y) t nil)))
      (progn
        (check "not_in" (list y (list str? list?)))
        (not (if (member x y) t nil)))
      )
  )
