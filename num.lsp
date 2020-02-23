(defun range (from to step / ret)
  (check "range" (list from num? to num? step num?))
  (setq ret nil)
  (cond ((= step 0) (setq ret (list from)))
        ((> step 0) 
         (while (> to from)
                (setq ret (consend from ret))
                (setq from (+ from step))))
        ((< step 0) 
         (while (< to from)
                (setq ret (consend from ret))
                (setq from (+ from step)))))
  ret
  )


(defun round ( n )
  (check "round" (list n num?))
  (fix (+ n (if (minusp n) -0.5 0.5)))
  )


(defun random ( / a c m)
  (setq m   4294967296.0
        a   1664525.0
        c   1013904223.0
        {random_x} (rem (+ c (* a (cond ({random_x}) ((getvar 'date))))) m)
        )
  (/ {random_x} m)
  )

(defun random_range (a b)
  (check "random_range" (list a int? b int?))
  (+ (min a b) (fix (* (random) (1+ (abs (- a b))))))
  )

(defun math_floor (x)
  (check "math_floor" (list x num?))
  (fix x)
  )

(defun math_ceil (x)
  (check "math_ceil" (list x num?))
  (if (< x 0)
      (+ -1 (fix x))
      (+ 1 (fix x))
      )
  )

;; exp

(defun math_exp (x)
  (check "exp" (list x num?))
  (exp x)
  )


(setq math_e (exp 1))

(defun log10 (x)
  (check "log10" (list x num?))
  (/ (log x) (log 10))
  )

(defun math_sin(x)
  (check "math_sin" (list x num?))
  (sin x)
  )


(defun math_cos(x)
  (check "math_cos" (list x num?))
  (cos x)
  )


(defun math_tan(x)
  (check "math_tan" (list x num?))
  (/ (sin x) (cos x))
  )

(defun math_asin (x)
  (check "math_asin" (list x num?))
  (cond
    ((<= -1.0 x 1.0)
     (atan x (sqrt (- 1.0 (* x x)))))
    (t 
      (progn
        (*error* (strcat "Error in math_asin, x not between 1 and -1: " (str x)))
        nil ))))

(defun math_acos (x)
  (check "math_acos" (list x num?))
  (cond
    ((<= -1.0 x 1.0)
     (atan (sqrt (- 1.0 (* x x))) x))
    (t 
      (progn
        (*error* (strcat "Error in math_acos, x not between 1 and -1: " (str x)))
        nil ))) 
  )

(defun math_atan (x)
  (check "math_atan" (list x num?))
  (atan x)
  )

(defun math_degree (x)
  (check "math_degree" (list x num?))
  (* 180 (/ x pi))
  )

(defun math_radians (x)
  (check "math_radians" (list x num?))
  (* pi (/ x 180.0))
  )
;; builtin max min sqrt

(defun math_divmod (x y / r d)
  (check "math_divmod" (list x num? y num?))
  (setq r (rem x y))
  (cond ((and (> y 0) (< r 0)) (setq r (+ r y)))
        ((and (< y 0) (> r 0)) (setq r (- r (abs y)))) 
        )
  (setq d ( / (- x r) y))
  (list d r)
  )

(defun 2+int? (x)
    (if (and (= (type x) 'int) (>= x 2)) t nil) )

(defun dec2base ( n b)
  (check "dec2base" (list n int? b 2+int?))
  (if (< n b)
      (chr (+ n (if (< n 10) 48 55)))
      (strcat (dec2base (/ n b) b) (dec2base (rem n b) b))
      )
  )

(defun base2dec ( n b)
  (check "base2dec" (list n str?  b 2+int?))
  ((lambda ( f ) (f (mapcar '(lambda ( x ) (- x (if (< x 65) 48 55))) (reverse (vl-string->list n)))))
   (lambda ( c ) (if c (+ (* b (f (cdr c))) (car c)) 0))
   )
  )

(defun base2base ( n b1 b2 )
  (check "base2base" (list n str?  b1 2+int? b2 2+int?))
  (dec2base (base2dec n b1) b2)
  )
