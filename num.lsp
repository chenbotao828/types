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

(defun 2->64_int? (x)
    (if (and (= (type x) 'int) (<= 2 x 64)) t nil) )

(defun dec2base (n b / dec2str)
  (check "dec2base" (list n num? b 2->64_int?))
  (defun dec2str (x)
    (cond ((<= 0 x 9) (chr (+ 48 x)))
          ((<= 10 x 35) (chr (+ 87 x)))
          ((<= 36 x 61) (chr (+ 29 x)))
          ((= x 62) "+")
          ((= x 63) "/")
          (t "")
          )
    )
  (if (< n b)
      (dec2str n)
      (strcat (dec2base (/ n b) b) (dec2base (rem n b) b))
      )
  )

(defun base2dec (n b / ret p ascii2dec)
  (check "base2dec" (list n str?  b 2->64_int?))
  (defun ascii2dec (a)
    (cond ((<= 48 a 57) (- a 48))
          ((<= 97 a 122) (- a 87))
          ((<= 65 a 90) (- a 29))
          ((= a 43 ) 62)
          ((= a 47) 63)
          (t "")
          )
    )  
  (setq n (vl-string->list n) ret 0 p (- (length n) 1))
  (while n
         (setq ret (+ ret (* (ascii2dec (car n)) (expt b p)))
               n (cdr n)
               p (- p 1)))
  ret
  )

(defun base2base ( n b1 b2 )
  (check "base2base" (list n str?  b1 2->64_int? b2 2->64_int?))
  (dec2base (base2dec n b1) b2)
  )
