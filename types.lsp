;; Check types
(import "types/check")
;; operator
(import "types/operator")
;; List
(import "types/list")
(import "types/str")

;; num

(defun int(x)
  (cond ((num? x) (fix x))
        ((str? x) (atoi x))
        (t (*error* "int: not num or str"))
  )
 )

(defun float (x)
 (cond ((num? x) (+ x 0.0))
       ((str? x) (atof x))
       (t (*error* "float: not num or str"))
  )
 )

(defun range (from to step / ret)
  (cond
    ((and (num? from) (num? to) (num? step))
     (progn
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
    ))
    (t (*error* "range: not num"))
  )
 )

;; builtin max min sqrt

(defun round ( n )
    (fix (+ n (if (minusp n) -0.5 0.5)))
)

;; math, random packages

;; (defun random ( / a c m )
;;     (setq m   4294967296.0
;;           a   1664525.0
;;           c   1013904223.0
;;           x (rem (+ c (* a (cond (x) ((getvar 'date))))) m)
;;     )
;;     (/ x m)
;; )

;; (defun randrange ( a b )
;;     (+ (min a b) (fix (* (random) (1+ (abs (- a b))))))
;; )
;; 
;; String

;; Symble

;; SUBR

;; Switch types
