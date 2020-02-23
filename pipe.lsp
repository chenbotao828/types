(defun pipe (x fs)
  (check "pipe" (list fs list?))
  (foreach 
    f fs
    (if (and (list? f) (= (car f) 'QUOTE)) (setq f (cdr f)))
    (cond
      ((func? f) (setq x (apply 'f (list x))))
      ((sym? f) (setq x (apply f (list x))))
      ((and (list? f) (= (car f) 'lambda)) 
       (setq x (apply f (list x))))
      ((and (list? f) (/= (car f) 'lambda)) 
       (setq x (apply (car f) (cons x (cdr f)))))
      ))
  x
  )

(defun pipe_func? (x)
  (or (func? x) (sym_func? x) (lambda? x)))

(defun as_func (f) 
  (check "as_func" (list f pipe_func?))
  (if (func? f) f (eval f)))

(defun concat (lst d / ret)
  (if (nil? d) (setq d ", "))
  (check "concat" (list lst (list nil? list?) d str?))
  (if (= nil lst) ""
      (progn
        (setq ret (str (car lst)))
        (foreach i (cdr lst) (setq ret (strcat ret d (str i))))
        ret
        )
      ) 
  )

(defun all (x f / ret) 
  (if (nil? f) (setq f (lambda (x) x)))
  (check "all" (list x (list list? nil?) f pipe_func?))
  (setq f (as_func f) ret t)
  (cond 
    ((nil? x) (setq ret nil))
    ((al? x) (setq x (al_keys al))))
  (while (and x ret)
         (setq ret (and ret (f (car x)))
               x (cdr x))
         )
  ret
  )

(defun tee (x)
  (foreach i (as_list x)
           (princ (str i))
           (princ "\n")
           )
  x
  )

(defun any (x f / ret)
  (if (nil? f) (setq f (lambda (x) x)))
  (check "any" (list x (list list? nil?) f pipe_func?))
  (setq f (as_func f) ret nil)
  (cond 
    ((nil? x) (setq ret t))
    ((al? x) (setq x (al_keys al))))
  (while (and x (not ret))
         (setq ret (or ret (f (car x)))
               x (cdr x))
         )
  ret
  )

(defun where (x f)
  (check "where" (list x (list list? nil?) f pipe_func?))
  (setq f (as_func f))
  (cond ((nil? x) nil)
        (t (vl-remove-if-not 'f x)))
  )
(setq filter where)

(defun reduce (x f / ret)
  (check "reduce" (list x (list list? nil?) f pipe_func?))
  (setq f (as_func f) ret (car x) x (cdr x))
  (while x (setq ret (f ret (car x))
                 x (cdr x)))
  ret
  )
(setq aggregate reduce)

(defun select (x f)
  (check "select" (list x (list list? nil?) f pipe_func?))
  (setq f (as_func f))
  (mapcar 'f x)
  )

(setq map select)

(defun take (x n)
  (check "take" (list x (list list? nil?) n +int?))
  (span x 0 n)
  )

(defun tail (x n)
  (check "tail" (list x (list list? nil?) n +int?))
  (reversed (take (reversed x) n))
  )

(defun take_while (x f / ret)
  (check "take_while" (list x (list list? nil?) f pipe_func?))
  (setq f (as_func f) )
  (while (apply 'f (list (car x)))
         (setq ret (consend (car x) ret)
               x (cdr x))) 
  ret
  )

(defun skip_while (x f / start ret)
  (check "skip_while" (list x (list list? nil?) f pipe_func?))
  (setq f (as_func f) )
  (foreach i x
           (if (not start)
               (if (not (apply 'f (list i)))
                   (setq start T)))
           (if start
               (setq ret (append ret (list i)))))
  ret)

(defun chain (x / fst ret)
  (check "chain" (list x (list list? nil?)))
  (foreach i x
           (check "chain's item" (list fst (list list? nil?)))
           )
  (apply 'append x)
  )

(defun chain_with (l lls)
  (check "chain_with" (list l (list list? nil?) lls list?))
  (append l lls)
  )

(defun flatten (l / ret sl)
  (check "flatten" (list l (list list? nil?)))
  (while l
         (setq sl (car l) l (cdr l))
         (if (list? sl)
             (setq l (append sl l))
             (setq ret (consend sl ret)))))

(setq traverse flatten)

(defun skip (l n)
  (check "skip" (list l (list list? nil?) n +int?))
  (while (and l (> n 0))
         (setq l (cdr l)
               n (- n 1)))
  l
  )

(setq count len)

(defun first (x)
  (check "first" (list x (list str? list? nil?) ))
  (cond ((list? x) (car x))
        ((str? x)  (car (as_list x)))
        (t nil)))

(defun rest (x)
  (check "rest" (list x (list str? list? nil?) ))
  (cond ((list? x) (cdr x))
        ((str? x) (apply 'strcat (cdr (as_list x))))
        (t nil)))


(defun last_one (x)
  (check "last_one" (list x (list str? list? nil?) ))
  (first (reversed x)) 
  )

(defun slice (x start end step / temp ret l is_str)
  (check "slice" (list x (list list? nil?)
                       start 0+int? end 0+int?
                       step (list nil? +int?)))
  (if (nil? step) (setq step 1))
  (if (> start end) (setq temp start start end end temp))
  (setq l (length x))
  (while (and (> end start) (< start l))
         (setq ret (consend (nth start x) ret)
               start (+ start step))
         )
  ret
  )

(defun zip_with (l1 l2)
  (check "zip_with" (list l1 (list list? nil?) l2 (list list? nil?) ))
  (zip (cons l1 l2))
  )

(defun groupby (x f / ret)
  (check "groupby" (list x (list list? nil?) f pipe_func?))
  (setq f (as_func f) ret nil)
  (foreach i x
           (setq ret (consend (cons (f i) i) ret))
           )
  (al_merge ret)
  )

(defun dedup (x f / ret fret)
  (check "dedup" (list x (list list? nil?) 
                       f (list nil? pipe_func?)))
  (if (nil? f) (setq f (lambda (x) x)))
  (setq f (as_func f) ret nil)
  (foreach i x
           (setq temp (f i))
           (if (not (member temp fret))
               (setq ret (consend i ret) 
                     fret (consend (f i) fret)))
           )
  ret
  )

(defun uniq (x f / ret)
  (check "uniq" (list x (list list? nil?) 
                      f (list nil? pipe_func?)))
  (if (nil? f) (setq f (lambda (x) x)))
  (setq f (as_func f) ret nil)
  (while x
         (if (nil? ret) (setq ret (list (car x)) ))
         (if (!= (f (car x)) (f (last_one ret)))
             (setq ret (consend (car x) ret))) 
         (setq x (cdr x) )
         )
  ret
  )


(defun : (x i)
  (if (list? x)
      (consend i x)
      (list x i)
      )
  )


(defun combinations ( l r )
;; (c '(1 2 3) 2) = 3*2*1 / 2 = 3
;; (1 2) (1 3) (2 3)
  (cond ((< r 2) (mapcar 'list l))
        (l (append
             (mapcar '(lambda ( x ) (cons (car l) x))
                     (combinations (cdr l) (1- r)))
             (combinations (cdr l) r)
             ))))

(defun permutation_all (L / displace core)
  (defun displace (L N)
    (if (> N 0)
        (apply
          (function (lambda (X)
                      (cons X (displace X (1- N)))))
          (list (append (cdr L) (list (car L)))))))
  
  (defun core (L)
    (displace L (length L)))
  (if (cdr L)
      (apply 'append
             (mapcar
               (function (lambda (X)
                           (mapcar
                             (function (lambda (Y)
                                         (cons (car X) Y)))
                             (permutation_all (cdr X)))))
               (reverse (core L))))
      (list L)))

(defun permutations (l r)
;; (p '(1 2 3) 2) = 3*2*1 / 1 = 6
;; (1 2) (1 3) (2 1) (2 3) (3 1) (3 2) 
  (sum (select (combinations l r) permutation_all)))

(setq transpose zip)
