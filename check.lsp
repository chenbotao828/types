(defun nil? (x)
  (null x)
  )

(defun not_nil? (x)
  (not (null x))
  )

(defun sym? (x)
  (if (= 'sym (type x))
      t nil))

(defun str? (x)
  (if (= 'str (type x))
      t nil))

(defun str0? (s)
  (if (and (str? s)
           (= 0 (read s))
           )
      t
      nil
      )
  )

(defun strnum? (s)
  (if (and (str? s)
           (num? (read s))
           )
      t
      nil
      )
  )

(defun list? (x)
  (if (= 'list (type x))
      t nil))

(defun func? (x)
  (if (member (type x) '(SUBR USUBR EXSUBR EXRXSUBR))
      t nil))

(defun sym_func? (x)
  (if (and (sym? x) (func? (eval x)))
      t nil))

(defun lambda?(x)
  (if (and (list? x) (func? (eval x)))
      t nil))

(defun dot_pair? (x)
  (if (and (list? x)
           (/= nil (cdr x))
           (atom (cdr x)))
      t nil))

(defun al? (l / all)
  (defun all (l f / ret i n)
    (setq ret T n 0)
    (if (not (func? f)) (setq f (eval f)))
    (while (and ret (< n (length l)))
           (setq i (nth n l))
           (setq ret (if (apply 'f (list i)) t nil))
           (setq n (+ 1 n)))
    ret
    )
  (cond
    ((dot_pair? l) nil)
    ((and 
       (list? l)
       (all l (lambda (x) (and 
                            (list? x)
                            (atom (car x)))))) t)
    (t nil)
    )
  )

(defun num? (x)
  (if (numberp x)
      t nil))

(defun int? (x)
  (if (= (type x) 'int) t nil) )

(defun 0+int? (x)
  (if (and (= (type x) 'int) (>= x 0)) t nil) )

(defun +int? (x)
  (if (and (= (type x) 'int) (> x 0)) t nil) )

(defun -int? (x)
  (if (and (= (type x) 'int) (< x 0)) t nil) )

(defun float? (x)
  (if (= (type x) 'real) t nil) )

(defun file? (f)
  (or 
    (findfile f)
    (findfile (strcat f ".lsp"))
    (findfile (strcat f ".fas"))
    (findfile (strcat f ".vlx"))
    )
  )

(defun ename? (x)
  (if (= 'ename (type x)) t nil)
  )

(defun check2func (f / sf)
  (check "check2func" (list f func?))
  (setq sf (str f))
  (span sf (1+ (str_rfind sf " ")) -2))

(defun check (check_name args_type?_list / l2al al te a tps any select te2msg join)
  (if {check_off} t 
      (progn
        (defun l2al (lst / len)
          ;; (l2al '("name" "Tom" "age" 18)) 
          ;; -> (("name" . "Tom") ("age" . 18))
          (cond 
            ((list? lst)
             (progn  
               (setq len (length lst))
               (cond 
                 ((= 0 len)
                  (progn 
                    (*error* "CheckError in check : list is nil")
                    nil))
                 ((= 1 (rem len 2))
                  (*error* (strcat "CheckError in check: list " (str lst) " is not paired")))
                 ((= len 2)
                  (list (cons (car lst) (cadr lst))))
                 (t 
                   (cons (cons (car lst) (cadr lst))
                         (l2al (cddr lst)))))))
            (t (*error* (strcat "CheckError in check: " (str lst) " is not list")))))
        (defun any (l f / ret i n)
          (setq ret nil n 0)
          (if (not (func? f)) (setq f (eval f)))
          (while (and (not ret) (< n (length l)))
                 (setq i (nth n l))
                 (setq ret (if (apply 'f (list i)) t nil))
                 (setq n (+ 1 n))
                 )
          ret
          ) 
        (defun select (l f / ret)
          (setq ret nil)
          (if (not (func? f)) (setq f (eval f)))
          (foreach i l
                   (setq ret (append ret (list (apply 'f (list i))))))
          ret)
        
        (defun te2msg (check_name te)
          (strcat "CheckError in " check_name ": "
                  (str_join "," (select te (lambda (e)
                                             (strcat (str (car e))
                                                     " is not " (str_join "/" (cdr e) )))) )
                  ))
        (if (/= (type check_name) 'str)
            (*error* (strcat "CheckError in check: " (str check_name) " is not str")))
        (setq al (l2al args_type?_list) te nil)
        (foreach 
          i al
          (progn
            (setq a (car i)
                  tps (cdr i)
                  temp_te nil
                  )
            (if (atom tps)
                (setq tps (list tps)))
            (foreach tp tps
                     (if (not (func? tp))
                         (*error* (strcat "CheckError in check: " (str tp) " is not func"))))
            (if (not (any tps (lambda (tp) (tp a))))
                (setq te (consend (cons a (select tps (lambda (tp) (check2func tp)))) te ))
                )))
        (if (= te nil)
            t
            (progn (*error* (te2msg check_name te))
                   nil)
            ;; (*error* (te2msg check_name te))
            )
        )
      )
  )

;; (if (*error* "???") 1 2) 