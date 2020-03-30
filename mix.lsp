(defun len (x)
  (check "len" (list x (list nil? list? str?)))
  (cond 
    ((nil? x) 0)
    ((dot_pair? x) 2)
    ((list? x) (length x))
    ((str? x) (length (gbk_str2lst x)))))

(defun get_nth (x n)
  (check "get_nth" (list x (list nil? dot_pair? list? str?)
                         n int?))
  (if (or (< n (- 1 (len x))) (>= n (len x)))
      (*error* (strcat "Out of range in get_nth: n " (str n))))
  (if (< n 0) (setq n (+ (% n (len x)) (len x))))
  (cond 
    ((nil? x) nil)
    ((dot_pair? x) (nth n (list (car x) (cdr x))))
    ((list? x) (nth n x))
    ((str? x) 
     ((lambda (xx)
        (if (list? xx)
            (gbk_lst2str xx)
            (gbk_lst2str (list xx))))
      (nth n (gbk_str2lst x))))))

(defun remove_nth (x n / lst temp)
  (check "remove_nth" (list x (list nil? dot_pair? list? str?)
                            n int?))
  (if (and (/= x nil) (or (< n (- 1 (len x))) (>= n (len x))))
      (*error* (strcat "Out of range in remove_nth: n " (str n))))
  (if (< n 0) (setq n (+ (% n (len x)) (len x))))
  (cond 
    ((nil? x) nil)
    ((dot_pair? x) (if (= n 0) (cdr x) (car x)))
    ((list? x) (progn
                 (setq temp -1)
                 (vl-remove-if '(lambda (y) (= (setq temp (1+ temp)) n)) x)
                 ))
    ((str? x) (apply 'strcat (remove_nth (as_list x) n)))))

(defun replace_nth (x n i / lst temp)
  (check "replace_nth" (list x (list nil? dot_pair? list? str?)
                             n int?))
  (if (and (/= x nil) (or (< n (- 1 (len x))) (>= n (len x))))
      (*error* (strcat "Out of range in replace_nth: n " (str n))))
  (if (< n 0) (setq n (+ (% n (len x)) (len x))))
  (cond 
    ((nil? x) nil)
    ((dot_pair? x) (if (= n 0) (cons i (cdr x)) (cons (car x) i)))
    ((list? x) (progn (setq temp -1) 
                      (mapcar '(lambda (y) (if (= n (setq temp (1+ temp)))
                                               i y)) x)))
    ((str? x) 
     (apply 'strcat (replace_nth (as_list x) n i))
     ))
  )

(defun insert_nth (x n i / ret)
  (check "insert_nth" (list x (list nil? dot_pair? list? str?)
                            n int?))
  (if (and (/= x nil) (or (< n (- -1 (len x))) (>= (- n 1) (len x))))
      (*error* (strcat "Out of range in insert_nth: n " (str n))))
  (if (< n 0) (setq n (+ 1 n (len x))))
  (cond 
    ((nil? x) (list i))
    ((dot_pair? x) (insert_nth (list (car x) (cdr x)) n i))
    ((list? x) (if (= n 0) (append (list i) x )
                (progn
                 (setq temp 0)
                 (apply 'append (mapcar '(lambda (a) (if (= n (setq temp (1+  temp))) (list a i) (list a))) x))
                )))
    ((str? x) 
     (setq lst (gbk_str2lst x))
     (strcat (gbk_lst2str (append (span lst 0 n)
                                  (gbk_str2lst i) (span lst n nil))))
     ))
  )
 
(defun swap_nth (x i j / vi vj)
  (check "swap_nth" (list x (list nil? dot_pair? list? str?)
                          i int? j int?))
  (if (and (/= x nil) (or (< i (- 1 (len x))) (>= (- i 1) (len x))))
      (*error* (strcat "Out of range in swap_nth: i " (str i))))
  (if (and (/= x nil) (or (< j (- 1 (len x))) (>= (- j 1) (len x))))
      (*error* (strcat "Out of range in swap_nth: j " (str j))))
  (if (< i 0) (setq i (+ (% i (len x)) (len x))))
  (if (< j 0) (setq j (+ (% j (len x)) (len x))))
  (setq vi (get_nth x i) vj (get_nth x j))
  (setq x (replace_nth x j vi ))
  (setq x (replace_nth x i vj )) 
  )

(defun span (x start end / ret is_str i l lspan)
  (defun lspan ( lst start end / rtn )
    (setq l (- end start))
    (setq l (if l (min l (- (length lst) start)) (- (length lst) start))
          start (+  start l)
          )
    (repeat l (setq rtn (cons (nth (setq start (1- start)) lst) rtn)))
    )
  (setq l (len x))
  (if (str? x) (setq is_str t) (setq is_str nil))
  (if (= start nil) (setq start 0))
  (if (= end nil) (setq end l))
  (check "span" (list x (list al? dot_pair? list? str? nil?)
                      start int?
                      end int?))
  (if (< start 0) (setq start (+ (rem start l) l)))
  (if (< end 0) (setq end (+ (rem end l) l)))
  (if is_str (setq x (gbk_str2lst x)))
  (setq ret (lspan x start end))
  (if is_str (gbk_lst2str ret) ret )
  )

(defun index (x i)
  (check "index" (list x (list nil? str? list?)))
  (cond 
    ((nil? x) nil)
    ((str? x) (vl-string-search i x))
    ((list? x) (vl-position i x))
    ))

(defun rindex (x i / ret)
  (check "rindex" (list x (list nil? str? list?)))
  (setq ret (index (reversed x) i))
  (if ret (- (len x) ret 1) nil)
  )

(defun indexall (s d / ds ss sl dl c ts ret)
  (check "indexall" (list s (list nil? str? list?)))
  (cond 
    ((nil? s) nil )
    ((str? s)
     (progn
       (check "indexall" (list d str?))
       (setq ds (gbk_str2lst d) ss (gbk_str2lst s) sl (len s) dl (len d) c 0 ret nil)
       (foreach i (range 0 sl 1)
                (if (= (get_nth ss i) (get_nth ds c))
                    (progn (setq c (+ 1 c)) (if (= c 1) (setq ts i)))
                    (setq c 0 ts nil)
                    )
                (if (= dl c)
                    (setq ret (consend ts ret) c 0)))ret))
    ((list? s)
     (progn
       (setq ret nil)
       (foreach i (range 0 (len s) 1)
                (if (== d (get_nth s i))
                    (setq ret (consend i ret))))
       ret
       ))))

(defun reversed (x)
  (check "reversed" (list x (list str? nil? list?)))
  (cond ((or (list? x) (nil? x)) (reverse x))
        ((str? x) (sum (reverse (as_list x))))
        ))

(defun consend (x l)
  (cond 
    ((or (list? l) (nil? l)) (append l (list x)))
    ((and (str? l) (str? x)) (strcat l x))
    (t (*error* "CheckError in consend: not list or str"))))

(defun as_list (x / ret) 
  (cond
    ((nil? x) nil)
    ((str? x)
     (progn
       (setq temp (gbk_str2lst x) ret nil) 
       (while temp
              (setq ret (consend (gbk_lst2str (list (car temp)))  ret)
                    temp (cdr temp)))
       ret
       ))
    ((list? x) x)
    (t (list x))
    )
  )

(defun in (x y)
  (cond 
    ((and (str? x) (str? y)) (if (vl-string-search x y) t nil))
    ((al? y) (if (member x (al_keys y)) t nil))
    ((nil? y) nil)
    (t (progn
         (check "in" (list y list?))
         (if (member x y) t nil)
         ))
    )
  
  )

(defun not_in (x y)
  (not (in x y))
  )
(defun contain (y x) (in x y))

(defun sum (lst)
  (check "sum" (list lst list? (car lst) (list str? num? list?)))
  (cond 
    ((str? (car lst)) 
     (progn
       (foreach i lst (check "sum item" (list i str?)))
       (apply 'strcat lst)
       )
     )
    ((num? (car lst)) 
     (progn
       (foreach i lst (check "sum item" (list i num?)))
       (apply '+ lst)
       )
     )
    ((list? (car lst)) 
     (progn
       (foreach i lst (check "sum item" (list i list?)))
       (apply 'append lst)
       )
     )
    )
  )

(defun multi (x n / ret)
  (check "multi" (list x (list str? num? list?)
                       n int?))
  (cond
    ((str? x) 
     (cond
       ((< n 0) (*error* (strcat "NotPostiveIntError in multi: " (str n))))
       ((= n 0) "")
       (t (progn
            (setq ret "")
            (repeat n (setq ret (strcat ret x)))
            )))
     )
    ((num? x) 
     (setq ret (* x n))
     )
    ((list? x) 
     (cond
       ((< n 0) (*error* (strcat "NotPostiveIntError in multi: " (str n))))
       ((= n 0) nil)
       (t (progn
            (setq ret nil)
            (repeat n (setq ret (append ret x)))
            ))) 
     )
    )
  )

(defun int(x)
  (check "int" (list x (list num? str?)))
  (cond ((num? x) (fix x))
        ((str? x) (atoi x))
        (t nil)
        )
  )

(defun floatit (x)
  (check "floatit" (list x (list num? str?)))
  (cond ((num? x) (+ x 0.0))
        ((str? x) (atof x))
        (t nil)
        )
  )

(defun random_choice (x)
  (check "random_choice" (list x (list nil? list? str?) ))
  (cond 
    ((nil? x) nil)
    (t (get_nth x (random_range 0 (- (len x) 1))))
    )
  )

(defun remove (x i)
  (check "remove" (list x (list nil? al? str? list?)))
  (cond 
    ((nil? x) nil)
    ((str? x) (progn (check "remove's item" (list i str?))
                     (str_replace x i "")))
    ((al? x) (vl-remove (assoc i x) x))
    ((list? x) (vl-remove i x))
    )
  )

(defun startswith (x i)
  (check "startswith" (list x (list nil? str? list?)))
  (cond
    ((nil? x) nil)
    ((str? x) 
     (progn
       (check "startswith" (list i str?))
       (if (= 0 (index x i)) t nil)))
    ((list? x) 
     (progn
       (if (= 0 (index x i)) t nil)))
    (t nil)
    )
  )

(defun countit (x i)
  (check "countit" (list x (list list? nil? str?)))
  (cond ((nil? x) 0)
        ((list? x)
         (len (indexall x i)))
        ((str? x)
         (progn
           (check "countit" (list i str?))
           (len (indexall x i)) 
           ))
        )
  )

(defun sort_list (lst func)
  (check "sort_list" (list lst (list list? nil?) func func?))
  (mapcar '(lambda (x) (nth x lst)) (vl-sort-i lst 'func) )
  )

(defun sort (lst)
  (check "sort" (list lst (list list? nil?)))
  (sort_list lst <<<) 
  )

(defun sort_by (lst func)
  (if (= nil func) (setq func <<<))
  (check "sort_by" (list lst (list list? nil?) func func?))
  (sort_list lst func) 
  )

(defun zip (x)
  (check "zip" (list x list?))
  (foreach i x (check "zip's list item" (list i list?)))
  (apply 'mapcar (cons 'list x))
  )

(defun enumerate (x / lst)
  (check "enumerate" (list x (list list? str? nil?)))
  (if (nil? x) nil
      (progn
        (setq lst (as_list x))
        (al_zip (range 0 (len lst) 1) lst)
        ))
  )

(defun input (x)
  (check "input" (list x str?))
  (getstring x)
  )

(defun bool (x)
  (if (in x (list 0 nil "")) nil t)
  )

;; (timeit '(where '(1 2 3) num?))
;; all input func in autolisp:
;; http://help.autodesk.com/view/ACD/2015/ENU/?guid=GUID-7F024C1B-EFB8-4F6C-9EFE-A6491210A4CD

;; later help dir open issubclass super hash 
