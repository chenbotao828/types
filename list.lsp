(defun len (x)
  (check "len" (list x (list list? str?)))
  (cond 
    ((dot_pair? x) 2)
    ((list? x) (length x))
    ((str? x) (strlen x))
    (t nil)))

(defun get_nth (x n / in_range?)
  (check "get_nth" (list x (list al? dot_pair? list? str?)
                         n int?))
  
  (if (or (< n (- 1 (len x))) (>= n (len x)))
      (*error* (strcat "Out of range in get nth: " (str n))))
  (if (< n 0) (setq n (+ (% n (len x)) (len x))))
  (cond 
    ((al? x) (cdr (assoc n x)))
    ((dot_pair? x) (nth n (list (car x) (cdr x))))
    ((list? x) (nth n x))
    ((str? x) (substr x (+ n 1) 1))
    (t nil)))

(defun span (x start end / ret)
  (check "span" (list x (list al? dot_pair? list? str?)
                      start int?
                      end int?))
  (if (< end 0) (setq end (+ (% end (len x)) (len x))))
  (if (str? x) (setq ret "") (setq ret nil))
  (foreach i (range start end 1)
           (setq ret (consend (get_nth x i) ret))))

(defun index (x i)
  (cond ((str? x) (vl-string-position (ascii i) x))
        ((list? x) (vl-position i x))
        (t (*error* "index: not list or str"))))

(defun reversed (x)
  (cond ((or (list? x) (nil? x)) (reverse x))
        ((str? x) (vl-list->string (reverse (vl-string->list x))))
        (t (*error* "reversed: not list or str"))))

(defun consend (x l)
  (cond 
    ((or (list? l) (nil? l)) (reversed (cons x (reversed l))))
    ((and (or (nil? l) (str? l)) (str? x)) (strcat l x))
    (t (*error* "consend: not list or str"))))

;; Assoc List
(defun dot (x key)
  (cdr (assoc key x)))

(setq __.__ "::")

(set (read __.__) dot)

(defun pushed (x key value)
  (if (or (nil? x) (al? x))
      (consend (cons key value) (vl-remove (assoc key x) x))
      (*error* "pushed: not associated list")))

(defun updated (al k v / ret)
  (setq ret (pushed al k v))
  (if (= (length al) (length ret))
      ret al))


(defun push (x_sym key value)
  (set x_sym (pushed (eval x_sym) key value)))

(defun update (x_sym key value)
  (set x_sym (updated (eval x_sym) key value)))

(defun ll2al (l1 l2 / len)
  ;; (ll2al '(a b c) '(1 2 3)) -> ((A . 1) (B . 2) (A . 3))
  (cond 
    ((and (list? l1) (list? l2) (= (length l1) (length l2)))
     (progn
       (setq len (length l1))
       (cond ((/= len (length l2)) nil)
             ((= len 0) nil)
             ((= len 1) (list (cons (car l1) (car l2))))
             (t (cons (cons (car l1) (car l2)) (ll2al (cdr l1) (cdr l2)))))))
    ((and (list? l1) (list? l2) (/= (length l1) (length l2)))
     (*error* "ll2al: list length not equal"))
    (t (*error* "ll2al: not list"))))


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
            (*error* "l2al empty")
            nil))
         ((= 1 (rem len 2))
          (*error* "l2al not paired"))
         ((= len 2)
          (list (cons (car lst) (cadr lst))))
         (t 
           (cons (cons (car lst) (cadr lst))
                 (l2al (cddr lst)))))))
    (t (*error* "l2al: not list"))))


(defun al2keys (al / len)
  ;; (al2keys '(('name."Tom") ("sex"."M"))) 
  ;; -> ((QUOTE NAME) "sex")
  (cond 
    ((al? al)
     (progn 
       (setq len (length al))
       (cond ((== len 0) nil)
             ((== len 1) (list (caar al)))
             (t (cons (caar al) (al2keys (cdr al)))))))
    
    (t (*error* "al2keys: not associated list"))))

(defun al_merge (al / kvs k v vs)
  ;; (al2kvs '((A . 1) (B . 2) (A . 3) (A . 4) (B . 3)))
  ;; -> ((A 1 3 4) (B 2 3))
  (cond
    ((al? al)
     (progn
       (setq kvs nil)
       (foreach l al
                (progn
                  (setq k (car l)
                        v (cdr l)
                        vs (dot kvs k))
                  (if vs (setq kvs (pushed kvs k (append vs (list v))))
                      (setq kvs (pushed kvs k (list v))))))
       kvs))
    
    (t (*error* "al2kvs: not associated list"))))
