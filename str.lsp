(defun str (x)
  (if (= (type x) 'str) x
      (vl-prin1-to-string x)
      )
  )

(defun gbk_str2lst (x / lst ret)
  (check "gbk_str2lst" (list x str?))
  (setq ret nil lst (vl-string->list x))
  (while (/= nil lst)
         (if (and (<= 129 (car lst) 254)
                  (<= 64 (cadr lst) 254))
             (setq ret (append 
                         ret
                         (list (list (car lst) (cadr lst)))
                         )
                   lst (cddr lst))
             (setq ret (append  ret (list (car lst)) )
                   lst (cdr lst))
             ))
  ret
  )

(defun gbk_lst2str (lst / flatten ret)
  (check "gbk_lst2str" (list lst (list list? nil?)))
  (defun flatten (l / ret sl)
    (while l
           (setq sl (car l) l (cdr l))
           (if (list? sl)
               (setq l (append sl l))
               (setq ret (consend sl ret)))))
  (setq ret (flatten lst))
  (foreach i ret 
           (check "gbk_lst2str item" (list i int?))
          ;  (if (not (<= 0 i 255))
          ;      (*error* (strcat "Out of range in gbk_lst2str item: "
          ;                       (str i) "not between 0 and 255"
          ;                       ))
          ;      )
  )
  (vl-list->string ret)
  )

(defun str_split (s d / po strlst xlen)
  (check "str_split" (list s str? d (list nil? str?)))
  (cond 
    ( (= d "") (*error* "Empty seperator in str_split") )
    ( (= d nil) 
     (progn
       (foreach i (list "\t" "\n" "\r") (setq s (str_replace s i " ")))
       (remove (str_split s " ") "")
       ))
    (t (progn 
    (setq xlen (1+ (strlen d)))
    (while (setq po (vl-string-search d s))
      (setq strlst (cons (substr s 1 po) strlst))
      (setq s (substr s (+ po xlen)))
    )
    (reverse (cons s strlst))
  ) ) ) )

(defun str_re_split (s d / L r)
  (check "str_re_split" (list s str? d (list nil? str?)))
  (cond
    ( (= d "") (*error* "Empty seperator in str_re_split") )
    ( (= d nil) 
     (progn
       (foreach i (list "\t" "\n" "\r") (setq s (str_replace s i " ")))
       (remove (str_re_split s " ") "")
       ))
    
    (t (progn
        (setq r (vlax-create-object "vbscript.regexp"))
        (vlax-put-property r 'Global 1)
        (vlax-put-property r 'Pattern (strcat "([^" d "]+)"))
        (vlax-for x (vlax-invoke r 'Execute s)(setq L (cons(vla-get-Value x) L)))
        (vlax-release-object r)
        (reverse L)
      )
    )
  )
  )

(defun str_splits (s ds)
  (check "str_splits" (list s str? ds list?))
  (str_re_split s (concat ds "|"))
  )

(defun str_splitlines (s)
  (check "str_splitlines" (list s str?))
  (str_splits s (list "\b" "\r" "\n" "\r\n"))
  )


(defun str_join (d lst / ret)
  (check "str_join" (list lst (list list? nil?) d str?))
  (if (= nil lst) ""
      (progn
        (setq ret (car lst))
        (foreach i (cdr lst) (setq ret (strcat ret d i)))
        ret
        )
      )
  )

(defun str_replace (x old new / temp)
  ;; (str_replace "abcabc" "a" "x") -> "xbcxbc"
  (check "str_replace" (list x str? old str? new str?))
  (setq temp nil)
  (while (/= temp x)
         (progn
           (setq temp x)
           (setq x (vl-string-subst new old temp))))
  x)

(defun str_format (string al)
  (check "str_format" (list string str? al (list list? al?)))
  (if (al? al)
      (foreach p al
               (setq string 
                     (str_replace string
                                  (strcat "{" (str (car p)) "}")
                                  (str (cdr p)))))
      (foreach i (range 0 (len al) 1)
               (setq string
                     (str_replace string
                                  (strcat "{" (str i) "}")
                                  (str (get_nth al i)))))))

;; sum in mix
;; multi in mix

(defun str_capitalize (x)
  (check "str_capitalize" (list x str?))
  (if (> (len x) 1)
      (strcat (strcase (get_nth x 0)) (span x 1 nil))
      (strcase x)
      )
  )

(defun str_center (x w fill / l l1 l2)
  (check "str_center" (list x str? w +int? fill str?))
  (setq l (len x)
        l1 (/ (- w l) 2 (len fill))
        l2 (/ (- w l (* l1 (len fill))) (len fill)))
  (if (or (< l1 0) (< l2 0)) x
      (strcat (multi fill l1) x (multi fill l2))
      )
  )

(defun str_count (x i / s1 s2 ret)
  (check "str_count" (list x str? i str?))
  (if (= i "")
      (setq ret (+ 1 (len x)))
      (progn
        (setq ret 0)
        (while (index x i) 
               (setq s1 (index x i)
                     s2 (+ s1 (len i))
                     ret (+ ret 1)
                     x (span x s2 nil)
                     ))))
  ret
  )

(defun str_endswith (x i)
  (check "str_endswith" (list x str? i str?))
  (if (>= (len x) (len i)) 
      (== (span x (- 0 (len i)) nil) i)
      t
      )
  )

(defun str_isalpha (x / i ret sl l)
  (check "str_isalpha" (list x str?))
  (setq i 0 ret t sl (gbk_str2lst x) l (len x))
  (while (and ret (> l i))
         (setq temp (get_nth sl i))
         (if (or (list? temp)
                 (not (or (<= 65 temp 90)
                          (<= 97 temp 122)))
                 )
             (setq ret nil)
             )
         (setq i (+ i 1))
         )
  ret
  )

(defun str_isdigit (x / i ret sl l)
  (check "str_isdigit" (list x str?))
  (setq i 0 ret t sl (gbk_str2lst x) l (len x))
  (while (and ret (> l i))
         (setq temp (get_nth sl i))
         (if (or (list? temp)
                 (not (<= 48 temp 57)))
             (setq ret nil)
             )
         (setq i (+ i 1))
         )
  ret
  )


(defun str_isalnum (x / i ret sl l)
  (check "str_isalnum" (list x str?))
  (setq i 0 ret t sl (gbk_str2lst x) l (len x))
  (while (and ret (> l i))
         (setq temp (get_nth sl i))
         (if (or (list? temp)
                 (not (or (<= 48 temp 57)
                          (<= 65 temp 90)
                          (<= 97 temp 122)))
                 )
             (setq ret nil)
             )
         (setq i (+ i 1))
         )
  ret
  )

;; isnumeric: deprecated

(defun str_isspace (x / i ret sl l)
  ;; space and \t \r \n
  (check "str_isspace" (list x str?))
  (setq i 0 ret t sl (gbk_str2lst x) l (len x))
  (while (and ret (> l i))
         (setq temp (get_nth sl i))
         (if (not (member temp '(9 10 13 32)))
             (setq ret nil)
             )
         (setq i (+ i 1))
         )
  ret
  )

(defun str_islower (x / i ret sl l upper?)
  (check "str_islower" (list x str?))
  (setq i 0 ret nil sl (gbk_str2lst x) l (len x) upper? nil)
  (while (and (not upper?) (> l i))
         (setq temp (get_nth sl i))
         (if (and (not (list? temp)) (or (<= 65 temp 90) (<= 97 temp 122)))
             (setq ret t)
             )
         (if (and (not (list? temp)) (<= 65 temp 90))
             (setq ret nil upper? t)
             )
         (setq i (+ i 1))
         )
  ret
  )

(defun str_isupper (x / i ret sl l lower?)
  (check "str_isupper" (list x str?))
  (setq i 0 ret nil sl (gbk_str2lst x) l (len x) lower? nil)
  (while (and (not lower?) (> l i))
         (setq temp (get_nth sl i))
         (if (and (not (list? temp)) (or (<= 65 temp 90) (<= 97 temp 122)))
             (setq ret t)
             )
         (if (and (not (list? temp)) (<= 97 temp 122))
             (setq ret nil lower? t)
             )
         (setq i (+ i 1))
         )
  ret
  )


(defun str_istitle (x)
  ;; first letter str_isupper and rest str_islower
  (check "str_istitle" (list x str?))
  (cond ((= (len x) 0) nil)
        ((= (len x) 1)
         (if (str_isupper (get_nth x 0)) t nil ) ) 
        (t (if (and (str_isupper (get_nth x 0))
                    (str_islower (span x 1 nil)))
               t nil
               )))
  )

(defun str_ljust (x width fill)
  (if (nil? fill) (setq fill " "))
  (check "str_ljust" (list x str? width +int? fill str?))
  (if (!= 1 (len fill))
      (*error* (strcat "fill_length != 1 in str_ljust: \"" fill "\""))
      (if (<= width (len x)) x
          (strcat x (multi fill (- width (len x))))
          )
      )  
  )

(defun str_rjust (x width fill)
  (if (nil? fill) (setq fill " "))
  (check "str_rjust" (list x str? width +int? fill str?))
  (if (!= 1 (len fill))
      (*error* (strcat "fill_length != 1 in str_rjust: \"" fill "\""))
      (if (<= width (len x)) x
          (strcat (multi fill (- width (len x))) x)
          )
      )  
  )

(defun str_lower (x / sl ret)
  (check "str_lower" (list x str?))
  (setq sl (gbk_str2lst x) ret nil)
  (foreach i sl
           (if (and (int? i) (<= 65 i 90))
               (setq ret (consend (+ i 32) ret))
               (setq ret (consend i ret))
               )
           )
  (gbk_lst2str ret)
  )

(defun str_upper (x / sl ret)
  (check "str_upper" (list x str?))
  (setq sl (gbk_str2lst x) ret nil)
  (foreach i sl
           (if (and (int? i) (<= 97 i 122))
               (setq ret (consend (- i 32) ret))
               (setq ret (consend i ret))
               )
           )
  (gbk_lst2str ret)
  )

(defun str_lstrip (x chars / sl cl meet)
  (if (nil? chars) (setq chars " "))
  (check "str_lstrip" (list x str? chars str?))
  (setq sl (gbk_str2lst x) cl (gbk_str2lst chars) meet t)
  (while meet
         (if (member (car sl) cl) (setq sl (cdr sl)) (setq meet nil))
         )
  (gbk_lst2str sl)
  )

(defun str_rstrip (x chars / sl cl meet)
  (if (nil? chars) (setq chars " "))
  (check "str_rstrip" (list x str? chars str?))
  (setq sl (reverse (gbk_str2lst x)) cl (gbk_str2lst chars) meet t)
  (while meet
         (if (member (car sl) cl) (setq sl (cdr sl)) (setq meet nil))
         )
  (gbk_lst2str (reverse sl))
  )

(defun str_strip (x chars)
  (if (nil? chars) (setq chars " "))
  (check "str_strip" (list x str? chars str?))
  (str_rstrip (str_lstrip x chars) chars)
  )

;; (defun str_maketrans (x intab outtab / ls ret id)
;;   (check "str_maketrans" (list x str? intab str? outtab str?))
;;   (cond ((= (len intab) (len outtab))
;;          (setq ls (gbk_str2lst x) ret nil intab
;;                (gbk_str2lst intab) outtab (gbk_str2lst outtab))
;;          (foreach i ls
;;                   (setq id (index intab i))
;;                   (if (not (nil? id))
;;                       (setq ret (consend (get_nth outtab id) ret))
;;                       (setq ret (consend i ret))
;;                       ))
;;          (gbk_lst2str ret)
;;          )
;;         (t (*error* (strcat "length not equal in str_maketrans: "
;;                             "\"" intab "\" and \"" outtab "\""
;;                             )))
;;         )
;;   )

(defun str_max (x / sl sl2)
  (check "str_max" (list x str?))
  (cond ((= x "") "")
        (t
          (setq sl (gbk_str2lst x) sl2 nil)
          (foreach i sl
                   (if (list? i)
                       (setq sl2 
                             (consend (+ (* 1000 (car i))(cadr i)) sl2))
                       ;; (print (cadr i))
                       (setq sl2 (consend i sl2))))
          (get_nth x (index sl2 (apply 'max sl2)))))
  )

(defun str_min (x / sl sl2)
  (check "str_min" (list x str?))
  (cond ((= x "") "")
        (t
          (setq sl (gbk_str2lst x) sl2 nil)
          (foreach i sl
                   (if (list? i)
                       (setq sl2 
                             (consend (+ (* 1000 (car i))(cadr i)) sl2))
                       ;; (print (cadr i))
                       (setq sl2 (consend i sl2))))
          (get_nth x (index sl2 (apply 'min sl2)))))
  )

(defun str_find (x i)
  (check "str_find" (list x (list nil? str? list?)))
  (index x i)
  )

(defun str_rfind (x i / ret)
  (check "str_find" (list x (list nil? str? list?)))
  (setq ret (index (reversed x) i))
  (if ret (- (len x) ret 1) nil)
  )

(defun str_swapcase (x / ret lst)
  (check "str_swapcase" (list x str?))
  (setq lst (gbk_str2lst x) ret nil)
  (foreach i lst
           (cond 
             ((and (int? i) (<= 65 i 90))
              (setq ret (consend (+ i 32) ret)))
             ((and (int? i) (<= 97 i 122))
              (setq ret (consend (- i 32) ret)))
             (t (setq ret (consend i ret)))
             )
           )
  (gbk_lst2str ret)
  )

(defun str_title (x / ret lst)
  (check "str_title" (list x str?))
  (setq lst (gbk_str2lst x) ret nil)
  (if (and (int? (car lst)) (<= 97 (car lst) 122))
      (setq ret (consend (- (car lst) 32) ret))
      (setq ret (consend (car lst) ret))
      )
  (foreach i (cdr lst)
           (if
             (and (int? i) (<= 65 i 90))
             (setq ret (consend (+ i 32) ret))
             (setq ret (consend i ret))
             )
           )
  (gbk_lst2str ret)
  )

(defun str_isdemical (x / ret l i lst temp)
  (check "str_isdemical" (list x str?))
  (setq ret t l (len x) i 0 lst (gbk_str2lst x))
  (while (and ret (< i (len x))) 
         (setq temp (get_nth lst i))
         (if (not (and (int? temp) (<= 48 temp 57)))
             (setq ret nil))
         (setq i (+ 1 i))
         )
  ret
  )
