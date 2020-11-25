;; get str of all type
;; _ -> str
(defun str (x)
  (if (= (type x) 'str) x
      (vl-prin1-to-string x)
      )
  )
;; transform str to list
;; str -> list (int , list of int)
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
;; transform list to str
;; list -> str
(defun gbk_lst2str (lst / ret)
  (check "gbk_lst2str" (list lst (list list? nil?)))
  (setq ret nil)
  (foreach i lst
           (if (= 'list (type i))
               (setq ret (append ret i))
               (setq ret (append ret (list i)))))
  (vl-list->string ret)
  )
;; split str into list
;; str -> list 
(defun str_split (s d / sl sl2 temp po strlst xlen)
  (check "str_split" (list s str? d (list nil? str?)))
  (cond 
    ( (= d "") (*error* "Empty seperator in str_split") )
    ( (= d nil) 
     (progn
       (setq sl (vl-string->list s) sl2 nil temp nil)
       (foreach i sl 
                (if (member i '(9 10 13 32))
                    (if temp
                        (setq sl2 (cons (reverse temp) sl2) temp nil))
                    (setq temp (cons i temp)))
                )
       (if temp (setq sl2 (cons (reverse temp) sl2)))
       (mapcar 'vl-list->string (reverse sl2) )
       )
     )
    (t (progn 
         (setq xlen (1+ (strlen d)))
         (while (setq po (vl-string-search d s))
                (setq strlst (cons (substr s 1 po) strlst))
                (setq s (substr s (+ po xlen)))
                )
         (reverse (cons s strlst))
         ))))
;; split str into list, using regular expression (vbscript)
;; str -> list
(defun str_re_split (s d / L r)
  (check "str_re_split" (list s str? d (list nil? str?)))
  (if (= d nil) (setq d " |\t|\n|\r"))
  (cond
    ( (= d "") (*error* "Empty seperator in str_re_split") )
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
;; split str into list, multi seperator
;; str -> list
(defun str_splits (s ds)
  (check "str_splits" (list s str? ds list?))
  (str_re_split s (concat ds "|"))
  )
;; split str into list, line by line 
;; str -> list
(defun str_splitlines (s)
  (check "str_splitlines" (list s str?))
  (str_split s "\n")
  )
;; join list by string 
;; str list ->  str
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
;; replace old part of string to new part 
;; str str str ->  str
(defun str_replace (string old new)
  (check "str_replace" (list string str? old str? new str?))
  (pipe string (list
                 (lambda (x)
                   (if (= old "")
                       (as_list string)
                       (str_split string old))
                   )
                 (list concat new)
                 ))
  )
;; formating string using list or al 
;; str (list al) ->  str
(defun str_format (string al)
  (check "str_format" (list string str? al (list list? al?)))
  (if (not (al? al)) (setq al (mapcar 'cons (range 0 (len al) 1) al)))
  (foreach p al
           (setq string 
                 (str_replace string
                              (strcat "{" (str (car p)) "}")
                              (str (cdr p))))))
;; capitalize string 
;; str -> str
(defun str_capitalize (x)
  (check "str_capitalize" (list x str?))
  (if (> (len x) 1)
      (strcat (strcase (get_nth x 0)) (span x 1 nil))
      (strcase x)
      )
  )
;; center string by width and filling
;; str int str-> str
(defun str_center (x w fill / l l1 l2 _multi)
  (check "str_center" (list x str? w +int? fill str?))
  (defun _multi (x n / ret)
    (setq ret "")
    (repeat n (setq ret (strcat ret x)))
    )
  (setq l (strlen x)
        l1 (/ (- w l) 2 (strlen fill))
        l2 (/ (- w l (* l1 (strlen fill))) (strlen fill)))
  (if (or (< l1 0) (< l2 0)) x
      (strcat (_multi fill l1) x (_multi fill l2))
      )
  )
;; count parts in string
;; str str -> int
(defun str_count (x i)
  (check "str_count" (list x str? i str?))
  (if (= "" i)
      (1+ (strlen x))
      (/ (- (strlen x) (strlen (apply 'strcat (str_split x i)))) (strlen i))
      )
  )
;; check if a string start with another
;; str str -> bool
(defun str_startwith (x i)
  (check "str_startwith" (list x str? i str?))
  (if (>= (strlen x) (strlen i)) 
      (= (substr x  1 (strlen i)) i)
      nil
      )
  )
;; check if a string end with another
;; str str -> bool
(defun str_endswith (x i)
  (check "str_endswith" (list x str? i str?))
  (if (>= (strlen x) (strlen i)) 
      (= (substr x  (1+(- (strlen x) (strlen i))) (strlen i)) i)
      nil
      )
  )
;; check if a string is all alphabet (a-z A-Z)
;; str -> bool
(defun str_isalpha (x / i ret sl l)
  (check "str_isalpha" (list x str?))
  (setq i 0 ret t sl (vl-string->list x) l (strlen x))
  (while (and ret (> l i))
         (setq temp (nth i sl))
         (if (not (or (<= 65 temp 90) (<= 97 temp 122)))
             (setq ret nil))
         (setq i (+ i 1)))
  ret
  )
;; check if a string is all digital (0-9)
;; str -> bool
(defun str_isdigit (x / i ret sl l)
  (check "str_isdigit" (list x str?))
  (setq i 0 ret t sl (vl-string->list x) l (strlen x))
  (while (and ret (> l i))
         (setq temp (nth i sl))
         (if (not (<= 48 temp 57))
             (setq ret nil)
             )
         (setq i (+ i 1))
         )
  ret
  )
;; check if string is demical
;; str -> str
(defun str_isdemical (x / ret l i lst temp)
  (check "str_isdemical" (list x str?))
  (setq ret t l (len x) i 0 lst (vl-string->list x))
  (while (and ret (< i (strlen x))) 
         (setq temp (nth i lst))
         (if (not (and (int? temp) (<= 48 temp 57)))
             (setq ret nil))
         (setq i (+ 1 i))
         )
  ret
  )
;; check if a string is all alphabet (a-z A-Z) and digital (0-9)
;; str -> bool
(defun str_isalnum (x / i ret sl l)
  (check "str_isalnum" (list x str?))
  (setq i 0 ret t sl (vl-string->list x) l (strlen x))
  (while (and ret (> l i))
         (setq temp (nth i sl))
         (if (not (or (<= 48 temp 57)
                      (<= 65 temp 90)
                      (<= 97 temp 122)))
             (setq ret nil))
         (setq i (+ i 1)))
  ret
  )
;; check if a string is all space (" " \t \r \n)
;; str -> bool
(defun str_isspace (x / i ret sl l)
  ;; space and \t \r \n
  (check "str_isspace" (list x str?))
  (setq i 0 ret t sl (vl-string->list x) l (strlen x))
  (while (and ret (> l i))
         (setq temp (nth i sl))
         (if (not (member temp '(9 10 13 32)))
             (setq ret nil))
         (setq i (+ i 1)))
  ret
  )
;; check if a string is lower (at least one alphabet a-z)
;; str -> bool
(defun str_islower (x)
  (check "str_islower" (list x str?))
  (if (and
        (= x (strcase x T))
        (vl-some '(lambda (i) (<= 97 i 122)) (vl-string->list x) )
        )
      t nil)
  )
;; check if a string is upper (at least one alphabet A-Z)
;; str -> bool
(defun str_isupper (x)
  (check "str_isupper" (list x str?))
  (if (and
        (= x (strcase x))
        (vl-some '(lambda (i) (<= 65 i 90)) (vl-string->list x)))
      t nil)
  )

;; check if a string is tittle (first letter is upper and rest is lower)
;; str -> bool
(defun str_istitle(x / ret _istitle _split)
  (defun _istitle (x / ret)
    (setq ret (mapcar '(lambda (i)
                        (cond ((<= 65 i 90) 2)
                              ((<= 97 i 122) 1)
                              (t 0))) 
                      (vl-string->list x)))
    (and (/= 2 (cadr ret)) (apply '>= ret))
    )
  (defun _split (s / sl sl2 temp)
    (setq sl (vl-string->list s) sl2 nil temp nil)
    (foreach i sl 
             (if (member i '(9 10 13 32))
                 (if temp
                     (setq sl2 (cons (reverse temp) sl2) temp nil))
                 (setq temp (cons i temp))))
    (if temp (setq sl2 (cons (reverse temp) sl2)))
    (mapcar 'vl-list->string (reverse sl2))
    )
  (setq ret (mapcar '_istitle (_split x)))
  (if ret (apply 'and ret)
      nil)
  )
;; left justified string of length width
;; str int str -> str
(defun str_ljust (x width fill / _multi)
  (if (nil? fill) (setq fill " "))
  (check "str_ljust" (list x str? width +int? fill str?))
  (defun _multi (x n / ret)
    (setq ret "")
    (repeat n (setq ret (strcat ret x)))
    )
  (if (/= 1 (strlen fill))
      (*error* (strcat "fill_length != 1 in str_ljust: \"" fill "\""))
      (if (<= width (strlen x)) x
          (strcat x (_multi fill (- width (strlen x))))
          )
      )  
  )

;; right justified string of length width
;; str int str -> str
(defun str_rjust (x width fill / _multi)
  (if (nil? fill) (setq fill " "))
  (check "str_rjust" (list x str? width +int? fill str?))
  (defun _multi (x n / ret)
    (setq ret "")
    (repeat n (setq ret (strcat ret x)))
    )
  (if (/= 1 (strlen fill))
      (*error* (strcat "fill_length != 1 in str_rjust: \"" fill "\""))
      (if (<= width (strlen x)) x
          (strcat (_multi fill (- width (strlen x))) x)
          )
      )  
  )

;; lower alphabet (a-z -> A-Z)
;; str -> str
(defun str_lower (x)
  (check "str_lower" (list x str?))
  (strcase x T)
  )

;; upper alphabet (A-Z -> a-z)
;; str -> str
(defun str_upper (x)
  (check "str_upper" (list x str?))
  (strcase x)
  )

;; left striped stirng using characters
;; str str -> str
(defun str_lstrip (x chars / sl cl meet)
  (if (nil? chars) (setq chars " \n\r\t"))
  (check "str_lstrip" (list x str? chars str?))
  (vl-string-left-trim chars x)
  )

;; right striped stirng using characters
;; str str -> str
(defun str_rstrip (x chars / sl cl meet)
  (if (nil? chars) (setq chars " "))
  (check "str_rstrip" (list x str? chars str?))
  (vl-string-right-trim chars x)
  )

;; left and right striped stirng using characters
;; str str -> str
(defun str_strip (x chars)
  (if (nil? chars) (setq chars " "))
  (check "str_strip" (list x str? chars str?))
  (vl-string-trim chars x)
  )

;; max character of string
;; str -> str
(defun str_max (x)
  (check "str_max" (list x str?))
  (pipe x (list
            gbk_str2lst
            (list select (lambda (i) (if (list? i) (+ (cadr i) (*  (car i) 1000)) i)))
            (lambda (i) (apply 'max i))
            (lambda (i) (if (> i 1000) (list (list (/ i 1000) (rem i 1000))) (list i)))
            gbk_lst2str
            ))
  )

;; min character of string
;; str -> str
(defun str_min (x)
  (check "str_min" (list x str?))
  (pipe x (list
            gbk_str2lst
            (list select (lambda (i) (if (list? i) (+ (cadr i) (*  (car i) 1000)) i)))
            (lambda (i) (apply 'min i))
            (lambda (i) (if (> i 1000) (list (list (/ i 1000) (rem i 1000))) (list i)))
            gbk_lst2str
            ))
  )

;; find index number of a string from another
;; str str -> int / nil
(defun str_find (x i / ret )
  (check "str_find" (list x str? i str?))
  (if (= i "") 0 (vl-string-search i x))
  )

;; find index number of a string from another, right side
;; str str -> int / nil
(defun str_rfind (x i / ret str_reverse)
  (check "str_rfind" (list x str? i str?))
  (defun str_reverse (x / sl ret) 
    (setq sl (vl-string->list x) ret nil)
    (while sl
           (progn
             (if  (and (<= 129 (car sl) 254)
                       (<= 64 (cadr sl) 254))
                 (setq ret (cons (car sl) (cons (cadr sl) ret))
                       sl (cddr sl))
                 (setq ret (cons (car sl) ret)
                       sl (cdr sl)))))
    (vl-list->string ret)
    )
  (if (= i "") (strlen x)
      (progn
        (setq ret (vl-string-search  (str_reverse i) (str_reverse x)))
        (if ret (- (strlen x) ret 1) nil)
        )
      )
  )

;; swap lower and upper case of string
;; str -> str
(defun str_swapcase (x / sl ret)
  (check "str_swapcase" (list x str?))
  (setq sl (vl-string->list x) ret nil)
  (while sl 
         (cond ((<= 129 (car sl) 254) 
                (setq  ret (cons (cadr sl) (cons (car sl) ret)) 
                      sl (cddr sl)))
               ((<= 65 (car sl) 90)
                (setq ret (cons (+ 32 (car sl)) ret )
                      sl (cdr sl)))
               ((<= 97 (car sl) 122)
                (setq ret (cons (- (car sl) 32) ret )
                      sl (cdr sl)))
               (t (setq ret (cons (car sl) ret ) 
                        sl (cdr sl)
                        ))))
  (vl-list->string (reverse ret))
  )
;; upper case first letter and lower case the rest
;; str -> str
(defun str_title (x / f s sl ret) 
  (check "str_title" (list x str?))
  (setq sl (vl-string->list x) 
        ret (cond
              ((<= 97 (car sl) 122) (list (- (car sl) 32)))
              (t (list (car sl)))
              ))
  (while (cdr sl)
         (setq f (car sl) s (cadr sl))
         (cond 
           ((and (member f '(9 10 13 32)) (<= 97 s 122) )
            (setq ret (cons (- s 32) ret) sl (cdr sl))
            )
           ((and (not (member f '(9 10 13 32))) (<= 65 s 90) )
            (setq ret (cons (+ s 32) ret) sl (cdr sl))
            )
           (t (setq ret (cons s ret) sl (cdr sl))
              )
           )
         )
  (vl-list->string (reverse ret))
  )