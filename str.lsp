(defun split (s d / p)
  (cond 
    ((not (str? s)) (*error* (strcat "split: " (str s) " not str")))
    ((not (str? d)) (*error* (strcat "split: " (str d) " not str")))
    ((and (str? s) (str? d))
         (if (= d "") 
          ((cons (car s) (split (cdr s) d)))
          (if (setq p (vl-string-search d s))
              (cons (substr s 1 p) 
                    (split (substr s (+ p 1 (strlen d))) d)) (list s))))
  )
)

(defun join (lst d / ret cdr_lst)
  (cond ((not (list? lst)) (*error* (strcat "join: " (str s) " not str")))
   )
  (progn
    (setq ret (car lst))
    (foreach i (cdr lst) (setq ret (strcat ret d i)))
    ret)
  )

(defun replace (string old new / temp)
  ;; (replace "abcabc" "a" "x") -> "xbcxbc"
  ;; (replace "abcabc" "a" "x") -> "xbcxbc"
  (setq temp nil)
  (while (/= temp string)
         (progn
          (setq temp string)
          (setq string (vl-string-subst new old temp))))
  string)

(defun format (string al)
  (foreach p al
    (setq string 
     (replace string
      (strcat "{" (car p) "}")
      (cdr p)))))
