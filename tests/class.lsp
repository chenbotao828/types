(deftest "class")

(assert (obj? '((CLASS . MAN) (PARENT) (PARAS))))
(assert (obj? '((PARENT . MAN) (PARAS))))
(assert (cls? '((CLASS . MAN) (PARENT) (PARAS))))
(assert (not (cls? '((PARENT . MAN) (PARAS)))))

(defcls 'object nil nil)
(assert-eq '_object ''((CLASS . OBJECT) (PARENT) (PARAS)))

(defmethod 'object 'exist T)
(assert-eq 'T '(dot _object 'exist))

(defmethod 'object 'class_str 
  (lambda (self) (str (do self 'class))))
(assert-eq '"OBJECT" '(do (object) '(class_str)))

(defcls 'man 'object '(name))
(defmethod 'man 'info 
  (lambda (self) (strcat (do self 'class_str) "'s name: " (do self 'name)))
  )
(assert-eq '"MAN's name: Tom" '(do (man "Tom") 'info))
