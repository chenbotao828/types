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

(defmethod 'man 'init (lambda (self) (list
    (cons 'nick_name (strcat "Happy " (do self 'name)) ))))
(assert-eq '"Happy Tom" '(dot (man "Tom") 'nick_name))

(defmethod 'man 'change_name 
  (lambda (self name) (al_upsert self 'name name))
  )
(setq {test_jerry} (man "Jerry"))
(do_set '{test_jerry} '(change_name "Tom"))
(assert-eq '"Tom" '(dot {test_jerry} 'name))

(assert-eq '_object '(parent _man))
(assert-eq 'object '(parent man))
(assert-eq '_object '(parent {test_jerry}))

(assert-eq ''object '(super_do _man 'class))
(assert-eq ''object '(super_do man 'class))
(assert-eq ''object '(super_do {test_jerry} 'class))

(setq _man nil man nil _object nil object nil {test_jerry} nil)
