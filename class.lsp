(defun obj? (x)
  (and (al? x) (or (dot x 'class) (dot x 'parent)))
  )

(defun cls? (x)
  (and (al? x) (dot x 'class))
  )

(defun inst (cls paras)
  (if (sym? cls) (setq cls (eval cls)))
  (check "inst" (list cls obj? paras (list nil? list?)
                      (dot cls 'paras) (list nil? list?)))
  (append (list (cons 'parent (dot cls 'class)))
          (al_zip (dot cls 'paras) paras))
  )

(defun underscore (sym)
  (read (strcat "_" (str sym))) 
  )

(defun defcls (sym psym paras / ret _sym)
  (check "defcls" (list sym sym? psym (list nil? sym?)))
  (if (not (nil? psym)) (check (strcat "defcls parent's sym \"" (str psym) "\"")
                               (list (eval (read (strcat "_" (str psym)))) obj?)))
  (foreach p paras (check "defcls paras" (list p sym? )))
  (setq _sym (underscore sym)
        ret (set _sym (list (cons 'class sym) (cons 'parent psym) (cons 'paras paras))))
  (defun-q-list-set sym (list paras (list 'inst _sym (cons 'list paras))))
  sym
  )

(defun defmethod (cls_sym sym method / _cls_sym)
  (check "defmethod" (list cls_sym sym? sym sym? ))
  (setq _cls_sym (underscore cls_sym))
  (check "defmethod" (list (eval _cls_sym) cls? ))
  (set _cls_sym (al_upsert (eval _cls_sym) sym method))
  sym
  )


(defun do (obj key_paras / key paras get value)
  (check "do" (list obj obj? key_paras (list sym? list?))) 
  (if (list? key_paras)
      (setq key (car key_paras) paras (cdr key_paras))
      (setq key key_paras paras nil)
      )
  (check "do" (list key sym? paras (list nil? list?))) 
  (defun get (obj key)
    (if (assoc key obj) (al_get obj key)
        (if (assoc 'parent obj)
            (get (eval (underscore (al_get obj 'parent))) key)
            nil
            )))
  (setq value (get obj key))
  (if (func? value)
      (apply 'value (append (list obj) paras))
      value
      )
  )
