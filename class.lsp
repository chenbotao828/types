(defun obj? (x) 
  (and (al? x) (or (dot x 'class) (dot x 'parent))))

(defun cls? (x) 
  (and (al? x) (dot x 'class)))

(defun _inst (cls paras / ret init_ret) 
  (if (sym? cls) (setq cls (eval cls)))
  (check 
    "_inst"
    (list cls 
          obj?
          paras
          (list nil? list?)
          (dot cls 'paras)
          (list nil? list?)))
  (setq ret      (append (list (cons 'parent (dot cls 'class))) 
                         (al_zip (dot cls 'paras) paras))
        init_ret (append ret (do ret '__init__)))
  (if init_ret init_ret ret))

(defun underscore (sym) 
  (read (strcat "_" (str sym))))

(defun defcls (sym psym paras / ret _sym) 
  (check "defcls" (list sym sym? psym (list nil? sym?)))
  (if (not (nil? psym)) 
    (check 
      (strcat "defcls parent's sym \"" (str psym) "\"")
      (list (eval (read (strcat "_" (str psym)))) obj?)))
  (foreach p paras (check "defcls paras" (list p sym?)))
  (setq _sym (underscore sym)
        ret  (set _sym 
                  (list (cons 'class sym) 
                        (cons 'parent psym)
                        (cons 'paras paras))))
  (defun-q-list-set sym (list paras (list '_inst _sym (cons 'list paras))))
  sym)

(defun defmethod (cls_sym sym method / _cls_sym) 
  (check "defmethod" (list cls_sym sym? sym sym?))
  (setq _cls_sym (underscore cls_sym))
  (check "defmethod" (list (eval _cls_sym) cls?))
  (if (= sym 'init) 
    (defmethod cls_sym '__init__ method)
    (set _cls_sym (al_upsert (eval _cls_sym) sym method)))
  sym)


(defun do (obj key_paras / key paras get value) 
  (check "do" (list obj (list cls? obj?) key_paras (list sym? list?)))
  (if (list? key_paras) 
    (setq key   (car key_paras)
          paras (cdr key_paras))
    (setq key   key_paras
          paras nil))
  (check "do" (list key sym? paras (list nil? list?)))
  (defun get (obj key) 
    (if (assoc key obj) 
      (al_get obj key)
      (if (assoc 'parent obj) 
        (get (eval (underscore (al_get obj 'parent))) key)
        nil)))
  (setq value (get obj key))
  (if (func? value) 
    (apply 'value (append (list obj) paras))
    value))

(defun do (obj key_paras / key paras get value) 
  (if (and (list? obj) (= '_inst (caadr obj)) (cls? (eval (cadadr obj)))) 
    (do (eval (cadadr obj)) key_paras)
    (progn 
      (check "do" (list obj (list cls? obj?) key_paras (list sym? list?)))
      (if (list? key_paras) 
        (setq key   (car key_paras)
              paras (cdr key_paras))
        (setq key   key_paras
              paras nil))
      (check "do" (list key sym? paras (list nil? list?)))
      (defun get (obj key) 
        (if (assoc key obj) 
          (al_get obj key)
          (if (assoc 'parent obj) 
            (get (eval (underscore (al_get obj 'parent))) key)
            nil)))
      (setq value (get obj key))
      (if (func? value) 
        (apply 'value (append (list obj) paras))
        value))))

(defun do_set (obj_sym key_paras) 
  (check 
    "do_set"
    (list obj_sym 
          sym?
          (eval obj_sym)
          (list cls? obj?)
          key_paras
          (list sym? list?)))
  (set obj_sym (do (eval obj_sym) key_paras)))
