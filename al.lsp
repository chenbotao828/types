;; Assoc List
;; get  remove  replace  insert
;; dot   x   al_update al_upsert


(defun al_make (lst / ret )
  (check "al_make" (list lst list?))
  (while lst
         (setq ret (append ret (list (cons (car lst) (cadr lst))))
               lst (cddr lst)))
  ret
  )
(setq l2al al_make)

(defun al_get (x key)
  (cdr (assoc key x)))
(setq dot al_get)

(defun al_insert (al key value)
  (check "al_insert" (list al (list nil? al?)))
  (if (dot al key)
      (*ERROR* (strcat "key existed in al_insert :" (str key)))
      (consend (cons key value) al)))

(defun al_remove (al key)
  (check "al_remove" (list al (list nil? al?)))
  (vl-remove-if (function (lambda (i) (== key (car i)))) al)
    )

(defun al_upsert (al key value)
  (check "al_upsert" (list al (list nil? al?)))
  (cons (cons key value) (vl-remove (assoc key al) al)))

(defun al_update (al k v / ret)
  (check "al_update" (list al (list nil? list?)))
  (setq ret (al_upsert al k v))
  (if (= (length al) (length ret))
      ret al))

(defun al_keys (al / ret)
  (check "al_keys" (list al (list nil? al?)))
  (while al (setq ret (append ret (list (car (car al))))
               al (cdr al)))
  ret
  )

(defun al_values (al / ret)
  (check "al_values" (list al (list nil? al?)))
  (while al (setq ret (append ret (list (cdr (car al))))
                  al (cdr al)))
  ret
  )

(defun al_zip (l1 l2)
  (check "al_zip" (list l1 (list list? nil?) l2 (list list? nil?)))
  (mapcar 'cons l1 l2)
  )
(setq ll2al al_zip)

(defun al_haskey (al key)
  (check "al_haskey" (list al al?))
  (in key (al_keys al))
  )

(defun al_fromkeys (al / ret)
  (check "al_fromkeys" (list al al?))
  (while al
         (setq ret (cons (cons (caar al) nil) ret)
               al (cdr al))
         )
  (sort ret)
  )

(defun al_merge (al / kvs k v vs)
  ;; (al_merge '((A . 1) (B . 2) (A . 3) (A . 4) (B . 3)))
  ;; -> ((A 1 3 4) (B 2 3))
  (check "al_merge" (list al al?))
  (setq kvs nil)
  (foreach l al
           (progn
             (setq k (car l)
                   v (cdr l)
                   vs (dot kvs k))
             (if vs (setq kvs (al_upsert kvs k (append vs (list v))))
                 (setq kvs (al_upsert kvs k (list v))))))
  (sort kvs)
  )

(defun al_upserts(al al2 / i k v)
  (check "al_upserts" (list al al? al2 al?))
  (while al2 (setq i (car al2)
                   k (car i)
                   v (cdr i)
                   al (al_upsert al k v)
                   al2 (cdr al2)
                   )
         )
  al
  )

(setq setattr al_upsert)
(setq getattr al_get)
(setq delattr remove)
