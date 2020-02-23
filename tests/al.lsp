(deftest "al")

(assert-eq '(list (cons 1 3) (cons 5 2) (cons 4 6))
  '(al_make (list 1 3 5 2 4 6)))

(assert-eq '2 '(al_get (list (cons 1 2) (cons 3 4)) 1))
(assert-eq 'nil '(al_get (list (cons 1 2) (cons 3 4)) 4))

(assert-eq '(list (cons 1 2) (cons 3 4) (cons 5 6))
  '(al_insert (list (cons 1 2) (cons 3 4)) 5 6))
(assert-error '(al_insert (list (cons 1 2) (cons 3 4)) 1 2))

(assert-eq '(list (cons 1 2) (cons 3 4) (cons 5 6))
  '(al_upsert (list (cons 1 2) (cons 3 4)) 5 6))
(assert-eq '(list (cons 1 3) (cons 3 4) )
  '(al_upsert (list (cons 1 2) (cons 3 4)) 1 3))

(assert-eq '(list (cons 1 2) (cons 3 6))
  '(al_update (list (cons 1 2) (cons 3 4)) 3 6))
(assert-eq '(list (cons 1 2) (cons 3 4))
  '(al_update (list (cons 1 2) (cons 3 4)) 4 6))

(assert-eq '(list 1 3 5)
  '(al_keys (list (cons 1 2) (cons 3 4) (cons 5 6))))
(assert-error '(al_keys (list 2 4)))

(assert-eq '(list 2 4 6)
  '(al_values (list (cons 1 2) (cons 3 4) (cons 5 6))))

(assert-eq '(al_make '(1 4 2 5 3 6)) 
           '(al_zip '(1 2 3) '(4 5 6)))

(assert-eq '(list (list 'A 1 3 4)(list 'B 2 5) )
  '(al_merge (list (cons 'A 1) (cons 'B 2) (cons 'A 3) (cons 'A 4) (cons 'B 5)))
  )
(assert-error '(al_merge (list 2 4)))

(assert-eq 't '(al_haskey (al_make '(1 4 2 5 3 6)) 1))

(assert-eq '(al_make '(1 nil 2 nil 3 nil))
           '(al_fromkeys (al_make '(1 4 2 5 3 6))))

(assert-eq ''((3 . 4) (1 . 1) (4 . 4))
           '(al_upserts (al_make '(1 2 3 4)) (al_make '(1 1 4 4))))