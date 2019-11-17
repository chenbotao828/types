(deftest "list")

(assert-eq '3 '(len (list 1 2 3)))
(assert-eq '3 '(len "123"))

(assert-eq '2 '(get_nth (list (cons 1 2) (cons 3 4) ) 1) )
(assert-eq '2 '(get_nth (cons 1 2) 1) )
(assert-eq '1 '(get_nth (list 0 1 ) 1) )
(assert-eq "1" '(get_nth "01" 1))

(assert-eq '(list 1 2) '(span (list 1 2 3) 0 2))
(assert-eq '"12" '(span "123" 0 2))
;; TODO span


(assert-eq '0 '(index "123" "1"))
(assert-eq '0 '(index (list 1 2 3) 1))
(assert-eq 'nil '(index (list 1 2 3) 5))

(assert-eq '"123" '(reversed "321") )
(assert-eq '(list 1 2 3) '(reversed (list 3 2 1)) )
(assert-error '(reversed 1) )

(assert-eq '"123" '(consend "3" "12") )
(assert-eq '(list 1 2 3) '(consend 3 (list 1 2)) )
(assert-error '(consend 3 "3") )

(assert-eq '2 '(dot (list (cons 1 2) (cons 3 4)) 1))

(assert-eq '(list (cons 1 2) (cons 3 4) (cons 5 6))
  '(pushed (list (cons 1 2) (cons 3 4)) 5 6))
(assert-eq '(list (cons 1 2) (cons 3 6))
  '(updated (list (cons 1 2) (cons 3 4)) 3 6))
(set 'test_al (list (cons 1 2) (cons 3 4)))
(assert-eq '(list (cons 1 2) (cons 3 4) (cons 5 6))
  '(push 'test_al 5 6))
(set 'test_al (list (cons 1 2) (cons 3 4)))
(assert-eq '(list (cons 1 2) (cons 3 6))
  '(update 'test_al 3 6))
(set 'test_al nil)

(assert-eq '(list (cons 1 2) (cons 3 4) (cons 5 6))
  '(ll2al (list 1 3 5) (list 2 4 6)))
(assert-error '(ll2al (list 1 3 5) (list 2 4 )))
(assert-error '(ll2al 1 (list 2 4 )))

(assert-eq '(list 1 3 5)
  '(al2keys (list (cons 1 2) (cons 3 4) (cons 5 6))))
(assert-error '(al2keys (list 2 4)))

(assert-eq '(list (list 'A 1 3 4)(list 'B 2 5) )
  '(al_merge (list (cons 'A 1) (cons 'B 2) (cons 'A 3) (cons 'A 4) (cons 'B 5)))
  )
(assert-error '(al_merge (list 2 4)))
