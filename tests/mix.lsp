(deftest "mix")

(assert-eq '3 '(len (list 1 2 3)))
(assert-eq '3 '(len "123"))
(assert-eq '2 '(len "가가"))
(assert-eq '0 '(len nil))

(assert-eq '2 '(get_nth (cons 1 2) 1) )
(assert-eq '1 '(get_nth (list 0 1 ) 1) )
(assert-eq "1" '(get_nth "01" 1))
(assert-eq "가" '(get_nth "0가1" 1))

(assert-eq "1234" '(remove_nth "01234" 0))
(assert-eq 'nil '(remove_nth nil 0))
(assert-eq '2 '(remove_nth (cons 1 2) 0))
(assert-eq '1 '(remove_nth (cons 1 2) 1))
(assert-eq '(list 1 2 3) '(remove_nth (list 0 1 2 3) 0))
(assert-error '(remove_nth (list 0 1 2 3) 111))

(assert-eq "11234" '(replace_nth "01234" 0 "1"))
(assert-eq 'nil '(replace_nth nil 0 1))
(assert-eq '(cons 2 2) '(replace_nth (cons 1 2) 0 2))
(assert-eq '(cons 1 1) '(replace_nth (cons 1 2) 1 1))
(assert-eq '(list 11 1 2 3) '(replace_nth (list 0 1 2 3) 0 11))
(assert-error '(replace_nth (list 0 1 2 3) 111))

(assert-eq "101234" '(insert_nth "01234" 0 "1"))
(assert-eq "11101234" '(insert_nth "01234" 0 "111"))
(assert-eq '(list 1) '(insert_nth nil 0 1))
(assert-eq '(list 222 1 2) '(insert_nth (cons 1 2) 0 222))
(assert-eq '(list 1 111 2) '(insert_nth (cons 1 2) 1 111))
(assert-eq '(list 11 0 1 2 3) '(insert_nth (list 0 1 2 3) 0 11))
(assert-error '(insert_nth (list 0 1 2 3) 111))

(assert-eq '(list 1 2) '(span (list 1 2 3) 0 2))
(assert-eq '"12" '(span "123" 0 2))
(assert-eq '"가1" '(span "가123" 0 2))

(assert-eq '0 '(index "123" "1"))
(assert-eq '0 '(index (list 1 2 3) 1))
(assert-eq 'nil '(index (list 1 2 3) 5))

(assert-eq 'nil '(indexall nil 5))
(assert-eq '(list 0 3) '(indexall "1231" "1" ))
(assert-eq '(list 0 3) '(indexall (list 1 2 3 1) 1 ))

(assert-eq '3 '(rindex "1231" "1"))
(assert-eq '3 '(rindex (list 1 2 3 1) 1))
(assert-eq 'nil '(rindex (list 1 2 3) 5))

(assert-eq '"123" '(reversed "321") )
(assert-eq '(list 1 2 3) '(reversed (list 3 2 1)) )
(assert-error '(reversed 1) )

(assert-eq '"123" '(consend "3" "12") )
(assert-eq '(list 1 2 3) '(consend 3 (list 1 2)) )
(assert-error '(consend 3 "3") )

(assert '(in "1" (list "1" 2 3)))
(assert '(not_in 4 (list 1 2 3)))
(assert '(in "1" "123"))
(assert '(not_in "4" "123"))
(assert '(in 1 (al_make '(1 2 3 4))))
(assert '(not_in 2 (al_make '(1 2 3 4))))
(assert-error '(in 1 "123"))
(assert-error '(in "1" 123))
(assert-error '(not_in "1" 123))

(assert-eq '(sum '("1" "2")) "12")
(assert-eq '(sum '(1 2)) 3)
(assert-eq '(sum '((1 2 3) (4 5 6))) ''(1 2 3 4 5 6))

(assert-eq '(multi "1" 3) '"111")
(assert-eq '(multi 1 3) '3)
(assert-eq '(multi (list 1 2) 3) '(list 1 2 1 2 1 2))

(assert-eq '(int 1.1) 1)
(assert-eq '(int "1.1") 1)
(assert-eq '(float 1) 1.0)
(assert-eq '(float "1") 1.0)


(assert '(random_choice "123"))
(assert '(random_choice (list 1 2 3)))

(assert-eq '"hi " '(remove "hi Tom" "Tom"))
(assert-eq '(list 1 2 3) '(remove (list 1 5 2 3 5 ) 5))
(assert-eq '(list (cons 1 2)) '(remove (list (cons 1 2) (cons 3 4)) 3))

(assert-eq 'nil '(startswith nil "Tom"))
(assert-eq 't '(startswith "123" "1"))
(assert-eq 'nil '(startswith "123" "2"))
(assert-eq 't '(startswith (list 1 2 3) 1))
(assert-eq 'nil '(startswith (list 1 2 3) 2))

(assert-eq '0 '(countit nil "Tom"))
(assert-eq '2 '(countit (list 1 2 1) 1))
(assert-eq '2 '(countit "112" "1"))
(assert-error '(countit "112" 1))

(assert-eq ''(1 1 2 2 3) '(sort_list '( 2 1 2 1 3) <))
(assert-eq ''(3 2 2 1 1) '(sort_list '( 2 1 2 1 3) >))

(assert-eq '(list nil 1 "a" "b" 'AA 'BB - +) 
           '(sort (list 1 nil - 'aa 'bb "a" "b" + )))
(assert-eq ''((1 3) ((1 2) 2) ((1 2) 3)) 
           '(sort '( ((1 2) 3) (1 3) ((1 2) 2) ))
           )

(assert-eq '(reversed (list nil 1 "a" "b" 'AA 'BB - +)) 
           '(sort_by (list 1 nil - 'aa 'bb "a" "b" + ) >>> ) )
(assert-eq ''(((1 2) 3) ((1 2) 2) (1 3)) 
           '(sort_by '( ((1 2) 3) (1 3) ((1 2) 2) ) >>> ) )


(assert-eq ''((1 a "1") (2 b "2") (3 c "3")) 
           '(zip '( (1 2 3) (a b c) ("1" "2" "3") ) ) )
(assert-error '(zip '( (1 2 3) 2 (a b c) ("1" "2" "3") ) ) )
(assert-eq '(al_make (list 0 "0" 1 "1" 2 "2" 3 "3")) '(enumerate "0123" ) )
(assert-eq '(al_make (list 0 0 1 1 2 2 3 3)) '(enumerate (list 0 1 2 3) ) )


(assert-eq 't '(bool (list 0 1 2 3) ) )
(assert-eq 'nil '(bool nil ) )
(assert-eq 'nil '(bool 0) )
(assert-eq 'nil '(bool "") )

