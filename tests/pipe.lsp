(deftest "pipe")

(assert-eq '9 '(pipe "123" '(as_list (select int) (select 1+) sum)))
(assert-eq '"123" '(pipe "1a2v3c" '(as_list (where str_isdemical) sum)))

(setq pipe_set_test 1)
(pipe_set 'pipe_set_test '((+ 3) sqrt))
(assert-eq '2 pipe_set_test)
(setq pipe_set_test nil)

(assert-eq 't '(pipe_func? '(lambda (x) (+ x 1))))
(assert-eq 't '(pipe_func? (function (lambda (x) (+ x 1)))))
(assert-eq 't '(pipe_func? (lambda (x) (+ x 1))))
(assert-eq 't '(pipe_func? +))

(assert-eq '+ '(as_func +))
(assert-eq '+ '(as_func '+))

(assert-eq '"1, 2, 3" '(concat '(1 2 3) nil))
(assert-eq '"1/2/3" '(concat '(1 2 3) "/"))

(assert-eq 't '(all '(1 2 3) int?) )
(assert-eq 't '(all '(t t t) nil) )
(assert-eq 'nil '(all '(1 2 3) (lambda (x) (= 1 (% x 2)))) )
(assert-eq 't '(all '(1  3) (lambda (x) (= 1 (% x 2)))) )

(assert-eq 'nil '(any '(1 2 3) str?) )
(assert-eq 't '(any '(t nil nil) nil) )
(assert-eq 't '(any '(1 2 3) (lambda (x) (= 1 (% x 2)))) )
(assert-eq 'nil '(any '(1  3) (lambda (x) (= 2 (% x 2)))) )

(assert-eq 'nil '(where nil str_isdemical) )
(assert-eq '"123" '(sum (where (as_list "1a2v3c") str_isdemical)) )
(assert-eq ''(nil nil nil) '(where '(nil 1 nil 1 nil) nil?) )

(assert-eq '6 '(reduce '(1 2 3) +) )
(assert-eq '6 '(reduce '(1 2 3) (lambda (x y) (+ x y))) )

(assert-eq ''(2 3 4) '(select '(1 2 3) 1+) )
(assert-eq ''(2 3 4) '(select '(1 2 3) (lambda (x) (+ x 1))) )

(assert-eq ''(1 2) '(take '(1 2 3) 2) )
(assert-eq ''(2 3) '(tail '(1 2 3) 2) )

(assert-eq ''(1 2 3) '(take_while '(1 2 3 "1" 4) int?) )
(assert-eq 'nil '(take_while '("2" 1 2 3 "1" 4) int?) )

(assert-eq ''("1" 4) '(skip_while '(1 2 3 "1" 4) int?) )
(assert-eq ''("2" 1 2 3 "1" 4) '(skip_while '("2" 1 2 3 "1" 4) int?) )

(assert-eq ''(1 2 (3) (4)) '(chain '((1) (2 (3)) ((4)))) )

(assert-eq ''(1 (2 3) ((4)) (5) (6 7)) '(chain_with '(1 (2 3) ((4))) '((5) (6 7) ) ))

(assert-eq ''(1 2 3 4) '(flatten '((1) (2 (3)) ((4)))) )

(assert-eq ''(2 3 4) '(skip '(1 2 3 4 ) 1) )

(assert-eq '1 '(first '(1 2 3 4 )) )
(assert-eq '"1" '(first "1234") )
(assert-eq 'nil '(first nil) )

(assert-eq ''(2 3 4) '(rest '(1 2 3 4 )) )
(assert-eq '"234" '(rest "1234") )
(assert-eq 'nil '(rest nil) )

(assert-eq '4 '(last_one '(1 2 3 4 )) )
(assert-eq '"4" '(last_one "1234") )
(assert-eq 'nil '(last_one nil) )

(assert-eq ''(1 3 5) '(slice '(1 2 3 4 5) 0 5 2))
(assert-eq ''(1 3 5) '(slice '(1 2 3 4 5) 0 15 2))
(assert-eq 'nil '(slice nil 0 5 2))

(assert-eq ''(("1" 1 4 7) ("2" 2 5 8) ("0" 0 3 6 9)) 
           '(groupby (range 0 10 1)
             '(lambda (x)
               (cond ((= 0 (% x 3)) "0")
                     ((= 1 (% x 3)) "1")
                     ((= 2 (% x 3)) "2") 
                     ))))
(assert-eq ''((0 0 2 4 6 8) (1 1 3 5 7 9))
           '(groupby (range 0 10 1)
             '(lambda (x)
               (% x 2))))
(assert-eq ''((T 0 1 2 3 4 5 6 7 8 9))
           '(groupby (range 0 10 1) 'int?))

(assert-eq ''(1 2 3) '(dedup '(1 1 2 2 3 3 1 2 3) nil) )
(assert-eq ''(1 2) '(dedup '(1 1 2 2 3 1 2 3) (lambda (x) (% x 2)))) 

(assert-eq ''(1 2 3 1 2 3) '(uniq '(1 1 2 2 3 3 1 2 3) nil) )
(assert-eq ''(1 2 3 2 3) '(uniq '(1 1 2 2 3 1 2 3) (lambda (x) (% x 2)))) 

(assert-eq ''(1 2) '(: 1 2) )

(assert-eq '6 '(length (permutations '(1 2 3) 2)))
(assert-eq '6 '(length (permutation_all '(1 2 3))))
(assert-eq '3 '(length (combinations '(1 2 3) 2)))

(assert-eq '1 '(_or (list nil nil 1)))
(assert-eq 'nil '(_or (list nil nil nil)))
