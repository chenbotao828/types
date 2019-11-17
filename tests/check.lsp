(deftest "check types")

(assert '(nil? nil))
(assert '(not (nil? t)))

(assert '(sym? 'HI))
(assert '(not (sym? "HI")))

(assert '(str? "Hi"))
(assert '(not (str? 1)))

(assert '(list? (list 1 2 3)))
(assert '(not (list? '())))

(assert '(func? +))
(assert '(func? (lambda (x) (+ x 1))))
(assert '(not (func? "HI")))

(assert '(dot_pair? (cons 1 2)))
(assert '(dot_pair? '(1 . 2)))
(assert '(not (dot_pair? (list 1 2))))

(assert '(al? (list (cons 1 2))))
(assert '(al? '((1 . 2))))
(assert '(not (al? (list 1 2))))
(assert '(not (al? (list (cons 1 2) 2))))

(assert '(num? 1))
(assert '(num? 1.1))
(assert '(num? 1.22e2))
(assert '(not (num? "1")))

(assert '(file? "altest/altest"))
(assert-eq 'nil '(file? "altest/altestxxx"))
