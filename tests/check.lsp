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

(assert '(sym_func? '+))

(assert '(lambda? '(lambda (x) (x))))
(assert '(lambda? (quote (lambda (x) (x)))))
(assert '(lambda? (function (lambda (x) (x)))))

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

(assert '(int? 1))
(assert '(int? 0))
(assert-eq 'nil '(int? 1.1))
(assert '(+int? 1))
(assert '(-int? -1))

(assert '(float? 1.1))
(assert-eq 'nil '(float? 1))

(assert '(file? "altest/altest"))
(assert-eq 'nil '(file? "altest/altestxxx"))

(assert-eq '(check2func nil?) '"NIL")
(assert '(check "test check" 
          (list 1 int? "x" (list str? list?))))
