(deftest "num")

(assert-eq '(range 0 3 1) ''(0 1 2))
(assert-eq '(range 3 0 -1) ''(3 2 1))

(assert-eq '(round 1.1) '1)
(assert-eq '(round 1.5) '2)

(setq {test_random} (random))
(assert '(and (>= {test_random} 0) (<= {test_random} 1)))

(setq {test_random} (random_range 0 100))
(assert '(and (>= {test_random} 0) (<= {test_random} 100)))
(setq {test_random} nil)

(assert-eq '(math_floor 1.9) 1)
(assert-eq '(math_ceil 1.1) 2)

(assert-eq '(log (math_exp 12)) '12.0)
(assert-eq '(log10 100) '2.0)
(assert-eq '(math_sin (/ pi 2)) '1.0)
(assert-eq '(math_cos pi) '-1.0)
(assert-eq '(math_tan 0) '0.0)
(assert-eq '(math_asin 1) '(/ pi 2))
(assert-eq '(math_acos 1) '0.0)
(assert-eq '(math_atan 0) '0.0)
(assert-eq '(math_degree pi) '180.0)
(assert-eq '(math_radians 180) 'pi)

(assert-eq '(list 2.0 -3.1) '(math_divmod -23.1 -10))
(assert-eq '(list -3.0 6.9) '(math_divmod -23.1 10))
(assert-eq '(list 2.0 3.1) '(math_divmod 23.1 10))
(assert-eq '(list -3.0 -6.9) '(math_divmod 23.1 -10))

(assert-eq '"101" '(dec2base 5 2))
(assert-eq '5 '(base2dec "101" 2))
(assert-eq '"32" '(base2base "1A"  16 8))
(assert-eq '"1A" '(base2base "32"  8 16))