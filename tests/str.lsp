(deftest "str")

(assert-eq '(gbk_lst2str (gbk_str2lst "123 abc °¡"))
           '"123 abc °¡")

(assert-error '(str_split "1234" ""))
(assert-eq '(list "" "" "" "") '(str_split "   " " "))
(assert-eq '(list "1" "2" "3" "4" "") '(str_split "1 2 3 4 " " "))
(assert-eq '(list "1" "2" "3" "4") '(str_split "1 2 3 4 " nil))
(assert-eq '(list "" "1" "2" "3" "4" "") '(str_split "aa1aa2aa3aa4aa" "aa"))
(assert-eq '(list "" "1" "2" "3" "4" "")
           '(str_split "°¡1°¡2°¡3°¡4°¡" "°¡"))

(assert-eq '(list "1" "2" "3" "4") '(str_re_split "1,2.3,4"  ",|."))

(assert-eq '(list "1" "2" "3" "4") '(str_splits "1,2.3,4" (list "," ".")))
(assert-eq '(list "1" "2" "3" "4") '(str_splits "1A2B3AB4" (list "A" "B" "AB")))

(assert-eq '(list "1" "2" "3" "4") '(str_splitlines "1\n2\n3\n4"))
(assert-eq '(list "1234") '(str_splitlines "1234"))

(assert-eq '"1 2 3" '(str_join " " (list "1" "2" "3")))
(assert-eq '"123" '(str_join "" (list "1" "2" "3")))
(assert-eq '"1aa2aa3" '(str_join "aa" (list "1" "2" "3")))
(assert-eq '"1°¡2°¡3" '(str_join "°¡" (list "1" "2" "3")))
(assert-eq '"" '(str_join "aaa" nil))

(assert-eq '"1 2 3" '(str_replace "1a2a3" "a" " "))
(assert-eq '"1 2 3" '(str_replace "1°¡2°¡3" "°¡" " "))
(assert-eq '"" '(str_replace "" "a" " "))
(assert-eq '"" '(str_replace "" "" ""))

(assert-eq '"11a22a°¡" '(str_format "{a}a{b}a{c}" (list (cons "a" 11)
                                                    (cons "b" 22)
                                                    (cons "c" "°¡"))))
(assert-eq '"11a22a°¡" '(str_format "{0}a{1}a{2}" (list 11 "22" "°¡")))

(assert-eq '"Hi" '(str_capitalize "hi"))
(assert-eq '"H" '(str_capitalize "h"))
(assert-eq '"HI" '(str_capitalize "HI"))
(assert-eq '"°¡" '(str_capitalize "°¡"))


(assert-eq '"--a--" '(str_center "a" 5 "-"))
(assert-eq '"--a---" '(str_center "a" 6 "-"))

(assert-eq '7 '(str_count "--a---" "" ))
(assert-eq '5 '(str_count "--a---" "-" ))
(assert-eq '2 '(str_count "--a---" "--" ))
(assert-eq '0 '(str_count "--a---" "----" ))

(assert-eq 't '(str_startwith "--a---" "--" ))
(assert-eq 'nil '(str_startwith "--a---" "n" ))
(assert-eq 'nil '(str_startwith "" "n" ))
(assert-eq 't '(str_endswith "--a---" "---" ))
(assert-eq 'nil '(str_endswith "" "n" ))

(assert-eq 't '(str_isalpha "aAZz" ))
(assert-eq 'nil '(str_isalpha "1aAZz" ))
(assert-eq 'nil '(str_isalpha "°¡" ))
(assert-eq 'nil '(str_isalpha "aa°¡1" ))

(assert-eq 't '(str_isdigit "123" ))
(assert-eq 'nil '(str_isdigit "a123" ))

(assert-eq 'nil '(str_isalnum "aa°¡1" ))
(assert-eq 't '(str_isalnum "aa1" ))

(assert-eq 'nil '(str_isspace "aA a°¡1" ))
(assert-eq 't '(str_isspace " \t \r \n" ))

(assert-eq 't '(str_islower "aa1°¡a" ))
(assert-eq 't '(str_islower "a" ))
(assert-eq 'nil '(str_islower "aAa°¡1" ))
(assert-eq 'nil '(str_islower "°¡1" ))
(assert-eq 'nil '(str_islower "A" ))

(assert-eq 'nil '(str_isupper "aa1°¡a" ))
(assert-eq 'nil '(str_isupper "a" ))
(assert-eq 't '(str_isupper "BAC°¡1" ))
(assert-eq 'nil '(str_isupper "aAa°¡1" ))
(assert-eq 'nil '(str_isupper "°¡1" ))
(assert-eq 't '(str_isupper "A" ))

(assert-eq 't '(str_istitle "Hello" ))
(assert-eq 't '(str_istitle "Hello World!" ))
(assert-eq 'nil '(str_istitle "Hello WORLD!" ))
(assert-eq 'nil '(str_istitle "HELLO" ))
(assert-eq 'nil '(str_istitle "" ))
(assert-eq 't '(str_istitle "A" ))
(assert-eq 't '(str_istitle "Hi!~" ))
(assert-eq 'nil '(str_istitle "Hi!i~" ))
(assert-eq 't '(str_istitle "H!!!" ))
(assert-eq 'nil '(str_istitle "!iii" ))

(assert-eq '"hi" '(str_ljust "hi" 1 nil ))
(assert-eq '"hi---" '(str_ljust "hi" 5 "-" ))
(assert-error '(str_ljust "hi" 5 "--" ))

(assert-eq '"hi" '(str_rjust "hi" 1 nil ))
(assert-eq '"---hi" '(str_rjust "hi" 5 "-" ))
(assert-error '(str_rjust "hi" 5 "--" ))

(assert-eq '"hi°¡~°¡" '(str_lower "Hi°¡~°¡"))
(assert-eq '"" '(str_lower ""))

(assert-eq '"HI°¡~°¡" '(str_upper "Hi°¡~°¡"))
(assert-eq '"" '(str_upper ""))

(assert-eq '"bzcdea" '(str_lstrip "abzcdea" "ad"))
(assert-eq '"abzcdea" '(str_lstrip " abzcdea" nil))
(assert-eq '"abzcde" '(str_rstrip "abzcdea" "ad"))
(assert-eq '"abzcdea" '(str_rstrip "abzcdea " nil))
(assert-eq '"bzcde" '(str_strip "abzcdea" "ad"))
(assert-eq '"abzcdea" '(str_strip " abzcdea " nil))

(assert-eq '"z" '(str_max "abzcde"))
(assert-eq '"°¡" '(str_max "ab°¡zcde"))
(assert-eq '"" '(str_max ""))

(assert-eq '"a" '(str_min "abzcde"))
(assert-eq '"a" '(str_min "ab°¡zcde"))
(assert-eq '"" '(str_min ""))

(assert-eq '0 '(str_find "abzcdea" "a"))
(assert-eq '0 '(str_find "" ""))
(assert-eq '6 '(str_rfind "abzcdea" "a"))
(assert-eq 'nil '(str_find "abzcdea" "1"))
(assert-eq 'nil '(str_rfind "abzcdea" "1"))

(assert-eq '"azAZ123" '(str_swapcase "AZaz123"))

(assert-eq '"Azaz123" '(str_title "AZaz123"))
(assert-eq '"  Aa\nZaz12 3" '(str_title "  Aa\nzAz12 3"))
(assert-eq '"  AÊÇa ZazÕâ12z12 3Ö®a \tÎÒ1ÊÇ" '(str_title "  AÊÇa zAzÕâ12z12 3Ö®A \tÎÒ1ÊÇ"))

(assert-eq 'nil '(str_isdemical "AZaz123"))
(assert-eq 't '(str_isdemical "123"))

