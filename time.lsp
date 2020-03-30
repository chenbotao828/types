(defun time_stamp ( / time_zone)
  (setq time_zone (/ (- 0 (getvar "timezone")) 1000))
  (- (* 86400.0 (- (getvar "date") 2440588)) (* time_zone 60 60)) 
  )

(defun objectid ( / random_str)
  (defun random_str (l / sl ret)
    (setq ret ""
          sl '("0" "1" "2" "3" "4" "5" "6" "7" "8" "9" "a" "b" "c" "d" "e" "f"))
    (repeat l 
           (setq ret (strcat ret (nth (fix (* (random) 16)) sl)))
            )
    ret
    )
  (strcat
    (dec2base (fix (time_stamp)) 16)
    (random_str 16)
    )
  )
