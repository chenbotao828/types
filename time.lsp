(defun time_stamp (time_zone)
  (if (nil? time_zone) (setq time_zone 8))
  (- (* 86400.0 (- (getvar "date") 2440588)) (* time_zone 60 60)) 
  )

(defun objectID ()
  (strcat
    (dec2base (fix (time_stamp 8)) 16)
    (dec2base (fix (* (random) (expt 16 16))) 16)
    )
  )
