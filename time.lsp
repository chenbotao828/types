(defun time_stamp (time_zone)
  (if (nil? time_zone) (setq time_zone 8))
  (- (* 86400.0 (- (getvar "date") 2440588)) (* time_zone 60 60)) 
  )
