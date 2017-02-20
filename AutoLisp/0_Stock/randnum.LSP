(defun randnum (/ modulus multiplier increment random)
  (if (not seed)
    (setq seed (getvar "DATE"))
  ) ;_ Fin de if
  (setq	modulus	   65536
	multiplier 25173
	increment  13849
	seed	   (rem (+ (* multiplier seed) increment) modulus)
	random	   (/ seed modulus)
  ) ;_ Fin de setq
) ;_ Fin de defun
