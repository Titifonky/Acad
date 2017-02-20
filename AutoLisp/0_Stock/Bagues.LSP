(defun c:ts ()
  (setq	x      0
	y      0
	nb     39
	lst    (list 12.734    13.052	 13.37	   13.689    14.007
		     14.326    14.644	 14.963	   15.281    15.6
		     15.918    16.236	 16.554	   16.873    17.191
		     17.509    17.828	 18.146	   18.465    18.783
		     19.101    19.42	 19.738	   20.056    20.375
		     20.693    21.011	 21.33	   21.648    21.966
		     22.285    22.603	 22.921	   23.24     23.877
		    )
	texte  '(
		 (0 . "MTEXT")
		 (100 . "AcDbEntity")
		 (410 . "Model")
		 (8 . "0")
		 (100 . "AcDbMText")
		 (10)
		 (40 . 3.5)
		 (41 . 10)
		 (46 . 0.0)
		 (71 . 5)
		 (72 . 5)
		 (1)
		 (7 . "Standard")
		 (11 1.0 0.0 0.0)
		 (50 . 0.0)
		)
	cercle '(
		 (0 . "CIRCLE")
		 (100 . "AcDbEntity")
		 (410 . "Model")
		 (8 . "0")
		 (100 . "AcDbCircle")
		 (10)
		 (40)
		)
  )
  (foreach di lst
    (setq nb (1+ nb))
    (setq txt (subst (list 10 (float x) (float y) 0) '(10) texte)
	  txt (subst (cons 1 (itoa nb)) '(1) txt)
    )
    (setq crc (subst (list 10 (float x) (float y) 0) '(10) cercle)
	  crc (subst (cons 40 (/ di 2.0)) '(40) crc)
    )
    (entmake txt)
    (entmake crc)
    (setq txt (subst (list 10 (float x) (float (- y 18)) 0) '(10) texte)
	  txt (subst (cons 1 (rtos di)) '(1) txt)
	  txt (subst '(40 . 2.5) '(40 . 4.0) txt)
    )
    (setq crc (subst (list 10 (float x) (float y) 0) '(10) cercle)
	  crc (subst (cons 40 (+ (/ di 2.0) 3)) '(40) crc)
    )
    (entmake txt)
    (entmake crc)
    (command "_-hatch" "p" "s" (list x (+ y (/ di 2.0) 1.0)) "")
    (setq x (+ x 35))
    (if	(> x 140)
      (setq x 0
	    y (- y 40)
      )
    )
  )
  (princ)
)