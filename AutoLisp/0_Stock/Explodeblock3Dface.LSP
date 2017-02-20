(defun c:sb (/ cmde setblock k i nb namedel nomentdel obj namedel)
  (setq cmde (GETVAR "CMDECHO"))
  (setvar "CMDECHO" 0)
  (command "_undo" "_m")
  (setq	setblock (ssget "X" (list '(0 . "INSERT")))
	k	 (sslength setblock)
	i	 0
	nb	 0
	namedel	 nil
  )
  (setq nbblock (sslength setblock))
  (princ
    (strcat "\nLe dessin contient " (itoa nbblock) " block(s)")
  )
  (initget "Oui Non")
  (setq reponse (getkword "\nVoulez vous continuer [Oui/Non] : "))
  (if (= reponse "Oui")
    (progn
      (repeat k
	(setq noment
	       (cdr
		 (assoc
		   -1
		   (entget
		     (cdr
		       (assoc
			 -2
			 (entget (tblobjname
				   "BLOCK"
				   (cdr (assoc 2 (entget (ssname setblock i))))
				 )
			 )
		       )
		     )
		   )
		 )
	       )
	)
	(setq nomentdel (ssname setblock i))
	(while noment
	  (setq obj (cdr (assoc 0 (entget noment))))
	  (if (= obj "3DFACE")
	    (setq namedel nomentdel)
	  )
	  (if (/= namedel nil)
	    (progn
	      (command "_explode" namedel)
	      (setq nb (1+ nb))
	    )
	  )
	  (setq namedel nil)
	  (setq noment (entnext noment))
	)
	(setq i (1+ i))
      )
      (princ)
      (princ "\nIl y a ")
      (princ nb)
      (princ
	" block(s) contenat des '3DFace' qui ont été décomposée(s)"
      )
    )
  )
  (command "_undo" "_m")
  (setvar "CMDECHO" cmde)
  (princ)
)