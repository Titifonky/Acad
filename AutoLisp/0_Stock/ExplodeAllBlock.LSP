(defun c:eb (/ cmde setblock k i nb namedel nomentdel obj namedel)
  (setq cmde (GETVAR "CMDECHO"))
  (setvar "CMDECHO" 0)
  (command "_undo" "_m")
  (setq	setblock (ssget "X" (list '(0 . "INSERT")))
	i	 0
  ) ;_ Fin de setq
  (princ "\nDécompose tout les blocks du dessin")
  (if (equal setblock nil)
    (progn
      (princ "\nLe dessin ne contient pas de blocks")
      (exit)
    ) ;_ Fin de progn
    (princ
      (strcat "\nLe dessin contient "
	      (itoa (setq k (sslength setblock)))
	      " block(s)"
      ) ;_ Fin de strcat
    ) ;_ Fin de princ
  ) ;_ Fin de if
  (initget "Oui Non")
  (setq reponse (getkword "\nVoulez vous continuer [Oui/Non] : "))
  (if (= reponse "Oui")
    (progn
      (repeat k
	(setq nomentdel (ssname setblock i))
	(command "_explode" nomentdel)
	(princ
	  (strcat (itoa (1+ i)) " blocks décomposé sur " (itoa k))
	) ;_ Fin de princ
	(princ "\n")
	(setq i (1+ i))
      ) ;_ Fin de repeat
      (princ)
      (princ "\nIl y a ")
      (princ k)
      (princ
	" block(s) qui ont été décomposée(s)"
      ) ;_ Fin de princ
    ) ;_ Fin de progn
  ) ;_ Fin de if
  (command "_undo" "_m")
  (setvar "CMDECHO" cmde)
  (princ)
) ;_ Fin de defun