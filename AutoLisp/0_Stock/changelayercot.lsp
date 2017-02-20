(defun changelayercot
       (layer / calquecourant ini nom_calque liste i reponse)
  (setq	calquecourant (getvar "clayer")
	flag	      1
	ini	      ""
	lay	      (strcase layer)
  ) ;_ Fin de setq
  (nomlayer lay flag)
  (setq	nom_calque
	 '(nil)
	i 0
  ) ;_ Fin de setq
  (repeat (length listcalque)
    (setq nom_calque
	   (append nom_calque
		   (list (nth (nth i listcalque) calque))
	   ) ;_ Fin de append
    ) ;_ Fin de setq
    (setq i (1+ i))
  ) ;_ Fin de repeat
  (cond
    ((zerop (length listcalque))
     (setvar "clayer" calquecourant)
    )
    ((equal (length listcalque) 1) (setvar "clayer" nomcalque))
    ((> (length listcalque) 1)
     (if (equal layer "cot")
       (setq layer "cotation")
     ) ;_ Fin de if
     (if (member calquecourant nom_calque)
       (setq calque calquecourant)
       (progn (setq i 1)
	      (foreach n '("users1" "users2" "users3" "users4" "users5")
		(setvar n "")
	      ) ;_ Fin de foreach
	      (if (<= (length listcalque) 5)
		(setq long (length listcalque))
		(setq long 5)
	      ) ;_ Fin de if
	      (repeat long
		(setvar	(strcat "users" (itoa i))
			(nth (nth (1- i) listcalque) calque)
		) ;_ Fin de setvar
		(setq i (1+ i))
	      ) ;_ Fin de repeat
	      (menucmd "POP25=PERSO")
	      (menucmd "POP25=*")
       ) ;_ Fin de progn
     ) ;_ Fin de if
    )
  ) ;_ Fin de cond
) ;_ Fin de defun