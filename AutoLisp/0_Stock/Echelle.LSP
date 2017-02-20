(defun c:ec (/ cmde echelle facteur ech)
  (prompt "Echelle")
  (setq cmde (GETVAR "CMDECHO"))
  (setvar "CMDECHO" 0)
  (setq
    echelle
     (strcat
       (rtos
	 (setq facteur (/ 1.0
			  (setq	ech
				 (getreal "\nDonnez le facteur d'echelle 1/x : "
				 ) ;_ Fin de getreal
			  ) ;_ Fin de setq
		       ) ;_ Fin de /
	 ) ;_ Fin de setq
	 2
	 8
       ) ;_ Fin de rtos
       "xp"
     ) ;_ Fin de strcat
  ) ;_ Fin de setq
  (princ "\nFacteur d'echelle :")
  (princ facteur)
  (princ (strcat "  -  Echelle : 1/" (rtos ech 2 0) "ème"))
  (command "_zoom" "e" echelle)
  (setvar "CMDECHO" cmde)
  (princ)
) ;_ Fin de defun
