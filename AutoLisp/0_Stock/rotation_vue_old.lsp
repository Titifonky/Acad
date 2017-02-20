(defun c:rv ()
  (setq	pt1	 (getpoint "\nIndiquez l'angle de reference :")
	anglbase (getangle pt1)
	cmde	 (getvar "cmdecho")
	rd	 (/ 180.000000000 pi)
  ) ;_ Fin de setq
  (setvar "angbase" (* anglbase rd))
  (setvar "cmdecho" 0)
  (setq
    anglrot (* (getorient pt1 "\nIndiquez l'angle de décalage :") rd)
  ) ;_ Fin de setq
  (command "_UCS" "_z" anglrot)
  (command "_PLAN" "courant")
  (setvar "angbase" 0)
  (setvar "cmdecho" cmde)
) ;_ Fin de defun
