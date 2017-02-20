
(vl-load-com)

;;; Variables globales pour la saisie
(setq Var_NomBloc (if Var_NomBloc
		    Var_NomBloc
		    "b"
		  )
      Var_LgDep	  (if Var_LgDep
		    Var_LgDep
		    50
		  )
      Var_LgDiv	  (if Var_LgDiv
		    Var_LgDiv
		    110
		  )
)

(defun Diviser_Courbe (/	p	 sel_courbes	   NomBloc
		       LgDep	LgDiv	 i	  Courbe   Lg
		       LgDiv	Nb	 dist	  distpt
		      )

  (setq	cmde (getvar "CMDECHO")
	snap (getvar "AUTOSNAP")
	osmode (getvar "OSMODE")
  )
  (setvar "CMDECHO" 0)
  (setvar "AUTOSNAP" 0)
  (setvar "OSMODE" 0)
  (and
    (setq p	      (princ "\nSelectionnez les courbes :")
	  sel_courbes (ssget '((-4 . "<OR")
			       (0 . "LINE")
			       (0 . "LWPOLYLINE")
			       (0 . "ARC")
			       (0 . "POLYLINE")
			       (0 . "SPLINE")
			       (-4 . "OR>")
			      )
		      )
    )
    (setq NomBloc     (getstring
			(strcat	"\nIndiquez le nom du bloc à aligner <"
				Var_NomBloc
				">: "
			)
		      )
	  Var_NomBloc (if
			(or (= NomBloc "") (= NomBloc " ") (= NomBloc nil))
			 Var_NomBloc
			 NomBloc
		      )
    )
    (setq LgDep	    (getreal
		      (strcat "\nIndiquez le décalage de départ <"
			      (rtos Var_LgDep 2)
			      ">: "
		      )
		    )
	  Var_LgDep (if	LgDep
		      LgDep
		      Var_LgDep
		    )
    )
    (setq LgDiv	    (getreal
		      (strcat "\nIndiquez l'intervalle maximum <"
			      (rtos Var_LgDiv 2)
			      ">: "
		      )
		    )
	  Var_LgDiv (if	LgDiv
		      LgDiv
		      Var_LgDiv
		    )
    )
  )

  (setq i 0)

  (repeat (sslength sel_courbes)
    (setq Courbe (ssname sel_courbes i)
	  Lg	 (Courbe-Longueur Courbe)
	  LgDiv	 (- Lg (* 2.0 Var_LgDep))
	  Nb	 (1+
		   (fix
		     (/	LgDiv
			Var_LgDiv
		     )
		   )
		 )
	  dist	 (/ LgDiv Nb)
	  distpt Var_LgDep
    )
    (if	(> Nb 1)
      (progn
	(command
	  "_INSERT"
	  Var_NomBloc
	  (Courbe-PointALaDistance Courbe Var_LgDep nil)
	  1
	  1
	  (RadToDeg (Courbe-AngleALaDistance Courbe Var_LgDep nil))
	)
	(command "_INSERT"
		 Var_NomBloc
		 (Courbe-PointALaDistance Courbe Var_LgDep t)
		 1
		 1
		 (RadToDeg (Courbe-AngleALaDistance Courbe Var_LgDep t))
	)
      )
      (setq dist (* Lg 0.5)
	    distpt 0.0
	    Nb	 2
      )
    )

    (repeat (1- Nb)
      (setq distpt (+ distpt dist))
      (command "_INSERT"
	       Var_NomBloc
	       (Courbe-PointALaDistance Courbe distpt nil)
	       1
	       1
	       (RadToDeg (Courbe-AngleALaDistance Courbe distpt nil))
      )
    )


    ;;(command "DIVISER" Courbe "b" Var_NomBloc "o" Nb)
    (setq i (1+ i))
  )

  (setvar "CMDECHO" cmde)
  (setvar "AUTOSNAP" snap)
  (setvar "OSMODE" osmode)

  (princ)
  (princ)
)

(defun c:divv (/)
  (Diviser_Courbe)
)
