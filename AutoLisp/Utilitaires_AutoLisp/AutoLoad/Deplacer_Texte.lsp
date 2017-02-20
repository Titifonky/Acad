
(vl-load-com)

(defun c:Dpt (/ RadToDeg sel_courbes pt_depart pt_arrivee angle_rot)

  (defun RadToDeg (ang)
    (/ (* 180.0 ang) pi)
  )

  (setq	cmde   (getvar "CMDECHO")
	snap   (getvar "AUTOSNAP")
	osmode (getvar "OSMODE")
  )
  
  (setvar "CMDECHO" 0)
  (setvar "AUTOSNAP" 0)
  (setvar "OSMODE" 0)

  (setq	p	    (princ "\nSelectionnez les courbes :")
	sel_courbes (ssget)
	pt_depart   (getpoint "\nPoint de départ :")
  )
  
  (setvar "AUTOSNAP" snap)
  (setvar "OSMODE" osmode)

  (setq
    pt_arrivee (getpoint "\nPoint d'arrivée :")
    angle_rot  (if (setq angle_rot (getangle pt_arrivée "\nAngle :"))
		 angle_rot
		 0
	       )
  )

  (setvar "AUTOSNAP" 0)
  (setvar "OSMODE" 0)

  (command "_MOVE" sel_courbes "" pt_depart pt_arrivee)

  (if (> angle_rot 0.0)
    (command "_ROTATE"
	     sel_courbes
	     ""
	     pt_arrivee
	     (RadToDeg angle_rot)
    )
  )

  (setvar "CMDECHO" cmde)
  (setvar "AUTOSNAP" snap)
  (setvar "OSMODE" osmode)

  (princ)
)
