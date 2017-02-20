(defun c:tl ()

					; Désactivation des variables système

  (setq	cmde	    (getvar "cmdecho")
	acc	    (getvar "osmode")
	liste_point nil
  )
  (setvar "cmdecho" 0)
  (setvar "osmode" 0)
  (princ "\n----- Developper de limon -----")

					; Selection des objets

  (if (setq al_entsel
	     (entsel
	       "\nSelectionnez le profil de référence près du départ :"
	     )
      )
    (setq
      vl_name_og_curve
		       (vlax-ename->vla-object (car al_entsel))
      al_dep_og_curve  (cadr al_entsel)
    )
    (progn (princ "\nVous n'avez rien selectionné\n")
	   (setvar "osmode" acc)
	   (exit)
    )
  )
  (princ
    "\nSelectionnez les nez de marches :"
  )
  (setq al_set (ssget '((0 . "POINT"))))
  (if (not al_set)
    (progn (princ "\nVous n'avez rien selectionné\n")
	   (setvar "osmode" acc)
	   (exit)
    )
  )

  (setvar "osmode" acc)

  (setq
    point_depart
     (getpoint
       "\nIndiquez le point de départ du développé :"
     )
  )

  (setq	decal	  (getdist "\nIndiquez la hauteur de marche (ou distance) :")
	new_decal 0
  )

  (setvar "osmode" 0)

					; Fin de selection des objets

					; Détermine le départ de référence par rapport à l'endroit selectionné sur la courbe

					; Courbe original

  (if (> (vlax-curve-getDistAtPoint
	   vl_name_og_curve
	   (vlax-curve-getClosestPointTo
	     vl_name_og_curve
	     al_dep_og_curve
	   )
	 )
	 (/ (vlax-get-property vl_name_og_curve 'length) 2.0)
      )
    (setq x (float (vlax-get-property vl_name_og_curve 'length)))
    (setq x 0.0)
  )

					; Point de départ

  (setq	point_depart_x (car point_depart)
	point_depart_y (cadr point_depart)
  )


					; Création des points

  (setq i 0)
  (repeat (sslength al_set)
    (setq vla_name (vlax-ename->vla-object (ssname al_set i))
	  position (vlax-curve-getDistAtPoint
		     vl_name_og_curve
		     (vlax-safearray->list
		       (vlax-variant-value
			 (vlax-get-property
			   vla_name
			   'coordinates
			 )
		       )
		     )
		   )
    )
    (if	position
      (setq
	dist (abs (- x position))
      )
      (progn
	(princ "\nCe point n'est pas sur la courbe")
	(if (vlax-method-applicable-p vla_name 'highlight)
	  (vlax-invoke-method vla_name 'highlight 1)
	)
      )
    )
    (if	dist
      (setq liste_point (append liste_point (list dist)))
    )
    (setq i (1+ i))
  )
  (setq liste_point (vl-sort liste_point '<))

  (command "_PLINE" point_depart)

  (foreach dist	liste_point
    (command (list (+ point_depart_x dist)
		   (+ point_depart_y new_decal)
	     )
    )
    (setq new_decal (+ decal new_decal))
    (command (list (+ point_depart_x dist)
		   (+ point_depart_y new_decal)
	     )
    )
  )

  (command "")

  (command "_LINE"
	   point_depart
	   (list (+ point_depart_x (last liste_point)) point_depart_y)
  )
  (command "")

					; Remise à jour des variables système

  (setvar "osmode" acc)
  (setvar "cmdecho" cmde)
  (princ)
)
