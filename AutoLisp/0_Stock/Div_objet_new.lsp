(princ "\nDIO - Distribuer des objets sur une courbe")

(defun c:dio (/		 activ_doc  ent	       ip	  entref     pt_insertion
	      nb_div	 reponse    start      end	  div	     param	array_point
	      vecteur	 rangle	    newent     point	  pt_deplace
	     )
  ;;Chargement des fonctions VisualLisp
  (vl-load-com)

  ;;Enregistrement des parametres
  (setq	cmde (getvar "cmdecho")
	acc  (getvar "osmode")
  )

  ;;Selection de l'entité de base
  (setq sel (entsel "\nSelectionnez l'objet de base :")
	entbase_name (car sel)
	entbase_ip (cadr sel)
	entbase_vlaname (vlax-ename->vla-object entbase_name))
  
  (vla-highlight entbase_vlaname 1)

  ;;Selection de l'entité à copier
  (setq sel (entsel "\nSelectionnez l'objet à distribuer :")
	entcopy_name (car sel)
	entcopy_ip (cadr sel)
	entcopy_vlaname (vlax-ename->vla-object entcopy_name))
  
  (vla-highlight entcopy_vlaname 1)

  (setq	ip (getpoint "\nSelectionnez le point de reférence :")
	nb_div (getint "\nNombre de segments :"))
  )

  ;;Desactivation des accrochages aux objets
  (setvar "cmdecho" 0)
  (setvar "osmode" 0)
  
  (initget "Oui Non")
  (setq reponse (getkword "\nAligner les objets sur la courbe [Oui/Non]: "))
  
  (setq	start	    (vlax-curve-getStartParam entbase_vlaname)
	end	    (vlax-curve-getEndParam entbase_vlaname)
	div	    (/ (abs (- (float end) (float start))) nb_div)
	param	    0.0)
  )
  (if (< end start)
    (setq start end)
  )
  (if (equal reponse "Oui")
    (progn (setq param start)
	   (repeat (1- nb_div)
	     (setq param (+ param div))
	     (setq point   (vlax-curve-getPointAtParam ent param)
		   vecteur (vlax-curve-getfirstDeriv ent param)
	     )
;;;	(setq vecteur (mapcar '(lambda (x) (* x 100000)) vecteur))
	     (setq rangle (angle '(0 0) (list (car vecteur) (cadr vecteur))))
	     (setq newent     (vla-copy entref)
		   pt_deplace (vlax-make-variant (vlax-safearray-fill array_point point))
	     )
	     (vla-move newent pt_insertion pt_deplace)
	     (vla-rotate newent pt_deplace rangle)
	   )
    )
    ;; Si réponse non
    (progn
      (setq param start)
      (repeat (1- nb_div)
	(setq param (+ param div)
	      point (vlax-curve-getPointAtParam ent param)
	)
	(setq newent (vla-copy entref))
	(vla-move newent pt_insertion (vlax-make-variant (vlax-safearray-fill array_point point)))
      )
    )
  )
  (vla-highlight ent 0)
  (vla-highlight entref 0)
  (setvar "osmode" acc)
  (setvar "cmdecho" cmde)
  (princ)
)

(defun c:mio (/		 activ_doc  ent	       ip	  entref     pt_insertion
	      nb_div	 reponse    start      end	  div	     param	array_point
	      vecteur	 rangle	    newent     point	  pt_deplace
	     )
  ;;Chargement des fonctions VisualLisp
  (vl-load-com)

  ;;Enregistrement des parametres
  (setq	cmde (getvar "cmdecho")
	acc  (getvar "osmode")
  )
  
  (cond
    ((equal choix "Div")
     	(setq indication ""
	      nb_div (getint "\nNombre de segments :"))
     )
    ((equal choix "Mes")
     	)
     )
  )
  ;;Selection de l'entité de base
  (setq sel (entsel (strcat "\nSelectionnez l'objet de base :" indication))
	entbase_name (car sel)
	entbase_ip (cadr sel)
	entbase_vlaname (vlax-ename->vla-object entbase_name))
  
  (vla-highlight entbase_vlaname 1)

  ;;Selection de l'entité à copier
  (setq sel (entsel "\nSelectionnez l'objet à distribuer :")
	entcopy_name (car sel)
	entcopy_ip (cadr sel)
	entcopy_vlaname (vlax-ename->vla-object entcopy_name))
  
  (vla-highlight entcopy_vlaname 1)

  (setq	ip (getpoint "\nSelectionnez le point de reférence :")
	dist (getdist "\nDistance entre les objets :")

  ;;Desactivation des accrochages aux objets
  (setvar "cmdecho" 0)
  (setvar "osmode" 0)
  
  (initget "Oui Non")
  (setq reponse (getkword "\nAligner les objets sur la courbe [Oui/Non]: "))
  
  (setq	start	    (vlax-curve-getStartParam entbase_vlaname)
	end	    (vlax-curve-getEndParam entbase_vlaname)
	div	    (/ (abs (- (float end) (float start))) nb_div)
	param	    0.0)
  )
  (if (< end start)
    (setq start end)
  )
  (if (equal reponse "Oui")
    (progn (setq param start)
	   (repeat (1- nb_div)
	     (setq param (+ param div))
	     (setq point   (vlax-curve-getPointAtParam ent param)
		   vecteur (vlax-curve-getfirstDeriv ent param)
	     )
;;;	(setq vecteur (mapcar '(lambda (x) (* x 100000)) vecteur))
	     (setq rangle (angle '(0 0) (list (car vecteur) (cadr vecteur))))
	     (setq newent     (vla-copy entref)
		   pt_deplace (vlax-make-variant (vlax-safearray-fill array_point point))
	     )
	     (vla-move newent pt_insertion pt_deplace)
	     (vla-rotate newent pt_deplace rangle)
	   )
    )
    ;; Si réponse non
    (progn
      (setq param start)
      (repeat (1- nb_div)
	(setq param (+ param div)
	      point (vlax-curve-getPointAtParam ent param)
	)
	(setq newent (vla-copy entref))
	(vla-move newent pt_insertion (vlax-make-variant (vlax-safearray-fill array_point point)))
      )
    )
  )
  (vla-highlight ent 0)
  (vla-highlight entref 0)
  (setvar "osmode" acc)
  (setvar "cmdecho" cmde)
  (princ)
)

 ;|«Visual LISP© Format Options»
(100 2 40 2 nil "Fin de " 100 9 0 0 0 T nil nil T)
;*** NE PAS AJOUTER de texte au-dessous du commentaire! ***|;
