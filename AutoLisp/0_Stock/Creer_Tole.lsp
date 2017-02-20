(defun c:rds (/ util point polyline pt ep_tole ri)
  (prompt " - Créer une tôle")
  (setq	util (vla-get-utility
	       (vla-get-activedocument (vlax-get-acad-object))
	     ) ;_ Fin de vla-get-utility
  ) ;_ Fin de setq
  (while
    (vl-catch-all-error-p
      (vl-catch-all-apply
	'vla-getentity
	(list util
	      'polyline
	      'pt
	      "\nSelectionnez le chemin (polyligne) : "
	) ;_ Fin de list
      ) ;_ Fin de vl-catch-all-apply
    ) ;_ Fin de vl-catch-all-error-p
     (princ "Aucun objet selectionné")
  ) ;_ Fin de while
;;;(setq point (vlax-make-safearray vlax-vbDouble '(0 . 2)))
  (vla-highlight polyline t)
  (setq	point (vlax-safearray->list
		(vlax-variant-value
		  (vla-getpoint
		    util
		    (vlax-3d-point
		      (setq pt (vlax-curve-getClosestPointTo
				 polyline
				 (vlax-safearray->list pt)
			       ) ;_ Fin de vlax-curve-getClosestPointTo
		      ) ;_ Fin de setq
		    ) ;_ Fin de vlax-3d-point
		    "\nSelectionnez le coté interieur de la tôle : "
		  ) ;_ Fin de vla-getpoint
		) ;_ Fin de vlax-variant-value
	      ) ;_ Fin de vlax-safearray->list
  ) ;_ Fin de setq
  (while
    (equal (setq ep_tole (vla-getreal
			   util
			   "\nIndiquez l'epaisseur de la tôle : "
			 ) ;_ Fin de vla-getreal
	   ) ;_ Fin de setq
	   0.0
    ) ;_ Fin de equal
     (princ "La valeur de l'épaisseur doit être supérieur à zéro"
     ) ;_ Fin de princ
  ) ;_ Fin de while
  (setq	ri (vla-getreal
	     util
	     "\nIndiquez la valeur du rayon interieur : "
	   ) ;_ Fin de vla-getreal
  ) ;_ Fin de setq
  (princ (strcat "Epaisseur de la tôle : "
		 (rtos ep_tole)
		 " mm   \\   Rayon interieur : "
		 (rtos ri)
		 " mm"
	 ) ;_ Fin de strcat
  ) ;_ Fin de princ
  (vla-highlight polyline 0)
;;; Fin des entrés de l'utilisateur
;;;
;;;Vecteur de decalage
  (setq	vect_dc	(mapcar	'(lambda (x y) (- x y))
			(inters	point
				(mapcar	'(lambda (x y) (+ x y))
					point
					(setq vect_courb
					       (vlax-curve-getFirstDeriv
						 polyline
						 (vlax-curve-getparamatpoint
						   polyline
						   pt
						 ) ;_ Fin de vlax-curve-getparamatpoint
					       ) ;_ Fin de vlax-curve-getFirstDeriv
					) ;_ Fin de setq
				) ;_ Fin de mapcar
				pt
				(mapcar	'(lambda (x y) (+ x y))
					pt
					(list (* -1 (cadr vect_courb))
					      (car vect_courb)
					      (caddr vect_courb)
					) ;_ Fin de list
				) ;_ Fin de mapcar
				nil
			) ;_ Fin de inters
			point
		) ;_ Fin de mapcar
  ) ;_ Fin de setq

;;;definition de la fonction de decalage
  (defun-q point_dc
	   (point vect dist / pt_rt)
	   (setq pt_rt
		  (mapcar '(lambda (x y) (+ x y))
			  point
			  (list	(* (cadr vect)
				   (setq fact (/ dist (distance '(0 0 0) vect)))
				) ;_ Fin de *
				(* (* -1 (car vect)) fact)
				(* (caddr vect) fact)
			  ) ;_ Fin de list
		  ) ;_ Fin de mapcar
	   ) ;_ Fin de setq
  ) ;_ Fin de defun-q
  (if (> (* (car vect_dc) (car vect_courb)) 0)
    (defun-q-list-set
      'point_dc
      '((point vect dist / pt_rt)
	(setq
	 pt_rt
	 (mapcar
	  '(lambda (x y) (+ x y))
	  point
	  (list
	   (*
	    (* -1 (cadr vect))
	    (setq fact (/ (distance '(0 0 0) vect) dist))
	   ) ;_ Fin de *
	   (* (car vect) fact)
	   (* (caddr vect) fact)
	  ) ;_ Fin de list
	 ) ;_ Fin de mapcar
	)
       )
    ) ;_ Fin de defun-q-list-set
  ) ;_ Fin de if
  (princ)
) ;_ Fin de defun
