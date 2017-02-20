(defun c:dpb ()
  (prompt "Routine spécial Hdvm")
  (setq	obj_dp (vlax-ename->vla-object
		 (car (entsel "\nSelectionnez le bloc à déplacer :"))
	       ) ;_ Fin de vlax-ename->vla-object
  ) ;_ Fin de setq
  (vla-highlight obj_dp t)
  (setq
    pt_obj_1 (vlax-safearray->list
	       (vlax-variant-value
		 (vla-get-InsertionPoint
		   (setq obj_1
			  (vlax-ename->vla-object
			    (car
			      (entsel
				"\nSelectionnez le bloc premier bloc de référence :"
			      ) ;_ Fin de entsel
			    ) ;_ Fin de car
			  ) ;_ Fin de vlax-ename->vla-object
		   ) ;_ Fin de setq
		 ) ;_ Fin de vla-get-InsertionPoint
	       ) ;_ Fin de vlax-variant-value
	     ) ;_ Fin de vlax-safearray->list
  ) ;_ Fin de setq
  (vla-highlight obj_1 t)
  (setq
    pt_obj_2   (vlax-safearray->list
		 (vlax-variant-value
		   (vla-get-InsertionPoint
		     (vlax-ename->vla-object
		       (car
			 (entsel
			   "\nSelectionnez le bloc deuxième bloc de référence :"
			 ) ;_ Fin de entsel
		       ) ;_ Fin de car
		     ) ;_ Fin de vlax-ename->vla-object
		   ) ;_ Fin de vla-get-InsertionPoint
		 ) ;_ Fin de vlax-variant-value
	       ) ;_ Fin de vlax-safearray->list
    sa	       (vlax-make-safearray vlax-vbdouble '(0 . 2))
    new_insert
	       (vlax-safearray-fill
		 sa
		 (mapcar '(lambda (x y) (/ (+ x y) 2.0))
			 pt_obj_1
			 pt_obj_2
		 ) ;_ Fin de mapcar
	       ) ;_ Fin de vlax-safearray-fill
  ) ;_ Fin de setq
  (vla-highlight obj_dp 0)
  (vla-highlight obj_1 0)
  (vla-put-InsertionPoint obj_dp new_insert)
) ;_ Fin de defun
