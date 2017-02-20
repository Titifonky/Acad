(defun c:dcs ()
  (if (equal dc nil)
    (setq dc 0)
  ) ;_ Fin de if
  (setq	didc
	 (getreal (strcat "\nSpécifiez la distance de décalage ou <"
			  (rtos dc 2 4)
			  ">: "
		  ) ;_ Fin de strcat
	 ) ;_ Fin de getreal
  ) ;_ Fin de setq
  (setq	util (vla-get-utility
	       (vla-get-activedocument (vlax-get-acad-object))
	     ) ;_ Fin de vla-get-utility
  ) ;_ Fin de setq
  (while
    (vl-catch-all-error-p
      (vl-catch-all-apply
	'vla-getentity
	(list util
	      'ent
	      'pt
	      "\nSélectionnez l'objet à décaler:"
	) ;_ Fin de list
      ) ;_ Fin de vl-catch-all-apply
    ) ;_ Fin de vl-catch-all-error-p
     (princ "Aucun objet selectionné")
  ) ;_ Fin de while
  (setq
    pt
     (getpoint "\nSpécifiez un point sur le côté à décaler:")
  ) ;_ Fin de setq
  (if (equal didc nil)
    (setq didc dc)
    (setq dc didc)
  ) ;_ Fin de if
  (setq new_ent (vla-offset ent didc))
  (vla-put-layer
    (car (vlax-safearray->list (vlax-variant-value new_ent)))
    (getvar "clayer")
  ) ;_ Fin de vla-put-layer
  (princ)
) ;_ Fin de defun
