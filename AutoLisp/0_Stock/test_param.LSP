(defun c:test ()
  (setq	util (vla-get-utility
	       (vla-get-activedocument (vlax-get-acad-object))
	     ) ;_ Fin de vla-get-utility
  ) ;_ Fin de setq
  (vla-getentity util 'ent 'ip "\nSelectionnez l'objet :") ;_ Fin de vla-getentity
  (setq	start (vlax-curve-getStartParam ent)
	end   (vlax-curve-getEndParam ent)
  ) ;_ Fin de setq
  (terpri)
  (princ start)
  (terpri)
  (princ end)
  (terpri)
  (setq	param (vla-getreal util "\nParamètre : ")
	point (vlax-curve-getPointAtParam ent param)
  ) ;_ Fin de setq
  (terpri)
  (princ point)
  (princ)
) ;_ Fin de defun
