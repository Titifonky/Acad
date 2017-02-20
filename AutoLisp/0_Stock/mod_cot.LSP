(defun c:mc (/ util ent ip)
  (setq	util (vla-get-utility
	       (vla-get-activedocument (vlax-get-acad-object))
	     ) ;_ Fin de vla-get-utility
  ) ;_ Fin de setq
  (vla-getentity
    util
    'ent
    'ip
    "\nSelectionnez la cote à modifier :"
  ) ;_ Fin de vla-getentity
  (if (equal echelle nil)
    (setq echelle 0)
  ) ;_ Fin de if
  (if (vlax-property-available-p ent 'scalefactor)
    (progn
      (setq reponse
	     (getint
	       (strcat "\nEchelle de la cote [" (itoa echelle) "] :")
	     ) ;_ Fin de getkword
      ) ;_ Fin de setq
      (if (equal reponse nil)
	(setq reponse echelle)
      ) ;_ Fin de if
      (setq echelle reponse)
      (vla-put-ScaleFactor ent echelle)
    ) ;_ Fin de progn
    (princ "\nCe n'est pas une cote")
  ) ;_ Fin de if
  (princ)
) ;_ Fin de defun
