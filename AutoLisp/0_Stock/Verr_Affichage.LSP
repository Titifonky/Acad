(defun c:fe ()
  (initget "V A")
  (setq	reponse
	 (getkword
	   "\nVerrouiller les fen�tres / Activer les fen�tres [V/A] : "
	 ) ;_ Fin de getkword
  ) ;_ Fin de setq
  (if (equal reponse "V")
    (setq active -1)
    (setq active 0)
  ) ;_ Fin de if
  (princ "\nSelectionnez les fen�tre :")
  (setq	set_ent	(ssget)
	i 0
	set_name '(nil)
  ) ;_ Fin de setq
  (if (not set_ent)
    (progn (princ "\nVous n'avez rien selectionn�") (exit))
  ) ;_ Fin de if
  (repeat (sslength set_ent)
    (setq ent (ssname set_ent i))
    (if	(equal (vla-get-ObjectName (vlax-ename->vla-object ent))
	       "AcDbViewport"
	) ;_ Fin de equal
      (setq
	set_name (append set_name (list (vlax-ename->vla-object ent)))
      ) ;_ Fin de setq
    ) ;_ Fin de if
    (setq i (1+ i))
  ) ;_ Fin de repeat
  (setq set_name (cdr set_name))
  (if (not set_name)
    (progn (princ "\nCet objet n'est pas une fen�tre !!!")
	   (exit)
    ) ;_ Fin de progn
  ) ;_ Fin de if
  (foreach n set_name (vla-put-DisplayLocked n active))
  (princ (strcat "\n"
		 (itoa (length set_name))
		 " fen�tres ont �t� "
		 (if (zerop active)
		   (princ "activ�es")
		   (princ "desactiv�es")
		 ) ;_ Fin de if
	 ) ;_ Fin de strcat
  ) ;_ Fin de princ
  (princ)
) ;_ Fin de defun
