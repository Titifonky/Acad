(defun choix_reac2 (a b)
  (princ "\n1 : ")
  (princ (cadr (ssgetfirst)))
  (princ (sslength (cadr (ssgetfirst))))
  (princ "\n2 : ")
  (princ setfirst)
  (princ (sslength setfirst))
  (princ "\n")
  (if (not
	(equal (sslength (cadr (ssgetfirst))) (sslength setfirst))
      ) ;_ Fin de not
    (cond
      ((and (cadr (ssgetfirst)) (= flag 1))
       (setq nb_sel (sslength (setq sel (cadr (ssgetfirst)))))
       (setq i 0) ;_ Fin de setq
       (while (setq ent (ssname sel i))
	 (if (not (equal (vla-get-layer (vlax-ename->vla-object ent)) ;_ Fin de vla-get-layer
			 c_layer
		  ) ;_ Fin de equal
	     ) ;_ Fin de not
	   (ssdel ent sel)
	 ) ;_ Fin de if
	 (setq i (1+ i))
	 (princ " 3 ")
       ) ;_ Fin de while
       (setq setfirst sel)
       (sssetfirst nil sel)
      )
    ) ;_ Fin de cond
  ) ;_ Fin de if
  (princ "2 : ")
  (princ setfirst)
  (princ (sslength setfirst))
  (princ "\n")
  (princ)
) ;_ Fin de defun



(vlr-remove-all :vlr-miscellaneous-Reactor)


(vlr-miscellaneous-reactor
  nil
  '((:vlr-pickfirstModified . choix_reac2))
) ;_ Fin de vlr-object-reactor
