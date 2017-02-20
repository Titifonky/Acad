;;selection du calque filtre ou desactivation de la selection par calque
(defun c:sc ()
  ;; flag : variable permettant de savoir si la selection par calque est activé ou non
  (if (equal flag 'nil)
    (setq flag 0)
  ) ;_ Fin de if
  (cond	((zerop (fix flag)) (setq flag 1))
	((>= flag 1) (setq flag 0))
  ) ;_ Fin de cond
  (princ (strcat "\nSelection par calque "
		 (if (zerop flag)
		   (princ "desactivé")
		   (princ "activé")
		 ) ;_ Fin de if
	 ) ;_ Fin de strcat
  ) ;_ Fin de princ
  (if (not (zerop flag))
    (progn (setq c_layer
		  (vla-get-layer
		    (vlax-ename->vla-object
		      (car (entsel "\nSelectionnez le calque de filtre :")
		      ) ;_ Fin de car
		    ) ;_ Fin de vlax-ename->vla-object
		  ) ;_ Fin de vla-get-layer
	   ) ;_ Fin de setq
	   (princ (strcat "\nLe calque filtre est " c_layer))
    ) ;_ Fin de progn
  ) ;_ Fin de if
  (if (equal c_layer nil)
    (setq flag 0)
  ) ;_ Fin de if
  (princ)
) ;_ Fin de defun


;;reaction
(defun choix_reac2 (a b)
  (if (equal setfirst nil)
    (setq setfirst (ssadd))
  ) ;_ Fin de if
  (if (and (not (equal (cadr (ssgetfirst)) nil))
	   (not (equal c_layer nil))
      ) ;_ Fin de and
    (progn
      (if (not
	    (equal (sslength (cadr (ssgetfirst))) (sslength setfirst))
	  ) ;_ Fin de not
	(cond
	  ((and (cadr (ssgetfirst)) (= flag 1))
	   (setq nb_sel	  (sslength (setq sel (cadr (ssgetfirst))))
		 i	  0
		 setfirst nil
		 set_ent  (ssadd)
	   ) ;_ Fin de setq
	   (princ "\nfiltrage...")
	   (repeat nb_sel
	     (setq ent (ssname sel i))
	     (if (not (equal ent nil))
	       (progn
		 (if
		   (equal (vla-get-layer (vlax-ename->vla-object ent)) ;_ Fin de vla-get-layer
			  c_layer
		   ) ;_ Fin de equal
		    (ssadd ent set_ent)
		 ) ;_ Fin de if
	       ) ;_ Fin de progn
	     ) ;_ Fin de if
	     (setq i (1+ i))
	   ) ;_ Fin de repeat
	   (setq setfirst set_ent)
	   (sssetfirst nil set_ent)
	   (princ (strcat "\n"
			  (itoa (sslength set_ent))
			  " objet(s) selectioné(s) - "
			  "Calque filtre : "
			  c_layer
		  ) ;_ Fin de strcat
	   ) ;_ Fin de princ
	   (setq set_ent nil)
	  ) ;_ Fin de cond
	) ;_ Fin de cond
      ) ;_ Fin de if
    ) ;_ Fin de progn
    (setq setfirst nil)
  ) ;_ Fin de if
  (princ)
) ;_ Fin de defun

(defun clenreacA ()
  (if (not (equal reac nil))
    (vlr-remove reac)
  ) ;_ Fin de if
) ;_ Fin de defun

;;reacteur de selection
(setq reac (vlr-miscellaneous-reactor
	     nil
	     '((:vlr-pickfirstModified . choix_reac2))
	   ) ;_ Fin de vlr-object-reactor
) ;_ Fin de setq
