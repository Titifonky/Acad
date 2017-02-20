(defun c:test ()
  (setq	list_layout
		     (layoutlist)
	dict_layout
		     (dictsearch (namedobjdict) "acad_layout")
	nom_layout
		     T
	i	     0
	vla_lnlayout
		     '(nil)
	no_plan	     (getstring "\nNumero des plans :")
	indice	     (getstring "\nIndice des plans :")
  ) ;_ Fin de setq


;;;;;;;;;;;;;;;;;;;


  (repeat (length list_layout)
    (setq nom_vl_layout
	   (vlax-ename->vla-object
	     (cdadr (setq dict_member
			   (member (cons 3 (nth i list_layout))
				   dict_layout
			   ) ;_ Fin de member
		    ) ;_ Fin de setq
	     ) ;_ Fin de cdadr
	   ) ;_ Fin de vlax-ename->vla-object
    ) ;_ Fin de setq
    (setq vla_lnlayout (append vla_lnlayout (list nom_vl_layout)))
    (setq dict_layout (cdr dict_member))
    (setq i (1+ i))
  ) ;_ Fin de repeat


;;;;;;;;;;;;;;;;;


  (setq	vla_lnlayout
	 (cdr vla_lnlayout)
	i 0
	k 1
	vla_order_lnlayout
	 '(nil)
  ) ;_ Fin de setq
  (repeat (1- (* (length vla_lnlayout) (length vla_lnlayout)))
    (setq taborder (vla-get-taborder (nth i vla_lnlayout)))
    (if	(= taborder k)
      (progn
	(setq vla_order_lnlayout
	       (append vla_order_lnlayout
		       (list (nth i vla_lnlayout))
	       ) ;_ Fin de append
	) ;_ Fin de setq
	(setq k	(1+ k)
	) ;_ Fin de setq
      ) ;_ Fin de progn
    ) ;_ Fin de if
    (setq i (1+ i))
    (if	(>= i (length vla_lnlayout))
      (setq i 0)
    ) ;_ Fin de if
  ) ;_ Fin de repeat


;;;;;;;;;;;;;;;;;;;;;;;;


  (setq	i 0
	k 1
	vla_lnlayout
	 (cdr vla_order_lnlayout)
  ) ;_ Fin de setq
  (repeat (length vla_lnlayout)
    (if
      (equal (strcase (substr (vla-get-name (nth i vla_lnlayout)) 1 1))
	     "P"
      ) ;_ Fin de equal
       (progn
	 (vla-put-name
	   (nth i vla_lnlayout)
	   (strcat "Plan no "
		   no_plan
		   "."
		   (itoa k)
		   " Indice "
		   indice
	   ) ;_ Fin de strcat
	 ) ;_ Fin de vla-put-name
	 (setq k (1+ k))
       ) ;_ Fin de progn
    ) ;_ Fin de if
    (setq i (1+ i))
  ) ;_ Fin de repeat
  (princ)
) ;_ Fin de defun
