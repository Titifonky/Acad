(defun choix_reac (reac cmd)
  (setq nom_cmd (car cmd))
  (if (equal nom_cmd "INSERT")
    (progn (edit_attribut_car) (setq insert (entlast)))
  ) ;_ Fin de if
) ;_ Fin de defun

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun edit_attribut_car (/	      nom_block	  block_def
			  block_num   block_ent	  list_att
			  init	      reponse	  valeur
			  set_block   i		  def_att
			  ent_mod
			 )
  (setq nom_block (assoc 2 (entget (entlast))))
  (setq	name_lay (strcase "Cart")
	flag	 1
  ) ;_ Fin de setq
  (nomlayer name_lay flag)
  (princ nomcalque)
  (setq layer_cartouche nomcalque)
  (setq	set_block (ssget "X" (list nom_block))
	i	  0
	def_att	  '(nil)
  ) ;_ Fin de setq
  (if (not (equal 'nil layer_cartouche))
    (progn
      (entmod (subst (cons 8 layer_cartouche)
		     (assoc 8 (entget (entlast)))
		     (entget (entlast))
	      ) ;_ Fin de subst
      ) ;_ Fin de entmod
      (entupd (entlast))
    ) ;_ Fin de progn
  ) ;_ Fin de if
  (repeat (sslength set_block)
    (setq block_deff (entget (ssname set_block i)))
    (if	(equal (strcase (substr (cdr (assoc 410 block_deff)) 1 1))
	       "P"
	) ;_ Fin de equal
      (progn
	(setq att_name (entnext (ssname set_block i))
	      att_info (vl-string-trim
			 "Plan no "
			 (cdr (assoc 410 block_deff))
		       ) ;_ Fin de vl-string-trim
	      no_plan  (substr att_info 1 (vl-string-search " " att_info))
	      indice   (substr att_info
			       (+ (vl-string-search "Indice" att_info) 7)
			       (+ (vl-string-search "Indice" att_info) 7)
		       ) ;_ Fin de substr
	) ;_ Fin de setq
	(while
	  (not (equal (assoc 0 (entget att_name)) (cons 0 "SEQEND")))
	   (if (equal (assoc 2 (entget att_name)) (cons 2 "INDICE"))
	     (entupd (cdar (entmod (subst (cons 1 indice)
					  (assoc 1 (entget att_name))
					  (entget att_name)
				   ) ;_ Fin de subst
			   ) ;_ Fin de entmod
		     ) ;_ Fin de cdar
	     ) ;_ Fin de entupd
	   ) ;_ Fin de if
	   (if (equal (assoc 2 (entget att_name)) (cons 2 "NO-PLAN"))
	     (entupd (cdar (entmod (subst (cons 1 no_plan)
					  (assoc 1 (entget att_name))
					  (entget att_name)
				   ) ;_ Fin de subst
			   ) ;_ Fin de entmod
		     ) ;_ Fin de cdar
	     ) ;_ Fin de entupd
	   ) ;_ Fin de if
	   (setq att_name (entnext att_name))
	) ;_ Fin de while
      ) ;_ Fin de progn
    ) ;_ Fin de if
    (setq i (1+ i))
  ) ;_ Fin de repeat
  (princ)
) ;_ Fin de defun

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(vl-load-com)
(vlr-remove-all)

(defun CLEANREAC ()
  (mapcar 'vlr-remove-all
	  '(:vlr-acdb-reactor
	    :vlr-editor-reactor
	    :vlr-linker-reactor
	    :vlr-object-reactor
	    :vlr-miscellaneous-Reactor
	    :vlr-command-reactor
	   )
  ) ;_ Fin de mapcar
) ;_ Fin de defun

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(CLEANREAC)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(vlr-command-reactor
  NIL
  '((:vlr-commandended . choix_reac))
) ;_ Fin de vlr-command-reactor

;;; (vlr-acdb-reactor nil '((:vlr-objectappended . choix_reac)))