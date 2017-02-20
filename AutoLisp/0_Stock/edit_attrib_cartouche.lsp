(defun c:eac (/		  nom_block   block_def	  block_num
	      block_ent	  list_att    init	  reponse
	      valeur	  set_block   i		  def_att
	      ent_mod
	     )
  (setq	nom_block
	 (assoc
	   2
	   (entget
	     (car
	       (entsel
		 "\nSelectionnez une reference de cartouche à mettre à jour sur les présentations :"
	       )
	     )
	   )
	 )
  )
  (setq	set_block (ssget "X" (list nom_block))
	i	  0
	def_att	  '(nil)
  )
  (repeat (sslength set_block)
    (setq block_deff (entget (ssname set_block i)))
    (if	(equal (strcase (substr (cdr (assoc 410 block_deff)) 1 1))
	       "P"
	)
      (progn
	(setq att_name (entnext (ssname set_block i))
	      att_info (vl-string-trim
			 "Plan no "
			 (cdr (assoc 410 block_deff))
		       )
	      no_plan  (substr att_info 1 (vl-string-search " " att_info))
	      indice   (substr att_info
			       (+ (vl-string-search "Indice" att_info) 7)
			       (+ (vl-string-search "Indice" att_info) 7)
		       )
	)
	(while (not
		 (equal	(assoc 0 (entget att_name))
			(cons 0 "SEQEND")
		 )
	       )
	  (if (equal (assoc 2 (entget att_name))
		     (cons 2 "INDICE")
	      )
	    (entupd (cdar (entmod (subst (cons 1 indice)
					 (assoc 1 (entget att_name))
					 (entget att_name)
				  )
			  )
		    )
	    )
	  )
	  (if (equal (assoc 2 (entget att_name))
		     (cons 2 "NO-PLAN")
	      )
	    (entupd (cdar (entmod (subst (cons 1 no_plan)
					 (assoc 1 (entget att_name))
					 (entget att_name)
				  )
			  )
		    )
	    )
	  )
	  (setq att_name (entnext att_name))
	)
;;;    (setq att_name (entnext (ssname set_block i)))
;;;    (while
;;;      (not
;;;	(equal (assoc 0 (entget att_name))
;;;	       (cons 0 "SEQEND")
;;;	)
;;;      )
;;;       (if (equal (assoc 2 (entget att_name))
;;;		  (cons 2 reponse)
;;;	   )
;;;	 (setq def_att (append def_att (list (entget att_name))))
;;;       )
;;;       (setq att_name (entnext att_name))
;;;    )
;;;    (setq i (1+ i))
      )
    )
    (setq i (1+ i))
  )
  (princ)
)