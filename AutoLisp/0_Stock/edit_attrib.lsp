(defun c:ea () ;_ Fin de defun
  (setq	nom_block
	 (assoc
	   2
	   (setq entname_org
		  (entget
		    (car
		      (entsel
			"\nSelectionnez une reference de bloc à modifier :"
		      ) ;_ Fin de entsel
		    ) ;_ Fin de car
		  ) ;_ Fin de entget
	   ) ;_ Fin de setq
	 ) ;_ Fin de assoc
  ) ;_ Fin de setq
  (editattrib nom_block entname_org)
) ;_ Fin de defun

(defun editattrib (nom_block	entname_org  /		  entname
		   block_def	block_num    block_ent	  list_att
		   init		reponse	     valeur	  set_block
		   i		def_att	     ent_mod
		   list_val_attrib	     pos_attrib	  pos
		  )
  (setq	entname		(entnext (cdr (assoc -1 entname_org)))
	list_val_attrib	'(nil)
  ) ;_ Fin de setq
  (while
    (not
      (equal (assoc 0 (entget entname))
	     (cons 0 "SEQEND")
      ) ;_ Fin de equal
    ) ;_ Fin de not
     (setq list_val_attrib
	    (append list_val_attrib
		    (list (cdr (assoc 1 (entget entname))))
	    ) ;_ Fin de append
     ) ;_ Fin de setq
     (setq entname (entnext entname))
  ) ;_ Fin de while
  (setq list_val_attrib (cdr list_val_attrib))
  (setq block_def (tblsearch "BLOCK" (cdr nom_block)))
  (setq block_num (cdr (assoc -2 block_def)))
  (setq	block_ent '(nil)
	init ""
  ) ;_ Fin de setq
  (while block_num
    (if	(equal (cons 0 "ATTDEF") (assoc 0 (entget block_num)))
      (setq block_ent (append block_ent (list (entget block_num))))
    ) ;_ Fin de if
    (setq block_num (entnext block_num))
  ) ;_ Fin de while
  (setq	block_ent  (cdr block_ent)
	pos_attrib '(nil)
	list_att   (strcat
		     "\n---------------------------------------------------------------\n\nListe des attributs présents dans le bloc '"
		     (cdr nom_block)
		     "' :\n"
		   ) ;_ Fin de strcat
  ) ;_ Fin de setq
  (foreach n block_ent
    (setq init (strcat " "
		       (substr (cdr (assoc 2 n)) 1 2)
		       (strcase (substr (cdr (assoc 2 n)) 3) T)
		       init
	       ) ;_ Fin de strcat
    ) ;_ Fin de setq
    (setq list_att (strcat list_att
			   "\n"
			   (cdr (assoc 3 n))
			   " : "
			   (substr (cdr (assoc 2 n)) 1 2)
			   (strcase (substr (cdr (assoc 2 n)) 3) T)
		   ) ;_ Fin de strcat
    ) ;_ Fin de setq
    (setq pos_attrib (append pos_attrib (list (cdr (assoc 2 n)))))
  ) ;_ Fin de foreach
  (setq pos_attrib (cdr pos_attrib))
  (setq init (substr init 2))
  (setq init (strcat init " " "Quitter"))
  (setq
    list_att
     (strcat
       list_att
       "\nQuitter"
       "\n\n---------------------------------------------------------------"
     ) ;_ Fin de strcat
  ) ;_ Fin de setq
  (textscr)
  (princ list_att)
  (initget init)
  (setq	reponse
	 (getkword
	   "\nIndiquez l'attribut que vous voulez modifier : "
	 ) ;_ Fin de getkword
  ) ;_ Fin de setq
  (if (equal reponse "Quitter")
    (progn
      (graphscr)
      (exit)
    ) ;_ Fin de progn
  ) ;_ Fin de if
  (setq reponse (strcase reponse))
  (setq
    pos	(- (length pos_attrib) (length (member reponse pos_attrib)))
  ) ;_ Fin de setq
  (setq
    valeur (getstring
	     T
	     (strcat "\nIndiquez la nouvelle valeur de l'attribut "
		     reponse
		     "\n"
		     "<"
		     (nth pos list_val_attrib)
		     ">"
		     " : "
	     ) ;_ Fin de strcat
	   ) ;_ Fin de getstring
  ) ;_ Fin de setq
  (if (equal valeur "")
    (setq valeur (nth pos list_val_attrib))
  ) ;_ Fin de if
  (setq	set_block (ssget "X" (list nom_block))
	i	  0
	def_att	  '(nil)
  ) ;_ Fin de setq
  (repeat (sslength set_block)
    (setq att_name (entnext (ssname set_block i)))
    (while
      (not
	(equal (assoc 0 (entget att_name))
	       (cons 0 "SEQEND")
	) ;_ Fin de equal
      ) ;_ Fin de not
       (if (equal (assoc 2 (entget att_name))
		  (cons 2 reponse)
	   ) ;_ Fin de equal
	 (setq def_att (append def_att (list (entget att_name))))
       ) ;_ Fin de if
       (setq att_name (entnext att_name))
    ) ;_ Fin de while
    (setq i (1+ i))
  ) ;_ Fin de repeat
  (setq def_att (cdr def_att))
  (foreach n def_att
    (setq ent_mod (entmod (subst (cons 1 valeur) (assoc 1 n) n)))
    (entupd (cdar ent_mod))
  ) ;_ Fin de foreach
  (setq set_block nil)
  (princ)
  (editattrib nom_block entname_org)
) ;_ Fin de defun
