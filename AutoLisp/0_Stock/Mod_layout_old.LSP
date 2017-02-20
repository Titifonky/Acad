(defun c:rp (/		   list_layout	 nb_layout     indice
	     list_deff1	   i		 list_deff     ordrename
	     nom_layout	   k		 dict_layout   model
	     ordrename_dict		 nom_layout_dict
	     cmde
	    )
  (setq cmde (GETVAR "CMDECHO"))
  (setvar "CMDECHO" 0)
  (command "_undo" "_m")
  (setq dict_layout (dictsearch (namedobjdict) "acad_layout"))
  (setq model (member '(3 . "Model") dict_layout))
  (setq	list_layout
	 (vl-remove-if
	   '(lambda (x)
	      (or (= x (car model)) (= x (cadr model)))
	    ) ;_ Fin de lambda
	   dict_layout
	 ) ;_ Fin de vl-remove-if
  ) ;_ Fin de setq
  (setq	list_layout
	 (mapcar 'cdr
		 (vl-remove-if-not
		   '(lambda (x) (= (car x) 350))
		   list_layout
		 ) ;_ Fin de vl-remove-if-not
	 ) ;_ Fin de mapcar
  ) ;_ Fin de setq
  (setq nb_layout (length list_layout))
  (princ
    "\nAttention!!!! Seront renommées seulement les présentations commencant par 'P'"
  ) ;_ Fin de princ
  (setq no_plan (getstring "\nNumero du plan :"))
  (setq indice (getstring "\nIndice du plan :"))
  (setq	list_deff1
	 '(nil)
	i 0
  ) ;_ Fin de setq
  (repeat nb_layout
    (setq list_deff1
	   (append list_deff1 (list (entget (nth i list_layout))))
    ) ;_ Fin de setq
    (setq i (1+ i))
  ) ;_ Fin de repeat
  (setq	list_deff1
	 (cdr list_deff1)
	list_deff '(nil)
	i 1
  ) ;_ Fin de setq
  (while (<= i nb_layout)
    (foreach n list_deff1
      (if (equal (cdr (assoc 71 n)) i)
	(setq list_deff (append list_deff (list n)))
      ) ;_ Fin de if
    ) ;_ Fin de foreach
    (setq i (1+ i))
  ) ;_ Fin de while
  (setq list_deff (cdr list_deff))
  (setq	ordrename
	 (mapcar 'car
		 (mapcar '(lambda (x)
			    (vl-remove-if-not
			      '(lambda (y)
				 (and (= (car y) 1)
				      (not (zerop (strlen (cdr y))))
				 ) ;_ Fin de and
			       ) ;_ Fin de lambda
			      x
			    ) ;_ Fin de vl-remove-if-not
			  ) ;_ Fin de lambda
			 list_deff
		 ) ;_ Fin de mapcar
	 ) ;_ Fin de mapcar
  ) ;_ Fin de setq
  (setq	i 0
	k 1
  ) ;_ Fin de setq
  (setq	ordrename_dict
	 (mapcar '(lambda (x) (setq x (cons 3 (cdr x))))
		 ordrename
	 ) ;_ Fin de mapcar
  ) ;_ Fin de setq
  (setq til (getvar "TILEMODE"))
  (setvar "TILEMODE" 1)
  (repeat (length ordrename_dict)
    (setq nom_layout_dict (cdr (nth i ordrename_dict)))
    (if	(equal (strcase (substr nom_layout_dict 1 1)) "P")
      (progn (setq dict_layout
		    (subst (cons 3
				 (strcat "Plan no "
					 no_plan
					 "."
					 (itoa k)
					 " Indice "
					 indice
				 ) ;_ Fin de strcat
			   ) ;_ Fin de cons
			   (nth i ordrename_dict)
			   dict_layout
		    ) ;_ Fin de subst
	     ) ;_ Fin de setq
	     (setq k (1+ k))
      ) ;_ Fin de progn
    ) ;_ Fin de if
    (setq i (1+ i))
  ) ;_ Fin de repeat
  (entmod dict_layout)
  (setq	i 0
	k 1
  ) ;_ Fin de setq
  (repeat (length ordrename)
    (setq nom_layout (cdr (nth i ordrename)))
    (if	(equal (strcase (substr nom_layout 1 1)) "P")
      (progn (setq layout (subst (cons 1
				       (strcat "Plan no "
					       no_plan
					       "."
					       (itoa k)
					       " Indice "
					       indice
				       ) ;_ Fin de strcat
				 ) ;_ Fin de cons
				 (nth i ordrename)
				 (nth i list_deff)
			  ) ;_ Fin de subst
	     ) ;_ Fin de setq
	     (setq k (1+ k))
	     (entmod layout)
      ) ;_ Fin de progn
    ) ;_ Fin de if
    (setq i (1+ i))
  ) ;_ Fin de repeat
  (command "_regenall")
  (command "_undo" "_m")
  (setvar "CMDECHO" cmde)
  (setvar "TILEMODE" til)
  (princ)
) ;_ Fin de defun
