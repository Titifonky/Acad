(defun verrouille_calque_reac
       (c d / first_lay lst_lay next_lay lay_name)
  (if (equal flag 1)
    (progn
      (setq first_lay (tblnext "LAYER" T)) ;_ Fin de setq
      (if (not (equal (cdr (assoc 2 first_lay)) c_layer))
	(progn (setq lay_name (tblobjname "LAYER" (cdr (assoc 2 first_lay)))) ;_ Fin de setq
	       (entmod (subst '(70 . 4)
			      (assoc 70 (entget lay_name))
			      (entget lay_name)
		       ) ;_ Fin de subst
	       ) ;_ Fin de entmod
	) ;_ Fin de progn
      ) ;_ Fin de if
      (while (setq next_lay (tblnext "LAYER" nil))
	(if (not (equal (cdr (assoc 2 next_lay)) c_layer))
	  (progn (setq
		   lay_name (tblobjname "LAYER" (cdr (assoc 2 next_lay)))
		 ) ;_ Fin de setq
		 (entmod (subst	'(70 . 4)
				(assoc 70 (entget lay_name))
				(entget lay_name)
			 ) ;_ Fin de subst
		 ) ;_ Fin de entmod
	  ) ;_ Fin de progn
	) ;_ Fin de if
      ) ;_ Fin de while
    ) ;_ Fin de progn
  ) ;_ Fin de if
  (princ)
) ;_ Fin de defun


(defun cleanreac1 ()
  (if (equal reac1 nil)
    (vlr-remove reac1)
  ) ;_ Fin de if
) ;_ Fin de defun

(setq reac1 (vlr-command-reactor
	      'nil
	      '((:vlr-commandWillStart . verrouille_calque_reac))
	    ) ;_ Fin de vlr-command-reactor
) ;_ Fin de setq
