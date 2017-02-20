(defun deverrouille_calque_reac
       (c d / first_lay lst_lay next_lay lay_name)
  (if (equal flag 1)
    (progn (setq first_lay (tblnext "LAYER" T))
	   (setq lay_name (tblobjname "LAYER" (cdr (assoc 2 first_lay)))) ;_ Fin de setq
	   (entmod (subst '(70 . 0)
			  (assoc 70
				 (entget lay_name) ;_ Fin de entget
			  ) ;_ Fin de assoc
			  (entget lay_name) ;_ Fin de entget
		   ) ;_ Fin de subst
	   ) ;_ Fin de entmod
	   (while (setq next_lay (tblnext "LAYER" nil))
	     (setq lay_name (tblobjname "LAYER" (cdr (assoc 2 next_lay))))
	     (entmod (subst '(70 . 0)
			    (assoc 70 (entget lay_name))
			    (entget lay_name)
		     ) ;_ Fin de subst
	     ) ;_ Fin de entmod
	   ) ;_ Fin de while
    ) ;_ Fin de progn
  ) ;_ Fin de if
) ;_ Fin de defun


(defun cleanreac2 ()
  (if (equal reac2 nil)
    (progn (vlr-remove reac2) (vlr-remove reac3))
  ) ;_ Fin de if
) ;_ Fin de defun

(setq reac2 (vlr-command-reactor
	      'nil
	      '((:vlr-commandEnded . deverrouille_calque_reac))
	    ) ;_ Fin de vlr-command-reactor
) ;_ Fin de setq

(setq reac3 (vlr-command-reactor
	      'nil
	      '((:vlr-commandCancelled . deverrouille_calque_reac))
	    ) ;_ Fin de vlr-command-reactor
) ;_ Fin de setq
