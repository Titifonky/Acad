(defun c:ren ()
  (regapp "DATAREF")
  (setq	ent	 (car (entsel "\nSelectionnez l'entité à nommer :"))
	ent_data (entget ent)
	name	 (getstring "\nNom de l'entité :")
	new_data (append ent_data
			 (list (list -3 (list "DATAREF" (cons 1000 name))))
		 ) ;_ Fin de append
  ) ;_ Fin de setq
  (entmod new_data)
  (princ (entget ent '("DATAREF")))
  (princ)
) ;_ Fin de defun

(defun c:sxd ()
  (setq sel (ssget "X" '((-3 ("DATAREF")))))
  (sssetfirst nil sel)
) ;_ Fin de defun
