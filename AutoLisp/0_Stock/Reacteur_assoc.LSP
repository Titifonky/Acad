(defun c:rec ()
  (princ "\nSelectionnez la première entité :\n")
  (while (null (setq obj (entsel))))
  (setq obj_1 (vlax-ename->vla-object (car obj)))
  (vla-Highlight obj_1 t)
  (princ "\nSelectionnez la deuxième entité :\n")
  (while (null (setq obj (entsel))))
  (setq	obj_2	      (vlax-ename->vla-object (car obj))
	list_obj      (list obj_1 obj_2)
	list_reac     (cdar (vlr-reactors :vlr-object-reactor))
	list_reac_obj '(nil)
  ) ;_ Fin de setq
  (mapcar '(lambda (x)
	     (foreach n	list_obj
	       (setq ent_reac (vlr-owners x))
	       (if (and (> (length ent_reac) 1) (member n ent_reac))
		 (setq list_reac_obj (append list_reac_obj (list n x)))
	       ) ;_ Fin de if
	     ) ;_ Fin de foreach
	   ) ;_ Fin de lambda
	  list_reac
  ) ;_ Fin de mapcar
  (setq list_reac_obj (cdr list_reac_obj))
  (vla-Highlight obj_1 0)
  (cond
    ((and (member obj_1 list_reac_obj)
	  (member obj_2 list_reac_obj)
     ) ;_ Fin de and
     (princ "Liaison non possible")
    )
    ((and (member obj_1 list_reac_obj)
	  (not (member obj_2 list_reac_obj))
     ) ;_ Fin de and
     ;;ajout de obj_2 au reacteur de obj_1
     (princ "add1")
    )
    ((and (not (member obj_1 list_reac_obj))
	  (member obj_2 list_reac_obj)
     ) ;_ Fin de and
     ;;ajout de obj_2 au reacteur de obj_1
     (princ "add2")
    )
    ((and (not (member obj_1 list_reac_obj))
	  (not (member obj_2 list_reac_obj))
     ) ;_ Fin de and
     (progn
       (princ "create")
       (reac_create obj_1 obj_2)
       (princ)
     ) ;_ Fin de progn
    )
  ) ;_ Fin de cond
) ;_ Fin de defun


(defun reac_create (obj1 obj2)
  (vlr-object-reactor
    (list obj1 obj2)
    (list (list_pt obj1) (list_pt obj2))
    '((:vlr-copied . Copie_Reac)
      (:vlr-modified . Modif_Reac)
      (:vlr-erased . Erase_Reac)
     )
  ) ;_ Fin de vlr-object-reactor
) ;_ Fin de defun

(defun Copie_Reac (Reacteur_Obj Reacteur_Reac Reacteur_Param)
  (princ)
) ;_ Fin de defun

(defun Modif_Reac (Reacteur_Obj Reacteur_Reac Reacteur_Param)
  (setq	Mod_link_Obj  Reacteur_Obj
	Mod_link_Reac Reacteur_Reac
  ) ;_ Fin de setq
  (princ)
) ;_ Fin de defun

(defun Erase_Reac (Reacteur_Obj Reacteur_Reac Reacteur_Param)
  (princ)
) ;_ Fin de defun
