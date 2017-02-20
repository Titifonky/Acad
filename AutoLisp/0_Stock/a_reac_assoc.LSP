(defun c:tr ()
  (princ "\nSelectionnez l'objet de base :")
  (while (null (setq obj_bs (entsel))))
  (setq obj_bs (vlax-ename->vla-object (car obj_bs)))
  (vla-Highlight obj_bs t)
  (princ "\nSelectionnez l'objet à lier :")
  (while (null (setq obj_li (entsel))))
  (setq obj_li (vlax-ename->vla-object (car obj_li)))
  (vla-Highlight obj_li t)
  (princ "\nIndiquez les points de référence :")
  (setq	pt_1 (getpoint)
	pt_2 (getpoint pt_1)
  ) ;_ Fin de setq
  (if (equal (car (nentselp pt_1))
	     (vlax-vla-object->ename obj_bs)
      ) ;_ Fin de equal
    (setq pt_bs	pt_1
	  pt_li	pt_2
    ) ;_ Fin de setq
    (setq pt_bs	pt_2
	  pt_li	pt_1
    ) ;_ Fin de setq
  ) ;_ Fin de if
  (vla-Highlight obj_bs 0)
  (vla-Highlight obj_li 0)
  (cond
    ((and (setq reacteur (as_object_reac obj_bs))
	  (not (as_object_reac obj_li))
     ) ;_ Fin de and
     (Reactor_add reacteur obj_li pt_li)
    )
    ((and (setq reacteur (as_object_reac obj_li))
	  (not (as_object_reac obj_bs))
     ) ;_ Fin de and
     (Reactor_add reacteur obj_bs pt_bs)
    )
    ((and (as_object_reac obj_bs) (as_object_reac obj_li))
     (princ
       "Liaison non possible, les objets ont déjà des liaisons"
     ) ;_ Fin de princ
    )
    (t
     (princ "\nCréation de la liaison ...    ")
     (Reactor_create obj_bs obj_li pt_bs pt_li)
    )
  ) ;_ Fin de cond
  (princ)
) ;_ Fin de defun

(defun Reactor_create
       (object_base object_instance point_base point_liaison)
  (vlr-object-reactor
    (list object_base object_instance)
    (list point_liaison point_base)
    '((:vlr-copied . Cop_Reac)
      (:vlr-modified . Mod_Reac)
      (:vlr-erased . Era_Reac)
     )
  ) ;_ Fin de vlr-object-reactor
  (princ "\nLiaison crée")
) ;_ Fin de defun

(defun Reactor_add (reactor object point)
  (vlr-owner-add reactor object)
  (vlr-data-set
    reactor
    (append (list point) (vlr-data reactor))
  ) ;_ Fin de vlr-data-set
  (princ "\nLiaison crée")
) ;_ Fin de defun

(defun Cop_Reac	(obj reactor pmt / pt)
  (if (equal (type (car pmt)) 'ENAME)
    (progn
      (setq pt (nth (vl-position obj (vlr-owners reactor))
		    (vlr-data reactor)
	       ) ;_ Fin de nth
      ) ;_ Fin de setq
      (setq copie_obj_reac (list reactor (car pmt) pt))
    ) ;_ Fin de progn
  ) ;_ Fin de if
  (princ)
) ;_ Fin de defun

(defun Mod_Reac	(obj reactor pmt)
  (princ)
) ;_ Fin de defun

(defun Era_Reac	(obj reactor pmt)
  (vlr-owner-remove reactor obj)
  (if (not (vlr-owners reactor))
    (vlr-remove reactor)
  ) ;_ Fin de if
  (princ)
) ;_ Fin de defun

(defun as_object_reac (object / list_reac list_reac_obj ent_reac x)
  (setq	list_reac     (cdar (vlr-reactors :vlr-object-reactor))
	list_reac_obj '(nil)
  ) ;_ Fin de setq
  (mapcar '(lambda (x)
	     (setq ent_reac (vlr-owners x))
	     (if (member object ent_reac)
	       (setq list_reac_obj (append list_reac_obj (list x)))
	     ) ;_ Fin de if
	   ) ;_ Fin de lambda
	  list_reac
  ) ;_ Fin de mapcar
  (setq list_reac_obj (cdr list_reac_obj))
) ;_ Fin de defun

(defun CleanReac ()
  (setq Reac_mod nil)
  (mapcar 'vlr-remove-all
	  '(:VLR-AcDb-reactor
	    :VLR-Editor-reactor
	    :VLR-Linker-reactor
	    :VLR-Object-reactor
	   )
  ) ;_ Fin de mapcar
) ;_ Fin de defun

(defun Commandend (reac cmd)
  (cond
    ((and (equal (car cmd) "COPY")
	  (not (zerop (length copie_obj_reac)))
     ) ;_ Fin de and
     (last copie_obj_reac)
     (Reactor_add
       (car copie_obj_reac)
       (vlax-ename->vla-object (cadr copie_obj_reac))
       pt
     ) ;_ Fin de Reactor_add
     (setq copie_obj_reac 'nil)
    )
  ) ;_ Fin de cond
) ;_ Fin de defun

(vlr-editor-reactor
  nil
  '((:vlr-commandended . Commandend))
) ;_ Fin de vlr-editor-reactor
