(defun c:etx (/		   ip		activ_doc    ent_lier
	      ent_ref	   texte	texte_override
	      texte_string
	      )
  (setq	activ_doc (vla-get-utility
		    (vla-get-activedocument (vlax-get-acad-object))
		    ) ;_ / vla-get-utility
	) ;_ / setq

  (while
    (vl-catch-all-error-p
      (vl-catch-all-apply
	'vla-getentity
	(list activ_doc
	      'ent_lier
	      'ip
	      "\nSelectionnez l'objet à lier :"
	      ) ;_ / list
	) ;_ / vl-catch-all-apply
      ) ;_ / vl-catch-all-error-p
     (princ "Aucun objet selectionné")
     ) ;_ / while

  (while
    (vl-catch-all-error-p
      (vl-catch-all-apply
	'vla-getentity
	(list activ_doc
	      'ent_ref
	      'ip
	      "\nSelectionnez l'objet de référence :"
	      ) ;_ / list
	) ;_ / vl-catch-all-apply
      ) ;_ / vl-catch-all-error-p
     (princ "Aucun objet selectionné")
     ) ;_ / while

  (setq	separateur
	 (vla-getstring
	   activ_doc
	   :vlax-false
	   (vlax-make-variant "\nSymbole de séparation : ")
	   ) ;_ / vla-getstring
	) ;_ / setq

  (if (not (or ent_lier ent_ref))
    (exit)
    ) ;_ / if

  (if (not (vlax-property-available-p ent_ref 'ObjectId))
    (exit)
    ) ;_ / if

  (setq	ref_object_id (vla-get-ObjectID ent_ref)
	texte	      (strcat "%<\\AcObjProp Object(%<\\_ObjId "
			      (itoa ref_object_id)
			      ">%).TextString>%"
			      ) ;_ / strcat
	) ;_ / setq

  (setq object_name (vla-get-ObjectName ent_lier))

  (cond
    ((vl-string-search "Dimension" object_name)

     (if (/= (setq texte_override (vla-get-TextOverride ent_lier))
	     ""
	     ) ;_ / /=
       (setq texte (strcat texte separateur texte_override))
       (setq texte (strcat texte "<>"))
       ) ;_ / if
     (vla-put-TextOverride ent_lier texte)
     )

    ((or (vl-string-search "Text" object_name)
	 (vl-string-search "MLeader" object_name)
	 ) ;_ / or

;;;     (if (setq texte_string (vla-get-TextString ent_lier))
;;;       (setq texte (strcat texte separateur texte_string))
;;;       ) ;_ / if
     (vla-put-TextString ent_lier texte)
     )

    (t nil)
    ) ;_ / cond

  (command "_zoom" "_o" (vlax-vla-object->ename ent_lier) "")

  (princ)
  ) ;_ / defun