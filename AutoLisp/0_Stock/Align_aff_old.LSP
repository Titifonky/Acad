(defun c:av ()
  (setq cmde (GETVAR "CMDECHO"))
  (setvar "CMDECHO" 0)
  (command "_undo" "_m")
  (command "_.pspace")
  (setq select (entget (car (entsel "\nSelectionnez la fenêtre :"))))
  (if
    (not
      (and (= (cdr (assoc '102 select)) "{ACAD_REACTORS")
	   (= (cdr
		(assoc '0 (setq view (entget (cdr (assoc '330 select)))))
	      )
	      "VIEWPORT"
	   )
      )
    )
     (if (= (cdr (assoc '0 select)) "VIEWPORT")
       (setq view select)
       ((princ "Ce n'est pas une fenêtre")
	 (exit)
       )
     )
  )
  (setq	no_vport     (cdr (assoc '69 view))
	view_vlaname (vlax-ename->vla-object (cdr (assoc '-1 view)))
	scale	     (vla-get-customscale view_vlaname)
  )
  (command "_.mspace")
  (setvar "CVPORT" no_vport)
  (setq
    pt_eo (getpoint
	    "\nIndiquez le point d'alignement dans l'espace objet :"
	  )
    pt_eo (trans pt_eo 1 0)
  )
  (command "_.pspace")
  (setq
    pt_ep (getpoint
	    "\nIndiquez le point d'alignement dans l'espace papier :"
	  )
    pt_ep (trans pt_ep 1 3)
  )
  (vla-put-customscale view_vlaname scale)

  (setq vplocked (vla-get-displaylocked view_vlaname))
  (vla-put-displaylocked view_vlaname 0)
  (command "_.mspace")
  (setvar "CVPORT" no_vport)
  (setq	pt_eo	 (trans pt_eo 0 1)
	pt_ep_eo (trans pt_ep 3 2)
	pt_ep_eo (trans pt_ep_eo 2 1)
  )
  (command "_.-pan" pt_eo pt_ep_eo)
  (command "_.pspace")

  (if (= vplocked ":vlax-true")
    (vla-put-displaylocked view_vlaname 1)
  )
  (setvar "CMDECHO" cmde)
  (princ)
)
