(defun c:av ()
  (setq cmde (GETVAR "CMDECHO"))
  (setvar "CMDECHO" 0)
  (command "_undo" "_m")
  (command "_.pspace")
  (setq	current_view (car (vports))
	ssview	     (ssget "_W"
			    (cadr current_view)
			    (caddr current_view)
			    '((0 . "VIEWPORT"))
		     )
	i	     0
	set_view     '()
  )
  (repeat (sslength ssview)
    (setq ent	   (ssname ssview i)
	  set_view
		   (append
		     set_view
		     (list
		       (cdr (assoc '69 (entget ent)))
		       (list
			 (setq name (vlax-ename->vla-object ent))
			 (vla-get-customscale name)
		       )
		     )
		   )
	  i	   (+ i 1)
    )
  )
  (command "_.mspace")
  (setq
    pt_eo (getpoint
	    "\nIndiquez le point d'alignement dans l'espace objet :"
	  )
    pt_eo (trans pt_eo 1 0)
  )
  (setq	used_vports
	 (getvar "cvport")
	scale nil
	view nil
  )
  (command "_.pspace")
  (if (setq data_view (member used_vports set_view))
    (setq scale	(last (cadr data_view))
	  view	(caadr data_view)
    )
  )
  (setq
    pt_ep (getpoint
	    "\nIndiquez le point d'alignement dans l'espace papier :"
	  )
    pt_ep (trans pt_ep 1 3)
  )
  (vla-put-customscale view scale)
  (command "_.mspace")
  (setq	pt_eo	 (trans pt_eo 0 1)
	pt_ep_eo (trans pt_ep 3 2)
	pt_ep_eo (trans pt_ep_eo 2 1)
  )
  (command "_.-pan" pt_eo pt_ep_eo)
  (command "_.pspace")
  (setvar "CMDECHO" cmde)
  (princ)
)
