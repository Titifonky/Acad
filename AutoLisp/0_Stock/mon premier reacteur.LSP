(defun choix_reac (reac cmd)
  (princ reac)
  (princ cmd)
  (vlax-dump-object (car cmd))
  (princ (entget (cadr cmd)))
  (princ)
;;;  (if (equal nom_cmd "LAYOUT_CONTROL")
;;;    (cond
;;;      ((= (
;;;    )
;;;  ) ;_ Fin de if
) ;_ Fin de defun

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun listlayout (/ list_layout dict_layout nom_layout)
  (setq	list_layout
	 (layoutlist)
	dict_layout
	 (dictsearch (namedobjdict) "acad_layout")
	nom_layout
	 T
	i 0
	vla_lnlayout1
	 '(nil)
  ) ;_ Fin de setq
  (repeat (length list_layout)
    (setq nom_vl_layout
	   (vlax-ename->vla-object
	     (cdadr (setq dict_member
			   (member (cons 3 (nth i list_layout))
				   dict_layout
			   ) ;_ Fin de member
		    ) ;_ Fin de setq
	     ) ;_ Fin de cdadr
	   ) ;_ Fin de vlax-ename->vla-object
    ) ;_ Fin de setq
    (setq vla_lnlayout1 (append vla_lnlayout1 (list nom_vl_layout)))
    (setq dict_layout (cdr dict_member))
    (setq i (1+ i))
  ) ;_ Fin de repeat
  (setq	vla_lnlayout1
	 (cdr vla_lnlayout1)
  ) ;_ Fin de setq
) ;_ Fin de defun

(vl-load-com)
(vlr-remove-all)

(defun CLEANREAC ()
  (mapcar 'vlr-remove-all
	  '(:vlr-acdb-reactor
	    :vlr-editor-reactor
	    :vlr-linker-reactor
	    :vlr-object-reactor
	   )
  ) ;_ Fin de mapcar
) ;_ Fin de defun

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(CLEANREAC)
(listlayout)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(vlr-acdb-reactor
  vla_lnlayout1
  '((:vlr-ObjectModified . choix_reac))
) ;_ Fin de vlr-acdb-reactor

;;;(vlr-object-reactor
;;;  vla_lnlayout1
;;;  '((:vlr-modified . choix_reac))
;;;) ;_ Fin de vlr-acdb-reactor



;;;(vlr-command-reactor
;;;  NIL
;;;  '((:vlr-commandended . choix_reac))
;;;) ;_ Fin de vlr-command-reactor

;;; (vlr-acdb-reactor nil '((:vlr-objectappended . choix_reac)))