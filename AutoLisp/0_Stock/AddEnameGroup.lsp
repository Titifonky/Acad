(defun addgroup	(group /)
  (princ "\nSelectionnez les objets à ajouter au groupe :")
  (setq	selectionset
	 (ssget)
	i 0
  )
  (while (< i (sslength selectionset))
    (isgroup (ssname selectionset i))
    (if	(not is)
      (progn
	(setq entname (list (cons 340 (ssname selectionset i))))
	(setq group (entmod (append group entname)))
      )
      (princ
	"\nLes groupes selectionné ne sont pas ajouté au groupe de reference"
      )
    )
    (setq i (1+ i))
  )
  (princ)
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun isgroup (numeroent /)
  (setq
    groupnumero
     (cdadr (member '(102 . "{ACAD_REACTORS")
		    (entget numeroent)
	    )
     )
  )
  (if groupnumero
    (progn
      (setq groupdef (entget groupnumero))
      ;;test si groupnumero est bien un groupe
      (if (equal (cadr groupdef) '(0 . "GROUP"))
	(setq is T)
	(setq is nil)
      )
    )
    (setq is nil)
  )
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun c:ag ()
  (setq	selectiongroup
	 (car
	   (entsel "\nSelectionnez le groupe de reference :")
	 )
  )
  (isgroup selectiongroup)
  (setq group groupdef)
  (if is
    (addgroup group)
    (progn
      (princ "\nCe n'est pas un groupe")
    )
  )
  (princ)
)

