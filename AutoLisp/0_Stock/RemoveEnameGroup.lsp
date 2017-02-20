(defun isgroup2 (numeroent /)
  (setq
    groupnumero
     (cdr (nth (- (length (entget numeroent))
		  (length (member '(102 . "}")
				  (entget numeroent)
			  )
		  )
		  1
	       )
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

(defun purggroup ()
  (setq dictgroup (dictsearch (namedobjdict) "acad_group"))
  (setq	dictgroup (mapcar 'cdr
			  (vl-remove-if-not
			    '(lambda (x) (= 350 (car x)))
			    dictgroup
			  )
		  )
  )
  (setq i 0)
  (while (< i (length dictgroup))
    (setq groupdf (entget (nth i dictgroup)))
    (if	(or (= 71 (car (last groupdf)))
	    (<= (length (member (assoc 340 groupdf) groupdf)) 1)
	)
      (entdel (nth i dictgroup))
    )
    (setq i (1+ i))
  )
)

(defun c:eg ()
  (setq pick (getvar "PICKSTYLE"))
  (setvar "PICKSTYLE" 0)
  (princ "\nSelectionnez les objets à enlever du groupe :")
  (setq	selectionset
	 (ssget)
	i 0
  )
  (while (< i (sslength selectionset))
    (isgroup2 (ssname selectionset i))
    (if	is
      (progn
	(setq entname (cons 340 (ssname selectionset i)))
	(entmod (vl-remove entname groupdef))
      )
    )
    (setq i (1+ i))
  )
  (purggroup)
  (setvar "PICKSTYLE" pick)
  (princ)
)