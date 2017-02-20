(defun c:dg ()
  ;;creer un jeu de selection
  (setq pick (getvar "PICKSTYLE"))
  (setvar "PICKSTYLE" 0)
  (setq	selection (ssget)
	i 0
	j 0
  )
  (while (< i (sslength selection))

    ;;recherche le numero du groupe
    (setq
      groupnumero
       (cdr (nth (- (length (entget (ssname selection i)))
		    (length (member '(102 . "}")
				    (entget (ssname selection i))
			    )
		    )
		    1
		 )
		 (entget (ssname selection i))
	    )
       )
    )

    ;;test si groupnumero n'est pas nil
    (if	groupnumero
      (progn
	(setq groupdef (entget groupnumero))
	;;test si groupnumero est bien un groupe
	(if (equal (cadr groupdef) '(0 . "GROUP"))
	  (entdel groupnumero)
	)
      )
    )
    (setq i (1+ i))
  )
  (setvar "PICKSTYLE" pick)
  (princ)
)