(defun c:pgp ()
  (setq dictgroup (dictsearch (namedobjdict) "acad_group"))
  (setq	dictgroup (mapcar 'cdr
			  (vl-remove-if-not
			    '(lambda (x) (= 350 (car x)))
			    dictgroup
			  )
		  )
  )
  (setq	i 0
	j 0
  )
  (while (< i (length dictgroup))
    (setq groupdf (entget (nth i dictgroup)))
    (if	(or (= 71 (car (last groupdf)))
	    (<= (length (member (assoc 340 groupdf) groupdf)) 1)
	)
      (progn
	(entdel (nth i dictgroup))
	(setq j (1+ j))
      )
    )
    (setq i (1+ i))
  )
  (princ (strcat "\n" (itoa j) " groupes ont été suprimés."))
  (princ)
)