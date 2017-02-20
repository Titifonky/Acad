;;; spyDictionnaries
;;; Retourne une liste triée de tous les groupes auquel un objet fait partie.
;;; Reçoit un objet (ename)
;;; Retourne une liste de noms.
;;; Par Serge cAMIRÉ, cAD-NOVATION. 2005-02-21
(defun spyDictionnaries	(object		   /
			 AllDictObjects	   AllDictObjectsLength
			 AllDictObjectsLength
			 existIn	   groupDictionnary
			 isMemberOf	   MyDictObjs
			 names		   objectGet
			 pos
			)
  (setq objectGet (entget object))
  (setq isMemberOf nil)

  (setq groupDictionnary (dictsearch (namedobjdict) "acad_group"))
  (if groupDictionnary
    (progn
      (setq names (mapcar 'cdr
			  (vl-remove-if-not
			    '(lambda (x) (= 3 (car x)))
			    groupDictionnary
			  )
		  )
      )
      (setq AllDictObjects
	     (mapcar 'cdr
		     (vl-remove-if-not
		       '(lambda (x) (= 350 (car x)))
		       groupDictionnary
		     )
	     )
      )
      ;; Si on ramasse des objets 330 non pertinents, ce n'est pas grave puisqu'on les filtrera
      ;; mais ça donne du code plus court
      (setq MyDictObjs
	     (mapcar 'cdr
		     (vl-remove-if-not
		       '(lambda (x) (= 330 (car x)))
		       objectGet
		     )
	     )
      )
      (setq AllDictObjectsLength (length AllDictObjects))
      (foreach MyDictObj MyDictObjs
	(setq existIn (member MyDictObj AllDictObjects))
	(if existIn
	  (progn
	    (setq pos (- AllDictObjectsLength (length existIn)))
	    (setq isMemberOf (cons (nth pos names) isMemberOf))
	  )
	)
      )
    )
  )
  (acad_strlsort isMemberOf)
)

(defun c:test (
	       /
	       object
	       objectSel
	       reponse
	      )
  (setq objectSel (entsel "\nChoisissez l'objet: "))
  (setq object (car objectSel))
  (setq reponse (spyDictionnaries object))
  (if reponse
    (progn
      (princ "\nLes groupes sont: ")
      (princ reponse)
    )
    (progn
      (princ "\nAucun groupe.")
    )
  )
  (princ)
)