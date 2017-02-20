(defun c:tsee ()
  (setvar "FILLETRAD" 20)
  (Raccorder_Polyligne (car (entsel)))
)


(defun c:RCP ()
  (setvar "CMDECHO" 0)

  (command "_zoom" "_e")

  (command "_qsave")

  (setq	Sel   (ssget "_X" '((0 . "LINE") (8 . "LIGNES DE PLIAGE")))
	i     0
	PtMin '(1000000 1000000)
	PtMax '(-1000000 -1000000)
  )

  (if Sel
    (repeat (sslength Sel)
      (setq Ligne (entget (ssname Sel i))
	    PtMin (mapcar 'min
			  PtMin
			  (car (dxf '10 Ligne))
			  (car (dxf '11 Ligne))
		  )
	    PtMax (mapcar 'max
			  PtMax
			  (car (dxf '10 Ligne))
			  (car (dxf '11 Ligne))
		  )
      )
      (setq i (1+ i))
    )
  )

  (setq	Sel (ssget "_W"
		   PtMin
		   PtMax
		   '((0 . "ARC,LINE"))
	    )
	i   0
  )

  (setvar "PEDITACCEPT" 1)
  (command "pedit" "m" Sel "")
  (command "j" 0.1 "")

;;;  (setq	PtMin (mapcar '+ PtMin '(10 10))
;;;	PtMax (mapcar '- PtMax '(10 10))
;;;  )

  (setq	Sel (ssget "_W"
		   PtMin
		   PtMax
		   '((0 . "LWPOLYLINE"))
	    )
	i   0
  )

  (setvar "FILLETRAD" 0.26)

  (if Sel
    (repeat (sslength Sel)
      (setq pl (ssname Sel i))
      (Raccorder_Polyligne pl)
      (setq i (1+ i))
    )
  )
  (princ)
)

(defun Raccorder_Polyligne (ent / lst)
  ;; Nettoyage de la polyligne
  ;; On supprime le dernier sommet si celui ci est superposé avec le premier
  ;; et on la déclare fermée
  (setq	data (entget ent)
	pts  (dxf '10 Data)
  )
  (cond
    ((and
       (= (cdr (assoc 0 data)) "LWPOLYLINE")
       (apply 'and
	      (mapcar (function (lambda (x1 x2) (equal x1 x2 1e-9)))
		      (car pts)
		      (last pts)
	      )
       )
     )
     (setq data	(reverse data)
	   data	(remove-ele (assoc 10 data) data)
	   data	(remove-ele (assoc 40 data) data)
	   data	(remove-ele (assoc 41 data) data)
	   data	(remove-ele (assoc 42 data) data)
	   data	(reverse data)
	   data	(subst (cons 70 1) (assoc 70 data) data)
     )
     (entmod data)
     (setq modifier t)
    )
    ((= (cdr (assoc 70 data)) 1)
     (setq modifier t)
    )
  )
  (setq modifier t)
  (if modifier
    (progn
      ;; Pour éviter d'avoir des pb de raccord
      ;; On décale la polyligne dans les deux sens.
      (setq rayon  (getvar "FILLETRAD")
	    enttmp (Courbe-Decaler ent rayon nil)
      )
      (entdel ent)
      (setq ent (Courbe-Decaler enttmp (* 2.0 rayon) t))
      (entdel enttmp)
      (setq enttmp (Courbe-Decaler ent rayon nil))
      (entdel ent)
      (setq ent	  enttmp
	    data  (entget ent)
	    ferme (= (cdr (assoc 70 data)) 1)
      )
      (setq lent (entlast))
      (command "_explode" ent)
      (setq lent (entnext lent))
      (while lent
	(setq lst  (cons lent lst)
	      lent (entnext lent)
	)
      )
      (setq lst (reverse lst))
      (mapcar (function
		(lambda	(seg1 seg2)
		  (setq	ptcomm (car (common-fuzz
				      (Courbe-PtDepartPtArrive seg1)
				      (Courbe-PtDepartPtArrive seg2)
				      1e-4
				    )
			       )
			deriv1 (Courbe-Derivee1
				 seg1
				 (Courbe-ParamAuPoint seg1 (Courbe-PointLePlusProche seg1 ptcomm))
			       )
			deriv2 (Courbe-Derivee1
				 seg2
				 (Courbe-ParamAuPoint seg2 (Courbe-PointLePlusProche seg2 ptcomm))
			       )
		  )
		  (if (not (colinear deriv1 deriv2 1e-5))
		    (progn
		      (setq pt1	(Courbe-PointALaDistance
				  seg1
				  0.1
				  (Courbe-Position seg1 ptcomm)
				)
			    pt2	(Courbe-PointALaDistance
				  seg2
				  0.1
				  (Courbe-Position seg2 ptcomm)
				)
		      )
;;;		      (make_point pt1 0)
;;;		      (make_point pt2 0)
		      (command "_fillet" (list seg1 pt1) (list seg2 pt2))
		    )
		  )

		)
	      )
	      lst
	      (if ferme
		(rot1 lst)
		(cdr lst)
	      )
      )
    )
  )
  (setq modifier nil)
)