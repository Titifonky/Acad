;;
;;
;;reçoit liste de données des entites
;;renvoi une liste avec les noms et les info suivantes
;;0 10 11 40 42 50 51 70
(defun div (listent /)
  (setq	listent	(vl-remove-if-not
		  '(lambda (x)
		     (or
		       (= 0 (car x))
		       (= 10 (car x))
		       (= 42 (car x))
		       (= 70 (car x))
		     )
		   )
		  listent
		)
  )
  (setq	listent
	 (cond
	   ((member '(70 . 1) listent)
	    (progn
	      (setq firstpoint (list (cadr (member '(70 . 1) listent))))
	      (setq listent (append listent firstpoint))
	    )
	   )
	   (t x)
	 )
  )
  (if (not (member '(0 . "LWPOLYLINE") listent))
    (exit)
  )


  (setq listpt (cddr listent))
  (setq listent (list listent))
  (setq	lg (- (length listpt) 1)
  )
  (setq i 0)
  (while (< i lg)
    (setq pts (list (nth i listpt)
		    (nth (1+ i) listpt)
		    (nth (+ 2 i) listpt)
	      )
    )
    (if	(= (cdr (nth 1 pts)) 0.0)
      (progn
	(setq newent (list
		       (list
			 (list 0 "LINE")
;;;			 (nth 0 pts)
;;;			 (subst 11 10 (nth 2 pts))
			 (list 12
			       (distance (cdr (nth 0 pts))
					 (cdr (nth 2 pts))
			       )
			 )

		       )
		     )
	)
	(setq listent (append listent newent))
      )
      (progn
	(setq
	  angl (/ (* (atan (cdr (nth 1 pts))) 4)
		  2
	       )
	)
	(setq
	  dist (/ (distance (cdr (nth 0 pts))
			    (cdr (nth 2 pts))
		  )
		  2
	       )
	)
	(setq
	  r (list 40 (/ dist (sin (abs angl))))
	)
	(setq ptc1 (nth 0 pts)
	      ptc2 (nth 2 pts)
	)
	(setq newent (list
		       (list
			 (list 0 "ARC")
;;;			 (nth 0 pts)
;;;			 (subst 11 10 (nth 2 pts))
			 (list 13 (abs (* angl (cadr r))))
			 (list 14 (abs angl))
			 r
		       )
		     )
	)
	(setq listent (append listent newent))
      )
    )
    (setq i (+ i 2))
  )

  (setq listent (cdr listent))
)

(defun dvpfn (ri ang ep typedvp /)
  (cond
    ((= typedvp 0)
     (if (< ri (* 3 ep))
       (setq dvpla (list (* (abs ang) (+ ri (/ ep 3)))
			 (* (abs ang) (+ ri (/ ep 3)))
		   )
       )
       (setq dvpla (list (* (abs ang) (+ ri (/ ep 2)))
			 (* (abs ang) (+ ri (/ ep 2)))
		   )
       )
     )
    )
    ((= typedvp 1)
     (setq dvpla (list ri ri))
    )
  )
)






(defun c:dvp ()
  (setvar "flatland" 0)
  (setq nomobj (car (entsel "\nSelectionnez la tôle à developper :")))
  (initget "Ci Fn")
  (setq	typedvp
	 (getkword
	   "\nSelectionnez le type de developpé - Fibre neutre [Fn] ou Cotes interieurs [Ci] :"
	 )
  )
  (setq	ptinsertion
	 (getpoint
	   "\nIndiquez le point d'insertion du developpé :"
	 )
  )
  (setq	typedvp	(if (= typedvp "Ci")
		  (setq typedvp 1)
		  (setq typedvp 0)
		)
  )
  (setq entdata (div (entget nomobj)))
  (setq entpos (list nil))
  (foreach x entdata
    (if	(assoc 12 x)
      (setq entpos (append entpos (list (car (assoc 12 x)))))
      (setq entpos (append entpos (list (car (assoc 13 x)))))
    )
  )
  (setq entpos (cdr entpos))
  (setq lg (length entpos))
  (princ)
  (setq i 0)
  (while (< i lg)
    (cond
      ((and (= (nth i entpos) 12)
	    (= (nth (1+ i) entpos) 12)
	    (= (nth (+ i 2) entpos) 12)
       )
       (progn (setq dp (1+ i)) (setq i (+ lg 1)))
      )
    )
    (setq i (1+ i))
  )
  (setq ep (cadr (assoc 12 (nth dp entdata))))
  (setq nbface (/ (- lg 2) 2))
  (setq	i 1
	k 0
	l 0
  )
  (setq	face1 (list nil)
	face2 (list nil)
  )
  (while (<= i nbface)
    (if	(>= (+ i dp) lg)
      (setq k (- (1- lg) (+ i dp)))
      (setq k (+ i dp))
    )
    (setq face1 (append face1 (list (nth k entdata))))
    (if	(< (- dp i) 0)
      (setq l (+ lg (- dp i)))
      (setq l (- dp i))
    )
    (setq face2 (append face2 (list (nth l entdata))))
    (setq i (1+ i))
  )
  (setq	face1
	      (cdr face1)
	face2
	      (cdr face2)
  )
  (setq	i   0
	dvp (list nil)
  )
  (setq lgface (length face1))
  (while (< i lgface)
    (if	(= (cadar (nth i face1)) "LINE")
      (setq dvpla (list (cadr (assoc 12 (nth i face1)))))
    )
    (if	(cadr (assoc 40 (nth i face1)))
      (if (< (cadr (assoc 40 (nth i face1)))
	     (cadr (assoc 40 (nth i face2)))
	  )
	(progn
	  (setq ri (cadr (assoc 40 (nth i face1))))
	  (setq ang (cadr (assoc 14 (nth i face1))))
	  (dvpfn ri ang ep typedvp)
	)
	(progn
	  (setq ri (cadr (assoc 40 (nth i face2))))
	  (setq ang (cadr (assoc 14 (nth i face2))))
	  (dvpfn ri ang ep typedvp)
	)
      )
    )
    (setq dvp (append dvp dvpla))
    (setq i (1+ i))
    (princ)
  )
  (setq dvp (cdr dvp))
  (setq lgdvp 0)
  (foreach n dvp (setq lgdvp (+ lgdvp (abs n))))
  (setq pvd (reverse dvp))
  (setq dvppt (list ptinsertion))
  (setq x1 (car ptinsertion))
  (foreach n dvp
    (setq x1 (+ x1 n))
    (setq dvppt (append dvppt (list (list x1 (cadr ptinsertion)))))
  )
  (setq dvppt (cdr dvppt))
  (setq x1 0)
  (setq x2 0)
  (setq
    pvdpt (list (list (car (last dvppt)) (+ ep (cadr (last dvppt)))))
  )
  (foreach n pvd
    (setq x1 (+ x1 n))
    (setq x2 (+ (- lgdvp x1) (car ptinsertion)))
    (setq
      pvdpt (append pvdpt (list (list x2 (+ ep (cadr ptinsertion)))))
    )
  )
  (setq pvdpt (append pvdpt (list ptinsertion)))
  (setq dvppts (append dvppt pvdpt))
  (setq listtrans '(nil))
  (setq	dvppts
	 (foreach n dvppts
	   (setq listtrans (append listtrans (list (append '(10) n))))
	 )
  )
  (setq listtrans (cdr listtrans))
  (if (tblsearch "layer" "01-1-Développé")
    (entmake (list (cons 0 "LAYER")
		   (cons 100 "AcDbSymbolTableRecord")
		   (cons 100 "AcDbLayerTableRecord")
		   (cons 2 "01-1-Développé")
		   (cons 70 64)
		   (cons 62 0)
		   (cons 6 "CONTINUOUS")
	     )
    )
  )
  (entmake (append (list '(0 . "LWPOLYLINE")
			 '(100 . "AcDbEntity")
			 '(100 . "AcDbPolyline")
			 (cons 70 1)
			 (cons 90 5)
			 (cons 8 "01-1-Développé")
			 (cons 62 256)
		   )
		   listtrans
	   )
  )
)