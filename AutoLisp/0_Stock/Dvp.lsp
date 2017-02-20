;;
;;
;;reçoit liste de données des entites
;;renvoi une liste avec les noms et les info suivantes
;;0 10 11 40 42 50 51 70
(defun div (listent  /	      firstpoint	listpt	 lg
	    newent   pts      angl     dist	r	 ptc1
	    ptc2
	   )
  (setq	listent	(vl-remove-if-not
		  '(lambda (x)
		     (or
		       (= 0 (car x))
		       (= 10 (car x))
		       (= 42 (car x))
		       (= 70 (car x))
		     ) ;_ Fin de or
		   ) ;_ Fin de lambda
		  listent
		) ;_ Fin de vl-remove-if-not
  ) ;_ Fin de setq
  (setq	listent
	 (cond
	   ((member '(70 . 1) listent)
	    (progn
	      (setq firstpoint (list (cadr (member '(70 . 1) listent))))
	      (setq listent (append listent firstpoint))
	    ) ;_ Fin de progn
	   )
	   (t x)
	 ) ;_ Fin de cond
  ) ;_ Fin de setq
  (if (not (member '(0 . "LWPOLYLINE") listent))
    (exit)
  ) ;_ Fin de if


  (setq listpt (cddr listent))
  (setq listent (list listent))
  (setq	lg (- (length listpt) 1)
  ) ;_ Fin de setq
  (setq i 0)
  (while (< i lg)
    (setq pts (list (nth i listpt)
		    (nth (1+ i) listpt)
		    (nth (+ 2 i) listpt)
	      ) ;_ Fin de list
    ) ;_ Fin de setq
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
			       ) ;_ Fin de distance
			 ) ;_ Fin de list

		       ) ;_ Fin de list
		     ) ;_ Fin de list
	) ;_ Fin de setq
	(setq listent (append listent newent))
      ) ;_ Fin de progn
      (progn
	(setq
	  angl (/ (* (atan (cdr (nth 1 pts))) 4)
		  2
	       ) ;_ Fin de /
	) ;_ Fin de setq
	(setq
	  dist (/ (distance (cdr (nth 0 pts))
			    (cdr (nth 2 pts))
		  ) ;_ Fin de distance
		  2
	       ) ;_ Fin de /
	) ;_ Fin de setq
	(setq
	  r (list 40 (/ dist (sin (abs angl))))
	) ;_ Fin de setq
	(setq ptc1 (nth 0 pts)
	      ptc2 (nth 2 pts)
	) ;_ Fin de setq
	(setq newent (list
		       (list
			 (list 0 "ARC")
;;;			 (nth 0 pts)
;;;			 (subst 11 10 (nth 2 pts))
			 (list 13 (abs (* angl (cadr r))))
			 (list 14 (abs angl))
			 r
		       ) ;_ Fin de list
		     ) ;_ Fin de list
	) ;_ Fin de setq
	(setq listent (append listent newent))
      ) ;_ Fin de progn
    ) ;_ Fin de if
    (setq i (+ i 2))
  ) ;_ Fin de while

  (setq listent (cdr listent))
) ;_ Fin de defun

(defun dvpfn (ri ang ep typedvp /)
  (cond
    ((= typedvp 0)
     (setq dvpla (list (* (abs ang) (+ ri (/ ep 2)))
		       (* (abs ang) (+ ri (/ ep 2)))
		 ) ;_ Fin de list
     ) ;_ Fin de setq
    )
    ((= typedvp 1)
     (setq dvpla (list ri ri))
    )
  ) ;_ Fin de cond
) ;_ Fin de defun






(defun c:dvp (/	       nomobj	typedvp	 ptinsertion	   entdata
	      entpos   lg	i	 dp	  k	   l
	      nbface   face1	face2	 dvp	  lgface   ri
	      ang      pvd	lgdvp	 x1	  dvppt	   pvdpt
	      x2       objectcolor	 dvppt1	  pvdpt1   dvppts
	      listtrans		n
	     )
  (setvar "flatland" 0)
  (setq nomobj (car (entsel "\nSelectionnez la tôle à developper :")))
  (initget "F C _Fn Ci")
  (setq	typedvp
	 (getkword
	   "\nSelectionnez le type de developpé - Fibre neutre [Fn] ou Cotes interieurs [Ci] :"
	 ) ;_ Fin de getkword
  ) ;_ Fin de setq
  (setq	ptinsertion
	 (getpoint
	   "\nIndiquez le point d'insertion du developpé :"
	 ) ;_ Fin de getpoint
  ) ;_ Fin de setq
  (setq	typedvp	(if (= typedvp "Ci")
		  (setq typedvp 1)
		  (setq typedvp 0)
		) ;_ Fin de if
  ) ;_ Fin de setq
  (setq entdata (div (entget nomobj)))
  (setq entpos (list nil))
  (foreach x entdata
    (if	(assoc 12 x)
      (setq entpos (append entpos (list (car (assoc 12 x)))))
      (setq entpos (append entpos (list (car (assoc 13 x)))))
    ) ;_ Fin de if
  ) ;_ Fin de foreach
  (setq entpos (cdr entpos))
  (setq lg (length entpos))
  (princ)
  (setq i 0)
  (while (< i lg)
    (cond
      ((and (= (nth i entpos) 12)
	    (= (nth (1+ i) entpos) 12)
	    (= (nth (+ i 2) entpos) 12)
       ) ;_ Fin de and
       (progn (setq dp (1+ i)) (setq i (+ lg 1)))
      )
    ) ;_ Fin de cond
    (setq i (1+ i))
  ) ;_ Fin de while
  (setq ep (cadr (assoc 12 (nth dp entdata))))
  (setq nbface (/ (- lg 2) 2))
  (setq	i 1
	k 0
	l 0
  ) ;_ Fin de setq
  (setq	face1 (list nil)
	face2 (list nil)
  ) ;_ Fin de setq
  (while (<= i nbface)
    (if	(>= (+ i dp) lg)
      (setq k (- (1- lg) (+ i dp)))
      (setq k (+ i dp))
    ) ;_ Fin de if
    (setq face1 (append face1 (list (nth k entdata))))
    (if	(< (- dp i) 0)
      (setq l (+ lg (- dp i)))
      (setq l (- dp i))
    ) ;_ Fin de if
    (setq face2 (append face2 (list (nth l entdata))))
    (setq i (1+ i))
  ) ;_ Fin de while
  (setq	face1
	      (cdr face1)
	face2
	      (cdr face2)
  ) ;_ Fin de setq
  (setq	i   0
	dvp (list nil)
  ) ;_ Fin de setq
  (setq lgface (length face1))
  (while (< i lgface)
    (if	(= (cadar (nth i face1)) "LINE")
      (setq dvpla (list (cadr (assoc 12 (nth i face1)))))
    ) ;_ Fin de if
    (if	(cadr (assoc 40 (nth i face1)))
      (if (< (cadr (assoc 40 (nth i face1)))
	     (cadr (assoc 40 (nth i face2)))
	  ) ;_ Fin de <
	(progn
	  (setq ri (cadr (assoc 40 (nth i face1))))
	  (setq ang (cadr (assoc 14 (nth i face1))))
	  (dvpfn ri ang ep typedvp)
	) ;_ Fin de progn
	(progn
	  (setq ri (cadr (assoc 40 (nth i face2))))
	  (setq ang (cadr (assoc 14 (nth i face2))))
	  (dvpfn ri ang ep typedvp)
	) ;_ Fin de progn
      ) ;_ Fin de if
    ) ;_ Fin de if
    (setq dvp (append dvp dvpla))
    (setq i (1+ i))
    (princ)
  ) ;_ Fin de while
  (setq dvp (cdr dvp))
  (setq lgdvp 0)
  (foreach n dvp (setq lgdvp (+ lgdvp (abs n))))
  (setq pvd (reverse dvp))
  (setq dvppt '(nil))
  (setq x1 (car ptinsertion))
  (foreach n dvp
    (setq x1 (+ x1 n))
    (setq dvppt (append dvppt (list (list x1 (cadr ptinsertion)))))
  ) ;_ Fin de foreach
  (setq
    dvppt (subst
	    (list (car ptinsertion) (cadr ptinsertion))
	    nil
	    dvppt
	  ) ;_ Fin de subst
  ) ;_ Fin de setq
  (setq x1 0)
  (setq x2 0)
  (setq
    pvdpt (list (list (car (last dvppt)) (+ ep (cadr (last dvppt)))))
  ) ;_ Fin de setq
  (foreach n pvd
    (setq x1 (+ x1 n))
    (setq x2 (+ (- lgdvp x1) (car ptinsertion)))
    (setq
      pvdpt (append pvdpt (list (list x2 (+ ep (cadr ptinsertion)))))
    ) ;_ Fin de setq
  ) ;_ Fin de foreach
  (setq
    pvdpt (append pvdpt
		  (list (list (car ptinsertion) (cadr ptinsertion)))
	  ) ;_ Fin de append
  ) ;_ Fin de setq
  (setq dvppts (append dvppt pvdpt))
  (setq listtrans '(nil))
  (setq	dvppts
	 (foreach n dvppts
	   (setq listtrans (append listtrans (list (append '(10) n))))
	 ) ;_ Fin de foreach
  ) ;_ Fin de setq
  (setq listtrans (cdr listtrans))
  (entmake (append (list '(0 . "LWPOLYLINE")
			 '(100 . "AcDbEntity")
			 '(100 . "AcDbPolyline")
			 (cons 70 1)
			 (cons 90 (length listtrans))
			 (cons 8 "0")
			 (cons 62 256)
		   ) ;_ Fin de list
		   listtrans
	   ) ;_ Fin de append
  ) ;_ Fin de entmake
  (entmod (subst '(70 . 1) '(70 . 0) (entget (entlast))))
  (setq listent (list (list (entlast))))
  (setq	dvppt1	    (cdr dvppt)
	pvdpt1	    (cddr (reverse pvdpt))
	i	    0
	objectcolor 256
	n	    0
  ) ;_ Fin de setq
  (while (< i (- (length dvppt1) 1))
    (if	(or (= i 1) (= (- i n) 3))
      (progn
	(setq n i)
	(setq objectcolor 1)
      ) ;_ Fin de progn
      (setq objectcolor 256)
    ) ;_ Fin de if
    (entmake (list '(0 . "LINE")
		   '(8 . "01-1-Développé")
		   (cons 62 objectcolor)
		   (append '(10) (nth i dvppt1))
		   (append '(11) (nth i pvdpt1))
	     ) ;_ Fin de list
    ) ;_ Fin de entmake
    (setq listent (append listent (list (list (entlast)))))
    (setq i (1+ i))
  ) ;_ Fin de while
  (setq
    listent (mapcar '(lambda (x) (setq x (cons 340 (car x)))) listent)
  ) ;_ Fin de setq
  (entmake (append (list '(0 . "GROUP")
			 '(100 . "AcDbGroup")
			 '(70 . 1)
			 '(71 . 1)
		   ) ;_ Fin de list
		   listent
	   ) ;_ Fin de append
  ) ;_ Fin de entmake
  (princ)
) ;_ Fin de defun
