;;
;;
;;reçoit liste de données des entites
;;renvoi une liste avec les noms et les info suivantes
;;0 10 11 40 42 50 51 70
(defun div (listent / firstpoint listpt pts lg newent)
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
  (setq listent (list listent))
  (setq	listent	(mapcar	'(lambda (x)
			   (cond
			     ((member '(70 . 1) x)
			      (progn
				(setq firstpoint
				       (list (cadr (member '(70 . 1) x)))
				)
				(setq x (append x firstpoint))
			      )
			     )
			     (t x)
			   )
			 )
			listent
		)
  )
  ;;transforme une polyligne en
  ;;arc et en ligne
  ;;

  (mapcar
    '(lambda (x)
       (cond
	 ((member '(0 . "LWPOLYLINE") x)
	  (progn
	    (setq listpt (cddr x))
	    (setq lg (length listpt))
	    (setq i 0)
	    (while (<= i lg)
	      (setq pts	(list (nth i listpt)
			      (nth (1+ i) listpt)
			      (nth (+ 2 i) listpt)
			)
	      )
	      (if (/= (car (nth 2 pts)) nil)
		(progn
		  (if (= (cdr (nth 1 pts)) 0.0)
		    (progn
		      (setq
			newent (list
				 (list
				   (cons 0 "LINE")
				   (nth 1 pts)
				   (subst 11 10 (nth 2 pts))
				 )
			       )
		      )
		      (setq listent (append listent newent))
		    )
		    (progn
		      (ptcarc pts)
		      (setq
			newent (list
				 (list
				   (cons 0 "ARC")
				   (cons 10 ptc)
				   (cons 40 (distance ptc ptc1))
				   (cons 50 ptc1)
				   (cons 51 ptc2)
				 )
			       )
		      )
		      (setq listent (append listent newent))

		    )
		  )
		)
	      )
	      (setq i (+ i 2))
	    )
	    ;;endwhile
	    (setq listent (cdr listent))
	  )
	  ;;endprogn
	 )
	 ;;end test expression
	 (t x)
       )
       ;;endcond
     )
    ;;endlambda
    listent
  )
  ;;endmapcar

)

(defun ptcarc (pts   /	   angl	 angint1     angint2	 angptc
	       listptc	   delta ptx   pty   ang   dist	 r1    r2
	       r     s	   m	 p     a     b	   c	 d     a1
	       b1    c1	   x1	 x2    y1    y2	   int1	 int2  ang
	      )
  (setq	ptc1 (nth 0 pts)
	ptc2 (nth 2 pts)
  )
  (setq	ptc1  (cdr ptc1)
	ptc2  (cdr ptc2)
	r1    (cdr r1)
	r2    (cdr r2)
	delta 1.0
	test  (rtos (abs (cdr (nth 1 pts))) 2 1)
  )
  ;;Angle au sommet de l'arc divisé par deux
  (setq	angl
	 (/ (* (atan (cdr (nth 1 pts))) 4)
	    2
	 )
  )
  ;;Distance entre les deux points
  (setq
    dist (/ (distance ptc1
		      ptc2
	    )
	    2
	 )
  )
  ;;Calcul du rayon à partir des données précédentes
  (setq	r1
	   (/ dist (sin (abs angl)))

	r2 r1
  )
  (if (eq test "1")
    (progn
      (setq ptx	(/ (+ (car ptc2) (car ptc1)) 2)
	    pty	(/ (+ (cadr ptc2) (cadr ptc1)) 2)
      )
      (setq ptc (list ptx pty))
    )
    (progn
      (setq a (car ptc1)
	    b (cadr ptc1)
	    c (car ptc2)
	    d (cadr ptc2)
	    r r1
	    s r2
      )
      (if (= b d)
	(setq diviseur 1)
	(setq diviseur (- b d))
      )
      (setq
	m (/ (-	(+ (expt a 2) (expt b 2))
		(expt c 2)
		(expt d 2)
	     )
	     (* 2 diviseur)
	  )

      )
      (setq p (/ (- a c) diviseur))
      (setq a1 (+ 1 (expt p 2)))
      (setq
	b1 (- (* 2 b p) (* 2 m p) (* 2 a))
      )
      (setq c1 (- (+ (expt a 2)
		     (expt b 2)
		     (expt m 2)
		  )
		  (expt r 2)
		  (* 2 b m)
	       )
      )
      (setq delta (- (expt b1 2) (* 4 a1 c1)))
      (setq x1 (/ (- (* b1 -1) (sqrt delta))
		  (float (* 2 a1))
	       )
      )
      (setq x2 (/ (+ (* b1 -1) (sqrt delta))
		  (float (* 2 a1))
	       )
      )
      (setq y1 (- m (* x1 p)))
      (setq y2 (- m (* x2 p)))
      (setq int1 (list x1 y1)
	    int2 (list x2 y2)
      )
      (setq ang (cdr (nth 1 pts)))
      (setq angint1 (angle ptc1 int1)
	    angint2 (angle ptc1 int2)
	    angptc  (angle ptc1 ptc2)
      )
      (setq angint1 (atof (angtos (- angint1 angptc) 3 6)
		    )
	    angint2 (atof (angtos (- angint2 angptc) 3 6)
		    )
      )
      (if (< angint1 angint2)
	(setq listptc (list 'int2 'int1))
	(setq listptc (list 'int1 'int2))
      )
      (if (> (* (abs angl) 2) pi)
	(setq listptc (reverse listptc))

      )
      (if (> ang 0)
	(setq ptc (eval (cadr listptc)))
	(setq ptc (eval (car listptc)))
      )
    )
  )
)


(defun c:trpoly	()
  (setvar "flatland" 0)
  (setq nomobj (car (entsel "\nSelectionnez la Polyligne :")))
  (setq entdata (div (entget nomobj)))
  (princ entdata)
  (prin1 "Nombre d'objets :")
  (prin1 (length entdata))
)
