;;
;;
;;reçoit liste de données des entites
;;renvoi une liste avec les noms et les info suivantes
;;0 10 11 40 42 50 51 70
(defun div (listent / i firstpoint listpt pts lg newent)
  (setq	listent	(vl-remove-if-not
		  '(lambda (x)
		     (or
		       (= 0 (car x))
		       (= 10 (car x))
		       (= 11 (car x))
		       (and (not (= 0.0 (cdr x))) (= 40 (car x)))
		       (= 42 (car x))
		       (= 50 (car x))
		       (= 51 (car x))
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
				   (nth 0 pts)
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

  (setq	listent
	 (vl-remove-if
	   '(lambda (x)
	      (cond ((member '(0 . "LWPOLYLINE") x) x)
		    (t nil)
	      )
	    )
	   listent
	 )
  )
)

(defun ptcarc (pts   /	   angl	 angint1     angint2	 angptc
	       listptc	   delta ptx   pty   ang   dist	 r1    r2
	       r     s	   m	 p     a     b	   c	 d     a1
	       b1    c1	   x1	 x2    y1    y2	   int1	 int2  ang
	      )
  (setq	angl
	 (/ (* (atan (cdr (nth 1 pts))) 4)
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
  (setq	r1 (cons 40
		 (/ dist (sin (abs angl)))
	   )
	r2 r1
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

;;;Recoit info lignes
;;;Retourne 0 ou 1 points

(defun intll (pt1 pt2 pt3 pt4 / int)
  (setq	pt1 (cdr pt1)
	pt2 (cdr pt2)
	pt3 (cdr pt3)
	pt4 (cdr pt4)
  )
  (setq int1 (inters pt1 pt2 pt3 pt4))
)

;;;Recoit info ligne et cercle ou arc
;;;Retourne 0, 1 ou 2 points

(defun intlca (pt1 pt2 ptc r / a b k m a1 b1 c1 delta x1 x2 y1 y2)
  (setq	pt1 (cdr pt1)
	pt2 (cdr pt2)
	ptc (cadr ptc)
	r   (cadr r)
  )
  (setq	k (car ptc)
	m (last ptc)
  )
  (setq	a (/ (float (- (cadr pt2) (cadr pt1)))
	     (- (car pt2) (car pt1))
	  )
  )
  (setq b (- (cadr pt1) (* a (car pt1))))
  (setq a1 (1+ (expt a 2)))
  (setq b1 (+ (* -2 k) (- (* (* 2 a) b) (* (* 2 a) m))))
  (setq
    c1 (- (+ (- (+ (expt k 2) (expt b 2)) (* (* 2 b) m)) (expt m 2))
	  (expt r 2)
       )
  )
  (setq delta (- (expt b1 2) (* 4 a1 c1)))
  (if (>= delta 0.0)
    (progn
      (if (= delta 0.0)
	(progn
	  (setq x1 (/ (- (* b1 -1) (sqrt delta)) (float (* 2 a1))))
	  (setq y1 (+ (* a x1) b))
	  (setq	int1 (osnap (list x1 y1) "INT")
		int2 nil
	  )
	)
	(progn
	  (setq x1 (/ (- (* b1 -1) (sqrt delta)) (float (* 2 a1))))
	  (setq x2 (/ (+ (* b1 -1) (sqrt delta)) (float (* 2 a1))))
	  (setq y1 (+ (* a x1) b))
	  (setq y2 (+ (* a x2) b))
	  (setq	int1 (osnap (list x1 y1) "INT")
		int2 (osnap (list x2 y2) "INT")
	  )
	)
      )
    )
    (setq int1 nil
	  int2 nil
    )
  )
)

;;;Recoit info cercles ou arcs
;;;Retourne 0, 1 ou 2 points

(defun intcca (ptc1 ptc2 r1 r2 / a b c d r s m p a1 b1 c1 delta	x1 x2 y1
	       y2)
  (setq	ptc1 (cdr ptc1)
	ptc2 (cdr ptc2)
	r1   (cadr r1)
	r2   (cadr r2)
  )
  (setq	a (car ptc1)
	b (cadr ptc1)
	c (car ptc2)
	d (cadr ptc2)
	r r1
	s r2
  )
  (setq	m (/ (-	(+ (expt s 2) (expt a 2) (expt b 2))
		(expt r 2)
		(expt c 2)
		(expt d 2)
	     )
	     (* 2 (- b d))
	  )
  )
  (setq p (/ (- a c) (- b d)))
  (setq a1 (+ 1 (expt p 2)))
  (setq b1 (- (* 2 b p) (* 2 m p) (* 2 a)))
  (setq	c1 (- (+ (expt a 2) (expt b 2) (expt m 2))
	      (expt r 2)
	      (* 2 b m)
	   )
  )
  (setq delta (- (expt b1 2) (* 4 a1 c1)))
  (if (>= delta 0.0)
    (progn
      (if (= delta 0.0)
	(progn
	  (setq x1 (/ (- (* b1 -1) (sqrt delta)) (float (* 2 a1))))
	  (setq y1 (- m (* x1 p)))
	  (setq	int1 (osnap (list x1 y1) "INT")
		int2 nil
	  )
	)
	(progn
	  (setq x1 (/ (- (* b1 -1) (sqrt delta)) (float (* 2 a1))))
	  (setq x2 (/ (+ (* b1 -1) (sqrt delta)) (float (* 2 a1))))
	  (setq y1 (- m (* x1 p)))
	  (setq y2 (- m (* x2 p)))
	  (setq	int1 (osnap (list x1 y1) "INT")
		int2 (osnap (list x2 y2) "INT")
	  )
	)
      )
    )
    (setq int1 nil
	  int2 nil
    )
  )
)


(defun c:diva ()
  (setvar "flatland" 0)
  (setq nomobj (car (entsel "\nSelectionnez l'objet à couper :")))
  (setq entdata (div (entget nomobj)))
  (princ "\nSelectionnez les objets coupants :")
  (setq objsel (ssget))
  (if (ssmemb nomobj objsel)
    (setq objsel (ssdel nomobj objsel))
  )
  (setq entitesdata (div (entget (ssname objsel 0))))
  (setq i 1)
  (while (ssname objsel i)
    (setq entitesdata
	   (append entitesdata (div (entget (ssname objsel i))))
    )
    (setq i (1+ i))
  )
  (setq i 0)
  (setq j 0)
  (setq listptint '(nil))
  (setq lg (length entdata))
  (setq lgs (length entitesdata))
  (while (< i lg)
    (setq entbase (nth i entdata))
    (while (< j lgs)
      (setq entdiv (nth j entitesdata))
      (cond
	((and (equal (cdar entbase) "LINE")
	      (equal (cdar entdiv) "LINE")
	 )
	 (intll	(assoc 10 entbase)
		(assoc 11 entbase)
		(assoc 10 entdiv)
		(assoc 11 entdiv)
	 )
	 (setq listptint (append (list int1) listptint))
	)
	((and (equal (cdar entbase) "LINE")
	      (or (equal (cdar entdiv) "ARC")
		  (equal (cdar entdiv) "CIRCLE")
	      )
	 )
	 (princ "lc")
	)
	((and (or (equal (cdar entbase) "ARC")
		  (equal (cdar entbase) "CIRCLE")
	      )
	      (or (equal (cdar entdiv) "ARC")
		  (equal (cdar entdiv) "CIRCLE")
	      )
	 )
	 (princ "cc")
	)
	((and (or (equal (cdar entbase) "ARC")
		  (equal (cdar entbase) "CIRCLE")
	      )
	      (equal (cdar entdiv) "LINE")
	 )
	 (princ "cl")
	)
	(t nil)
      )
      (setq j (1+ j))
    )
    (setq i (1+ i))
  )
)
