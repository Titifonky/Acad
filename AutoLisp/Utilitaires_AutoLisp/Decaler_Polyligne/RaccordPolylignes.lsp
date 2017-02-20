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

  (command "pedit" "m" Sel "")
  (command "j" 0.1 "")

  (setq	PtMin (mapcar '+ PtMin '(10 10))
	PtMax (mapcar '- PtMax '(10 10))
  )

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
      ;; Nettoyage de la polyligne
      ;; On supprime le dernier sommet si celui ci est superposé avec le premier
      ;; et on la déclare fermée
      (setq data (entget pl)
	    pts	 (dxf '10 Data)
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
	 (setq data (reverse data)
	       data (remove-ele (assoc 10 data) data)
	       data (remove-ele (assoc 40 data) data)
	       data (remove-ele (assoc 41 data) data)
	       data (remove-ele (assoc 42 data) data)
	       data (reverse data)
	       data (subst (cons 70 1) (assoc 70 data) data)
	 )
	 (entmod data)
	 (setq modifier t)
	)
	((= (cdr (assoc 70 data)) 1)
	 (setq modifier t)
	)
      )
      (if modifier
	(progn
	  (Raccord_Polyligne pl)
	)
      )
      (setq modifier nil)
      (setq i (1+ i))
    )
  )
  (princ)

)
(defun c:tst ()
  (Raccord_Polyligne (car (entsel)))
)

(defun Raccord_Polyligne (ent / data pts ferme rayon enttmp lst seg)

  ;; Pour éviter d'avoir des pb de raccord
  ;; On décale la polyligne dans les deux sens.
  (setq	rayon  (getvar "FILLETRAD")
	enttmp (Courbe-Decaler ent rayon nil)
  )
  (entdel ent)
  (setq ent (Courbe-Decaler enttmp (* 2.0 rayon) t))
  (entdel enttmp)
  (setq enttmp (Courbe-Decaler ent rayon nil))
  (entdel ent)
  (setq	ent   enttmp
	data  (entget ent)
	ferme (= (cdr (assoc 70 data)) 1)
  )

  (if (= (cdr (assoc 0 data)) "LWPOLYLINE")
    (progn
      (setq lst	 (ListeSegments ent)
	    data (car lst)
	    data (if (> (length data) 2)
		   (setq data (ArcToPoint
				(car data)
				(cadr data)
				(caddr data)
				(cadddr data)
				(last data)
			      )
			 data (list (cons 10 (cadr data))
				    (list 42 (car data) (last data))
			      )
		   )
		   (mapcar 'cons '(10) data)
		 )
      )
      (mapcar
	(function
	  (lambda (seg1 seg2 / ent1 ent2 int pt1 pt2)
	    (cond
	      ;; Arc Arc
	      ((and (> (length seg1) 2) (> (length seg2) 2))
	       (and (setq ent1 (DecalerArc seg1 rayon t))
		    (setq ent2 (DecalerArc seg2 rayon t))
		    (setq int (IntersArcArc ent1 ent2))
	       )
	       (and (not int)
		    (setq ent1 (DecalerArc seg1 rayon nil))
		    (setq ent2 (DecalerArc seg2 rayon nil))
		    (setq int (IntersArcArc ent1 ent2))
	       )
	       (and (setq int (car int))
		    (setq pt1 (ProjeterPointSurArc seg1 int))
		    (setq pt2 (ProjeterPointSurArc seg2 int))
		    (if	(equal (distance pt1 pt2) 0 1e-9)
		      (setq
			data (append data
				     (list (cons 10 pt1)
					   (list 42 (car seg2) (last seg2))
				     )
			     )
		      )
		      (setq data
			     (append
			       data
			       (list
				 (cons 10 pt1)
				 (list
				   42
				   int
				   (if (< (distance (car seg1) int) (cadddr seg1))
				     (last seg1)
				     (not (last seg1))
				   )
				 )
				 (cons 10 pt2)
				 (list 42 (car seg2) (last seg2))
			       )
			     )
		      )
		    )
	       )
	      )

	      ;; Arc Ligne
	      ((and (> (length seg1) 2) (= (length seg2) 2))
	       (and (setq ent1 (DecalerArc seg1 rayon t))
		    (setq ent2 (DecalerLigne seg2 rayon t))
		    (setq int (IntersArcLigne ent1 ent2))
	       )
	       (and (not int)
		    (setq ent1 (DecalerArc seg1 rayon nil))
		    (setq ent2 (DecalerLigne seg2 rayon nil))
		    (setq int (IntersArcLigne ent1 ent2))
	       )

	       (and (setq int (car int))
		    (setq pt1 (ProjeterPointSurArc seg1 int))
		    (setq pt2 (ProjeterPointSurLigne seg2 int))
		    (if	(equal (distance pt1 pt2) 0 1e-9)
		      (setq data (append data (list (cons 10 pt1))))
		      (setq data
			     (append
			       data
			       (list
				 (cons 10 pt1)
				 (list
				   42
				   int
				   (if (< (distance (car seg1) int) (cadddr seg1))
				     (last seg1)
				     (not (last seg1))
				   )
				 )
				 (cons 10 pt2)
			       )
			     )
		      )
		    )
	       )
	      )

	      ;; Ligne Arc
	      ((and (= (length seg1) 2) (> (length seg2) 2))
	       (and (setq ent1 (DecalerLigne seg1 rayon t))
		    (setq ent2 (DecalerArc seg2 rayon t))
		    (setq int (IntersArcLigne ent2 ent1))
	       )
	       (and (not int)
		    (setq ent1 (DecalerLigne seg1 rayon nil))
		    (setq ent2 (DecalerArc seg2 rayon nil))
		    (setq int (IntersArcLigne ent2 ent1))
	       )
	       (and (setq int (car int))
		    (setq pt1 (ProjeterPointSurLigne seg1 int))
		    (setq pt2 (ProjeterPointSurArc seg2 int))
		    (if	(equal (distance pt1 pt2) 0 1e-9)
		      (setq
			data (append data
				     (list (cons 10 pt1)
					   (list 42 (car seg2) (last seg2))
				     )
			     )
		      )
		      (setq
			data (append
			       data
			       (list
				 (cons 10 pt1)
				 (list 42 int (trigo (car seg1) (cadr seg1) int))
				 (cons 10 pt2)
				 (list 42 (car seg2) (last seg2))
			       )
			     )
		      )
		    )
	       )
	      )

	      ;; Ligne Ligne
	      ((and (= (length seg1) 2) (= (length seg2) 2))
	       (and (setq ent1 (DecalerLigne seg1 rayon t))
		    (setq ent2 (DecalerLigne seg2 rayon t))
		    (setq int (IntersLigneLigne ent1 ent2))
	       )
	       (and (not int)
		    (setq ent1 (DecalerLigne seg1 rayon nil))
		    (setq ent2 (DecalerLigne seg2 rayon nil))
		    (setq int (IntersLigneLigne ent1 ent2))
	       )
	       (and int
		    (setq pt1 (ProjeterPointSurLigne seg1 int))
		    (setq pt2 (ProjeterPointSurLigne seg2 int))
		    (if	(equal (distance pt1 pt2) 0 1e-9)
		      (setq data (append data (list (cons 10 pt1))))
		      (setq
			data (append
			       data
			       (list
				 (cons 10 pt1)
				 (list 42
				       int
				       (trigo (car seg1) (cadr seg1) int)
				 )
				 (cons 10 pt2)
			       )
			     )
		      )
		    )
	       )
	      )
	    )
	  )
	)
	lst
	(rot1 lst)
      )
      (setq data (if (= (length (car lst)) 2)
		   (cdr data)
		   (cddr data)
		 )
	    data (append
		   (list (car data))
		   (mapcar
		     (function
		       (lambda (pt1 c pt2)
			 (if (= (car c) 42)
			   (cons 42
				 (PtToBulge (cadr c) (cdr pt1) (cdr pt2) (last c))
			   )
			   c
			 )
		       )
		     )
		     data
		     (rot1 data)
		     (rot2 data)
		   )
		 )
	    data (butlast data)
      )
      (CreerPloyligne data ferme)
      (setq data (entget ent)
	    data (subst '(8 . "EFFACER") (assoc 8 data) data)
	    )
      (entmod data)
    )
  )
  (princ)
)

(defun CreerPloyligne (Lstdxf f /)
  (entmake
    (append
      (list
	'(0 . "LWPOLYLINE")
	'(100 . "AcDbEntity")
	'(67 . 0)
	'(410 . "Model")
	;;'(8 . "0")
	'
	 (100
	  .
	  "AcDbPolyline"
	 )
      )
      (list (cons '90 (length (dxf '10 Lstdxf))))
      (list (cons '43 0.0))
      (list (cons '70
		  (if f
		    1
		    0
		  )
	    )
      )
      Lstdxf
    )
  )
  (entlast)
)

;; PROJETERPOINTSURARC
;; Renvoi le point projeté sur l'arc
(defun ProjeterPointSurArc (Arc Pt)
  (setq	ct  (car Arc)
	ang (angle ct Pt)
	ptp (polar ct ang (cadddr Arc))
  )
  (if (TstAngle ang (cadr Arc) (caddr Arc))
    ptp
    nil
  )
)

;; PROJETERPOINTSURLIGNE
;; Renvoi le point projeté sur la ligne
(defun ProjeterPointSurLigne (Ligne Pt)
  (setq	ptp (list (- (+ (car Pt) (cadar Ligne)) (cadadr Ligne))
		  (- (+ (cadr Pt) (caadr Ligne)) (caar Ligne))
	    )
	ptp (inters Pt ptp (car Ligne) (cadr Ligne) nil)
	;; On calcul le point d'intersection
	ptp (inters Pt ptp (car Ligne) (cadr Ligne) t)
	    ;; On verifie si le point est sur la ligne
  )
)


;; DECALERARC
;; Renvoi un arc décalé
(defun DecalerArc (Arc Dist Cote / r)
  (setq	r    (cadddr Arc)
	Dist (*	(if (xor Cote (last Arc))
		  -1
		  1
		)
		Dist
	     )
  )
  (if (< Dist r)
    (subst (+ r Dist) r Arc)
  )
)

;; DECALERLIGNE
;; Renvoi une ligne décalée
(defun DecalerLigne (Ligne Dist Cote / v vp pt)
  (setq	v  (vect (car Ligne) (cadr Ligne))
	vp (vperp v)
	vp (vunit vp)
	vp (vxs	vp
		(* (if Cote
		     -1
		     1
		   )
		   Dist
		)
	   )
	pt (v+v (car Ligne) vp)
  )
  (list pt (v+v pt v))
)

;; INTERSARCARC
;; Renvoi les points d'intersection entre deux arcs
(defun IntersArcArc (Arc1   Arc2   /	  ct1	 ct2	r1     r2     ang1
		     ang2   c	   alpha  beta	 depA1	arrA1  depA2  arrA2
		    )
  (setq	ct1   (car Arc1)
	ct2   (car Arc2)
	r1    (cadddr Arc1)
	r2    (cadddr Arc2)
	ang1  (angle ct1 ct2)
	ang2  (angle ct2 ct1)
	c     (distance ct1 ct2)
	depA1 (cadr Arc1)
	arrA1 (caddr Arc1)
	depA2 (cadr Arc2)
	arrA2 (caddr Arc2)
  )

  (cond
    ;; Si r2 = 0 ou r1 = 0 ou la distance entre les deux centre est supérieure à la somme des rayons
    ;; il n'y a pas d'intersection
    ((or (zerop r2) (zerop r1) (> (- c (+ r1 r2) 1e-9) 0.0))
     nil
    )

    ;; Si arrA1 = depA2 et que les points d'arrivé et de départ sont identiques
    ;; Ils sont tangent donc il n'y a qu'un point
    ((and (equal arrA1 depA2 1e-9)
	  (apply 'and
		 (mapcar (function (lambda (x1 x2) (equal x1 x2 1e-9)))
			 (polar ct1 arrA1 r1)
			 (polar ct2 depA2 r2)
		 )
	  )
     )
     (list (polar ct1 arrA1 r1))
    )
    ;; Si la distance entre les deux centre est égale à la somme des rayons
    ;; Une intersection possible
    ((equal c (+ r1 r2) 1e-9)
     (if (and (TstAngle ang1 depA1 arrA1)
	      (TstAngle ang2 depA2 arrA2)
	 )
       (list (polar ct1 ang1 r1))
     )
    )
    ;; Sinon
    ;; Deux intersections possible
    (t
     (setq
       alpha (acos
	       (/ (- (+ (expt r1 2) (expt c 2)) (expt r2 2)) (* 2 r1 c))
	     )
       beta  (acos
	       (/ (- (+ (expt r2 2) (expt c 2)) (expt r1 2)) (* 2 r2 c))
	     )
     )
     (append (if (and (TstAngle (+ ang1 alpha) depA1 arrA1)
		      (TstAngle (- ang2 beta) depA2 arrA2)
		 )
	       (list (polar ct1 (+ ang1 alpha) r1))
	     )
	     (if (and (TstAngle (- ang1 alpha) depA1 arrA1)
		      (TstAngle (+ ang2 beta) depA2 arrA2)
		 )
	       (list (polar ct1 (- ang1 alpha) r1))
	     )
     )
    )
  )
)

;; INTERSARCLIGNE
;; Renvoi le point d'intersection entre un arc et une ligne
(defun IntersArcLigne
		      (Arc Ligne / ct r pt ang dst alpha depA arrA depL arrL)
  (setq	ct   (car Arc)
	r    (cadddr Arc)
	pt   (list (- (+ (car ct) (cadar Ligne)) (cadadr Ligne))
		   (- (+ (cadr ct) (caadr Ligne)) (caar Ligne))
	     )
	pt   (inters ct pt (car Ligne) (cadr Ligne) nil)
	ang  (angle ct pt)
	dst  (distance ct pt)
	depA (cadr Arc)
	arrA (caddr Arc)
	depL (min (angle ct (car Ligne)) (angle ct (cadr Ligne)))
	arrL (max (angle ct (car Ligne)) (angle ct (cadr Ligne)))
  )
  (cond
    ;; Si r = 0 ou dst = 0
    ;; il n'y a pas d'intersection
    ((or (zerop r) (zerop dst))
     nil
    )
    ;; Si la distance entre la ligne et le centre est égale au rayon
    ;; Une intersection possible
    ((equal r dst 1e-9)
     (if (and (TstAngle ang depA arrA)
	      (TstAngle ang depL arrL)
	 )
       (list pt)
     )
    )
    ;; Sinon
    ;; Deux intersections possible
    (t
     (setq alpha (acos (/ dst r)))
     (append (if (and (TstAngle (+ ang alpha) depA arrA)
		      (TstAngle (+ ang alpha) depL arrL)
		 )
	       (list (polar ct (+ ang alpha) r))
	     )
	     (if (and (TstAngle (- ang alpha) depA arrA)
		      (TstAngle (- ang alpha) depL arrL)
		 )
	       (list (polar ct (- ang alpha) r))
	     )
     )
    )
  )
)

;; INTERSLIGNELIGNE
;; Renvoi le point d'intersection entre deux lignes
(defun IntersLigneLigne	(Ligne1 Ligne2)
  (inters (car Ligne1)
	  (cadr Ligne1)
	  (car Ligne2)
	  (cadr Ligne2)
	  t
  )
)

;; SEGMENT
;; Renvoi le segment No de la polyligne
(defun Segment (Pl No / Data Bulge Points Pt1 Pt2 Lst B)
  (setq Data (entget Pl))
  (and (= (cdr (assoc 0 Data)) "LWPOLYLINE")
       (setq No (fix No))
       (> No 0)
       (< No (+ (length (massoc 10 Data)) (cdr (assoc 70 Data))))
       (setq Data   (if	(cdr (assoc 70 Data))
		      (appendlst Data (assoc 10 Data))
		      Data
		    )
	     Bulge  (dxf '42 Data)
	     Points (dxf '10 Data)
       )
       (setq Pt1 (nth (1- No) Points)
	     Pt2 (nth No Points)
	     Lst (if (zerop (setq B (nth (1- No) Bulge)))
		   (list Pt1 Pt2)
		   (BulgeToArc Pt1 Pt2 B)
		 )
       )
  )
  Lst
)

;; LISTESEGMENTS
;; Renvoi la liste des segments de la polyligne
(defun ListeSegments (Pl / Data Bulge Points Pt1 Pt2 Lst B)
  (setq Data (entget Pl))
  (if (= (cdr (assoc 0 Data)) "LWPOLYLINE")
    (progn
      (setq No	   1
	    Rp	   (1- (+ (length (massoc 10 Data)) (cdr (assoc 70 Data))))
	    Data   (if (cdr (assoc 70 Data))
		     (appendlst Data (assoc 10 Data))
		     Data
		   )
	    Bulge  (dxf '42 Data)
	    Points (dxf '10 Data)
      )

      (repeat Rp
	(setq Pt1 (nth (1- No) Points)
	      Pt2 (nth No Points)
	      Lst (append Lst
			  (list	(if (zerop (setq B (nth (1- No) Bulge)))
				  (list Pt1 Pt2)
				  (BulgeToArc Pt1 Pt2 B)
				)
			  )
		  )
	      No  (1+ No)
	)
      )
    )
  )
  Lst
)

;; ==================================================================================================

(defun TstAngle	(AngleTst AngleDep AngleArr)
  (setq AngleTst (ang<2pi (- AngleTst AngleDep)))
  (and (>= AngleTst (- 0 1e-9))
       (<= AngleTst (+ (ang<2pi (- AngleArr AngleDep)) 1e-9))
  )
)



;;;===================================================================================

(defun reverse_pline (ent / e_lst vtx v_lst p_lst l_vtx)
  (setq e_lst (entget ent))
  (cond
    ((= (cdr (assoc 0 e_lst)) "POLYLINE")
     (setq vtx (entnext ent))
     (while (= (cdr (assoc 0 (entget vtx))) "VERTEX")
       (setq v_lst (cons (entget vtx) v_lst)
	     vtx   (entnext vtx)
       )
     )
    )
    ((= (cdr (assoc 0 e_lst)) "LWPOLYLINE")
     (setq p_lst (vl-remove-if-not
		   '(lambda (x)
		      (member (car x) '(10 40 41 42))
		    )
		   e_lst
		 )
	   e_lst (vl-remove-if
		   '(lambda (x)
		      (member x p_lst)
		    )
		   e_lst
		 )
     )
     (while p_lst
       (setq v_lst (cons
		     (list (car p_lst) (cadr p_lst) (caddr p_lst) (cadddr p_lst))
		     v_lst
		   )
	     p_lst (member (assoc 10 (cdr p_lst)) (cdr p_lst))
       )
     )
    )
  )
  (setq	l_vtx (last v_lst)
	l_vtx (subst (cons 40 (cdr (assoc 41 (car v_lst))))
		     (assoc 40 l_vtx)
		     l_vtx
	      )
	l_vtx (subst (cons 41 (cdr (assoc 40 (car v_lst))))
		     (assoc 41 l_vtx)
		     l_vtx
	      )
	l_vtx (subst (cons 42 (- (cdr (assoc 42 (car v_lst)))))
		     (assoc 42 l_vtx)
		     l_vtx
	      )
  )
  (setq	v_lst
	 (mapcar
	   '(lambda (x y)
	      (setq x (subst (cons 40 (cdr (assoc 41 y))) (assoc 40 x) x)
		    x (subst (cons 41 (cdr (assoc 40 y))) (assoc 41 x) x)
		    x (subst (cons 42 (- (cdr (assoc 42 y)))) (assoc 42 x) x)
	      )
	    )
	   v_lst
	   (cdr v_lst)
	 )
  )
  (if (= (logand 1 (cdr (assoc 70 e_lst))) 1)
    (setq v_lst (append (list l_vtx) v_lst))
    (setq v_lst (append v_lst (list l_vtx)))
  )
  (cond
    ((= (cdr (assoc 0 e_lst)) "POLYLINE")
     (mapcar 'entmake
	     (append (list e_lst) v_lst (list (entget vtx)))
     )
     (entdel ent)
    )
    ((= (cdr (assoc 0 e_lst)) "LWPOLYLINE")
     (setq e_lst (append e_lst (apply 'append v_lst)))
     (entmod e_lst)
    )
  )
)

;;; R_PLINE Fonction d'appel

(defun c:rp (/ ent)
  (while (not (setq ent (car (entsel)))))
  (if (or (= (cdr (assoc 0 (entget ent))) "LWPOLYLINE")
	  (and (= (cdr (assoc 0 (entget ent))) "POLYLINE")
	       (zerop (logand 240 (cdr (assoc 70 (entget ent)))))
	  )
      )
    (reverse_pline ent)
    (prompt "\nEntité non valide")
  )
  (princ)
)