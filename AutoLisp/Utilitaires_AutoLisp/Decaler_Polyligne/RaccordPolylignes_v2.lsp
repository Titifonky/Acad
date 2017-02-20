(defun c:tt ()
  (mapcar 'EstLigne (ListeSegments (car (entsel))))
)


(defun c:tstt ()
  (setvar "FILLETRAD" 50)
  (Raccord_Polyligne (car (entsel)))
)

(defun Raccord_Polyligne (ent / data pts ferme rayon enttmp lst)

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
    )
  )

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
      (setq lst (ListeSegments ent))
      (mapcar
	(function
	  (lambda (seg1 seg2 /)
	    (setq d (Intersection seg1 seg2))
	    (make_point (car d) 0)
	    (make_point (cadr d) 0)
	    (make_point (caddr d) 0)
	    (redraw)
	  )
	)
	(if ferme
	  lst
	  (butlast lst)
	)
	(if ferme
	  (rot1 lst)
	  (cdr lst)
	)
      )
    )
  )
  (princ)
)

(defun CreerPolyligne (Lstdxf f /)
  (entmake
    (append
      (list
	'(0 . "LWPOLYLINE")
	'(100
	  .
	  "AcDbEntity"
	 )
	'(67 . 0)
	'(410 . "Model")
	'(100
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


(defun Intersection (seg1 seg2 / ent1 ent2 int)
  (cond
    ;; Arc Arc
    ((and (EstArc seg1) (EstArc seg2))
     (and (setq ent1 (DecalerArc seg1 rayon t))
	  (setq ent2 (DecalerArc seg2 rayon t))
	  (setq int (Inters_Arc-Arc ent1 ent2))
     )
     (and (not int)
	  (setq ent1 (DecalerArc seg1 rayon nil))
	  (setq ent2 (DecalerArc seg2 rayon nil))
	  (setq int (Inters_Arc-Arc ent1 ent2))
     )
     (list (setq int (car int))
	   (ProjeterPointSurArc seg1 int)
	   (ProjeterPointSurArc seg2 int)
     )
    )

    ;; Arc Ligne
    ((and (EstArc seg1) (EstLigne seg2))
     (and (setq ent1 (DecalerArc seg1 rayon t))
	  (setq ent2 (DecalerLigne seg2 rayon t))
	  (setq int (Inters_Arc-Ligne ent1 ent2))
     )
     (and (not int)
	  (setq ent1 (DecalerArc seg1 rayon nil))
	  (setq ent2 (DecalerLigne seg2 rayon nil))
	  (setq int (Inters_Arc-Ligne ent1 ent2))
     )

     (list (setq int (car int))
	   (ProjeterPointSurArc seg1 int)
	   (ProjeterPointSurLigne seg2 int)
     )
    )

    ;; Ligne Arc
    ((and (EstLigne seg1) (EstArc seg2))
     (and (setq ent1 (DecalerLigne seg1 rayon t))
	  (setq ent2 (DecalerArc seg2 rayon t))
	  (setq int (Inters_Arc-Ligne ent2 ent1))
     )
     (and (not int)
	  (setq ent1 (DecalerLigne seg1 rayon nil))
	  (setq ent2 (DecalerArc seg2 rayon nil))
	  (setq int (Inters_Arc-Ligne ent2 ent1))
     )
     (list (setq int (car int))
	   (ProjeterPointSurLigne seg1 int)
	   (ProjeterPointSurArc seg2 int)
     )
    )

    ;; Ligne Ligne
    ((and (EstLigne seg1) (EstLigne seg2))
     (and (setq ent1 (DecalerLigne seg1 rayon t))
	  (setq ent2 (DecalerLigne seg2 rayon t))
	  (setq int (Inters_Ligne-Ligne ent1 ent2))
     )
     (and (not int)
	  (setq ent1 (DecalerLigne seg1 rayon nil))
	  (setq ent2 (DecalerLigne seg2 rayon nil))
	  (setq int (Inters_Ligne-Ligne ent1 ent2))
     )
     (list int
	   (ProjeterPointSurLigne seg1 int)
	   (ProjeterPointSurLigne seg2 int)
     )
    )
  )
)



;; PROJETERPOINTSURARC
;; Renvoi le point projeté sur l'arc
(defun ProjeterPointSurArc (Arc Pt)
  (setq	Arc (BulgeToArc (car Arc) (cadr Arc) (caddr Arc))
	ct  (car Arc)
	ang (angle ct Pt)
	ptp (polar ct ang (cadddr Arc))
  )
  (if (TstAngle ang (cadr Arc) (caddr Arc))
    ptp
  )
)

;; PROJETERPOINTSURLIGNE
;; Renvoi le point projeté sur la ligne
(defun ProjeterPointSurLigne (Ligne Pt / pt1 pt2)
  (setq	pt1 (car Ligne)
	pt2 (caddr Ligne)
	ptp (list (- (+ (car Pt) (cadr pt1)) (cadr pt2))
		  (- (+ (cadr Pt) (car pt2)) (car pt1))
	    )
	ptp (inters Pt ptp pt1 pt2 nil)
	;; On calcul le point d'intersection
	ptp (inters Pt ptp pt1 pt2 t)
	    ;; On verifie si le point est sur la ligne
  )
)


;; DECALERARC
;; Renvoi un arc décalé
(defun DecalerArc (Arc Dist Cote / c b r d)
  (setq	c (BulgeCenter (car Arc) (cadr Arc) (caddr Arc))
	b (cadr Arc)
	r (distance (car Arc) c)
	d (* (if (xnor Cote (not (minusp b)))
	       1
	       -1
	     )
	     Dist
	  )
  ) 
  (if (< d r)
    (list (polar c (angle c (car Arc)) (+ r d))
	  b
	  (polar c (angle c (caddr Arc)) (+ r d))
    )
  )
)

;; DECALERLIGNE
;; Renvoi une ligne décalée
(defun DecalerLigne (Ligne Dist Cote / v vp pt)
  (setq	v  (vect (car Ligne) (caddr Ligne))
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
  (list pt 0.0 (v+v pt v))
)

;; Inters_Arc-Arc
;; Renvoi les points d'intersection entre deux arcs
(defun Inters_Arc-Arc (Arc1   Arc2   /	    ct1	   ct2	  r1	 r2
		       ang1   ang2   c	    alpha  beta	  depA1	 arrA1
		       depA2  arrA2
		      )
  (setq	Arc1  (BulgeToArc (car Arc1) (cadr Arc1) (caddr Arc1))
	Arc2  (BulgeToArc (car Arc2) (cadr Arc2) (caddr Arc2))
	ct1   (car Arc1)
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

;; Inters_Arc-Ligne
;; Renvoi le point d'intersection entre un arc et une ligne
(defun Inters_Arc-Ligne
       (Arc Ligne / ct r pt ang dst alpha depA arrA depL arrL)
  (setq	Arc  (BulgeToArc (car Arc) (cadr Arc) (caddr Arc))
	ct   (car Arc)
	r    (cadddr Arc)

	pt1 (car Ligne)
	pt2 (caddr Ligne)
	pt (list (- (+ (car ct) (cadr pt1)) (cadr pt2))
		  (- (+ (cadr ct) (car pt2)) (car pt1))
	    )
	pt   (inters ct pt pt1 pt2 nil)
	ang  (angle ct pt)
	dst  (distance ct pt)
	depA (cadr Arc)
	arrA (caddr Arc)
	depL (min (angle ct pt1) (angle ct pt2))
	arrL (max (angle ct pt1) (angle ct pt2))
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

;; Inters_Ligne-Ligne
;; Renvoi le point d'intersection entre deux lignes
(defun Inters_Ligne-Ligne (Ligne1 Ligne2)
  (inters (car Ligne1)
	  (caddr Ligne1)
	  (car Ligne2)
	  (caddr Ligne2)
	  t
  )
)

;; SEGMENT
;; Renvoi le segment No de la polyligne
(defun Segment (Pl No / Data Bulge Points)
  (setq Data (entget Pl))
  (and (= (cdr (assoc 0 Data)) "LWPOLYLINE")
       (setq No (fix No))
       (> No 0)
       (< No (+ (length (massoc 10 Data)) (cdr (assoc 70 Data))))
       (setq Data   (if	(= (cdr (assoc 70 Data)) 1)
		      (appendlst Data (assoc 10 Data))
		      Data
		    )
	     Bulge  (dxf '42 Data)
	     Points (dxf '10 Data)
	     Lst    (list (nth (1- No) Points)
			  (nth (1- No) Bulge)
			  (nth No Points)
		    )
       )
  )
  Lst
)

;; LISTESEGMENTS
;; Renvoi la liste des segments de la polyligne
(defun ListeSegments (Pl / Data Bulge Points)
  (setq Data (entget Pl))
  (if (= (cdr (assoc 0 Data)) "LWPOLYLINE")
    (progn
      (setq Bulge  (dxf '42 Data)
	    Points (dxf '10 Data)
	    Points2 (if (= (cdr (assoc 70 Data)) 1)
		     (rot1 Points)
		     (cdr Points)
		   )
      )
      (mapcar 'list Points Bulge Points2)
    )
  )
)

(defun EstLigne	(lst)
  (zerop (cadr lst))
)

(defun EstArc (lst)
  (not (EstLigne lst))
)

;; ==================================================================================================

(defun TstAngle	(AngleTst AngleDep AngleArr)
  (setq AngleTst (ang<2pi (- AngleTst AngleDep)))
  (and (>= AngleTst (- 0 1e-9))
       (<= AngleTst (+ (ang<2pi (- AngleArr AngleDep)) 1e-9))
  )
)