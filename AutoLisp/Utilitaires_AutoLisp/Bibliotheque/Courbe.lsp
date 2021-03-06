(vl-load-com)


;;; Renvoi le cot� sur lequel se situe un point
;;; d�but : T
;;; fin : nil

(defun Courbe-Position (Courbe Point)
  (if (> (Courbe-DistanceAuPoint
	   Courbe
	   (Courbe-PointLePlusProche Courbe Point)
	   't
	 )
	 (* (Courbe-Longueur Courbe) 0.5)
      )
    'nil
    't
  )
)


;;; Renvoi le point le plus proche
(defun Courbe-PointLePlusProche	(Courbe lPoint)
  (V2D (vlax-curve-getClosestPointTo
	 (vlax-ename->vla-object Courbe)
	 lPoint
       )
  )
)

;;; Renvoi la distance � un point donn� � partir du debut ou de la fin
(defun Courbe-DistanceAuPoint (Courbe Point Fin / dist)
  (setq	dist (vlax-curve-getDistAtPoint
	       (vlax-ename->vla-object Courbe)
	       Point
	     )
  )
  (if Fin
    dist
    (- (Courbe-Longueur Courbe) dist)
  )
)

;;; Renvoi la liste des distances aux points donn�s � partir du debut ou de la fin
(defun Courbe-ListeDistancesAuxPoints (Courbe lPoint Fin / dist)
  (mapcar (function
	    (lambda (pt) (Courbe-DistanceAuPoint Courbe pt Fin))
	  )
	  lPoint
  )
)

;;; Renvoi le point � la distance donn�e � partir du debut ou de la fin
(defun Courbe-PointALaDistance (Courbe Dist Fin)
  (setq	Dist (if Fin
	       Dist
	       (- (Courbe-Longueur Courbe) Dist)
	     )
  )
  (V2D (vlax-curve-getPointAtDist
	 (vlax-ename->vla-object Courbe)
	 Dist
       )
  )
)

;;; Renvoi le point de depart et d'arriv�
(defun Courbe-PtDepartPtArrive (Courbe)
  (list	(V2D
	  (vlax-curve-getStartPoint (vlax-ename->vla-object Courbe))
	)
	(V2D
	  (vlax-curve-getEndPoint (vlax-ename->vla-object Courbe))
	)
  )
)



;;; Renvoi la liste des points aux distances donn�es � partir du debut ou de la fin
(defun Courbe-ListePointsAuxDistances (Courbe lDist Fin)
  (mapcar (function
	    (lambda (dist) (Courbe-PointALaDistance Courbe dist Fin))
	  )
	  lDist
  )
)

;;; Renvoi le parametre � la distance donn�e suivant � partir du debut ou de la fin
(defun Courbe-ParamALaDistance (Courbe Dist Fin)
  (setq	Dist (if Fin
	       Dist
	       (- (Courbe-Longueur Courbe) Dist)
	     )
  )
  (vlax-curve-getParamAtDist
    (vlax-ename->vla-object Courbe)
    Dist
  )
)

;;; Renvoi le parametre au point donn�e
(defun Courbe-ParamAuPoint (Courbe Point)
  (vlax-curve-getParamAtPoint
    (vlax-ename->vla-object Courbe)
    Point
  )
)


;;; Renvoi la derivee 1 d'une courbe
(defun Courbe-Derivee1 (Courbe Param /)
  (V2D (vlax-curve-getFirstDeriv
	 (vlax-ename->vla-object Courbe)
	 Param
       )
  )
)

;;; Renvoi l'angle de la derivee 1 d'une courbe
(defun Courbe-AngleALaDistance (Courbe Dist Fin)
  (vangleh (Courbe-Derivee1
	     Courbe
	     (Courbe-ParamALaDistance Courbe Dist Fin)
	   )
  )
)

;;; Renvoi la derivee 2 d'une courbe
(defun Courbe-Derivee2 (Courbe Param /)
  (V2D (vlax-curve-getSecondDeriv
	 (vlax-ename->vla-object Courbe)
	 Param
       )
  )
)

;;; Renvoi la longueur d'une courbe
(defun Courbe-Longueur (Courbe / prop vCourbe)
  (setq vCourbe (vlax-ename->vla-object Courbe))

  (- (vlax-curve-getDistAtParam
       vCourbe
       (vlax-curve-getEndParam vCourbe)
     )
     (vlax-curve-getDistAtParam
       vCourbe
       (vlax-curve-getStartParam vCourbe)
     )
  )

;;;  (cond
;;;    ((member '(0 . "ARC") (entget Courbe))
;;;     (setq prop 'ArcLength)
;;;    )
;;;    ((member '(0 . "CIRCLE") (entget Courbe))
;;;     (setq prop 'Circumference)
;;;    )
;;;    ((member '(0 . "LWPOLYLINE") (entget Courbe))
;;;     (setq prop 'Length)
;;;    )
;;;    ((member '(0 . "LINE") (entget Courbe))
;;;     (setq prop 'Length)
;;;    )
;;;  )
;;;  (if prop
;;;    (float
;;;      (vlax-get-property (vlax-ename->vla-object Courbe) prop)
;;;    )
;;;  )
)


;;; 0 : aucun prolongement
;;; 1 : prolonge la courbe 1
;;; 2 : prolonge la courbe 2
;;; 3 : prolonge les deux courbes

(defun Courbe-Intersection (Courbe1 Courbe2 Prolonge / point)
  (setq	point (vlax-variant-value
		(vla-intersectwith
		  (vlax-ename->vla-object Courbe1)
		  (vlax-ename->vla-object Courbe2)
		  Prolonge
		)
	      )
  )
  (if (> (vlax-safearray-get-u-bound point 1) 0)
    (mapcar 'V2D (split-list (vlax-safearray->list point) 3))
  )
)

(defun Courbe-Decaler (Courbe Dist Cote)
  (setq	Dist (*	Dist
		(if Cote
		  1
		  -1
		)
	     )
  )
  (vl-catch-all-apply
    'vla-Offset
    (list (vlax-ename->vla-object Courbe) Dist)
  )
  (entlast)
)

;; Bulge to Arc  -  Lee Mac
;; p1 - start vertex
;; p2 - end vertex
;; b  - bulge
;; dir  - trigo
;; Returns: (<center> <start angle> <end angle> <radius> <dir>)

(defun BulgeToArc (p1 b p2 / c r)
  (setq	r (/ (* (distance p1 p2) (1+ (* b b))) 4 b)
	c (polar p1 (+ (angle p1 p2) (- (/ pi 2) (* 2 (atan b)))) r)
  )
  (if (minusp b)
    (list c (angle c p2) (angle c p1) (abs r))
    (list c (angle c p1) (angle c p2) (abs r))
  )
)

;; Point to Bulge
;; c     - center
;; a1,a2 - start, end angle
;; r     - radius
;; Returns: <bulge>

(defun PtToBulge (c pt1 pt2 Dir)
  (setq	ang (if	Dir
	      (AngleTrigo (angle c pt1) (angle c pt2))
	      (AngleTrigo (angle c pt2) (angle c pt1))
	    )
  )
  (* (if Dir
       1
       -1
     )
     (tan (/ ang
	     4.0
	  )
     )
  )
)

;; Angle to Bulge
;; c     - center
;; a1,a2 - start, end angle
;; r     - radius
;; Returns: <bulge>

(defun AngleToBulge (c a1 a2 Dir)
  (setq	ang (if	Dir
	      (AngleTrigo a1 a2)
	      (AngleTrigo a2 a1)
	    )
  )
  (* (if Dir
       1
       -1
     )
     (tan (/ ang
	     4.0
	  )
     )
  )
)

;; Bulge Center  -  Lee Mac
;; p1 - start vertex
;; p2 - end vertex
;; b  - bulge
;; Returns the center of the arc described by the given bulge and vertices

(defun BulgeCenter (p1 b p2)
  (polar p1
	 (+ (angle p1 p2) (- (/ pi 2) (* 2 (atan b))))
	 (/ (* (distance p1 p2) (1+ (* b b))) 4 b)
  )
)

(defun AngleTrigo (AngleDep AngleArr)
  (ang<2pi (- AngleArr AngleDep))
)