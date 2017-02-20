
(defun c:gtg (/ selection p courbe ed)
  (setq selection (SelEnList (ssget '((0 . "LWPOLYLINE")))))
  (foreach p selection
    (setq courbes (Decaler_Courbe p 4))
    (if	courbes
      (progn
	(foreach c courbes
	  (setq	ed (entget c)
		ed (subst (cons 8 "Decalage") (assoc 8 ed) ed)
	  )
	  (entmod ed)
	)
      )
    )
  )

  (princ)
)

(defun Decaler_Courbe (ename dist / l pt1 pt2 newl)
  (setq	lastent	(entlast)
	pt1	(Centre_De_Gravite ename)
	pt2	(Courbe-PointLePlusProche ename pt1)
  )
  (command "_offset" dist (list ename pt2) pt1 "")
  (DerniereEntitesCrees lastent)
)

(defun Centre_De_Gravite (ename / ed obj centroid)
  (setvar "CMDECHO" 0)
  (command "_copy" ename "" '(0 0 0) '(0 0 0))
  (setq ename (entlast))
  (command "_region" ename "")
  (setq	ename (entlast)
	ed    (entget ename)
	ed    (subst (cons 60 "1") (assoc 60 ed) ed)
  )
  (entmod ed)
  (setq	obj	 (vlax-ename->vla-object ename)
	centroid (vlax-safearray->list
		   (vlax-variant-value (vla-get-centroid obj))
		 )
  )
  (entdel ename)
  centroid
)

(defun DerniereEntitesCrees (ent / a b Liste)
  (setq	Liste 'nil
	a     ent
  )
  (if a
    (while (setq b (entnext a))
      (setq Liste (cons b Liste)
	    a	  b
      )
    )
  )
  Liste
)

;;; Renvoi le point le plus proche
(defun Courbe-PointLePlusProche	(Courbe lPoint)
  (V2D (vlax-curve-getClosestPointTo
	 (vlax-ename->vla-object Courbe)
	 lPoint
       )
  )
)

;; V2D
;; Retourne un vecteur 2D
;;
;; Arguments : un vecteur
(defun v2d (l)
  (mapcar '+ (append l '(0.0 0.0)) '(0.0 0.0))
)

(defun SelEnList (sset / i bcl ss nb lst)
  (setq	i   -1
	lst '()
  )
  (defun bcl (ss nb)
    (if	(setq ent (ssname ss (setq nb (1+ nb))))
      (setq lst (cons ent (bcl ss nb)))
    )
  )
  (if sset
    (bcl sset i)
    'nil
  )
)