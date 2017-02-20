(defun repart  (lg coef nb seg_dp seg_fn / dif mod list_dist seg mo seg_tr)
  (setq	dif	  (- seg_fn seg_dp)
	mod	  (sm nb 0)
	mod	  (/ dif mod)
	seg_tr	  (/ (- lg dif) nb)
	list_dist '()
	seg	  0
	mo	  0
	)
  (repeat nb
    (setq seg_tr    (+ seg_tr mod)
	  mo	    (+ mo mod)
	  list_dist (cons seg_tr list_dist)
	  )
    )
  )
(defun sm  (nb rs /)
  (if (/= nb 0)
    (progn
      (setq rs (+ rs nb)
	    nb (- nb 1))
      (sm nb rs))
    rs
    )
  )
(defun pt-courbe  (dist obj cote / sel)
  ;; si cote = t -> à partir début sinon à partir de la fin
  (setq	sel (last obj)
	obj (vlax-ename->vla-object (car obj)))
  (if (not cote)
    (setq dist (- (vla-get-length obj) dist)))
  (if (< (vlax-curve-getDistAtPoint obj (vlax-curve-getClosestPointTo obj sel))
	 (/ (vla-get-length obj) 2.0))
    (vlax-curve-getPointAtDist obj dist)
    (vlax-curve-getPointAtDist obj (- (vla-get-length obj) dist))))

(defun dst-courbe  (pt obj cote / lg sel)
  ;; si cote = t -> à partir début sinon à partir de la fin
  (setq	sel (last obj)
	obj (vlax-ename->vla-object (car obj)))
  (if (not cote)
    (setq lg (vla-get-length obj))
    (setq lg 0.0))
  (if (< (vlax-curve-getDistAtPoint obj (vlax-curve-getClosestPointTo obj sel))
	 (/ (vla-get-length obj) 2.0))
    (abs (- lg (vlax-curve-getDistAtPoint obj (vlax-curve-getClosestPointTo obj pt))))
    (abs (- lg
	    (abs (- (vla-get-length obj)
		    (vlax-curve-getDistAtPoint obj (vlax-curve-getClosestPointTo obj pt))))))))

(defun c:gent  (/	       list_var	      var	     list_pt_extd   list_pt_intd
		ent_lsp_extd   ent_lsp_intd   lg_extd	     lg_intd	    ent_lsp_extd
		ent_lsp_intd   lg_extd	      lg_intd	     pt_dp_extd	    pt_dp_intd
		pts_extd)

  (setq	list_pt_extd '(nil 2200.0 7250.0 10300.0 13350.0 16400.0 19450.0 25550.0 28600.0 31650.0
		       34700.0)
	list_pt_intd '(nil 0.0 3055.0 6110.0 9165.0 12220.0 15275.0 18330.0 21385.0 24435.0 27490.0
		       30525.0))
  (setq	list_var
	 '(("ENT" "gent_extd" "Selectionnez l'EXTRADOS près du départ")
	   ("ENT" "gent_intd" "Selectionnez l'INTRADOS près du départ")
	   ("INTEGER" "gent_dp_tr" "Noeud de départ (sur l'extrado) pour la modif du treillis")
					;("REAL" "gent_extd_rpt" "Le rapport de repartition pour l'EXTRADOS")
					;("REAL" "gent_intd_rpt" "Le rapport de repartition pour l'INTRADOS")
	   ("REAL" "gent_dist_tr" "Lg 1er segement sur extrados")
	   ("INTEGER" "gent_nb" "Nb de divisions")))
  (foreach var	list_var
    (cond ((= (cadr var) "gent_extd_rpt")
	   (if (not gent_extd_rpt)
	     (setq gent_gent_extd_rpt 1.0)))
	  ((= (cadr var) "gent_intd_rpt")
	   (if (not gent_intd_rpt)
	     (setq gent_gent_extd_rpt 1.0)))
	  ((= (cadr var) "gent_dp_tr")
	   (if (not gent_dp_tr)
	     (setq gent_gent_dp_tr 4)))
	  ((= (cadr var) "gent_ect")
	   (if (not gent_ect)
	     (setq gent_gent_ect 50.0)))
	  ((= (cadr var) "gent_nb")
	   (if (not gent_nb)
	     (setq gent_gent_nb 5))))
    (question var "gent"))
  (setq	ent_lsp_extd (car gent_extd)
	ent_lsp_intd (car gent_intd)
	lg_extd	     (vla-get-length (vlax-ename->vla-object ent_lsp_extd))
	lg_intd	     (vla-get-length (vlax-ename->vla-object ent_lsp_intd))
	pt_dp_extd   (pt-courbe gent_dist_tr gent_extd 'nil)
	pt_dp_intd   (vlax-curve-getClosestPointTo (vlax-ename->vla-object ent_lsp_intd) pt_dp_extd)
	pts_extd     (repart (- lg_extd (nth gent_dp_tr list_pt_extd) gent_dist_tr)
			     gent_extd_rpt
			     gent_nb
			     (- (* 2.0 gent_dist_tr) 3000.0)
			     (-	(nth gent_dp_tr list_pt_extd)
				(nth (- gent_dp_tr 1) list_pt_extd)))
	pts_intd     (repart
		       (- lg_intd
			  (nth gent_dp_tr list_pt_intd)
			  (* (dst-courbe pt_dp_intd gent_intd 'nil) 2.0))
		       gent_extd_rpt
		       (- gent_nb 1)
		       (- (* 2.0 (dst-courbe pt_dp_intd gent_intd 'nil)) 1000.0)
		       (- (nth gent_dp_tr list_pt_intd)
			  (nth (- gent_dp_tr 1) list_pt_intd))))
  (setq lg_pt (nth gent_dp_tr list_pt_extd))
  (foreach dt  pts_extd
    (setq lg_pt	(+ lg_pt dt)
	  pt	(pt-courbe lg_pt gent_extd 't))
    (entmake (list '(0 . "POINT") '(410 . "Model") '(100 . "AcDbPoint") (cons 10 pt)))
    )
  (setq lg_pt (nth gent_dp_tr list_pt_intd))
  (foreach dt  pts_intd
    (setq lg_pt	(+ lg_pt dt)
	  pt	(pt-courbe lg_pt gent_intd 't))
    (entmake (list '(0 . "POINT") '(410 . "Model") '(100 . "AcDbPoint") (cons 10 pt)))
    )
  (princ)
  )
 ;|«Visual LISP© Format Options»
(100 2 40 0 nil "Fin de " 100 9 0 0 0 T T nil T)
;*** NE PAS AJOUTER de texte au-dessous du commentaire! ***|;
