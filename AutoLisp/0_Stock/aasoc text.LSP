(defun modif_poly (obj rea arg-list)
  (if (and (vlax-read-enabled-p obj)
	   (vlax-read-enabled-p (vlr-data rea))
      ) ;_ Fin de and
    (vla-put-textstring
      (vlr-data rea)
      (rtos (vla-get-area obj))
    ) ;_ Fin de vla-put-textstring
    (vlr-remove rea)
  ) ;_ Fin de if
) ;_ Fin de defun

(defun effacer_pt (obj rea arg-list)
  (vlax-ldata-delete obj "Patrick_35" (vlr-data rea))
  (vlax-ldata-delete (vlr-data rea) "Patrick_35" obj)
  (vlr-remove rea)
) ;_ Fin de defun

(defun c:rea (/ n sel_poly sel_text)
  (while (not sel_poly)
    (setq sel_poly
	   (car
	     (entsel "\nSélection de la polyligne/spline/cercle : ")
	   ) ;_ Fin de car
    ) ;_ Fin de setq
    (if	sel_poly
      (if (or (= (cdr (assoc 0 (entget sel_poly))) "POLYLINE")
	      (= (cdr (assoc 0 (entget sel_poly))) "LWPOLYLINE")
	      (= (cdr (assoc 0 (entget sel_poly))) "SPLINE")
	      (= (cdr (assoc 0 (entget sel_poly))) "CIRCLE")
	  ) ;_ Fin de or
	(progn
	  (setq sel_poly (vlax-ename->vla-object sel_poly))
	  (if (/= (vla-get-objectname sel_poly) "AcDbCircle")
	    (if	(= :vlax-false (vla-get-closed sel_poly))
	      (progn
		(alert "Cette Polyligne/Spline n'est pas fermée")
		(setq sel_poly nil)
	      ) ;_ Fin de progn
	    ) ;_ Fin de if
	  ) ;_ Fin de if
	  (if sel_poly
	    (if	(vlax-ldata-get sel_poly "Patrick_35")
	      (progn
		(alert "Polyligne/Spline/Cercle déjà associé à un texte"
		) ;_ Fin de alert
		(setq sel_poly nil)
	      ) ;_ Fin de progn
	    ) ;_ Fin de if
	  ) ;_ Fin de if
	) ;_ Fin de progn
	(progn
	  (alert "Ce n'est pas une Polyligne/Spline/Cercle")
	  (setq sel_poly nil)
	) ;_ Fin de progn
      ) ;_ Fin de if
    ) ;_ Fin de if
  ) ;_ Fin de while
  (if sel_poly
    (progn
      (while (not sel_text)
	(setq sel_text (car (entsel "\nSélection du texte : ")))
	(if sel_text
	  (if (or (= (cdr (assoc 0 (entget sel_text))) "TEXT")
		  (= (cdr (assoc 0 (entget sel_text))) "MTEXT")
	      ) ;_ Fin de or
	    (progn
	      (setq sel_text (vlax-ename->vla-object sel_text))
	      (if (vlax-ldata-get sel_text "Patrick_35")
		(progn
		  (alert
		    "Texte déjà associé à une Polyligne/Spline/Cercle"
		  ) ;_ Fin de alert
		  (setq sel_text nil)
		) ;_ Fin de progn
	      ) ;_ Fin de if
	    ) ;_ Fin de progn
	    (progn
	      (setq sel_text nil)
	      (alert "Ce n'est pas un texte")
	    ) ;_ Fin de progn
	  ) ;_ Fin de if
	) ;_ Fin de if
      ) ;_ Fin de while
      (if sel_text
	(progn
	  (vla-put-textstring sel_text (rtos (vla-get-area sel_poly)))
	  (vlr-object-reactor
	    (list sel_poly)
	    sel_text
	    '((:vlr-modified . modif_poly) (:vlr-erased . effacer_pt))
	  ) ;_ Fin de vlr-object-reactor
	  (vlr-object-reactor
	    (list sel_text)
	    sel_poly
	    '((:vlr-erased . effacer_pt))
	  ) ;_ Fin de vlr-object-reactor
	  (vlax-ldata-put sel_poly "Patrick_35" sel_text)
	  (vlax-ldata-put sel_text "Patrick_35" sel_poly)
	) ;_ Fin de progn
      ) ;_ Fin de if
    ) ;_ Fin de progn
  ) ;_ Fin de if
  (princ)
) ;_ Fin de defun

(defun refaire_reacteurs (/ entp entt n)
  (vl-load-com)
  (if (not appli_patrick_35)
    (progn
      (setq appli_patrick_35
	     (ssget "x"
		    (list (cons 102 "{ACAD_XDICTIONARY"))
	     ) ;_ Fin de ssget
      ) ;_ Fin de setq
      (if appli_patrick_35
	(progn
	  (setq n 0)
	  (while (setq entp (ssname appli_patrick_35 n))
	    (setq entp (vlax-ename->vla-object entp))
	    (setq entt (vlax-ldata-get entp "Patrick_35"))
	    (vlr-object-reactor
	      (list entp)
	      entt
	      '((:vlr-modified . modif_poly) (:vlr-erased . effacer_pt))
	    ) ;_ Fin de vlr-object-reactor
	    (vlr-object-reactor
	      (list entt)
	      entp
	      '((:vlr-erased . effacer_pt))
	    ) ;_ Fin de vlr-object-reactor
	    (setq n (1+ n))
	  ) ;_ Fin de while
	) ;_ Fin de progn
      ) ;_ Fin de if
    ) ;_ Fin de progn
  ) ;_ Fin de if
  (princ)
) ;_ Fin de defun

(refaire_reacteurs)