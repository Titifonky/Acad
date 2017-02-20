(defun c:cvf
       (/ setface layername lg i defpoint sommetpoly layer couleur)
  (setq cmde (GETVAR "CMDECHO"))
  (setvar "CMDECHO" 0)
  (command "_undo" "_m")
  (setq setface (ssget "_X" (list (cons 0 "3DFACE"))))
  (initget "0 G")
  (setq	layername
	 (getkword
	   "\nLaisser l'entité dans le calque d'origine [G] ou la placer dans le calque '0' [0] :"
	 ) ;_ Fin de getkword
  ) ;_ Fin de setq
  (if (not (= setface 'nil))
    (progn
      (setq lg	     (sslength setface)
	    i	     0
	    defpoint '(nil)
      ) ;_ Fin de setq
      (while (< i lg)
	(setq sommetpoly
	       (mapcar 'cdr
		       (vl-remove-if-not
			 '(lambda (x)
			    (or	(= (car x) 10)
				(= (car x) 11)
				(= (car x) 12)
				(= (car x) 13)
			    ) ;_ Fin de or
			  ) ;_ Fin de lambda
			 (entget (ssname setface i))
		       ) ;_ Fin de vl-remove-if-not
	       ) ;_ Fin de mapcar
	) ;_ Fin de setq
	(setq sommetpoly
	       (mapcar '(lambda (x) (append '(10) x)) sommetpoly)
	) ;_ Fin de setq
	(if (= layername "0")
	  (progn
	    (setq couleur (cons 62 256))
	    (setq layer '(8 . "0"))
	  ) ;_ Fin de progn
	  (progn
	    (setq couleur (assoc 62 (entget (ssname setface i))))
	    (setq layer (assoc 8 (entget (ssname setface i))))
	  ) ;_ Fin de progn
	) ;_ Fin de if
	(if (= couleur 'nil)
	  (setq couleur (cons 62 256))
	) ;_ Fin de if
	(entmake (append (list '(0 . "LWPOLYLINE")
			       '(100 . "AcDbEntity")
			       '(100 . "AcDbPolyline")
			       (cons 70 1)
			       (cons 90 (length sommetpoly))
			       layer
			       couleur
			 ) ;_ Fin de list
			 sommetpoly
		 ) ;_ Fin de append
	) ;_ Fin de entmake
	(entmod (subst '(70 . 1) '(70 . 0) (entget (entlast))))
	(entdel (ssname setface i))
	(setq i (1+ i))
      ) ;_ Fin de while
    ) ;_ Fin de progn
    (princ "\nAucune '3DFace' dans le dessin")
  ) ;_ Fin de if
  (command "_undo" "_m")
  (setvar "CMDECHO" cmde)
  (princ)
) ;_ Fin de defun
