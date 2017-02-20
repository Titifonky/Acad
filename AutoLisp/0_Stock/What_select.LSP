(defun c:ws
       (/ l_ent e_sel pt_sel js l_js l_ent e_sel dxf_ent key)
  (while (null (setq e_sel (entsel))))
  (setq
    pt_sel (osnap (cadr e_sel) "_nearest")
    js	   (ssget "_C" pt_sel pt_sel)
    l_js   (ssnamex js)
  ) ;_ Fin de setq
  (foreach n l_js
    (if	(eq (type (cadr n)) 'ENAME)
      (setq l_ent (cons (cadr n) l_ent))
    ) ;_ Fin de if
  ) ;_ Fin de foreach
  (setq e_sel (ssadd))
  (sssetfirst nil (ssadd (car l_ent) e_sel))
  (if (> (length l_ent) 1)
    (progn
      (setq dxf_ent (entget (car l_ent)))
      (grtext
	-1
	(strcat	"Entité: "
		(cdr (assoc 0 dxf_ent))
		" Calque: "
		(cdr (assoc 8 dxf_ent))
		(if (assoc 62 dxf_ent)
		  (strcat " Couleur: " (itoa (cdr (assoc 62 dxf_ent))))
		  ""
		) ;_ Fin de if
		(if (assoc 6 dxf_ent)
		  (strcat " TypeLigne: " (cdr (assoc 6 dxf_ent)))
		  ""
		) ;_ Fin de if
	) ;_ Fin de strcat
      ) ;_ Fin de grtext
      (princ
	"\n pour faire défiler entité suivante; /[Espace]/Click+droit pour finir!."
      ) ;_ Fin de princ
      (while
	(and (not (member (setq key (grread T 4 2)) '((2 13) (2 32))))
	     (/= (car key) 25)
	) ;_ Fin de and
	 (cond
	   ((eq (car key) 3)
	    (setq l_ent	(append (cdr l_ent) (list (car l_ent)))
		  e_sel	(ssadd)
	    ) ;_ Fin de setq
	    (sssetfirst nil (ssadd (car l_ent) e_sel))
	    (setq dxf_ent (entget (car l_ent)))
	    (grtext
	      -1
	      (strcat
		"Entité: "
		(cdr (assoc 0 dxf_ent))
		" Calque: "
		(cdr (assoc 8 dxf_ent))
		(if (assoc 62 dxf_ent)
		  (strcat " Couleur: " (itoa (cdr (assoc 62 dxf_ent))))
		  ""
		) ;_ Fin de if
		(if (assoc 6 dxf_ent)
		  (strcat " TypeLigne: " (cdr (assoc 6 dxf_ent)))
		  ""
		) ;_ Fin de if
	      ) ;_ Fin de strcat
	    ) ;_ Fin de grtext
	   )
	 ) ;_ Fin de cond
      ) ;_ Fin de while
      (grtext)
    ) ;_ Fin de progn
  ) ;_ Fin de if
  (prin1)
) ;_ Fin de defun
