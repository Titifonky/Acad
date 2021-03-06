(defun c:pf ()
  (setq	cmde (getvar "CMDECHO")
	hp   (getvar "HPBOUND")
	snap (getvar "AUTOSNAP")
  ) ;_ Fin de setq
  (setvar "CMDECHO" 0)
  (setvar "HPBOUND" 0)
  (setvar "AUTOSNAP" 2)
  (setq list_ent 'nil)
  ;; cr�ation des r�gions
  (setq repere (entlast))
  (initget 1)
  (while
    (setq
      point (getpoint
	      "\nMontrez le point interne des r�gions � assembler : "
	    ) ;_ Fin de getpoint
    ) ;_ Fin de setq
     (command "_.-boundary" point "")
     (setq derent (list (entlast)))
     (setq list_ent (append derent list_ent))
     (mapcar
       '(lambda	(x)
	  (vlax-invoke-method (vlax-ename->vla-object x) 'highlight 1)
	) ;_ Fin de lambda
       list_ent
     ) ;_ Fin de mapcar
  ) ;_ Fin de while

  ;; mise en surbrillance des r�gions
  (foreach x list_ent (redraw x 1))

  ;; annulations de la mise en surbrillance des r�gions

  (mapcar
    '(lambda (x)
       (vlax-invoke-method (vlax-ename->vla-object x) 'highlight 0)
     ) ;_ Fin de lambda
    list_ent
  ) ;_ Fin de mapcar
  ;; recup�ration du numero de la derni�re entit�e cr�e

  ;; union des r�gions
  (setq ss (ssadd))
  (foreach x list_ent (ssadd x ss))
  (if (and ss (> (sslength ss) 1))
    (command "._union" ss "")
  ) ;_ Fin de if
  (if (/= repere (entlast))
    (progn
      (setq ss (ssadd))
      (setq repere (entlast))
      (command "._explode" (entlast) "")
      (setq x (entnext repere))
      (while x
	(if (not (equal (cdr (assoc 0 (entget x))) "SPLINE"))
	  (setq ss (ssadd x ss))
	) ;_ Fin de if
	(setq x (entnext x))
      ) ;_ Fin de while
    ) ;_ Fin de progn
  ) ;_ Fin de if
  (command "pedit" "m" ss "" "o" "j" "" "")
  (setq poly (ssadd (entlast)))
  (vl-cmdf "_move" poly "")
  (setvar "CMDECHO" cmde)
  (setvar "HPBOUND" hp)
  (setvar "AUTOSNAP" snap)
  (princ)
) ;_ Fin de defun