(defun c:su ()
  (setq	cmde (getvar "CMDECHO")
	hp   (getvar "HPBOUND")
  ) ;_ Fin de setq
  (setvar "CMDECHO" 0)
  (setvar "HPBOUND" 0)
  (princ "\nSelectionnez les entitées de base")
  (setq	ss_regbase (ssadd)
	ss_regsous (ssadd)
	ss_entbase
		   (ssget '((-4 . "<OR")
			    (-4 . "<AND")
			    (0 . "LWPOLYLINE")
			    (70 . 1)
			    (-4 . "AND>")
			    (-4 . "<AND")
			    (0 . "LWPOLYLINE")
			    (70 . 129)
			    (-4 . "AND>")
			    (0 . "CIRCLE")
			    (-4 . "OR>")
			   )
		   ) ;_ Fin de ssget
  ) ;_ Fin de setq
  (princ "\nSelectionnez les entitées à soustraire")
  (setq
    ss_entsous
     (ssget '((-4 . "<OR")
	      (-4 . "<AND")
	      (0 . "LWPOLYLINE")
	      (70 . 1)
	      (-4 . "AND>")
	      (-4 . "<AND")
	      (0 . "LWPOLYLINE")
	      (70 . 129)
	      (-4 . "AND>")
	      (0 . "CIRCLE")
	      (-4 . "OR>")
	     )
     ) ;_ Fin de ssget
  ) ;_ Fin de setq
  (if (or (not ss_entbase) (not ss_entsous))
    (quit)
    (progn
      (setq lg-ssbase (sslength ss_entbase))
      (setq lg-sssous (sslength ss_entsous))
    ) ;_ Fin de progn
  ) ;_ Fin de if
  (setq rep1 (entlast))
  (command "_REGION" ss_entbase "")
  (setq rep2 (entlast))
  (repeat lg-ssbase
    (ssadd (setq entity (entnext rep1)) ss_regbase)
    (setq rep1 entity)
  ) ;_ Fin de while
  (setq rep1 (entlast))
  (command "_REGION" ss_entsous "")
  (setq rep2 (entlast))
  (repeat lg-sssous
    (ssadd (setq entity (entnext rep1)) ss_regsous)
    (setq rep1 entity)
  ) ;_ Fin de while
  (command "_SUBTRACT" ss_regbase "" ss_regsous "")
  (setq rep1 (entlast))
  (command "._explode" rep1 "")
  (setq ss_ent (ssadd (setq rep2 (entlast))))
  (while (not (equal (setq entity (entnext rep1)) rep2))
    (ssadd entity ss_ent)
    (setq rep1 entity)
  ) ;_ Fin de while
  (command "pedit" "m" ss_ent "" "o" "j" "" "")
  (setvar "CMDECHO" cmde)
  (setvar "HPBOUND" hp)
  (princ)
) ;_ Fin de defun