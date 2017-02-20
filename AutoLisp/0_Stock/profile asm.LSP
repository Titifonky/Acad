(defun c:un ()
  (setq	cmde (getvar "CMDECHO")
	hp   (getvar "HPBOUND")
  ) ;_ Fin de setq
  (setvar "CMDECHO" 0)
  (setvar "HPBOUND" 0)
  (setq	ss_reg (ssadd)
	ss_ent (ssget '((-4 . "<OR")
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
  (if (not ss_ent)
    (quit)
    (setq lg-ss (sslength ss_ent))
  ) ;_ Fin de if
  (setq rep1 (entlast))
  (command "_REGION" ss_ent "")
  (setq rep2 (entlast))
  (repeat lg-ss
    (ssadd (setq entity (entnext rep1)) ss_reg)
    (setq rep1 entity)
  ) ;_ Fin de while
  (if (and ss_reg (> (sslength ss_reg) 1))
    (command "._union" ss_reg "")
  ) ;_ Fin de if
  (setq rep1 (entlast))
  (if (/= rep1 rep2)
    (progn
      (command "._explode" rep1 "")
      (setq ss_ent (ssadd (setq rep2 (entlast))))
      (while (not (equal (setq entity (entnext rep1)) rep2))
	(ssadd entity ss_ent)
	(setq rep1 entity)
      ) ;_ Fin de while
    ) ;_ Fin de progn
    (command "_erase" ss_reg)
  ) ;_ Fin de if
  (command "pedit" "m" ss_ent "" "o" "j" "" "")
  (setvar "CMDECHO" cmde)
  (setvar "HPBOUND" hp)
  (princ)
) ;_ Fin de defun