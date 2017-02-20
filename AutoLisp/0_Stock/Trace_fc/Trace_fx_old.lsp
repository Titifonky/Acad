(defun c:tf ()
  (vl-load-com)
  (setq	cmde (getvar "cmdecho")
	acc  (getvar "osmode")
	flat (getvar "flatland")
  )
  (setvar "cmdecho" 0)
  (setvar "osmode" 0)
  (setvar "flatland" 0)
  
  (command "_undo" "_m")

  (setq depart (getreal "\nDepart de l'intervalle :"))
  (setq fin (getreal "\nFin de l'intervalle :"))
  (setq nb (getint "\nNombre de pas :"))
  (setq expression (getstring "\nEquation à tracer :"))
  (setq echelle (getreal "\nRapport d'echelle :"))
  (setq pitch (/ (- fin depart) (float nb)))
  (setq x depart)

  (command "polylign")

  (while (<= x fin)
    (command
      (list
	(* x echelle)
	(c:cal expression)
      )
    )
    (princ x)
    (setq x (+ x pitch))
  )

  (command "")

  (setvar "cmdecho" cmde)
  (setvar "flatland" flat)
  (setvar "osmode" acc)
  (princ)
)