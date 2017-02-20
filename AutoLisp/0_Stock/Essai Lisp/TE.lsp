(defun c:te ()
  (setvar "PDMODE" 3)

  (setq poly (entget (setq selpoly (car (nentsel)))))
  (command "liste" selpoly "")
  (setq longueurtempo (getvar "perimeter"))
  (setq listept nil)

  (setq derent (entlast))

  (command "DIVISER" derent 5)

  (setq ent (entlast))
  (while (not (equal ent derent))
					;et là tu fais ta sauce
    (entdel (entlast))
    (setq ent (entlast))
    (setq listept (append listept (cdr (assoc 10 (entget ent)))))
  )					;fin du while
)