(defun c:sel (/ ent lay_name typ_ent stat_lay)
  (while (null (setq ent (entsel "\nChoix de l'entité: "))))
  (setq	lay_name (cdr (assoc 8 (entget (car ent))))
	typ_ent	 (cdr (assoc 0 (entget (car ent))))
  ) ;_ Fin de setq
  (if (eq typ_ent "INSERT")
    (alert
      "\nATTENTION l'entité sélectionnée appartient à un Bloc ou XRef, calque d'insertion sélectionné."
    ) ;_ Fin de alert
  ) ;_ Fin de if
  (setq stat_lay (tblsearch "LAYER" lay_name))
  (if (eq (boole 1 (cdr (assoc 70 stat_lay)) 4) 4)
    (alert (strcat "\nLe calque: " lay_name " est verrouillé!"))
    (progn
      (princ
	(strcat	"\nL'entité sélectionnée appartient au calque: "
		lay_name
	) ;_ Fin de strcat
      ) ;_ Fin de princ
      (princ
	"\nChoix des objet par capture, fenêtre, trajet ou individuel."
      ) ;_ Fin de princ
      (sssetfirst nil (ssget (list (cons 8 lay_name))))
;;;      (command "_.properties")
    ) ;_ Fin de progn
  ) ;_ Fin de if
  (prin1)
) ;_ Fin de defun
