(defun c:pagp ()
  (setq dictgroup (dictsearch (namedobjdict) "acad_group"))
  (setq	dictgroup (mapcar 'cdr
			  (vl-remove-if-not
			    '(lambda (x) (= 350 (car x)))
			    dictgroup
			  ) ;_ Fin de vl-remove-if-not
		  ) ;_ Fin de mapcar
  ) ;_ Fin de setq
  (initget "Oui Non")
  (setq	reponse
	 (getkword
	   (strcat
	     "\nIl y a "
	     (itoa (length dictgroup))
	     " groupes dans le dessin, voulez vous les supprimer ? [Oui/Non] :"
	   ) ;_ Fin de strcat
	 ) ;_ Fin de getkword
  ) ;_ Fin de setq
  (if (equal reponse "Non")
    (exit)
  ) ;_ Fin de if
  (setq	i 0
  ) ;_ Fin de setq
  (repeat (length dictgroup)
    (entdel (nth i dictgroup))
    (setq i (1+ i))
  ) ;_ Fin de while
  (princ (strcat "\n" (itoa i) " groupes ont été suprimés."))
  (princ)
) ;_ Fin de defun

(prompt "Tapez PAGP pour lancer la commande")
