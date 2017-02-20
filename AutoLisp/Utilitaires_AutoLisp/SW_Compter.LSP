;;; Compter le nb de pièces
(defun c:swttl (/ Sel i txt s tt)
;;; on selectionne tout les textes du calque QUANTITE
  (setq	Sel 'nil
	Sel (ssget "_X" '((0 . "MTEXT") (8 . "QUANTITE") (410 . "Model")))
	i   0
	tt  0
  )
;;; On fait le total
  (if Sel
    (repeat (sslength Sel)
      (setq txt	(ssname Sel i)
	    i	(1+ i)
	    tt	(+ tt (sw_nb txt))
      )
    )
  )
  (princ (strcat "\nNb total de pièces : " (itoa tt)))
  (princ)
)

(defun sw_nb (ent / e)
  (setq e (entget ent))
  (if (= (cdr (assoc 0 e)) "MTEXT")
    (atoi (cdr (assoc 1 e)))
    0
  )
)
 ;|«Visual LISP© Format Options»
(100 2 40 2 nil "Fin de " 100 9 0 0 0 T nil nil T)
;*** NE PAS AJOUTER de texte au-dessous du commentaire! ***|;
