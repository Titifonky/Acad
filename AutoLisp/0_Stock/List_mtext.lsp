(defun c:rpm ()

  (gc)

  (setq	table_1	'(
		  (0 . "ACAD_TABLE")
		  (100 . "AcDbEntity")
		  (67 . 0)
		  (410 . "Model")
		  (100 . "AcDbBlockReference")
		  (10 0.0 0.0 0.0)
		  (41 . 1.0)
		  (42 . 1.0)
		  (43 . 1.0)
		  (50 . 0.0)
		  (70 . 0)
		  (71 . 0)
		  (44 . 0.0)
		  (45 . 0.0)
		  (210 0.0 0.0 1.0)
		  (100 . "AcDbTable")
		  (11 1.0 0.0 0.0)
		  (91 . 2)
		  (92 . 4)
		  (93 . 0)
		  (94 . 0)
		  (95 . 0)
		  (96 . 0)
		 )
	table_2	'(
		  (141 . 10.0)
		  (141 . 10.0)
		  (142 . 60.0)
		  (142 . 60.0)
		  (142 . 60.0)
		  (142 . 60.0)
		  (171 . 1)
		  (172 . 0)
		  (173 . 0)
		  (174 . 0)
		  (175 . 4)
		  (176 . 1)
		  (91 . 0)
		  (178 . 0)
		  (145 . 0.0)
		  (92 . 0)
		  (301 . "CELL_VALUE")
		  (93 . 6)
		  (90 . 4)
		  (1 . "Nomenclature")
		  (304 . "ACVALUE_END")
		  (171 . 1)
		  (173 . 1)
		  (175 . 1)
		  (176 . 1)
		  (91 . 0)
		  (178 . 0)
		  (145 . 0.0)
		  (92 . 0)
		  (301 . "CELL_VALUE")
		  (93 . 3)
		  (90 . 512)
		  (94 . 0)
		  (300 . "")
		  (302 . "")
		  (304 . "ACVALUE_END")
		  (171 . 1)
		  (173 . 1)
		  (175 . 1)
		  (176 . 1)
		  (91 . 0)
		  (178 . 0)
		  (145 . 0.0)
		  (92 . 0)
		  (301 . "CELL_VALUE")
		  (93 . 3)
		  (90 . 512)
		  (94 . 0)
		  (300 . "")
		  (302 . "")
		  (304 . "ACVALUE_END")
		  (171 . 1)
		  (173 . 1)
		  (175 . 1)
		  (176 . 1)
		  (91 . 0)
		  (178 . 0)
		  (145 . 0.0)
		  (92 . 0)
		  (301 . "CELL_VALUE")
		  (93 . 3)
		  (90 . 512)
		  (94 . 0)
		  (300 . "")
		  (302 . "")
		  (304 . "ACVALUE_END")
		  (171 . 1)
		  (301 . "CELL_VALUE")
		  (93 . 6)
		  (90 . 4)
		  (1 . "Référence")
		  (304 . "ACVALUE_END")
		  (171 . 1)
		  (301 . "CELL_VALUE")
		  (93 . 6)
		  (90 . 4)
		  (1 . "Désignation")
		  (304 . "ACVALUE_END")
		  (171 . 1)
		  (301 . "CELL_VALUE")
		  (93 . 6)
		  (90 . 4)
		  (1 . "Nb")
		  (304 . "ACVALUE_END")
		  (171 . 1)
		  (301 . "CELL_VALUE")
		  (93 . 6)
		  (90 . 4)
		  (1 . "Masse (kg)")
		  (304 . "ACVALUE_END")
		 )

	range	'((171 . 1)
		  (172 . 0)
		  (173 . 0)
		  (174 . 0)
		  (175 . 1)
		  (176 . 1)
		  (91 . 0)
		  (178 . 0)
		  (145 . 0.0)
		  (92 . 0)
		  (301 . "CELL_VALUE")
		  (93 . 6)
		  (90 . 4)
		  (1 . "")
		  (304 . "ACVALUE_END")
		 )
  )

					; Désactivation des variables système

  (setq	cmde (getvar "cmdecho")
	acc  (getvar "osmode")
  )

  (setvar "cmdecho" 0)
  (setvar "osmode" 0)
  (princ "\n----- Référencement de texte multiple -----")

					; Selection des objets

  (princ
    "\nSelectionnez les textes multiples à référencer :"
  )
  (setq	al_set (ssget '((-4 . "<OR")
			(0 . "MTEXT")
			(-4 . "OR>")
		       )
	       )
  )

  (if (not al_set)
    (progn (princ "\nVous n'avez rien selectionné\n")
	   (setvar "osmode" acc)
	   (exit)
    )
  )

					; redefini le point d'insertion du tableau

  (terpri)

  (setq	table_1
	 (subst	(cons 10
		      (getpoint "Indiquez le point d'insertion du tableau:")
		)
		'(10 0.0 0.0 0.0)
		table_1
	 )
  )

					; recuperation des index des textmult dans la selection al_set

					; initialisation des variables

  (setq	fin	   (sslength al_set)
	list_index (list
		     (substr (cdr (assoc '1 (entget (ssname al_set 0)))) 5)
		   )
	i	   1
  )

					; Algo

  (while (< i fin)
    (setq list_index
	   (append
	     list_index
	     (list
	       (substr (cdr (assoc '1 (entget (ssname al_set i))))
		       5
	       )
	     )
	   )
    )
    (setq i (1+ i))
  )

					; suppression des doublons dans la liste list_index

					; initialisation des variables

  (setq	list_index
	 (vl-sort list_index '<)
	list_tri '()
  )

					; Algo

  (foreach index list_index
    (if	(not (member index list_tri))
      (setq list_tri (append list_tri (list index)))
    )
  )

					; Modifie le nombre d'entrées dans la table

  (setq
    table_1 (subst (cons 91 (+ 2 (length list_tri)))
		   '(91 . 2)
		   table_1
	    )
  )

					; Modifie la hauteur des lignes dans le tableau

  (setq list_range '((141 . 10.0)))

  (repeat (length list_tri)
    (setq list_range (cons '(141 . 10.0) list_range))
  )

  (setq table_1 (append table_1 list_range))

  (setq	table	    (append table_1 table_2)
	table_suite nil
  )


					; compte le nombre d'occurence occ dans la liste liste_n


  (foreach index list_tri

					; initialisation des variables pour la fonction "compte"

    (setq nb	 0
	  list_n list_index
	  occ	 index
    )

    (compte)

    (princ nb)
    (terpri)

    (setq table_suite
	   (append table_suite (subst (cons 1 index) '(1 . "") range))
    )
    (setq table_suite (append table_suite range))
    (setq
      table_suite
       (append table_suite
	       (subst (cons 1 (itoa nb)) '(1 . "") range)
       )
    )
    (setq table_suite (append table_suite range))
  )


  (entmake (append table table_suite))

					; Remise à jour des variables système

  (setvar "osmode" acc)
  (setvar "cmdecho" cmde)
  (princ)
)

					; Compteur recurssif

(defun compte ()
  (if (car list_n)
    (progn
      (setq test   (car list_n)
	    list_n (cdr list_n)
      )
      (if (= test occ)
	(setq nb (1+ nb))
      )
    )
  )
  (if list_n
    (compte)
  )
)

(princ "\nListe le Texte d'une selection - tapez RPM - par Etienne Canuel")
