(vl-load-com)

					;Sw_NettoyerDXF

(defun Sw_NettoyerDXF (/)

;;;;;; On supprime la liste des pièces soudées
;;;  (setq	Sel (ssget "_X" '((2 . "SW_TABLEANNOTATION_0") (410 . "Model")))
;;;	i   0
;;;  )
;;;
;;;  (if sel
;;;    (repeat (sslength Sel)
;;;      (entdel (ssname Sel i))
;;;      (setq i (1+ i))
;;;    )
;;;  )

;;; On crée les calques suivant
  (command "_-layer"	   "n"		   "NOTE DE PLIAGE"
	   "n"		   "LIGNES DE PLIAGE"
	   ""
	  )

;;; Selection des annotations de pliage
  (setq	Sel 'nil
	Sel (ssget "_X" '((1 . "*PLI*") (410 . "Model")))
	i   0
  )

;;; On les change de calque
  (if Sel
    (repeat (sslength Sel)
      (setq lst	(entget (ssname Sel i))
	    lst	(subst (cons 8 "NOTE DE PLIAGE") (assoc 8 lst) lst)
	    lst	(subst (cons 62 256) (assoc 62 lst) lst)
      )
      (entmod lst)
      (setq i (1+ i))
    )
  )

;;; Selection des lignes de pliage
  (setq	Sel 'nil
	Sel (ssget "_X"
		   '((410 . "Model")
		     (-4 . "<AND")
		     (6 . "SLD*")
		     (-4 . "AND>")
		     (-4 . "<NOT")
		     (6 . "SLD-Solid")
		     (-4 . "NOT>")
		    )
	    )
	i   0
  )

;;; On recommence, changement de calque
  (if Sel
    (repeat (sslength Sel)
      (setq lst	(entget (ssname Sel i))
	    lst	(subst (cons 8 "LIGNES DE PLIAGE") (assoc 8 lst) lst)
	    lst	(subst (cons 62 256) (assoc 62 lst) lst)
      )
      (entmod lst)
      (setq i (1+ i))
    )
  )

;;; On selectionne les descriptions des corps sous forme de bloc annonyme
  (setq	Sel 'nil
	Sel (ssget "_X"
		   '((0 . "INSERT") (8 . "0") (2 . "`*U*") (410 . "Model"))
	    )
	i   0
  )

;;; On explose tout ca
  (if Sel
    (repeat (sslength Sel)
      (setq blk	(ssname Sel i)
	    i	(1+ i)
      )
      (command "_EXPLODE" blk "")
    )
  )

;;; On renregistre la dernière entite cree
;;; et on selectionne tout les bloc du calque QUANTITE
  (setq	Entite (entlast)
	Sel    (ssget "_X"
		      '((0 . "INSERT") (8 . "QUANTITE") (410 . "Model"))
	       )
	i      0
  )

;;; On les explose
  (if Sel
    (repeat (sslength Sel)
      (setq blk	(ssname Sel i)
	    i	(1+ i)
      )
      (command "_EXPLODE" blk)
    )
  )

;;; On boucle sur les dernieres entites explosees
;;; , on les change de calque et de type de ligne
  (setq Entite (entnext Entite))
  (while Entite
    (setq ed (entget Entite)
	  ed (subst '(8 . "QUANTITE") (assoc 8 ed) ed)
	  ed (subst '(62 . 256) (assoc 62 ed) ed)
    )
    (entmod ed)
    (setq Entite (entnext Entite))
  )

;;; On reselectionne
  (setq	Entite (entlast)
	Sel    'nil
	Sel    (ssget "_X"
		      '((0 . "INSERT") (8 . "QUANTITE") (410 . "Model"))
	       )
	i      0
  )

;;; On reexplose
  (if Sel
    (repeat (sslength Sel)
      (setq blk	(ssname Sel i)
	    i	(1+ i)
      )
      (command "_EXPLODE" blk)
    )
  )

;;; On reselectionne
;;; et on fusionne les textes
  (setq	Sel 'nil
	Sel (ssget "_X"
		   '((0 . "TEXT") (8 . "QUANTITE") (410 . "Model"))
	    )
  )
  (if Sel
    (progn
      (setq Sel	(vl-remove-if 'listp (mapcar 'cadr (ssnamex Sel)))
	    Sel	(vl-sort Sel
			 (function (lambda (ent1 ent2)
				     (setq pt1 (cdr (assoc '10 (entget ent1)))
					   pt2 (cdr (assoc '10 (entget ent2)))
				     )
				     (if (< (cadr pt1) (cadr pt2))
				       'T
				       (if (< (car pt1) (car pt2))
					 'T
					 'nil
				       )
				     )
				   )
			 )
		)
	    i	-1
      )
      (if (> (length Sel) 1)
	(while (< i (1- (length Sel)))
	  (setq	ss (ssadd)
		i  (1+ i)
		ss (ssadd (nth i Sel) ss)
		i  (1+ i)
		ss (ssadd (nth i Sel) ss)
	  )
	  (command "TXT2MTXT" ss "")
	)
      )

      (setq Sel	'nil
	    Sel	(ssget "_X"
		       '((0 . "MTEXT") (8 . "QUANTITE") (410 . "Model"))
		)
	    i	0
      )

      (if Sel
	(repeat	(sslength Sel)
	  (setq	ed  (entget (ssname Sel i))
		val (cdr (assoc '41 ed))
		ed  (subst (cons '41 (+ val 30)) (assoc '41 ed) ed)
		i   (1+ i)
	  )
	  (entmod ed)

	)
      )


    )
  )

;;; On rechange de calque
  (setq Entite (entnext Entite))
  (while Entite
    (setq ed (entget Entite)
	  ed (subst '(8 . "QUANTITE") (assoc 8 ed) ed)
	  ed (subst '(62 . 256) (assoc 62 ed) ed)
    )
    (entmod ed)
    (setq Entite (entnext Entite))
  )

  (setq Sel 'nil)

;;; On selectionne tout les blocs et on explose tout
  (setq	Entite (entlast)
	Sel    (ssget "_X" '((0 . "INSERT") (410 . "Model")))
	i      0
  )

  SW_TABLEANNOTATION_1

  (if Sel
    (repeat (sslength Sel)
      (setq blk	(ssname Sel i)
	    i	(1+ i)
      )
      (if
	(not
	  (wcmatch (cdr (assoc 2 (entget blk))) "SW_TABLEANNOTATION*")
	)
	 (command "_EXPLODE" blk)
      )
    )
  )

;;; On convertis les splines en polyligne compose d'arcs
  (setvar "PLINECONVERTMODE" 1)

  (setq	Sel (ssget "_X" '((0 . "SPLINE") (410 . "Model")))
	i   0
  )

  (if sel
    (repeat (sslength Sel)
      (setq spl (ssname Sel i))
      (command "_splinedit" spl "_p" 1)
      (setq i (1+ i))
    )
  )

;;; On change la couleur des calques
  (setq	ListeCalques
	 ((lambda (/ l r)
	    (acad_strlsort
	      (while (setq l (tblnext "layer" (not l)))
		(setq r (cons (cdr (assoc 2 l)) r))
	      )
	    )
	  )
	 )
  )
  (setq	Couleurs '(("NOTE DE PLIAGE" . 126)
		   ("LIGNES DE PLIAGE" . 252)
		   ("COTATION" . 14)
		   ("ANNOTATIONS" . 36)
		  )
  )

  (foreach c Listecalques
    (if	(setq a (assoc c Couleurs))
      (command "_-layer" "ch" (car a) "co" (cdr a) "" "")
    )
  )

  (setvar "CLAYER" "GRAVURE")

  ;;; on selectionne tout les textes du calque GRAVURE
  (setq	Sel 'nil
	Sel (ssget "_X"
		   '((0 . "MTEXT") (8 . "GRAVURE") (410 . "Model"))
	    )
	i      0
  )

;;; On les converti en ligne
  (if Sel
    (repeat (sslength Sel)
      (setq txt	(ssname Sel i)
	    i	(1+ i)
      )
      (sssetfirst nil nil)
      (command "zoom" "_o" txt "")
      (sssetfirst nil (ssadd txt))
      (c:txtexp)
    )
  )

;;; on nettoie
  (command "_zoom" "et")
  (command "_regenall")
  (setvar "MIRRTEXT" 0)
  (command "_-PURGE" "_all" "" "_no")
  (command "_UCSICON" "no")
  (command "_QSAVE")
  (setq Sel 'nil)

  (command "_change"
	   (ssget "_X" '((410 . "Model")))
	   ""
	   "_p"
	   "_co"
	   "duc"
	   ""
  )

  (command "_change"
	   (ssget "_X" '((410 . "Model")))
	   ""
	   "_p"
	   "ep"
	   "duc"
	   ""
  )

  (princ)
)

(defun c:sw (/)
  (Sw_NettoyerDXF)
)

;;; Supprimer les gravures
(defun c:swsg (/ Sel i)

;;; On supprime les textes à graver
  (setq	Sel (ssget "_X" '((8 . "GRAVURE") (410 . "Model")))
	i   0
  )

  (if sel
    (repeat (sslength Sel)
      (entdel (ssname Sel i))
      (setq i (1+ i))
    )
  )

  (princ)
)