;;; SURFDEV -Gilles Chanteau- 08/07/06
;;; Crée un bloc (region) représentant le développé d'un maillage 3D ouvert ou
;;; fermé à 2 sommets M (surface réglée).
;;; La précision du développé augmente avec le nombre de mailles N (SURFTAB1)
;;; Après positionnement du bloc (insertion et rotation), il est proposé à
;;; l'utilisateur d'exécuter un miroir pour afficher l'autre face du développé.
;;; Par défaut, le bloc est nommé SurfDev_1, SurfDev_2, en fonction de l'existence de
;;; blocs du même nom dans le dessin. Il est proposé à l'utilisateur de le renommer.
;;;
;;; NOTA : si les surfaces développables sont toujours des surfaces réglées, toutes
;;; les surfaces réglées ne sont pas forcemrent développables. Surfdev n'évaluant
;;; pas la développabilité, créera un "développé erroné" pour celles-ci


;;; SQR retourne le carré du nombre

(defun sqr (num)
  (if (numberp num)
    (* num num)
  )
)

;;; ACOS Retourne l'arc cosinus du nombre

(defun acos (num)
  (if (<= -1 num 1)
    (atan (sqrt (- 1 (sqr num))) num)
  )
)

;;; THIRD_PT Retourne le point qui est à dist1 de pt1 et dist2 de pt2

(defun third_pt	(pt1 pt2 dist1 dist2 /)
  (cond
    ((zerop dist1) pt1)
    ((zerop dist2) pt2)
    (T
     (polar
       pt1
       (+ (angle pt1 pt2)
	  (acos
	    (/ (+ (sqr (distance pt1 pt2)) (sqr dist1) (- (sqr dist2)))
	       (* 2 (distance pt1 pt2) dist1)
	    )
	  )
       )
       dist1
     )
    )
  )
)

;;; Index_name Crée un nom indexé dont l'indice n'est pas présent dans la table spécifiée

(defun index_name (tbl prfx / nom compt)
  (setq	nom   (strcat prfx "1")
	compt 1
  )
  (while (tblsearch tbl nom)
    (setq compt	(1+ compt)
	  nom	(strcat prfx (itoa compt))

    )
  )
  nom
)

;;; Liste des sommets d'une polyligne 3D

(defun 3dpoly_pts (ent / pt pts)
  (while (setq pt (cdr (assoc 10 (entget (entnext ent)))))
    (setq ent (entnext ent)
	  pts (cons pt pts)
    )
  )
  pts
)

;;; Redéfinition de *error*

(defun SurfDev_err (msg)
  (if (= msg "Fonction annulée")
    (princ)
    (princ (strcat "\nErreur: " msg))
  )
  (command "_undo" "_end")
  (command "_u")
  (setq	*error*	m:err
	m:err nil
  )
  (princ)
)

;;; Fonction principale

(defun c:SurfDev
		 (/ osm	cmd del	obj lst	n lst1 lst2 rslt1 rslt2	n1 n2 ss
		  ind new)
  (setq	m:err	*error*
	*error*	SurfDev_err
  )
  (setq	osm (getvar "OSMODE")
	cmd (getvar "CMDECHO")
	del (getvar "DELOBJ")
  )
  (while
    (not
      (setq
	obj (car (entsel "\Sélectionnez la surface à développer: "))
      )
    )
  )
  (if (and (= (cdr (assoc 0 (entget obj))) "POLYLINE")
	   (zerop (logand (cdr (assoc 70 (entget obj))) 207))
	   (< (cdr (assoc 71 (entget obj))) 3)
      )
    (progn
      (command "_undo" "_begin")
      (setq lst	(3dpoly_pts obj)
	    n	0
	    n1	0
	    n2	0
      )
      (repeat (/ (length lst) 2)
	(setq lst1 (cons (nth n lst) lst1)
	      n	   (1+ n)
	)
      )
      (repeat (/ (length lst) 2)
	(setq lst2 (cons (nth n lst) lst2)
	      n	   (1+ n)
	)
      )
      (if (equal (car lst1) (car lst2) 1e-9)
	(setq rslt1 (cons '(0 0 0) rslt1)
	      rslt2 (cons '(0 0 0) rslt2)
	      rslt1 (cons (polar '(0 0 0)
				 (angle (car lst1) (cadr lst1))
				 (distance (car lst1) (cadr lst1))
			  )
			  rslt1
		    )
	      rslt2 (cons (third_pt (car rslt1)
				    '(0 0 0)
				    (distance (cadr lst1) (cadr lst2))
				    (distance (car lst2) (cadr lst2))
			  )
			  rslt2
		    )
	      lst1  (cdr lst1)
	      lst2  (cdr lst2)
	)
	(setq rslt1 (cons '(0 0 0) rslt1)
	      rslt2 (cons (polar '(0 0 0)
				 (angle (car lst1) (car lst2))
				 (distance (car lst1) (car lst2))
			  )
			  rslt2
		    )
	)
      )
      (repeat (1- (length lst1))
	(setq rslt1 (cons (third_pt
			    (car rslt1)
			    (car rslt2)
			    (distance (nth n1 lst1)
				      (nth (setq n1 (1+ n1)) lst1)
			    )
			    (distance (nth n2 lst2) (nth n1 lst1))
			  )
			  rslt1
		    )
	)
	(if (equal (nth n1 lst1) (nth (1+ n2) lst2) 1e-9)
	  (setq	rslt2 (cons (car rslt1) rslt2)
		n2    (1+ n2)
	  )
	  (setq	rslt2
		 (cons (third_pt
			 (car rslt1)
			 (car rslt2)
			 (distance (nth n1 lst1) (nth (1+ n2) lst2))
			 (distance (nth n2 lst2)
				   (nth (setq n2 (1+ n2)) lst2)
			 )
		       )
		       rslt2
		 )
	  )
	)
      )
      (setvar "CMDECHO" 0)
      (setvar "OSMODE" 0)
      (setvar "DELOBJ" 1)
      (setq ss (ssadd))
      (foreach l (list rslt1 rslt2)
	(if (not
	      (vl-every '(lambda (pt) (equal pt (car l) 1e-9)) l)
	    )
	  (progn
	    (command "_.spline")
	    (mapcar 'command l)
	    (command "" "" "")
	    (ssadd (entlast) ss)
	  )
	)
      )
      (foreach fun (list 'car 'last)
	(if (not (equal	(apply fun (list rslt1))
			(apply fun (list rslt2))
			1e-9
		 )
	    )
	  (progn
	    (command "_.line"
		     (apply fun (list rslt1))
		     (apply fun (list rslt2))
		     ""
	    )
	    (ssadd (entlast) ss)
	  )
	)
      )
      (command "_.region" ss "")
      (command "_.chprop" (entlast) "" "_layer" "0" "")
      (setq ind (index_name "BLOCK" "SurfDev_"))
      (command "_.block" ind '(0 0 0) (entlast) "")
      (setvar "OSMODE" osm)
      (command "_.insert" ind "_scale" 1)
      (princ "\nSpécifiez le point d'insertion: ")
      (command pause)
      (princ (strcat "\nSpécifiez l'angle de rotation <"
		     (angtos 0)
		     ">: "
	     )
      )
      (command pause)
      (setq bloc (entlast))
      (initget "Oui Non")
      (if (= "Oui"
	     (getkword "\nEffectuer un miroir ? [Oui/Non] < Non >: ")
	  )
	(progn
	  (command "_mirror"
		   bloc
		   ""
		   (setq pt (cdr (assoc 10 (entget bloc))))
		   "_non"
		   (polar pt 0.0 1)
		   "_yes"
		   "_.explode"
		   bloc
		   "_.block"
		   ind
		   "_yes"
		   pt
		   "_previous"
		   ""
	  )
	  (command "_.insert" ind "_scale" 1)
	  (princ "\nSpécifiez le point d'insertion: ")
	  (command pause)
	  (princ (strcat "\nSpécifiez l'angle de rotation <"
			 (angtos 0)
			 ">: "
		 )
	  )
	  (command pause)
	)
      )
      (initget "Oui Non")
      (if (= "Oui"
	     (getkword (strcat "\nRenommer le bloc \""
			       ind
			       "\" ? [Oui/Non] < Non >: "
		       )
	     )
	  )
	(progn
	  (initget 1)
	  (setq new (getstring "\Entrez le nouveau nom: "))
	  (command "_.rename" "_block" ind new)
	)
      )
      (command "_.undo" "_end")
      (setvar "CMDECHO" cmd)
      (setvar "DELOBJ" del)
    )
    (princ
      "\nL'objet sélectionné n'est pas une surface développable."
    )
  )
  (setq	*error*	m:err
	m:err nil
  )
  (princ)
)