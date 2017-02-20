(princ
  "\n================================================================")
(princ
  "\n|          Exporter un tableau vers un fichier csv             |")
(princ
  "\n|          par Etienne Canuel                                  |")
(princ
  "\n|          Tapez exp_tab pour lancer la routine                |")
(princ
  "\n================================================================")

;;;(vl-load-com)

(defun c:exp_tab  (/ selection ename lst_x lst_y xmin xmax ymin	ymax lg_lst_x lg_lst_y lst_text	tableau	ligne fichier flag)
  (princ "Exporter un tableau vers excel\n")
  ;; Selection des objets et renvoi une liste des noms des entités
  (setq	lst_x	  '()
	lst_y	  '()
	lst_text  '()
	tab	  '()
	ligne	  '()
	selection (ss->lst
		    (ssget
		      '((-4 . "<OR") (0 . "LINE") (0 . "TEXT") (-4 . "OR>")))))

  (if (not selection)
    (progn (princ "Aucun objet selectionné") (quit)))

  ;; Tri les entités suivant TEXT ou LINE
  (foreach ename  selection
    (setq edata	(entget ename)
	  edata	(list (cdr (assoc '0 edata))
		      (cdr (assoc '1 edata))
		      (rdc (cdr (assoc '11 edata)))
		      (rdc (cdr (assoc '10 edata)))))

    (cond

;;; Si TEXT : recuperer l'index et la position du point d'insertion
      ((= (car edata) "TEXT")
       (setq lst_text (cons (rdc (cdr edata)) lst_text)))

;;; Si LINE : regarde si la ligne est verticale ou horizontale
;;; et met la valeur x dans lst_x et la valeur y dans lst_y
      ((= (car edata) "LINE")
       ;; si x1=x2 renvoi x sinon renvoi 0
       (setq edata (mapcar '(lambda (x y)
			      (if (= x y)
				(list y)))
			   (nth 2 edata)
			   (nth 3 edata))
	     lst_x (append (car edata) lst_x)
	     lst_y (append (last edata) lst_y)))
      (t)))


;;; Tri les listes
  (setq	lst_x	 (tri lst_x '<)
	lst_y	 (tri lst_y '>)
	xmax (apply 'max lst_x)
	xmin (apply 'min lst_x)
	ymax (apply 'max lst_y) 
	ymin (apply 'min lst_y)
	lg_lst_x (1- (length lst_x))
	lg_lst_y (1- (length lst_y))
	globale	 lst_text
	lst_text (tri lst_text
		      '(lambda (e1 e2) (< (caadr e1) (caadr e2))))
	lst_text (tri lst_text
		      '(lambda (e1 e2) (> (cadadr e1) (cadadr e2))))
	)

  
  

;;; Creer un tableau de liste
  (setq temp_x lst_x)
  (repeat lg_lst_y
    (setq ymax	(car lst_y)
	  ymin	(cadr lst_y)
	  lst_y	(cdr lst_y))
    (repeat lg_lst_x
      (setq xmax  (cadr temp_x)
	    xmin  (car temp_x)
	    temp_x (cdr temp_x)
	    text  (caar lst_text)
	    point (cadar lst_text))

      (if (and (and (< (car point) xmax)
		    (>= (car point) xmin))
	       (and (< (cadr point) ymax)
		    (>= (cadr point) ymin)))

	(progn (setq ligne (cons text ligne))
	       (setq lst_text (cdr lst_text)))

	(setq ligne (cons "" ligne))
	)
      )
    (setq tableau (append (list (reverse ligne)) tableau)
	  ligne	  '()
	  temp_x  lst_x)
    )

;;; Converti le tableau en liste
  (setq tableau (lst->csv (reverse tableau)))

;;; Enregistre le tableau dans un fichier
  (setq	fichier	(getfiled "Emplacement du fichier"
			  (strcat "Tableau")
			  "csv"
			  1))
  (if (setq flag (open fichier "w"))
    (progn (write-line tableau flag) (close flag)))

  (princ))



;;;================================      Fonctions     ==================================================

(defun remove-if  (fun from)
  (cond	((atom from) from)		;nil or symbol (return that)
	((apply fun (list (car from))) (remove-if fun (cdr from)))
	(t (cons (car from) (remove-if fun (cdr from))))))


;;; Converti une liste de liste en tableau csv
(defun lst->csv	 (lst / ligne txt)
  (setq	txt (apply 'strcat
		   (mapcar '(lambda (x) (strcat x "\n"))
			   (mapcar '(lambda (y)
				      (apply 'strcat
					     (mapcar '(lambda (z) (strcat z ";")) y))
				      )
				   lst)))
	txt (substr txt 1 (- (strlen txt) 1)))
  )

;;; Fonction inverse de cdr : renvoi tout les élément d'une liste sauf le dernier
(defun rdc  (lst /)
  (if (listp lst)
    (reverse (cdr (reverse lst)))))

;;; Prend un jeu de selection et renvoi une liste d'ename

(defun ss->lst	(sset / i bcl ss nb lst)
  (setq	i   0
	lst '())
  (if sset
    (progn (repeat (sslength sset)
	     (setq lst (cons (ssname sset i) lst)
		   i   (1+ i))
	     )
	   (setq lst (reverse lst))
	   )
    'nil))

;;; Fonction tri

(defun tri  (lst func / lg inv flag inv cle t1 t2 lst_tri)
  (setq	lst_tri	'()
	flag t)
  (if (>= (setq lg (length lst)) 2)
    (while flag
      (repeat (1- lg)
	(setq t1   (car lst)
	      t2   (cadr lst)
	      inv  (apply func (list t1 t2))
	      cle  (apply func (list t2 t1))
	      ;; permet d'eviter les boucles dans le cas de valeurs égales
	      inv  (or inv (not cle))
	      ;; determine si une inversion a été faite pendant la boucle repeat
	      flag (and flag inv))

	(if inv
	  (setq	lst_tri	(cons t1 lst_tri)
		lst	(cdr lst))
	  (setq	lst_tri	(cons t2 lst_tri)
		lst	(cons t1 (cddr lst)))
	  )
	)
      (setq lst	    (reverse (cons (car lst) lst_tri))
	    lst_tri '()
	    flag    (not flag))
      ))
  lst
  )


 ;|«Visual LISP© Format Options»
(200 2 40 0 nil "Fin de " 60 9 0 0 0 T T nil T)
;*** NE PAS AJOUTER de texte au-dessous du commentaire! ***|;
