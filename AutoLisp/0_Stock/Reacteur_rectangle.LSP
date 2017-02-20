;;;Fonction de suppression de tous les réacteurs présents dans le dessin
;;;à utiliser s'il y a un problème
(defun CleanReac ()
  (setq Reac_mod nil)
  (mapcar 'vlr-remove-all
	  '(:VLR-AcDb-reactor
	    :VLR-Editor-reactor
	    :VLR-Linker-reactor
	    :VLR-Object-reactor
	   )
  ) ;_ Fin de mapcar
) ;_ Fin de defun

;;;Fonction de création du rectangle et initialisation du pointeur de reacteur

(defun c:rcr ()
  (command "RECTANGLE")
  (setq flag_addreactor t)
  (princ " Rectangle parametrique")
  (princ)
) ;_ Fin de defun

;;;Fonction d'extraction des sommets du rectangle

(defun list_pt (object / liste)
  (setq	liste (vlax-safearray->list
		(vlax-variant-value (vla-get-coordinates object))
	      ) ;_ Fin de vlax-safearray->list
	liste (list (list (nth 0 liste) (nth 1 liste))
		    (list (nth 2 liste) (nth 3 liste))
		    (list (nth 4 liste) (nth 5 liste))
		    (list (nth 6 liste) (nth 7 liste))
	      ) ;_ Fin de list
  ) ;_ Fin de setq
) ;_ Fin de defun

;;;Fonction de comparaison de listes
;;;Elle ppermet de determiner le sommet déplacé

(defun compare_pos (list1 list2 / liste i)
  (setq liste '(nil))
  (setq i 0)
  (while (not (or (null list1) (null list2)))
    (if	(not (equal (car list1) (car list2)))
      (progn
	(setq liste (append liste (list i)))
	(setq list1 (cdr list1)
	      list2 (cdr list2)
	      i	    (1+ i)
	) ;_ Fin de setq
      ) ;_ Fin de progn
      (setq list1 (cdr list1)
	    list2 (cdr list2)
	    i	  (1+ i)
      ) ;_ Fin de setq
    ) ;_ Fin de if
  ) ;_ Fin de while
  (setq liste (cdr liste))
) ;_ Fin de defun

;;;Fonction un peu plus poussé de "nth"
;;;qui repart du début de la liste si l'indicateur
;;;de position est plus grand que la longueur de la liste

(defun nth_pos (pos liste / lg position)
  (setq lg (length liste))
  (setq position (nth (rem pos lg) liste))
) ;_ Fin de defun

;;;Fonction de modification des sommets

(defun mod_sommet (list_pt_old	       list_pt_new	   object
		   /	     pt_a      pt_b	 pt_c	   pt_d
		   pt_b_new  pt_d_new  pos	 ptv
		  )
  (if (setq pos (car (compare_pos list_pt_old list_pt_new)))
    (progn
      (setq pt_a (nth_pos (+ pos 2) list_pt_new)
	    pt_b (nth_pos (+ pos 3) list_pt_new)
	    pt_c (nth_pos pos list_pt_new)
	    pt_d (nth_pos (1+ pos) list_pt_new)
      ) ;_ Fin de setq
      (setq
	pt_b_new (inters
		   pt_a
		   pt_b
		   pt_c
		   (list (+ (car pt_c) (- (car pt_d) (car pt_a)))
			 (+ (last pt_c) (- (last pt_d) (last pt_a)))
		   ) ;_ Fin de list
		   nil
		 ) ;_ Fin de inters
	pt_d_new (inters
		   pt_a
		   pt_d
		   pt_c
		   (list (+ (car pt_c) (- (car pt_b) (car pt_a)))
			 (+ (last pt_c) (- (last pt_b) (last pt_a)))
		   ) ;_ Fin de list
		   nil
		 ) ;_ Fin de inters
	ptv	 (vlax-make-safearray vlax-vbdouble '(0 . 1))
      ) ;_ Fin de setq
      (vla-put-coordinate
	object
	(rem (+ pos 3) (length list_pt_new))
	(vlax-safearray-fill
	  ptv
	  (list (car pt_b_new) (cadr pt_b_new))
	) ;_ Fin de vlax-safearray-fill
      ) ;_ Fin de vla-put-coordinate
      (vla-put-coordinate
	object
	(rem (1+ pos) (length list_pt_new))
	(vlax-safearray-fill
	  ptv
	  (list (car pt_d_new) (cadr pt_d_new))
	) ;_ Fin de vlax-safearray-fill
      ) ;_ Fin de vla-put-coordinate
    ) ;_ Fin de progn
  ) ;_ Fin de if
) ;_ Fin de defun

;;;Fonction de création du réacteur de l'objet rectangle

(defun Reactor_add_create (object)
  (vlr-object-reactor
    (list object)
    (list_pt object)
    '((:vlr-copied . Cop_Reac)
      (:vlr-modified . Mod_Reac)
      (:vlr-erased . Era_Reac)
     )
  ) ;_ Fin de vlr-object-reactor
) ;_ Fin de defun

;;;Fonction associé au réacteur de modification

(defun Cop_Reac	(Reacteur_Obj Reacteur_Reac Reacteur_Param)
  (setq	Reac_Obj   Reacteur_Obj
	Reac_Reac  Reacteur_Reac
	Reac_Param Reacteur_Param
  ) ;_ Fin de setq
) ;_ Fin de defun

(defun Mod_Reac	(Reacteur_Obj Reacteur_Reac Reacteur_Param)
  (setq	Reac_Obj  Reacteur_Obj
	Reac_Reac Reacteur_Reac
  ) ;_ Fin de setq
) ;_ Fin de defun

;;;Fonction associé au réacteur de suppression de l'objet

(defun Era_Reac	(Reacteur_Obj Reacteur_Reac Reacteur_Param)
  (vlr-remove Reacteur_Reac)
) ;_ Fin de defun

;;;Fonction associé au réacteur de fin de commande
;;;Elle lance la création du reacteur d'objet associé au rectangle
;;;Elle permet de lancer le reacteur de modification des sommets
;;;par la commande "GRIP_STRETCH"
;;;Si l'objet subit une modification, les coordonnées de ses sommets sont mis à jour.
;;;Dans le cas ou l'objet ne contient plus quatre sommets, le reacteur est supprimé

(defun Reac_cmdended (Reactor Cmd)
  (cond
    ((equal Cmd '("RECTANGLE"))
     (if flag_addreactor
       (progn
	 (Reactor_add_create (vlax-ename->vla-object (entlast)))
	 (setq flag_addreactor nil)
       ) ;_ Fin de progn
     ) ;_ Fin de if
    )
    ((and (equal Cmd '("GRIP_STRETCH"))
	  (not (equal Reac_Obj 'nil))
     ) ;_ Fin de and
     (progn
       (setq List_old_pt (vlr-data Reac_Reac)
	     List_new_pt (list_pt Reac_Obj)
       ) ;_ Fin de setq
       (mod_sommet List_old_pt List_new_pt Reac_Obj)
       (vlr-data-set
	 Reac_Reac
	 (setq List_new_pt (list_pt Reac_Obj))
       ) ;_ Fin de vlr-data-set
       (if (and	(not (equal Mod_link_Obj nil))
		(not (equal Mod_link_Reac nil))
	   ) ;_ Fin de and
	 (progn
	   (setq list_object
		  (vl-remove Mod_link_Obj
			     (vlr-owners Mod_link_Reac)
		  ) ;_ Fin de vl-remove
	   ) ;_ Fin de setq
	   (setq
	     vecteur (mapcar '(lambda (x y)
				(mapcar '(lambda (a b) (- b a)) x y)
			      ) ;_ Fin de lambda
			     List_old_pt
			     List_new_pt
		     ) ;_ Fin de mapcar
	   ) ;_ Fin de setq
	   (foreach ent	list_object
	     (setq List_old_pt (list_pt ent)
		   List_new_pt (mapcar
				 '(lambda (x y)
				    (mapcar '(lambda (a b) (+ a b)) x y)
				  ) ;_ Fin de lambda
				 List_old_pt
				 vecteur
			       ) ;_ Fin de mapcar
	     ) ;_ Fin de setq
	     (mod_sommet List_old_pt List_new_pt ent)
	   ) ;_ Fin de foreach
	   (setq Mod_link_Obj	'nil
		 Mod_link_Reac	'nil
		 Mod_link_Param	'nil
	   ) ;_ Fin de setq
	 ) ;_ Fin de progn
       ) ;_ Fin de if
       (setq Reac_Obj 'nil
	     Reac_Reac 'nil
       ) ;_ Fin de setq
     ) ;_ Fin de progn
    )
    ((and (equal Cmd '("COPY"))
	  (not (equal Reac_Obj 'nil))
     ) ;_ Fin de and
     (Reactor_add_create
       (vlax-ename->vla-object (car Reac_Param))
     ) ;_ Fin de Reactor_add_create
    )
    ((and (not (equal Cmd '("ERASE")))
	  (not (equal Reac_Obj 'nil))
     ) ;_ Fin de and
     (if (> (1+	(vlax-safearray-get-u-bound
		  (vlax-variant-value (vla-get-coordinates Reac_Obj))
		  1
		) ;_ Fin de vlax-safearray-get-u-bound
	    ) ;_ Fin de 1+
	    8
	 ) ;_ Fin de >
       (vlr-remove Reac_Reac)
       (progn
	 (vlr-data-set Reac_Reac (list_pt Reac_Obj))
	 (setq Reac_Obj	'nil
	       Reac_Reac 'nil
	       Reac_Param
		'nil
	 ) ;_ Fin de setq
       ) ;_ Fin de progn
     ) ;_ Fin de if
    )
  ) ;_ Fin de cond
  (setq	Reac_obj 'nil
	List_old_pt
	 'nil
	List_new_pt
	 'nil
  ) ;_ Fin de setq
) ;_ Fin de defun

;;;Reacteur d'edition commandended

(setq reac_ended
       (vlr-editor-reactor
	 nil
	 '((:vlr-commandended . Reac_cmdended))
       ) ;_ Fin de vlr-editor-reactor
) ;_ Fin de setq

(princ "\n")

(prompt
  "::::::::::::  Tapez \"RCR\" pour dessiner un rectangle paramétrique  :::::::::::"
) ;_ Fin de prompt

(princ)