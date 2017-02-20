(defun CleanReac ()
  (mapcar 'vlr-remove-all
	  '(:VLR-AcDb-reactor
	    :VLR-Editor-reactor
	    :VLR-Linker-reactor
	    :VLR-Object-reactor
	   )
  ) ;_ Fin de mapcar
) ;_ Fin de defun

(cleanreac)

					;Fonction de comparaison de liste
					;Elle renvoie une liste contenant la position des atome qui ne sont pas égaux

(defun compare_pos (list1 list2 / liste)
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

					;Fonction de recuperation de points
					;Elle renvoie une liste contenant les points de l'objet donné

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

					;Fonction de renvoi de la position d'un atome dans une liste
					;Elle renvoie la position

(defun nth_pos (pos liste / lg position)
  (setq lg (length liste))
  (setq position (nth (rem pos lg) liste))
) ;_ Fin de defun



(defun mod_sommet (list_pt_old list_pt_new object)
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

					;Fonction de modification de l'objet rectangle

(defun mod_rectangle (object)
  (princ (vlr-owners ModReactor))
  (if (vlax-property-available-p object 'coordinates)
    (setq list_pt_new (list_pt object))
    (exit)
  ) ;_ Fin de if
  (mod_sommet list_pt_new list_pt_old object)
  (if (vlax-property-available-p object 'coordinates)
    (setq list_pt_old (list_pt object))
  ) ;_ Fin de if
  (setq param 0)
) ;_ Fin de defun

(defun Rectangle (reactor cmd)
  (cond
    ((equal (car cmd) "RECTANGLE")
     (progn
       (setq last_ent (vlax-ename->vla-object (entlast)))
       (if (vlax-property-available-p last_ent 'coordinates)
	 (setq list_pt_old (list_pt last_ent))
       ) ;_ Fin de if
       (setq ModReactor
			(vlr-object-reactor
			  (list last_ent)
			  "Mod Reactor"
			  '((:vlr-modified . Mod_Rec))
			) ;_ Fin de vlr-object-reactor
	     EraReactor
			(vlr-object-reactor
			  (list last_ent)
			  "Era Reactor"
			  '((:vlr-erased . Era_Rec))
			) ;_ Fin de vlr-object-reactor
       ) ;_ Fin de setq
     ) ;_ Fin de progn
    )
    ((and (equal (car cmd) "GRIP_STRETCH") (equal param 1))
     (mod_rectangle object)
    )
    (t
     (if (vlax-property-available-p object 'coordinates)
       (setq list_pt_old (list_pt object))
     ) ;_ Fin de if
    )
  ) ;_ Fin de cond
  (princ)
) ;_ Fin de defun

					;Réacteur de modification de l'objet rectangle

(defun Mod_Rec (obj reactor pmt)
  (setq i 0)
  (setq object obj)
  (setq param 1)
  (princ)
) ;_ Fin de defun

(defun Era_Rec (obj reactor pmt)
  (vlr-remove reactor)
) ;_ Fin de defun

					;Réacteur d'edition

(vlr-editor-reactor
  nil
  '((:vlr-commandended . Rectangle))
) ;_ Fin de vlr-editor-reactor

					;GRIP_STRETCH