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

(cleanreac)

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

(defun nth_pos (pos liste / lg position)
  (setq lg (length liste))
  (setq position (nth (rem pos lg) liste))
) ;_ Fin de defun

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

(defun Reactor_add_create (object)
  (if (not Reac_mod)
    (progn
      (setq liste     (list object)
	    Reac_open (vlr-object-reactor
			liste
			"Reac open"
			'((:vlr-openedForModify . Open_Reac))
		      ) ;_ Fin de vlr-object-reactor
	    Reac_mod  (vlr-object-reactor
			liste
			"Reac mod"
			'((:vlr-modified . Mod_Reac))
		      ) ;_ Fin de vlr-object-reactor
	    Reac_era  (vlr-object-reactor
			liste
			"Reac era"
			'((:vlr-erased . Era_Reac))
		      ) ;_ Fin de vlr-object-reactor
      ) ;_ Fin de setq
    ) ;_ Fin de progn
    (progn
      (vlr-owner-add Reac_open object)
      (vlr-owner-add Reac_mod object)
      (vlr-owner-add Reac_era object)
    ) ;_ Fin de progn
  ) ;_ Fin de if
) ;_ Fin de defun

(defun Open_Reac (Reacteur_obj Reacteur_Reac Reacteur_Param)
  (princ (entget (vlax-vla-object->ename Reacteur_obj)))
) ;_ Fin de defun

(defun Mod_Reac	(Reacteur_obj Reacteur_Reac Reacteur_Param)
  (setq Reac_obj Reacteur_obj)
) ;_ Fin de defun

(defun Era_Reac	(Reacteur_obj Reacteur_Reac Reacteur_Param)
  (setq Reac_obj Reacteur_obj)
  (vlr-owner-remove Reac_era Reac_obj)
  (vlr-owner-remove Reac_mod Reac_obj)
  (vlr-owner-remove Reac_open Reac_obj)
  (if (<= (length (vlr-owners Reac_era)) 1)
    (progn
      (vlr-remove Reac_open)
      (vlr-remove Reac_mod)
      (vlr-remove Reac_era)
      (setq Reac_era nil
	    Reac_mod nil
	    Reac_open nil
      ) ;_ Fin de setq
    ) ;_ Fin de progn
  ) ;_ Fin de if
) ;_ Fin de defun

(defun Reac_cmdended (Reactor Cmd)
  (cond
    ((equal Cmd '("RECTANGLE"))
     (Reactor_add_create (vlax-ename->vla-object (entlast)))
    )
    ((equal Cmd '("GRIP_STRETCH"))
     (setq List_new_pt (list_pt Reac_obj))
     (mod_sommet List_old_pt List_new_pt Reac_obj)
    )
  ) ;_ Fin de cond
  (setq	Reac_obj 'nil
	List_old_pt
	 'nil
	List_new_pt
	 'nil
  ) ;_ Fin de setq
) ;_ Fin de defun

(setq reac_ended
       (vlr-editor-reactor
	 nil
	 '((:vlr-commandended . Reac_cmdended))
       ) ;_ Fin de vlr-editor-reactor
) ;_ Fin de setq
