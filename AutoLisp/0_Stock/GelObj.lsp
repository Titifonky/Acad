;; GELOBJ (version 2.1) 09/11/2007
;; Pour geler des calques spécifiés uniquement dans l'onglet objet
;; Merci à Patrick_35 pour vlr-miscellaneous-reactor

(defun c:go	(/ lst tog)

  (vl-load-com)

  (or *acdoc* (setq *acdoc* (vla-get-activeDocument (vlax-get-acad-object))))
  (or *layers* (setq *layers* (vla-get-layers *acdoc*)))

  (if (= (getvar "TILEMODE") 1)
    (progn
      (setq tog (vlax-ldata-get "GelerOngletObjet" "toggled"))
      (or *GelerOngletObjet*
	  (setq	*GelerOngletObjet*
		 (vlr-miscellaneous-reactor
		   nil
		   '((:vlr-layoutSwitched . gelobjrea))
		 )
	  )
      )
      (vlax-for	l *layers*
	(if (= (vla-get-Freeze l) :vlax-true)
	  (setq lst (cons (vla-get-Name l) lst))
	)
      )
      (foreach l (exclusive (mapcar 'car tog) lst)
	(setq tog (vl-remove (assoc l tog) tog))
      )
      (vlax-ldata-put
	"GelerOngletObjet"
	"disable"
	(cons (getvar "CLAYER") (exclusive lst (mapcar 'car tog)))
      )
      (setq lst tog)
      (setq lst
	     (mapcar
	       (function
		 (lambda (x)
		   (cond
		     ((assoc x tog))
		     (T (cons x (vla-get-freeze (vla-item *layers* x))))
		   )
		 )
	       )
	       (getlayers
		 "Geler dans l'onglet objet"
		 (mapcar 'car tog)
		 (vlax-ldata-get "GelerOngletObjet" "disable")
	       )
	     )
      )
      (foreach l (exclusive tog lst)
	(vla-put-Freeze (vla-item *layers* (car l)) (cdr l))
      )
      (vlax-ldata-put "GelerOngletObjet" "toggled" lst)
      (if lst
	(foreach l lst
	  (vla-put-Freeze (vla-item *layers* (car l)) :vlax-true)
	)
	(progn
	  (vlr-remove *GelerOngletObjet*)
	  (setq *GelerOngletObjet* nil)
	)
      )
      (vla-regen *acdoc* acAllViewports)
    )
    (alert
      "Vous devez être dans l'onglet objet pour lancer GELOBJ."
    )
  )
  (princ)
)

;;=========================================================;;

(defun gelobjrea (rea lay / false true tog)
  (setq tog (vlax-ldata-get "GelerOngletObjet" "toggled"))
  (vlax-for l *layers*
       (if (= (vla-get-Freeze l) :vlax-false)
	 (setq false (cons (vla-get-Name l) false))
	 (setq true (cons (vla-get-Name l) true))
       )
     )
  (cond
    ((= (car lay) "Model")
     (foreach l	(append
		  (exclusive true (mapcar 'car tog))
		  (common true (mapcar 'car tog))
		  )
       (if (assoc l tog)
	 (vlax-ldata-put
	   "GelerOngletObjet"
	   "toggled"
	   (vl-remove (assoc l tog) tog)
	 )
       )
     )
     (vlax-ldata-put
       "GelerOngletObjet"
       "disable"
       (cons (getvar "CLAYER") (exclusive true (mapcar 'car tog)))
     )
     (foreach l	tog
       (if (= (getvar "CLAYER") (car l))
	 (vlax-ldata-put
	   "GelerOngletObjet"
	   "toggled"
	   (vl-remove l tog)
	 )
	 (vla-put-Freeze (vla-item *layers* (car l)) :vlax-true)
       )
     )
    )
    (T
     (foreach l	(common (mapcar 'car tog) false)
       (vlax-ldata-put
	 "GelerOngletObjet"
	 "toggled"
	 (vl-remove (assoc l tog) tog)
       )
     )
     (foreach l	tog
       (vla-put-Freeze (vla-item *layers* (car l)) (cdr l))
     )
    )
  )
  (vla-regen *acdoc* acAllViewports)
)

;;=========================================================;;

;;; EXCLUSIVE
;;; Retourne une liste contenant les éléments appartenant exclusivement à l1
;;; (exclusive '(1 2 3 4) '( 2 3 4 5)) -> (1)

(defun exclusive (l1 l2)
  (if l1
    (if	(member (car l1) l2)
      (exclusive (cdr l1) l2)
      (cons (car l1) (exclusive (cdr l1) l2))
    )
  )
)

;;=========================================================;;

;;; COMMON Retourne la liste des éléments communs à lst1 et lst2
;;; (COMMON '(1 2 3 4) '( 2 3 4 5)) -> (2 3 4)
(defun common (l1 l2)
  (if l1
    (if	(member (car l1) l2)
      (cons (car l1) (common (cdr l1) l2))
      (common (cdr l1) l2)
    )
  )
)

;;=========================================================;;

;;; SUBLIST Retourne une sous-liste
;;;
;;; Arguments
;;; lst : une liste
;;; start : l'index de départ de la sous liste (premier élément = 0)
;;; leng : la longueur (nombre d'éléments) de la sous-liste (ou nil)
;;;
;;; Exemples :
;;; (sublist '(1 2 3 4 5 6) 2 2) -> (3 4)
;;; (sublist '(1 2 3 4 5 6) 2 nil) -> (3 4 5 6)

(defun sublist (lst start leng / n r)
  (if (or (not leng) (< (- (length lst) start) leng))
    (setq leng (- (length lst) start))
  )
  (setq n (+ start leng))
  (repeat leng
    (setq r (cons (nth (setq n (1- n)) lst) r))
  )
)

;;=========================================================;;

;; GETLAYERS (gile) 02/12/07
;; Retourne la liste des calques cochés dans la boite de dialogue
;;
;; arguments
;; titre : le titre de la boite de dialogue ou nil (defaut = Choisir les calques)
;; lst1 : la liste des calques à pré-cochés ou nil
;; lst2 : la liste des calques non cochables (grisés) ou nil

(defun getlayers (titre	lst1 lst2 / toggle_column tmp file lay layers len dcl_id)

  (defun toggle_column (lst)
    (apply 'strcat
	   (mapcar
	     (function
	       (lambda (x)
		 (strcat ":toggle{key="
			 (vl-prin1-to-string x)
			 ";label="
			 (vl-prin1-to-string x)
			 ";}"
		 )
	       )
	     )
	     lst
	   )
    )
  )

  (setq	tmp  (vl-filename-mktemp "tmp.dcl")
	file (open tmp "w")
  )
  (while (setq lay (tblnext "LAYER" (not lay)))
    (setq layers (cons (cdr (assoc 2 lay)) layers))
  )
  (setq	layers (vl-sort layers '<)
	len    (length layers)
  )
  (write-line
    (strcat
      "GetLayers:dialog{label="
      (cond (titre (vl-prin1-to-string titre))
	    ("\"Choisir les calques\"")
      )
      ";:boxed_row{:column{"
      (cond
	((< len 12) (toggle_column layers))
	((< len 24)
	 (strcat (toggle_column (sublist layers 0 (/ len 2)))
		 "}:column{"
		 (toggle_column (sublist layers (/ len 2) nil))
	 )
	)
	((< len 45)
	 (strcat (toggle_column (sublist layers 0 (/ len 3)))
		 "}:column{"
		 (toggle_column (sublist layers (/ len 3) (/ len 3)))
		 "}:column{"
		 (toggle_column (sublist layers (* (/ len 3) 2) nil))
	 )
	)
	(T
	 (strcat (toggle_column (sublist layers 0 (/ len 4)))
		 "}:column{"
		 (toggle_column (sublist layers (/ len 4) (/ len 4)))
		 "}:column{"
		 (toggle_column (sublist layers (/ len 2) (/ len 4)))
		 "}:column{"
		 (toggle_column (sublist layers (* (/ len 4) 3) nil))
	 )
	)
      )
      "}}spacer;ok_cancel;}"
    )
    file
  )
  (close file)
  (setq dcl_id (load_dialog tmp))
  (if (not (new_dialog "GetLayers" dcl_id))
    (exit)
  )
  (foreach n lst1
    (set_tile n "1")
  )
  (foreach n lst2
    (mode_tile n 1)
  )
  (action_tile
    "accept"
    "(setq lst nil)
    (foreach n layers
    (if (= (get_tile n) \"1\")
    (setq lst (cons n lst))))
    (done_dialog)"
  )
  (start_dialog)
  (unload_dialog dcl_id)
  (vl-file-delete tmp)
  lst
)

;;=========================================================;;

;; Chargement au démarrage (acaddoc.lsp ou MNL)

(and
  (not *GelerOngletObjet*)
  (vlax-ldata-get "GelerOngletObjet" "toggled")
  (setq	*GelerOngletObjet*
	 (vlr-miscellaneous-reactor
	   nil
	   '((:vlr-layoutSwitched . gelobjrea))
	 )
  )
)