;;;=================================================================
;;;
;;; JSD.LSP V2.00
;;;
;;; Jeu de Sélection de ce qui a été créé
;;;
;;; Copyright (C) Patrick_35
;;;
;;;=================================================================

(defun x_  ()
  $$
  )

(defun xx_  (/ sel sel_l)
  (if (and $$ $)
    (progn
      (setq sel	  (ssadd)
	    sel_l (exclusive (list_ss $) (list_ss $$)))
      (mapcar '(lambda (x) (ssadd x sel)) sel_l)
      )
    (setq sel $)
    )
  sel
  )

(defun xxx_  (/ sel sel_l)
  (if (or $ $$)
    (progn
      (setq sel	  (ssadd)
	    sel_l (remove_doubles (append (list_ss $) (list_ss $$))))
      (mapcar '(lambda (x) (ssadd x sel)) sel_l)
      )
    (setq sel $)
    )
  sel
  )

(defun sel_  (/ se)
  (if (setq se (x_))
    (setq ù (if	(ssname se 1)
	      se
	      (ssname se 0)))
    )
  (if (setq se (xx_))
    (setq ùù (if (ssname se 1)
	       se
	       (ssname se 0)))
    )
  (if (setq se (xxx_))
    (setq ùùù (if (ssname se 1)
		se
		(ssname se 0)))
    )
  (princ)
  )

(defun commencer_jeu__selection	 (Rea Cde)
  (setq	$  'nil
	$$ 'nil)
  (cond	((member (car Cde)
		 '("COPY" "MIRROR" "GRIP_MIRROR" "ARRAY" "MOVE"	"ROTATE" "OFFSET" "SCALE" "STRETCH"
		   "LENGTHEN"))
	 (if (and $_ (< (sslength $_) 100))
	   (setq $$ $_)
	   (setq $$ 'nil))
	 (setq $ (ssadd)))
	((member (car Cde)
		 '("LINE"      "CIRCLE"	   "POINT"     "ARC"	   "PLINE"     "RAY"
		   "XLINE"     "MLINE"	   "3DPOLY"    "POLYGON"   "RECTANG"   "DONUT"
		   "SPLINE"    "ELLIPSE"   "REGION"    "MTEXT"	   "DTEXT"     "HELIX"))
	 (setq $$ (ssadd)))
	(t)
	)
  )

(defun pickfirst_jeu__selection	 (Rea Cde)
  (setq $_ (last (ssgetfirst)))
  (princ)
  )

(defun ajouter_jeu_de_selection	 (Rea Obj / se)
  (cond
    ((and $ (< (sslength $) 50)) (ssadd (cadr Obj) $) (sel_))
    ((and $$ (< (sslength $$) 50)) (ssadd (cadr Obj) $$) (sel_))
    )
  (princ)
  )

(defun enlever_jeu_de_selection	 (Rea Obj / se)
  (cond
    ($ (ssdel (cadr Obj) $) (sel_))
    ($$ (ssadd (cadr Obj) $$) (sel_))
    )
  (princ)
  )

(defun faire_jeu_de_selection  ()
  (setq	mrea_jsd  (vlr-command-reactor
		    nil
		    (list (cons :vlr-commandWillStart (function commencer_jeu__selection))
			  )
		    )
	mrea_pick (vlr-miscellaneous-reactor
		    nil
		    (list (cons :vlr-pickfirstModified (function pickfirst_jeu__selection))
			  )
		    )
	mrea_fin  (vlr-acdb-reactor
		    nil
		    (list
		      (cons :vlr-objectAppended (function ajouter_jeu_de_selection))
		      (cons :vlr-objectUnErased (function ajouter_jeu_de_selection))
		      (cons :vlr-objectErased (function enlever_jeu_de_selection))
		      )
		    )
	)
  (princ "\nSélection de ce qui est créé Actif, utilisez !ù sur la ligne de commande")
  (princ)
  )

(vl-load-com)
(if (not mrea_jsd)
  (faire_jeu_de_selection)
  )
(princ)

(defun c:x  (/ sel)
  (setq sel (x_))
  (sssetfirst sel sel)
  (princ)
  )

(defun c:xx  (/ sel)
  (setq sel (xx_))
  (sssetfirst sel sel)
  (princ)
  )

(defun c:xxx  (/ sel)
  (setq sel (xxx_))
  (sssetfirst sel sel)
  (princ)
  )

 ;|«Visual LISP© Format Options»
(100 2 40 0 nil "Fin de " 100 9 0 0 0 T T nil T)
;*** NE PAS AJOUTER de texte au-dessous du commentaire! ***|;
