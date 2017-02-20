;;; question or ideas to my program
;;; contact me cadplayer@gmail.com!

(defun c:swpd (/ i			; increment
	     pd				; area from hatch
	     pds			; 
	     pdttl			; total area result
	     densite)
  (setvar "CMDECHO" 0)
  (setq	pd 0
	pds 0
	pdttl 0
	densite	7.85
	lsthach	nil
  )
  (prompt (strcat "\nCalcul du poids des pieces" "\nSelectionnez la polyligne et son texte ! "))
  (while (/= (setq ss (ssget '((-4 . "<OR") (0 . "*POLYLINE") (0 . "MTEXT") (-4 . "OR>")))) nil)
    (if	(>= (sslength ss) 2)
      (progn (setq lst	 (_sel ss)
		   nb	 (sw_nb (car lst))
		   ep	 (sw_ep (car lst))
		   pd	 (/ (* (_areaOfObject (cdr lst)) ep densite) 1000000.0)
		   pds	 (* nb pd)
		   pdttl (+ pdttl pds)
	     )
	     (command "_hatch" "SOLID" "_s" (cdr lst) "")
	     (setq lsthach (append lsthach (list (entlast))))
	     (princ (strcat "\nPoids des pieces : "
			    (itoa nb)
			    " × "
			    (rtos pd 2 2)
			    " kg = "
			    (rtos pds 2 2)
			    " kg\nPoids total : "
			    (rtos pdttl 2 2)
			    " kg"
		    )
	     )
      )
    )
  )
  (mapcar 'entdel lsthach)
  (princ)
)

;;; calculate area of object
(defun _areaOfObject (en / curve area)
  (setq curve (vlax-ename->vla-object en))
  (if (vl-catch-all-error-p (setq area (vl-catch-all-apply 'vlax-curve-getArea (list curve))))
    nil
    area
  )
  (progn (command "._area" "_O" en) (getvar "area"))
)

;;; _selection (texte . polyligne)
(defun _sel (ss /)
  (if (= (cdr (assoc 0 (entget (ssname ss 0)))) "MTEXT")
    (cons (ssname ss 0) (ssname ss 1))
    (cons (ssname ss 1) (ssname ss 0))
  )
)

(defun sw_ep (ent / e txt pos ep rech)
  (setq	e    (entget ent)
	rech "(ep"
  )
  (if (= (cdr (assoc 0 e)) "MTEXT")
    (setq txt (vl-string-translate "," "." (strcase (cdr (assoc 1 e)) t))
	  pos (vl-string-search rech txt)
	  ep  (atof (substr txt (+ 1 pos (strlen rech))))
    )
    0
  )
)

 ;|«Visual LISP© Format Options»
(100 2 40 2 nil "Fin de " 100 9 0 0 0 T nil nil T)
;*** NE PAS AJOUTER de texte au-dessous du commentaire! ***|;
