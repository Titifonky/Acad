(defun c:O () (setvar "CLAYER" "0"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun c:ct () (setvar "CLAYER" "01-Construction"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun nomlayer
       (name_lay flag / lst_lay first_lay next_lay lgmax lg cc)
  (setq	first_lay (tblnext "LAYER" T)
	lst_lay	  (list	(if (zerop (cdr (assoc 70 first_lay)))
			  (cdr (assoc 2 first_lay))
			  nil
			) ;_ Fin de if
		  ) ;_ Fin de list
  ) ;_ Fin de setq
  (while (setq next_lay (tblnext "LAYER" nil))
    (if	(zerop (cdr (assoc 70 next_lay)))
      (setq lst_lay (cons (cdr (assoc 2 next_lay)) lst_lay))
    ) ;_ Fin de if
  ) ;_ Fin de while
  (setq lst_lay (reverse lst_lay))
  (setq	calque	lst_lay
	lst_lay	(mapcar '(lambda (x) (strcat " " x " ")) lst_lay)
  ) ;_ Fin de setq
  (setq lgmax 0)
  (foreach n lst_lay
    (setq lg (strlen n))
    (if	(> lg lgmax)
      (setq lgmax lg)
    ) ;_ Fin de if
  ) ;_ Fin de foreach
  (setq	lgmax	(1+ lgmax)
	lst_lay	(mapcar	'(lambda (x)
			   (setq lg (- lgmax (strlen x)))
			   (repeat lg (setq x (strcat " " x)))
			 ) ;_ Fin de lambda
			lst_lay
		) ;_ Fin de mapcar
  ) ;_ Fin de setq
  (setq i 1)
  (repeat (strlen name_lay)
    (if	(= (ascii (substr name_lay i 1)) 32)
      (setq name_lay (strcat (substr name_lay 1 (1- i))
			     "*"
			     (substr name_lay (1+ i))
		     ) ;_ Fin de strcat
      ) ;_ Fin de setq
    ) ;_ Fin de if
    (setq i (1+ i))
  ) ;_ Fin de repeat
  (setq	name_lay (strcat name_lay "*")
	lg	 (- lgmax (strlen name_lay))
  ) ;_ Fin de setq
  (repeat lg (setq name_lay (strcat "*" name_lay)))
  (setq	i 0
	listcalque
	 '(nil)
  ) ;_ Fin de setq
  (while (< i (length lst_lay))
    (if	(wcmatch (strcase (nth i lst_lay)) name_lay)
      (setq listcalque (append listcalque (list i)))
    ) ;_ Fin de if
    (setq i (1+ i))
  ) ;_ Fin de while
  (setq listcalque (cdr listcalque))
  (if (zerop flag)
    (changelayer listcalque)
    (setq nomcalque (nth (car listcalque) calque))
  ) ;_ Fin de if
) ;_ Fin de defun

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun changelayer (listcalque / liste)
  (if (<= (length listcalque) 1)
    (progn (setq cc (nth (car listcalque) calque))
	   (setvar "CLAYER" cc)
	   (princ (strcat "\nLe calque "
			  cc
			  " est maintenant le calque courant"
		  ) ;_ Fin de strcat
	   ) ;_ Fin de princ
    ) ;_ Fin de progn
    (progn (foreach n '("users1" "users2" "users3" "users4" "users5")
	     (setvar n "")
	   ) ;_ Fin de foreach
	   (if (<= (length listcalque) 5)
	     (setq long (length listcalque))
	     (setq long 5)
	   ) ;_ Fin de if
	   (setq i 1)
	   (repeat long
	     (setvar (strcat "users" (itoa i))
		     (nth (nth (1- i) listcalque) calque)
	     ) ;_ Fin de setvar
	     (setq i (1+ i))
	   ) ;_ Fin de repeat
	   (menucmd "POP25=PERSO")
	   (menucmd "POP25=*")
    ) ;_ Fin de progn
  ) ;_ Fin de if
  (princ)
) ;_ Fin de defun

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun c:y ()
  (setq	name_lay
	 (strcase
	   (getstring
	     T
	     "\nEntrez le nom de calque recherché (joker * accepté): "
	   ) ;_ Fin de getstring
	 ) ;_ Fin de strcase
  ) ;_ Fin de setq
  (setq flag 0)
  (nomlayer name_lay flag)
) ;_ Fin de defun